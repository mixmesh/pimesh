%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_key_serv).

-export([start_link/0, start_link/2]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-export([is_locked/1]).

-export([hw_reset/0]).

-define(INT_PIN,  17).   %% gpio on raspberry pi
-define(RESET_PIN, 27).  %% gpio on raspberry pi

%% 2-colored led (red-green-yellow)
-define(LED_RED,   {row,6}).  %% gpio on tca8418
-define(LED_GREEN, {row,7}).  %% gpio on tca8418

-define(LED_COM,   {row,4}).  %% communication ectivity
-define(LED_APP,   {row,5}).  %% application status

%% default lock code keys
-define(KEY_Asterisk, 31).
-define(KEY_Number,   33).

-define(BLINK_ON_TMO, 500).
-define(BLINK_OFF_TMO, 1000).

%% max time in ms between keys. (fixme)
-define(KEY_WAIT_TIME, 5000).
%% max time for pin code entry in ms (fixme)
-define(PINCODE_WAIT_TIME, 20000).
%% min back-off time in ms, for failed input attempt
-define(BACK_OF_TIME_1, 3).     %% first wait 3s
-define(BACK_OF_TIME_2, 4).     %% first wait 4s
-define(BACK_OF_TIME_3, 5).     %% first wait 5s
-define(BACK_OF_TIME_4, 10).    %% first wait 10s
-define(BACK_OF_TIME_5, 60).    %% first wait 1m
-define(BACK_OF_TIME_6, 3400).  %% first wait 1h
-define(BACK_OF_TIME_7, 86400).  %% max wait one day

-define(PWM_PERIOD, 1000000).   %% 1KHz

-record(level,
	{
	 pin,
	 steady,
	 charge_low,
	 charge_high
	}).

-define(BAT_LED_5, {col,7}).  %% green (fully charged)
-define(BAT_LED_4, {col,6}).  %% blue
-define(BAT_LED_3, {col,5}).  %% blue
-define(BAT_LED_2, {col,4}).  %% blue
-define(BAT_LED_1, {col,3}).  %% blue

%%           B1  B2  B3  B4  G
%% (0-5)     0   0   0   0   0
%% (5-10)    x   0   0   0   0
%% (10-30)   1   0   0   0   0
%% (30-40)   1   x   0   0   0
%% (40-60)   1   1   0   0   0
%% (60-70)   1   1   x   0   0
%% (70-80)   1   1   1   0   0
%% (80-90)   1   1   1   x   0
%% (90-100)  1   1   1   1   0
%% (80-90)   1   1   1   x   0
%% (95-100)  1   1   1   1   1

%% {Pin, Steady, Charge-low, Charge-high}
-define(LEVEL_LIST,
	[#level{pin=?BAT_LED_1, steady=20,  charge_low=15, charge_high=20},
	 #level{pin=?BAT_LED_2, steady=40,  charge_low=30, charge_high=40},
	 #level{pin=?BAT_LED_3, steady=60,  charge_low=50, charge_high=60},
	 #level{pin=?BAT_LED_4, steady=80,  charge_low=70, charge_high=80},
	 #level{pin=?BAT_LED_5, steady=95,  charge_low=90, charge_high=95}]).

-record(state,
	{
	 parent,
	 tca8418,
	 locked = true,
	 %% pincode = "123456",    %% digest? 
	 pincode = "4567",
	 pincode_len = 4,       %% needed for digest!? and missing enter key
	 pincode_enter_key,     %% accept without enter key
	 %% pincode_enter_key = ?KEY_Number,  %% must enter with '#' after code
	 prev_key,  %% keep last key PRESSED! clear on release
	 pincode_lock_key1 = ?KEY_Asterisk,
	 pincode_lock_key2 = ?KEY_Number,
	 toggle = false,
	 blink_tmo = ?BLINK_OFF_TMO,	 
	 attempts = 0,
	 backoff = false,
	 count = 0,   %% number of keys total since start of attempts
	 code = [],   %% entered code (fixme digest)
	 %% battery status
	 soc = 0,
	 charging = false,
	 charging_set = false,
	 activity = false,      %% last activity
	 pwm = 0.0              %% last pwm value (0-100)
	}).

is_locked(Serv) ->
    serv:call(Serv, is_locked).

start_link() ->
    start_link(1,false).

start_link(Bus,Reset) ->
    application:start(tree_db),
    application:start(xbus),
    application:start(i2c),
    application:start(gpio),
    application:start(pwm),
    ?spawn_server(fun(Parent) -> init(Parent, Bus, Reset) end,
		  fun ?MODULE:message_handler/1).

init(Parent, Bus, Reset) ->
    {ok,TCA8418} = i2c_tca8418:open1(Bus),
    %% ok = i2c_tca8418:open(Bus),
    %% TCA8418 = Bus,
    gpio:init(?INT_PIN),
    gpio:init(?RESET_PIN),

    lists:foreach(
      fun(#level{pin=Pin}) ->
	      i2c_tca8418:gpio_init(TCA8418, Pin)
      end, ?LEVEL_LIST),
    i2c_tca8418:gpio_init(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_init(TCA8418, ?LED_RED),
    i2c_tca8418:gpio_init(TCA8418, ?LED_COM),
    i2c_tca8418:gpio_init(TCA8418, ?LED_APP),

    pwm:export(0, 0),  %% export PWM0 (pin 12, GPIO_18)

    %% sleep since gpio:init is async, fixme somehow...
    %% we could use low-level api but we still need the 
    %% to poll on the gpioxyz/value file for interrupts...
    timer:sleep(1000), 

    %% now we should be able to use GPIO & PWM
    lists:foreach(
      fun(#level{pin=Pin}) ->
	      i2c_tca8418:gpio_output(TCA8418, Pin),
	      i2c_tca8418:gpio_clr(TCA8418, Pin)
      end, ?LEVEL_LIST),

    i2c_tca8418:gpio_output(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_output(TCA8418, ?LED_RED),
    i2c_tca8418:gpio_output(TCA8418, ?LED_COM),
    i2c_tca8418:gpio_output(TCA8418, ?LED_APP),
    
    set_led(TCA8418, off),
    set_com(TCA8418, false),
    set_app(TCA8418, false),

    pwm:set_period(0, 0, ?PWM_PERIOD),
    pwm:set_duty_cycle(0, 0, trunc(?PWM_PERIOD*0.5)),
    pwm:enable(0,0),

    gpio:set_direction(?INT_PIN, in),
    gpio:set_interrupt(?INT_PIN, falling),

    gpio:set_direction(?RESET_PIN, high),

    if Reset -> hw_reset();
       true -> ok
    end,

    configure(TCA8418, {4,3}),

    Events = i2c_tca8418:read_events(TCA8418),
    State0 = #state { parent=Parent, tca8418=TCA8418, pwm = 0.5 },
    State  = scan_events(Events, State0),

    xbus:sub(<<"mixmesh.*">>),

    {ok, State}.

%% reset tca8418 - reset all registers
hw_reset() ->
    timer:sleep(10),
    gpio:clr(?RESET_PIN),
    timer:sleep(10),
    gpio:set(?RESET_PIN),
    timer:sleep(10).
    
configure(I2C,{3,3}) ->
    i2c_tca8418:configure_3x3(I2C);
configure(I2C,{4,3}) ->
    i2c_tca8418:configure_4x3(I2C).

message_handler(State=#state{tca8418=TCA8418,parent=Parent}) ->
    BlinkTmo = if State#state.backoff ->
		       infinity;
		  true ->
		       State#state.blink_tmo
	       end,
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, is_locked} ->
            {reply, From, State#state.locked, State};

        {gpio_interrupt, 0, ?INT_PIN, _Value} ->
	    io:format("pin ~w, value=~w\n", [?INT_PIN,_Value]),
	    Events = i2c_tca8418:read_events(TCA8418),
	    State1 = if State#state.backoff ->
			     State;
			true ->
			     scan_events(Events, State)
		     end,
	    {noreply, State1};

	{xbus, <<"mixmesh.battery.soc">>, #{ value := SOC }} ->
            Charging = State#state.charging,
            Set = State#state.charging_set,
            update_soc(State#state.tca8418, SOC, Charging, Set),
            State1 = State#state { soc = SOC, charging_set = not Set },
	    {norepy, State1};

        {xbus, <<"mixmesh.battery.charging">>, #{ value := Charging }} ->
            State1 = State#state { charging = Charging },
	    {noreply, State1};

	{xbus, <<"mixmesh.node.activity">>, #{ value := Activity }} ->
	    set_com(State#state.tca8418, Activity),
            State1 = State#state{ activity = Activity },
            {noreply, State1};

        {xbus, <<"mixmesh.keypad.pwm">>, #{ value := PWM }} ->
	    Duty = trunc(?PWM_PERIOD*(PWM/100)),
	    pwm:set_duty_cycle(0, 0, Duty),
	    if Duty =:= 0 ->  %%  range? < 5?
		    pwm:disable(0, 0);
	       true ->
		    pwm:enable(0, 0)
	    end,
            {noreply, State#state { pwm = PWM }};

	{xbus, _, _} ->  %% ignore new xbus mixmesh.* messages not handled
	    {noreply, State};

	{timeout,_TRef,backoff} ->  %% backoff period is over
	    {noreply, State#state { backoff = false }};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after BlinkTmo ->
	    Toggle = not State#state.toggle,
	    Tmo = 
		if Toggle ->
			set_led(TCA8418, yellow),
			?BLINK_ON_TMO;
		   true ->
			set_led(TCA8418, off),
			?BLINK_OFF_TMO
		end,
	    {noreply, State#state { toggle = Toggle, blink_tmo = Tmo }}
    end.
%% either 
scan_events([{press,Key}|Es],State) when 
      not State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2
      ; %% or keys the other order
      not State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key2,
      Key =:= State#state.pincode_lock_key1 ->
    %% Lock device
    set_led(State#state.tca8418, off),
    State1 = State#state { locked   = true,
			   backoff  = false,
			   attempts = 0,
			   count    = 0,
			   code     = [],
			   prev_key = undefined,
			   toggle   = false,
			   blink_tmo = ?BLINK_OFF_TMO },
    scan_events(Es, State1);
scan_events([{press,Key}|Es],State) when 
      State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2
      ;
      State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key2,
      Key =:= State#state.pincode_lock_key1 ->
    %% device is already locked, just reset code?
    scan_events(Es, State#state { prev_key = undefined });
scan_events([{press,Key}|Es], State) ->
    io:format("PRESS ~s\n", [[i2c_tca8418:keycode_to_sym(Key)]]),
    if State#state.locked ->
	    set_led(State#state.tca8418, green);
       true ->
	    ok
    end,
    State1 = add_key(Key, State),
    scan_events(Es, State1#state { prev_key = Key} );
scan_events([{release,Key}|Es], State) ->
    io:format("RELEASE ~s\n", [[i2c_tca8418:keycode_to_sym(Key)]]),
    State1 = 
	if State#state.locked ->
		set_led(State#state.tca8418, off),
		case State#state.pincode_enter_key of
		    undefined -> check_pincode(State, false);
		    Key -> check_pincode(State, true);
		    _ -> State
		end;
	   true ->
		State
	end,
    scan_events(Es, State1#state { prev_key = undefined });
scan_events([Event|Es], State) ->
    io:format("key_serv: ignore event ~w\n", [Event]),
    scan_events(Es, State);
scan_events([], State) ->
    State.

%% fixme digest!
check_pincode(State, Enter) ->
    io:format("CODE=~s\n", [State#state.code]),
    if State#state.code =:= State#state.pincode ->
	    set_led(State#state.tca8418, green),
	    State#state { locked = false,
			  attempts = 0,
			  count = 0,
			  code = [],
			  toggle = true,
			  blink_tmo = infinity };
       Enter -> %% enter key was pressed 
	    failed_attempt(State#state.attempts + 1,
			   State#state { code = [] });
       true ->
	    Len = length(State#state.code),
	    Count = State#state.count,
	    if Len > 0, (Count rem State#state.pincode_len) =:= 0 ->
		    Attempt = Count div State#state.pincode_len,
		    failed_attempt(Attempt, State);
	       true ->
		    State
	    end
    end.

failed_attempt(Attempts, State) ->
    BackOff_Ms = backoff_s(Attempts)*1000,
    erlang:start_timer(BackOff_Ms, self(), backoff),
    set_led(State#state.tca8418, red),
    State#state { backoff = true, attempts = Attempts }.
    

backoff_s(1) -> ?BACK_OF_TIME_1;
backoff_s(2) -> ?BACK_OF_TIME_2;
backoff_s(3) -> ?BACK_OF_TIME_3;
backoff_s(4) -> ?BACK_OF_TIME_4;
backoff_s(5) -> ?BACK_OF_TIME_5;
backoff_s(6) -> ?BACK_OF_TIME_6;
backoff_s(_) -> ?BACK_OF_TIME_7.

set_led(TCA8418, off) ->
    i2c_tca8418:gpio_clr(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_clr(TCA8418, ?LED_RED);
set_led(TCA8418, red) ->
    i2c_tca8418:gpio_clr(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_set(TCA8418, ?LED_RED);
set_led(TCA8418, green) ->
    i2c_tca8418:gpio_set(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_clr(TCA8418, ?LED_RED);
set_led(TCA8418, yellow) ->
    i2c_tca8418:gpio_set(TCA8418, ?LED_GREEN),
    i2c_tca8418:gpio_set(TCA8418, ?LED_RED).

set_com(TCA8418, true) ->
    i2c_tca8418:gpio_set(TCA8418, ?LED_COM);
set_com(TCA8418, false) ->
    i2c_tca8418:gpio_clr(TCA8418, ?LED_COM).

set_app(TCA8418, true) ->
    i2c_tca8418:gpio_set(TCA8418, ?LED_APP);
set_app(TCA8418, false) ->
    i2c_tca8418:gpio_clr(TCA8418, ?LED_APP).


%% add key when 0-9 to pincode make sure length is
%% at most pincode_len
add_key(Key, State) ->
    Sym = i2c_tca8418:keycode_to_sym(Key),
    if Sym >= $0, Sym =< $9 ->
	    Code = State#state.code ++ [Sym],
	    Count = State#state.count + 1,
	    Len = length(Code),
	    if Len =< State#state.pincode_len ->
		    State#state { code = Code, count = Count };
	       true ->
		    [_|Code1] = Code,
		    State#state { code = Code1, count = Count }
	    end;
       true ->
	    State
    end.

%% update battry charging status leds
update_soc(TCA8418, Soc, Charging, Set) ->
    lists:foreach(
      fun(#level{pin=Pin, steady=Steady, 
	       charge_low=Low, charge_high=High}) ->
	      if Soc > Steady; Charging, Soc >= Low, Soc =< High, Set ->
		      i2c_tca8418:gpio_set(TCA8418, Pin);
		 true ->
		      i2c_tca8418:gpio_clr(TCA8418, Pin)
	      end
      end, ?LEVEL_LIST).
