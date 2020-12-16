%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Keep track on pisuger batter and application status
%%% @end
%%% Created : 19 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_battery_serv).

-export([start_link/0, start_link/1]).
-export([get_voltage/1, get_soc/1]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 1000).

-define(BAT_LED_5, {col,8}).  %% green (fully charged)
-define(BAT_LED_4, {col,7}).  %% blue
-define(BAT_LED_3, {col,6}).  %% blue
-define(BAT_LED_2, {col,5}).  %% blue
-define(BAT_LED_1, {col,4}).  %% blue

-record(level,
	{
	 pin,
	 steady,
	 charge_low,
	 charge_high
	}).

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
	 ip5209,
	 tca8418,
	 voltage,
	 soc,
	 soc0,  %% last reported soc
	 toggle = false
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(i2c),
    application:start(gpio),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

get_voltage(Serv) ->
    serv:call(Serv, get_voltage).

get_soc(Serv) ->
    serv:call(Serv, get_soc).

init(Parent, Bus) ->
    {ok,TCA8418} = i2c_tca8418:open1(Bus),
    lists:foreach(
      fun(#level{pin=Pin}) ->
	      i2c_tca8418:gpio_init(TCA8418, Pin),
	      i2c_tca8418:gpio_output(TCA8418, Pin),
	      i2c_tca8418:gpio_clr(TCA8418, Pin)
      end, ?LEVEL_LIST),

    xbus:pub_meta(<<"mixmesh.battery.voltage">>,
		  [{unit,"V"},
		   {description, "Battery voltage."},
		   {range, {3.1, 4.16}}]),
    xbus:pub_meta(<<"mixmesh.battery.soc">>,
		  [{unit,"%"},
		   {description, "State of charge."},
		   {range, {0.0, 100.0}}]),
    xbus:pub_meta(<<"mixmesh.battery.charging">>,
		  [{unit,"bool"},
		   {description, "Is battery charging."}]),
    {ok,IP5209}  = i2c_ip5209:open1(Bus),
    V0 = i2c_ip5209:read_voltage(IP5209),
    Charging = i2c_ip5209:is_power_plugged_2led(IP5209),
    SOC0 = i2c_ip5209:parse_voltage_level(V0),
    update_soc(TCA8418, SOC0, Charging, true),
    {ok, #state { parent=Parent,
    	 	  ip5209 = IP5209, tca8418 = TCA8418,
		  voltage=V0, soc=SOC0, soc0=SOC0 }}.
		  
message_handler(State=#state{tca8418=TCA8418,ip5209=IP5209,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, get_soc} ->
            {reply, From, State#state.soc};

        {call, From, get_voltage} ->
            {reply, From, State#state.voltage};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after ?SAMPLE_INTERVAL ->
	    %% FIXME: handle eremoteio, when ip5209 goes away,
	    %% when USB (power/gadget) is connected at the same
	    %% time as the PiSuer2 cabel is connected!
	    V1 = i2c_ip5209:read_voltage(IP5209),
	    SOC1 = i2c_ip5209:parse_voltage_level(V1),
	    Charging = i2c_ip5209:is_power_plugged_2led(IP5209),
	    SOC0 = if abs(SOC1 - State#state.soc0) > 1.0 ->
			   SOC1;
		      true ->
			   State#state.soc0
		   end,
	    xbus:pub(<<"mixmesh.battery.voltage">>, V1),
	    xbus:pub(<<"mixmesh.battery.soc">>, SOC0),
	    xbus:pub(<<"mixmesh.battery.charging">>, Charging),
	    Toggle = not State#state.toggle,
	    update_soc(TCA8418, SOC0, Charging, Toggle),
	    {noreply, State#state{voltage=V1,soc=SOC1, soc0=SOC0,
	    	      		  toggle=Toggle}}
    end.

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
