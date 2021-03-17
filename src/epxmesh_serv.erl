%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Epx simulator of piMesh
%%% @end
%%% Created : 28 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(epxmesh_serv).

-export([start_link/0, start_link/1]).
-export([start_fb/0]).
-export([start_pine/0]).
-export([message_handler/1]).
-export([is_locked/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").


-define(LED_COM,   {row,4}).  %% yellow
-define(LED_APP,   {row,5}).  %% green (steady)
-define(LED_GREEN, {row,6}).  %% gpio on tca8418
-define(LED_RED,   {row,7}).  %% gpio on tca8418
-define(BAT_LED_5, {col,7}).  %% green (fully charged)
-define(BAT_LED_4, {col,6}).  %% blue
-define(BAT_LED_3, {col,5}).  %% blue
-define(BAT_LED_2, {col,4}).  %% blue
-define(BAT_LED_1, {col,3}).  %% blue

%% window ratio
-define(SCALE_Y, 370).
-define(SCALE_X, 240).

%% -define(DIGIT_FONT_SIZE, 46).
%% -define(LABEL_FONT_SIZE, 10).

%% fixme scale a bit
-define(DIGIT_FONT_SIZE, 96).
-define(LABEL_FONT_SIZE, 20).

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
	 blink_tmr = undefined,   %% current timer
	 attempts = 0,
	 backoff = false,
	 count = 0,   %% number of keys total since start of attempts
	 code = [],   %% entered code (fixme digest)
	 %% battery status
	 soc = 0,
	 charging = false,
	 charging_set = false,
	 activity = 1000,       %% length of activity pulse
	 activity_tmr = undefined :: reference(),  %% activity timer
	 pwm = 0.0,             %% last pwm value (0-100)
	 %%
	 window,
	 pixels,
	 screen,
	 screen_map,
	 digit_font,
	 label_font,
	 pressed = []
	}).

is_locked(Serv) ->
    serv:call(Serv, is_locked).

start_fb() -> start_link([{backend,"fb"}]).

%% 1020 to keep scale factor
start_pine() -> start_link([{backend,"fb"},{width,720},{height,1020}]).

start_link() -> start_link([]).

start_link(Opts) ->
    application:ensure_all_started(xbus),
    case proplists:get_value(backend,Opts) of
	undefined ->
	    epx:start(),
	    B = epx_backend:default(),
	    H = epx:backend_info(B, height) - (30+70),
	    W = trunc(?SCALE_X * (H / ?SCALE_Y)),
	    ?spawn_server(
	       fun(Parent) ->
		       init(Parent,
			    0, 0, W, H, 
			    ?DIGIT_FONT_SIZE, ?LABEL_FONT_SIZE)
	       end,
	       fun ?MODULE:message_handler/1);
	    
	BackendName ->
	    application:load(epx),
	    application:set_env(epx, backend, BackendName),
	    application:ensure_all_started(epx),
	    B = epx_backend:default(),
	    [_Format1|_] = epx:backend_info(B, pixel_formats), %% match?
	    %% W = epx:backend_info(B, width),
	    %% H = epx:backend_info(B, height),
	    B = epx_backend:default(),
	    H = case proplists:get_value(width, Opts, 0) of
		    0 -> epx:backend_info(B, height);
		    H0 -> H0
		end,
	    W = case proplists:get_value(height, Opts, 0) of
		    0 ->
			%% epx:backend_info(B, width);
			trunc(?SCALE_X * (H / ?SCALE_Y));
		    W0 -> W0
		end,
	    ?spawn_server(
	       fun(Parent) ->
		       init(Parent,
			    30, 39, W, H, 
			    ?DIGIT_FONT_SIZE, ?LABEL_FONT_SIZE)
	       end,
	       fun ?MODULE:message_handler/1)
    end.


init(Parent, X, Y, W, H, DigitFontSize, LabelFontSize) ->
    Window = epx:window_create(X, Y, W, H, [button_press,button_release]),
    epx:window_attach(Window),
    Pixels = epx:pixmap_create(W, H),
    Screen = epx:pixmap_create(W, H),
    epx:pixmap_attach(Screen),
    {ok,DigitFont} = epx_font:match([{name,"Arial"}, {size,DigitFontSize}]),
    {ok,LabelFont} = epx_font:match([{name,"Arial"}, {size,LabelFontSize}]),
    ScreenMap = screen_map(W, H, DigitFont,LabelFont),
    TCA8418 = undefined,
    set_led(TCA8418, yellow),
    TRef = start_timer(?BLINK_ON_TMO, blink),    

    xbus:sub(<<"mixmesh.*">>),
    {ok, #state { 
	    parent = Parent,
	    tca8418 = TCA8418,
	    blink_tmr = TRef,
	    toggle = false,
	    window = Window,
	    pixels = Pixels,
	    screen = Screen,
	    screen_map = ScreenMap,
	    digit_font = DigitFont,
	    label_font = LabelFont,
	    pwm = 80.0
	   }}.

message_handler(State=#state{parent=Parent}) ->
    draw(State),
    epx:pixmap_copy_to(State#state.pixels,State#state.screen),
    update(State#state.screen,State#state.window),
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, is_locked} ->
            {reply, From, State#state.locked, State};

	{epx_event, Win, close} when Win =:= State#state.window ->
	    epx:pixmap_detach(State#state.screen),
	    epx:window_detach(State#state.window),
	    ok;

	{epx_event, Win, {button_press, [left|_], Where}} when 
	      Win =:= State#state.window ->
	    case find_key(Where, State#state.screen_map) of
		false ->
		    {noreply, State};
		Key ->
		    State1 = scan_events([{press,Key}], State),
		    Pressed0 = State#state.pressed,
		    Pressed = [Key|Pressed0],
		    {noreply, State1#state{ pressed = Pressed}}
	    end;

	{epx_event, Win, {button_release, [left|_], _Where}} when 
	      Win =:= State#state.window ->
	    State1 = lists:foldl(
		       fun(Key, Si) ->
			       scan_events([{release,Key}], Si)
		       end, State, State#state.pressed),
	    {noreply, State1#state { pressed = [] }};

	{xbus, _, #{ topic := <<"mixmesh.battery.soc">>, value := SOC }} ->
            Charging = State#state.charging,
            Set = State#state.charging_set,
            update_soc(State#state.tca8418, SOC, Charging, Set),
            State1 = State#state { soc = SOC, charging_set = not Set },
	    {noreply, State1};

        {xbus, _, #{ topic := <<"mixmesh.battery.charging">>,
		     value := Charging }} ->
            State1 = State#state { charging = Charging },
	    {noreply, State1};
	
	{xbus, _, #{ topic := <<"mixmesh.node.activity">>,
		     value := Activity }} ->
	    if is_reference(State#state.activity_tmr) ->
		    {noreply, State#state { activity = Activity }};
	       true ->
		    set_com(State#state.tca8418, true),
		    Timer = start_timer(Activity, no_activity),
		    {noreply, State#state{ activity = Activity,
					   activity_tmr = Timer }}
	    end;
	{xbus, _, #{ topic := <<"mixmesh.node.running">>,
		     value := Running }} ->
	    set_app(State#state.tca8418, Running),
            {noreply, State};

        {xbus, _, #{ topic := <<"mixmesh.keypad.pwm">>, value := PWM }} ->
            {noreply, State#state { pwm = PWM }};

        {xbus, _, #{ topic := <<"mixmesh.keypad.led">>, value := Led }} ->
	    %% test pin-led
	    set_led(State#state.tca8418, Led),
	    {noreply, State};

	{xbus, _, _} ->  %% ignore new xbus mixmesh.* messages not handled
	    {noreply, State};

	{timeout,_TRef, no_activity} ->
	    Tmr = if State#state.activity =:= 0 ->
			  set_com(State#state.tca8418, false),
			  undefined;
		     true ->
			  start_timer(State#state.activity, no_activity)
		  end,
	    {noreply, State#state{ activity_tmr = Tmr }};

	{timeout,_TRef,backoff} ->  %% backoff period is over
	    set_led(State#state.tca8418, yellow),
	    TRef = start_timer(?BLINK_ON_TMO, blink),
	    {noreply, State#state { backoff = false,
				    blink_tmr = TRef, toggle = false }};
	
	{timeout,_TRef,blink} when _TRef =:= State#state.blink_tmr ->
	    Toggle = not State#state.toggle,
	    Tmo = if State#state.backoff ->
			  infinity;
		     Toggle ->
			  set_led(State#state.tca8418, yellow),
			  ?BLINK_ON_TMO;
		     true ->
			  set_led(State#state.tca8418, off),
			  ?BLINK_OFF_TMO
		  end,
	    TRef = start_timer(Tmo, blink),
	    {noreply, State#state { toggle = Toggle, blink_tmr = TRef }};

	{timeout,_TRef,blink} ->
	    {noreply, State};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

start_timer(infinity, _) ->
    undefined;
start_timer(Timeout, Message) ->
    Timeout1 = max(50, Timeout),
    erlang:start_timer(Timeout1, self(), Message).

cancel_timer(undefined) ->
    true;
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
		{timeout, TRef, _} ->
		    true
	    after 0 ->
		    false
	    end;
	_Remain ->
	    true
    end.

update(Pix,Win) ->
    Width = epx:pixmap_info(Pix, width),
    Height = epx:pixmap_info(Pix, height),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, Width, Height).

-define(Rh(D,I), trunc(((D)*(I))/(?SCALE_Y))).
-define(Rw(D,I), trunc(((D)*(I))/(?SCALE_X))).

-define(TOP(H),    ?Rh((H),50)).
-define(LEFT(W),   ?Rw((W),20)).
-define(DX(W),     ?Rw((W),60)).
-define(DY(H),     ?Rh((H),60)).
-define(PADX(W),   ?Rw((W),10)).
-define(PADY(H),   ?Rh((H),10)).
-define(BOTTOM(H), ?Rh((H),50)).
-define(RIGHT(W),  ?Rw((W),20)).
-define(BORDER(W), ?Rw((W),4)).
-define(COLOR1, silver).

-define(R0(H), (?TOP(H)+0*?DY(H))).
-define(R1(H), (?TOP(H)+1*(?DY(H)+?PADY(H)))).
-define(R2(H), (?TOP(H)+2*(?DY(H)+?PADY(H)))).
-define(R3(H), (?TOP(H)+3*(?DY(H)+?PADY(H)))).

-define(C0(W), (?LEFT(W)+0*(?DX(W)+?PADX(W)))).
-define(C1(W), (?LEFT(W)+1*(?DX(W)+?PADX(W)))).
-define(C2(W), (?LEFT(W)+2*(?DX(W)+?PADX(W)))).

screen_map(W, H, DigitFont,LabelFont) ->
    [begin
	 {Fw,Fh} = epx_font:dimension(DigitFont,[Char]),
	 Ascent = epx:font_info(DigitFont, ascent),
	 R0 = max(Fw, Fh),
	 Bw = ?BORDER(W),
	 OffsX = Bw+(R0-Fw) div 2,
	 OffsY = Bw+((R0-Fh) div 2) + Ascent,
	 #{ 
	    type  => key,
	    char  => Char,
	    border_color => black,
	    font_color => black,
	    highlight_color => white,
	    background_color => Color,
	    bounding_box => {X, Y, R0+2*Bw, R0+2*Bw},
	    offset  => {OffsX, OffsY} }
     end ||
	{X,Y,Char,Color} <-
	    [{?C0(W),?R0(H),$1,?COLOR1},
	     {?C1(W),?R0(H),$2,?COLOR1},
	     {?C2(W),?R0(H),$3,?COLOR1},
	     {?C0(W),?R1(H),$4,?COLOR1},
	     {?C1(W),?R1(H),$5,?COLOR1},
	     {?C2(W),?R1(H),$6,?COLOR1},
	     {?C0(W),?R2(H),$7,?COLOR1},
	     {?C1(W),?R2(H),$8,?COLOR1},
	     {?C2(W),?R2(H),$9,?COLOR1},
	     {?C0(W),?R3(H),$?,yellow},
	     {?C1(W),?R3(H),$0,?COLOR1},
	     {?C2(W),?R3(H),$#,green}] ] ++

	[begin
	     Dim = epx_font:dimension(LabelFont,Name),
	     Ascent = epx_font:info(LabelFont,ascent),
	     Descent = epx_font:info(LabelFont,descent),
	     # { 
		 type => group,
		 label => Name,
		 border_color => black,
		 font_color => black,
		 background_color => white,
		 bounding_box => Rect,
		 led_list => lists:zip(lists:seq(1,length(LedList)),LedList),
		 label_dimension => Dim,
		 label_ascent => Ascent,
		 label_descent => Descent
	       }
	 end ||
	    {Name,Rect,LedList} <-
		[{"bat",{?Rw(W,10),?Rh(H,10),?Rw(W,100),?Rh(H,30)},
		  [{?BAT_LED_1,blue},{?BAT_LED_2,blue},
		   {?BAT_LED_3,blue},{?BAT_LED_4,blue},
		   {?BAT_LED_5,green}]},
		 {"app",{W-?Rw(W,50),?Rh(H,10),?Rw(W,40),?Rh(H,30)},
		  [{?LED_APP,green},{?LED_COM,yellow}]},
		 {"pin",{?Rw(W,10),H-?Rh(H,40),?Rw(W,23),?Rh(H,30)},
		  %% dual-led mix is yellow
		  [{{?LED_GREEN,green},{?LED_RED,red}}]}
		]
	].

find_key(Pos, [#{type:=key, char:=Char, bounding_box := BoundingBox} | Maps]) ->
    case mouse_in_circle(Pos, BoundingBox) of
	true ->
	    Char;
	false ->
	    find_key(Pos, Maps)
    end;
find_key(Pos, [_|Maps]) ->
    find_key(Pos, Maps);
find_key(_Pos, []) ->
    false.

draw(State = #state { pixels = Pixels, screen_map = ScreenMap}) ->
    W = epx:pixmap_info(Pixels, width),
    H = epx:pixmap_info(Pixels, height),
    epx:pixmap_fill(Pixels, white),
    epx_gc:set_font(State#state.digit_font),
    epx_gc:set_background_color(gray),
    Pressed = State#state.pressed,
    lists:foldl(
      fun(#{ type := key,
	     char := Key,
	     bounding_box := BoundingBox,
	     offset := Offset,
	     border_color := BorderColor,
	     font_color := FontColor,
	     background_color := BackgroundColor,
	     highlight_color := HighlightColor
	   }, _Acc) ->
	      epx_gc:set_fill_style(solid),
	      epx_gc:set_fill_color(BorderColor),
	      epx:draw_ellipse(Pixels, BoundingBox),
	      case lists:member(Key, Pressed) of
		  true ->
		      epx_gc:set_fill_color(HighlightColor);
		  false ->
		      epx_gc:set_fill_color(BackgroundColor)
	      end,
	      epx:draw_ellipse(Pixels, inset_rect(BoundingBox, 
						  {?BORDER(W),?BORDER(W)})),
	      set_font_color(FontColor),
	      case Key of
		  $? ->
		      %% draw black inner circle
		      Box = inset_rect(BoundingBox, {?Rh(H,15),?Rh(H,15)}),
		      epx_gc:set_fill_style(none),
		      epx_gc:set_border_width(?BORDER(W)),
		      epx_gc:set_border_color(black),
		      %% epx_gc:set_fill_color(black),
		      epx:draw_ellipse(Pixels, Box),
		      epx_gc:set_border_width(0);
		  $# ->
		      %% draw black square
		      Box = inset_rect(BoundingBox, {?Rh(H,20),?Rh(H,20)}),
		      epx_gc:set_fill_style(solid),
		      epx_gc:set_fill_color(black),
		      epx:draw_rectangle(Pixels, Box);
		  _ -> %% draw char
		     {X,Y,_,_} = offset_rect(BoundingBox, Offset),
		     epx:draw_char(Pixels, X, Y, Key)
	      end;
	 (_, Acc) ->
	      Acc
      end, ok, ScreenMap),

    epx_gc:set_font(State#state.label_font),

    lists:foldl(
      fun(#{ type := group,
	     label := Name,
	     border_color := BorderColor,
	     background_color := BackgroundColor,
	     font_color := FontColor,
	     bounding_box := BoundingBox,
	     label_dimension := Dim,
	     label_ascent := Ascent,
	     led_list := LedList
	   }, _Acc) ->
	      epx_gc:set_fill_style(none),
	      epx_gc:set_foreground_color(BorderColor),
	      epx:draw_rectangle(Pixels, BoundingBox),
	      {Bx,By,Bw,Bh} = BoundingBox,
	      Wl  = Bw div length(LedList),
	      Hi  = Bh div 2,
	      R   = min(Wl-2,Hi),
	      epx_gc:set_fill_style(solid),
	      lists:foreach(
		fun
		    ({I,{{Led1,LedColor1},{Led2,LedColor2}}}) ->
			case {get(Led1),get(Led2)} of
			    {true,true} ->
				Color = blend(LedColor1,LedColor2),
				set_led_color(Color, State#state.pwm);
			    {true,_} ->
				set_led_color(LedColor1, State#state.pwm);
			    {_,true} ->
				set_led_color(LedColor2, State#state.pwm);
			    _ ->
				set_led_color(LedColor1, 0.0)
			end,
			Xi = Bx + (I-1)*Wl + 2,
			Yi = By + 4,
			epx:draw_ellipse(Pixels, {Xi,Yi,R,R});

		    ({I,{Led,LedColor}}) ->
			case get(Led) of
			    true ->
				set_led_color(LedColor, State#state.pwm);
			    _ ->
				set_led_color(LedColor, 0.0)
			end,
			Xi = Bx + (I-1)*Wl + 2,
			Yi = By + 4,
			epx:draw_ellipse(Pixels, {Xi,Yi,R,R})

		end, LedList),
	      epx_gc:set_fill_style(solid),
	      {DW,DH} = Dim,
	      Dx = ?Rw(W,2),
	      Xl = Bx + 2*Dx,
	      Yl = By + Bh - Ascent,
	      epx_gc:set_fill_color(BackgroundColor), %% pink
	      epx:draw_rectangle(Pixels, {Bx+Dx,Yl,DW+2*Dx,DH}),
	      set_font_color(FontColor),
	      epx:draw_string(Pixels, Xl, Yl+Ascent, Name);
	 (_, Acc) ->
	      Acc
      end, ok, ScreenMap).

blend(green, red) ->
    yellow.
    
mouse_in_circle({Xi,Yi,_}, BoundingBox) ->
    case epx_rect:contains(BoundingBox, {Xi,Yi}) of
	false ->
	    false;
	true ->
	    {X,Y,W,H} = BoundingBox,
	    Xc = X + (W div 2),
	    Yc = Y + (H div 2),
	    R = max(W,H),
	    Xd = (Xi-Xc),
	    Yd = (Yi-Yc),
	    L = math:sqrt(Xd*Xd + Yd*Yd),
	    L =< R
    end.

offset_rect({X,Y,W,H},{Dx,Dy}) ->
    {X+Dx,Y+Dy,W,H}.

inset_rect({X,Y,W,H},{Dx,Dy}) ->
    {X+Dx,Y+Dy,W-2*Dx,H-2*Dy}.

set_led_color(Color, Pwm) when is_atom(Color); is_list(Color) ->
    Pwm01 = Pwm/100,
    {H,S,L} = epx_color:rgb_to_hsl(Color),
    epx_gc:set_fill_color(epx_color:hsl_to_rgb({H,S,L*Pwm01})).

set_font_color(Name) when is_atom(Name); is_list(Name) ->
    {R,G,B} = epx_color:from_name(Name),
    epx_gc:set_foreground_color({0,R,G,B}).

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
			   toggle   = false
			 },
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
    io:format("PRESS ~s\n", [[Key]]),
    if State#state.locked ->
	    set_led(State#state.tca8418, green);
       true ->
	    ok
    end,
    State1 = add_key(Key, State),
    scan_events(Es, State1#state { prev_key = Key} );
scan_events([{release,Key}|Es], State) ->
    io:format("RELEASE ~s\n", [[Key]]),
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
	    cancel_timer(State#state.blink_tmr),
	    set_led(State#state.tca8418, green),
	    State#state { locked = false,
			  attempts = 0,
			  count = 0,
			  code = [],
			  toggle = true,
			  blink_tmr = undefined
			};
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
    cancel_timer(State#state.blink_tmr),
    start_timer(BackOff_Ms, backoff),
    set_led(State#state.tca8418, red),
    State#state { backoff = true, 
		  blink_tmr = undefined, 
		  attempts = Attempts }.

backoff_s(1) -> ?BACK_OF_TIME_1;
backoff_s(2) -> ?BACK_OF_TIME_2;
backoff_s(3) -> ?BACK_OF_TIME_3;
backoff_s(4) -> ?BACK_OF_TIME_4;
backoff_s(5) -> ?BACK_OF_TIME_5;
backoff_s(6) -> ?BACK_OF_TIME_6;
backoff_s(_) -> ?BACK_OF_TIME_7.

set_led(TCA8418, off) ->
    gpio_clr(TCA8418, ?LED_GREEN),
    gpio_clr(TCA8418, ?LED_RED);
set_led(TCA8418, red) ->
    gpio_clr(TCA8418, ?LED_GREEN),
    gpio_set(TCA8418, ?LED_RED);
set_led(TCA8418, green) ->
    gpio_set(TCA8418, ?LED_GREEN),
    gpio_clr(TCA8418, ?LED_RED);
set_led(TCA8418, yellow) ->
    gpio_set(TCA8418, ?LED_GREEN),
    gpio_set(TCA8418, ?LED_RED).

set_com(TCA8418, true) ->
    gpio_set(TCA8418, ?LED_COM);
set_com(TCA8418, false) ->
    gpio_clr(TCA8418, ?LED_COM).

set_app(TCA8418, true) ->
    gpio_set(TCA8418, ?LED_APP);
set_app(TCA8418, false) ->
    gpio_clr(TCA8418, ?LED_APP).


%% add key when 0-9 to pincode make sure length is
%% at most pincode_len
add_key(Sym, State) ->
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
		      gpio_set(TCA8418, Pin);
		 true ->
		      gpio_clr(TCA8418, Pin)
	      end
      end, ?LEVEL_LIST).

gpio_set(_TCA8418, Led) ->
    put(Led, true).

gpio_clr(_TCA8418, Led) ->
    put(Led, false).
