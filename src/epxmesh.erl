%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Epx simulator of piMesh
%%% @end
%%% Created : 28 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(epxmesh).

-export([start/0]).
-export([start_pine/0]).

-define(LED_COM,   {row,4}).  %% yellow
-define(LED_APP,   {row,5}).  %% green (steady)
-define(RED_LED,   {row,6}).  %% gpio on tca8418
-define(GREEN_LED, {row,7}).  %% gpio on tca8418
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

start() ->
    start(#{ led_pwm => 100,
	     led_active => [?LED_APP,
			    ?BAT_LED_1, ?BAT_LED_2] }).

start_pine() ->
    start_fb(#{ led_pwm => 100,
		led_active => [?LED_APP,
			       ?BAT_LED_1, ?BAT_LED_2] }).

%% window start (scaled to display height - 100)
start(State) ->
    application:ensure_all_started(xbus),
    epx:start(),
    %% W = 240, H = 370,
    %% W = 720, H = 1440,
    %% max height with same ratio
    B = epx_backend:default(),
    H = epx:backend_info(B, height) - (30+70),
    W = trunc(?SCALE_X * (H / ?SCALE_Y)),
    common_start(30, 30, W, H, ?DIGIT_FONT_SIZE, ?LABEL_FONT_SIZE, State).

start_fb(State) ->
    application:ensure_all_started(xbus),
    application:load(epx),
    ok = application:load(epx),
    application:set_env(epx, backend, "fb"),
    application:ensure_all_started(epx),
    B = epx_backend:default(),
    [_Format1|_] = epx:backend_info(B, pixel_formats), %% match?
    %% W = epx:backend_info(B, width),
    %% H = epx:backend_info(B, height),
    B = epx_backend:default(),
    H = epx:backend_info(B, height),
    W = epx:backend_info(B, width),
    %% W = trunc(?SCALE_X * (H / ?SCALE_Y)),
    common_start(0, 0, W, H, ?DIGIT_FONT_SIZE, ?LABEL_FONT_SIZE, State).


common_start(X, Y, W, H, DigitFontSize, LabelFontSize, State) ->
    Win = epx:window_create(X, Y, W, H, [button_press,button_release]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W, H),
    epx:pixmap_attach(Pix),
    {ok,DigitFont} = epx_font:match([{name,"Arial"}, {size,DigitFontSize}]),
    {ok,LabelFont} = epx_font:match([{name,"Arial"}, {size,LabelFontSize}]),
    ScreenMap = screen_map(W, H, DigitFont,LabelFont),
    State1 = State#{ digit_font => DigitFont,
		     label_font => LabelFont },
    xbus:sub(<<"mixmesh.battery.soc">>),
    xbus:sub(<<"mixmesh.node.activity">>),
    xbus:sub(<<"mixmesh.system.enabled">>),
    xbus:sub(<<"mixmesh.keypad.pwm">>),
    xbus:pub(<<"mixmesh.keypad.installed">>, true),
    loop(Pix,Win,ScreenMap,State1).



loop(Pix,Win,ScreenMap,State) ->
    draw(Pix,ScreenMap,State),
    update(Pix,Win),
    receive
	{epx_event, Win, close} ->
	    epx:pixmap_detach(Pix),
	    epx:window_detach(Win),
	    ok;

	{epx_event, Win, {button_press, [left|_], Where}} ->
	    case find_key(Where, ScreenMap) of
		false ->
		    loop(Pix,Win,ScreenMap,State);
		Key ->
		    Pressed0 = maps:get(pressed, State, []),
		    Pressed = [Key|Pressed0],
		    loop(Pix,Win,ScreenMap,State#{ pressed => Pressed})
	    end;

	{epx_event, Win, {button_release, [left|_], _Where}} ->
	    loop(Pix,Win,ScreenMap,State#{ pressed => []});

	{xbus, <<"mixmesh.battery.soc">>, #{ value := SOC }} ->
	    Charging = maps:get(charging,State,false),
	    Set = maps:get(charging_set,State,false),
	    Active0 = maps:get(led_active,State,[]),
	    Active1 = update_soc(SOC, Charging, Set, Active0),
	    State1 = State#{ led_active => Active1 },
	    loop(Pix,Win,ScreenMap,State1);

	{xbus, <<"mixmesh.battery.charging">>, #{ value := Charging }} ->
	    State1 = State#{ charging => Charging },
	    loop(Pix,Win,ScreenMap,State1);

	{xbus, <<"mixmesh.node.activity">>, #{ value := Activity }} ->
	    Active0 = maps:get(led_active,State,[]),
	    Active1 = if Activity ->
			      set_led(?LED_COM, Active0);
			 true ->
			      clr_led(?LED_COM, Active0)
		      end,
	    State1 = State#{ led_active => Active1 },
	    loop(Pix,Win,ScreenMap,State1);

	{xbus, <<"mixmesh.keypad.pwm">>, #{ value := PWM }} ->
	    State1 = State#{ led_pwm => PWM },
	    loop(Pix,Win,ScreenMap,State1);	    
	
	Other ->
	    io:format("Got event ~p\n", [Other]),
	    loop(Pix,Win,ScreenMap,State)
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
		  [{?GREEN_LED,green}]}]
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

draw(Pix, ScreenMap, State) ->
    W = epx:pixmap_info(Pix, width),
    H = epx:pixmap_info(Pix, height),
    epx:pixmap_fill(Pix, white),
    epx_gc:set_font(maps:get(digit_font,State)),
    epx_gc:set_background_color(gray),
    Pressed = maps:get(pressed,State,[]),
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
	      epx:draw_ellipse(Pix, BoundingBox),
	      case lists:member(Key, Pressed) of
		  true ->
		      epx_gc:set_fill_color(HighlightColor);
		  false ->
		      epx_gc:set_fill_color(BackgroundColor)
	      end,
	      epx:draw_ellipse(Pix, inset_rect(BoundingBox, 
					       {?BORDER(W),?BORDER(W)})),
	      set_font_color(FontColor),
	      case Key of
		  $? ->
		      %% draw black inner circle
		      Box = inset_rect(BoundingBox, {?Rw(W,15),?Rh(H,15)}),
		      epx_gc:set_fill_style(none),
		      epx_gc:set_border_width(?BORDER(W)),
		      epx_gc:set_border_color(black),
		      %% epx_gc:set_fill_color(black),
		      epx:draw_ellipse(Pix, Box),
		      epx_gc:set_border_width(0);
		  $# ->
		      %% draw black square
		      Box = inset_rect(BoundingBox, {?Rw(W,20),?Rh(H,20)}),
		      epx_gc:set_fill_style(solid),
		      epx_gc:set_fill_color(black),
		      epx:draw_rectangle(Pix, Box);
		  _ -> %% draw char
		     {X,Y,_,_} = offset_rect(BoundingBox, Offset),
		     epx:draw_char(Pix, X, Y, Key)
	      end;
	 (_, Acc) ->
	      Acc
      end, ok, ScreenMap),
    epx_gc:set_font(maps:get(label_font,State)),
    LedActive = maps:get(led_active,State,[]),
    LedPwm = maps:get(led_pwm,State,100.0),
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
	      epx:draw_rectangle(Pix, BoundingBox),
	      {Bx,By,Bw,Bh} = BoundingBox,
	      Wl  = Bw div length(LedList),
	      Hi  = Bh div 2,
	      R   = min(Wl-2,Hi),
	      epx_gc:set_fill_style(solid),
	      lists:foreach(
		fun({I,{Led,LedColor}}) ->
			case lists:member(Led, LedActive) of
			    true ->
				set_led_color(LedColor, LedPwm);
			    false ->
				set_led_color(LedColor, 0.0)
			end,
			Xi = Bx + (I-1)*Wl + 2,
			Yi = By + 4,
			epx:draw_ellipse(Pix, {Xi,Yi,R,R})
		end, LedList),
	      epx_gc:set_fill_style(solid),
	      {DW,DH} = Dim,
	      Dx = ?Rw(W,2),
	      Xl = Bx + 2*Dx,
	      Yl = By + Bh - Ascent,
	      epx_gc:set_fill_color(BackgroundColor), %% pink
	      epx:draw_rectangle(Pix, {Bx+Dx,Yl,DW+2*Dx,DH}),
	      set_font_color(FontColor),
	      epx:draw_string(Pix, Xl, Yl+Ascent, Name);
	 (_, Acc) ->
	      Acc
      end, ok, ScreenMap).
    
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

update_soc(SOC, Charging, Set, Active) ->
    lists:foldl(
      fun(#level{pin=Pin, steady=Steady, 
		 charge_low=Low, charge_high=High}, Active0) ->
	      if SOC > Steady; Charging, SOC >= Low, SOC =< High, Set ->
		      set_led(Pin, Active0);
		 true ->
		      clr_led(Pin, Active0)
	      end
      end, Active, ?LEVEL_LIST).

set_led(Pin, List) ->
    case lists:member(Pin, List) of
	true -> List;
	false -> [Pin|List]
    end.

clr_led(Pin, List) ->
    case lists:member(Pin, List) of
	true -> List -- [Pin];
	false -> List
    end.
    
