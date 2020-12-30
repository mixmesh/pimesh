%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Epx simulator of piMesh
%%% @end
%%% Created : 28 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(epxmesh).

-export([start/0]).

-define(TOP,  50).
-define(LEFT, 20).
-define(DX, 60).
-define(DY, 60).
-define(PADX, 10).
-define(PADY, 10).
-define(BOTTOM, 50).
-define(RIGHT, 20).
-define(BORDER, 4).
-define(COLOR1, silver).
-define(WIDTH,  (?LEFT+?RIGHT+3*?DX+2*?PADX)).
-define(HEIGHT, (?TOP+?BOTTOM+4*?DY+3*?PADY)).

-define(LED_COM,   {row,4}).  %% yellow
-define(LED_APP,   {row,5}).  %% green (steady)
-define(RED_LED,   {row,6}).  %% gpio on tca8418
-define(GREEN_LED, {row,7}).  %% gpio on tca8418
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


start() ->
    start(#{ led_pwm => 100,
	     led_active => [?LED_APP,
			    ?BAT_LED_1, ?BAT_LED_2] }).

start(State) ->
    application:ensure_all_started(xbus),
    W = ?WIDTH,
    H = ?HEIGHT, 
    epx:start(),
    Win = epx:window_create(30, 30, W, H, [button_press,button_release]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W, H),
    epx:pixmap_attach(Pix),
    {ok,DigitFont} = epx_font:match([{name,"Arial"},{size,46}]),
    {ok,LabelFont} = epx_font:match([{name,"Arial"},{size,10}]),
    ScreenMap = screen_map(DigitFont,LabelFont),
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


-define(R0, (?TOP+0*?DY)).
-define(R1, (?TOP+1*(?DY+?PADY))).
-define(R2, (?TOP+2*(?DY+?PADY))).
-define(R3, (?TOP+3*(?DY+?PADY))).

-define(C0, (?LEFT+0*(?DX+?PADX))).
-define(C1, (?LEFT+1*(?DX+?PADX))).
-define(C2, (?LEFT+2*(?DX+?PADX))).


screen_map(DigitFont,LabelFont) ->
    [begin
	 {Fw,Fh} = epx_font:dimension(DigitFont,[Char]),
	 Ascent = epx:font_info(DigitFont, ascent),
	 R0 = max(Fw, Fh),
	 Bw = ?BORDER,
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
	    [{?C0,?R0,$1,?COLOR1},{?C1,?R0,$2,?COLOR1},{?C2,?R0,$3,?COLOR1},
	     {?C0,?R1,$4,?COLOR1},{?C1,?R1,$5,?COLOR1},{?C2,?R1,$6,?COLOR1},
	     {?C0,?R2,$7,?COLOR1},{?C1,?R2,$8,?COLOR1},{?C2,?R2,$9,?COLOR1},
	     {?C0,?R3,$?,yellow},{?C1,?R3,$0,?COLOR1},{?C2,?R3,$#,green}] ] ++

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
		[{"bat",{10,10,100,30},
		  [{?BAT_LED_1,blue},{?BAT_LED_2,blue},
		   {?BAT_LED_3,blue},{?BAT_LED_4,blue},
		   {?BAT_LED_5,green}]},
		 {"app",{?WIDTH-50,10,40,30},
		  [{?LED_APP,green},{?LED_COM,yellow}]},
		 {"pin",{10,?HEIGHT-40,23,30},[{?GREEN_LED,green}]}]
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
	      epx:draw_ellipse(Pix, inset_rect(BoundingBox, {?BORDER,?BORDER})),
	      set_font_color(FontColor),
	      {X,Y,_,_} = offset_rect(BoundingBox, Offset),
	      epx:draw_char(Pix, X, Y, Key);
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
	      {X,Y,W,H} = BoundingBox,
	      Wl = W div length(LedList),
	      Hi  = H div 2,
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
			Xi = X + (I-1)*Wl + 2,
			Yi = Y + 4,
			epx:draw_ellipse(Pix, {Xi,Yi,R,R})
		end, LedList),
	      epx_gc:set_fill_style(solid),
	      {DW,DH} = Dim,
	      Xl = X + 2,
	      Yl = Y + H - Ascent,
	      epx_gc:set_fill_color(BackgroundColor), %% pink
	      epx:draw_rectangle(Pix, {Xl,Yl,DW,DH}),
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
    
