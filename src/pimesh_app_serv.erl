%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    start the app monitor leds
%%% @end
%%% Created :  4 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_app_serv).

-export([start_link/0, start_link/1]).
-export([set_activity/2]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 1000).

-define(LED_COM, {row,4}).  %% yellow
-define(LED_APP, {row,5}).  %% green (steady)

-record(state,
	{
	 parent,
	 activity=0,
	 activity_tmo=500,
	 toggle=false,
	 tca8418
	}).


start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(i2c),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

set_activity(Serv, Act) ->
    serv:call(Serv, {set_activity, Act}).


init(Parent, Bus) ->
    {ok,TCA8418} = i2c_tca8418:open1(Bus),
    i2c_tca8418:gpio_init(TCA8418, ?LED_COM),
    i2c_tca8418:gpio_output(TCA8418, ?LED_COM),
    i2c_tca8418:gpio_clr(TCA8418, ?LED_COM),

    i2c_tca8418:gpio_init(TCA8418, ?LED_APP),
    i2c_tca8418:gpio_output(TCA8418, ?LED_APP),
    i2c_tca8418:gpio_set(TCA8418, ?LED_APP),

    {ok, #state { parent=Parent,
    	 	  tca8418 = TCA8418 }}.


message_handler(State=#state{tca8418=TCA8418,
			     activity_tmo=ActivityTmo,
			     parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, {set_activity,A}} ->
	    Activity = State#state.activity + A,
            {reply, From, ok,State#state { activity = Activity }}; 

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};

        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply

    after ActivityTmo ->
	    Toggle = not State#state.toggle,
	    if Toggle ->
		    i2c_tca8418:gpio_set(TCA8418, ?LED_COM);
	       true ->
		    i2c_tca8418:gpio_clr(TCA8418, ?LED_COM)
	    end,
	    {noreply, State#state{toggle=Toggle}}
    end.
			

