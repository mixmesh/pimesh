%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    start the app monitor leds
%%% @end
%%% Created :  4 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_app_serv).

-export([start_link/0, start_link/1]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 1000).

-record(state,
	{
	 parent,
	 activity=0,
	 activity_tmo=500,
	 toggle=false
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

init(Parent, _Bus) ->
    xbus:pub_meta(<<"mixmesh.node.running">>, 
		  [{unit,"bool"},
		   {description, "MixMesh is running."}]),
    xbus:pub_meta(<<"mixmesh.node.activity">>, 
		  [{unit,"bool"},
		   {description, "MixMesh communication."}]),
    {ok, #state { parent=Parent }}.

message_handler(State=#state{activity_tmo=ActivityTmo,
			     parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};

        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply

    after ActivityTmo ->
	    Toggle = not State#state.toggle, %% FIXME: get from nodis
	    publish(Toggle),
	    {noreply, State#state{toggle=Toggle}}
    end.
			
publish(Activity) ->
    xbus:pub(<<"mixmesh.node.running">>, true),
    xbus:pub(<<"mixmesh.node.activity">>, Activity).
