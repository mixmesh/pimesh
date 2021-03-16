%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    start the app monitor
%%% @end
%%% Created :  4 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_app_serv).

-export([start_link/0, start_link/1]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-record(state,
	{
	 parent,
	 activity=0,
	 activity_tmo=1000,
	 last_ping_count = 0
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
    NPing = nodis:read_node_counter(ping),
    {ok, #state { parent=Parent, last_ping_count = NPing }}.

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
	    NPing = nodis:read_node_counter(ping),
	    NUp = nodis:read_node_counter(up),
	    %% DPing = NPing - State#state.last_ping_count,
	    Activity =
		if NUp =:= 0 ->
			0;
		   true ->
			max(100, trunc(1000 - (math:log2(NUp*8)*100)))
		end,
	    publish(Activity),
	    {noreply, State#state{last_ping_count=NPing}}
    end.
			
publish(Activity) ->
    xbus:pub(<<"mixmesh.node.running">>, true),
    xbus:pub(<<"mixmesh.node.activity">>, Activity).
