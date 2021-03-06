-module(pimesh_sup).

-behaviour(supervisor).

%% external exports
-export([start_link/0, start_link/1, stop/0]).

%% supervisor callbacks
-export([init/1]).

start_link(Args) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).


stop() ->
    exit(normal).


init(_Args) ->
    AppServ =
	{pimesh_app_serv, {pimesh_app_serv, start_link, []},
	 permanent, 5000, worker, [pimesh_app_serv]},
    ServList = case config:lookup([system,hardware], none) of
		   pimesh ->
		       [{pimesh_gps_serv, {pimesh_gps_serv, start_link, []},
			 permanent, 5000, worker, [pimesh_gps_serv]},

			{pimesh_battery_serv, 
			 {pimesh_battery_serv, start_link, []},
			 permanent, 5000, worker, [pimesh_battery_serv]},

			{pimesh_serv, {pimesh_serv, start_link, []},
			 permanent, 5000, worker, [pimesh_serv]}];
		   epxmesh ->
		       [{sys_battery_serv, 
			 {sys_battery_serv, start_link, []},
			 permanent, 5000, worker, [sys_battery_serv]},

			{epxmesh_serv, {epxmesh_serv, start_link, []},
			 permanent, 5000, worker, [epxmesh_serv]}];
		   none ->
		       []
	       end,
    {ok,{{one_for_all,3,5}, [AppServ|ServList]}}.
