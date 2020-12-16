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
    KeyServ =
	{pimesh_key_serv, {pimesh_key_serv, start_link, []},
	 permanent, 5000, worker, [pimesh_key_serv]},
    BatteryServ = 
	{pimesh_battery_serv, {pimesh_battery_serv, start_link, []},
	 permanent, 5000, worker, [pimesh_battery_serv]},
    GpsServ = 
	{pimesh_gps_serv, {pimesh_gps_serv, start_link, []},
	 permanent, 5000, worker, [pimesh_gps_serv]},

    {ok,{{one_for_all,3,5}, [AppServ,KeyServ,BatteryServ,GpsServ]}}.


