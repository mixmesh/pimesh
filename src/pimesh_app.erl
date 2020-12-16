-module(pimesh_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([config_change/3]).

%% Exported: start

start(_Type, _StartArgs) ->
    pimesh_sup:start_link([]).

%% Exported: stop
stop(_State) ->
    ok.

config_change(Changed,New,Removed) ->
    pimesh:config_change(Changed,New,Removed).
