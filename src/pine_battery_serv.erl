%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Keep track on pisuger batter and application status
%%% @end
%%% Created : 19 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(pine_battery_serv).

-export([start_link/0]).
-export([get_voltage/1, get_soc/1]).
%% serv callback
-export([message_handler/1]).
%% test
-export([read_voltage/0]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SYS_BATTERY, "/sys/class/power_supply/axp20x-battery").
-define(VOLTAGE_NOW, filename:join(?SYS_BATTERY, "voltage_now")).
-define(VOLTAGE_MIN, filename:join(?SYS_BATTERY, "voltage_design_min")).
-define(VOLTAGE_MAX, filename:join(?SYS_BATTERY, "voltage_design_max")).

-define(SAMPLE_INTERVAL, 1000).

-record(state,
	{
	 parent,
	 charging,
	 voltage,
	 voltage_min,
	 voltage_max,
	 soc,
	 soc0   %% last reported soc
	}).

start_link() ->
    ?spawn_server(fun(Parent) -> init(Parent) end,
		  fun ?MODULE:message_handler/1).

get_voltage(Serv) ->
    serv:call(Serv, get_voltage).

get_soc(Serv) ->
    serv:call(Serv, get_soc).

init(Parent) ->
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
    Vmin = read_voltage_min(),
    Vmax = read_voltage_max(),
    V0 = read_voltage(),
    Charging = is_power_plugged(),
    SOC0 = parse_voltage_level(V0,Vmin,Vmax),
    {ok, #state { parent=Parent,
		  charging = Charging,
		  voltage_min = Vmin,
		  voltage_max = Vmax,
		  voltage=V0, soc=SOC0, soc0=SOC0 }}.
		  
message_handler(State=#state{parent=Parent}) ->
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
	    V1 = read_voltage(),
	    SOC1 = parse_voltage_level(V1,
				       State#state.voltage_min,
				       State#state.voltage_max),
	    Charging = is_power_plugged(),
	    SOC0 = if abs(SOC1 - State#state.soc0) > 1.0 ->
			   SOC1;
		      true ->
			   State#state.soc0
		   end,
	    xbus:pub(<<"mixmesh.battery.voltage">>, V1),
	    xbus:pub(<<"mixmesh.battery.soc">>, SOC0),
	    xbus:pub(<<"mixmesh.battery.charging">>, Charging),
	    {noreply, State#state{voltage=V1,
				  charging=Charging,
				  soc=SOC1, soc0=SOC0}}
    end.

parse_voltage_level(V, Vmin, Vmax) when Vmin < Vmax ->
    V1 = max(V, Vmin),
    V2 = min(V1, Vmax),
    100*((V2 - Vmin) / (Vmax - Vmin)).

is_power_plugged() ->
    case file:read_file(filename:join(?SYS_BATTERY, "status")) of
	{ok,<<"Charging\n">>} -> true;
	{ok,<<"Discharging\n">>} -> false;
	_ -> error
    end.

read_voltage_min() ->
    read_voltage(?VOLTAGE_MIN).
read_voltage_max() ->
    read_voltage(?VOLTAGE_MAX).

read_voltage() ->
    read_voltage(?VOLTAGE_NOW).

read_voltage(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    try string:to_integer(Bin) of
		{error,_} -> false;
		{V, _} -> V / 1000000
	    catch
		error:_ -> false
	    end;
	{error,_} ->
	    false
    end.
