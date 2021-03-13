%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Keep track on pisuger batter and application status
%%% @end
%%% Created : 19 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(pimesh_battery_serv).

-export([start_link/0, start_link/1]).
-export([get_voltage/1, get_soc/1]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 1000).

-record(state,
	{
	 parent,
	 ip5209,
	 voltage,
	 soc,
	 soc0  %% last reported soc
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(tree_db),
    application:start(xbus),
    application:start(i2c),
    application:start(gpio),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

get_voltage(Serv) ->
    serv:call(Serv, get_voltage).

get_soc(Serv) ->
    serv:call(Serv, get_soc).

init(Parent, Bus) ->
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
    {ok,IP5209}  = i2c_ip5209:open1(Bus),
    V0 = i2c_ip5209:read_voltage(IP5209),
    Charging = i2c_ip5209:is_power_plugged_2led(IP5209),
    SOC0 = i2c_ip5209:parse_voltage_level(V0),
    publish(V0, SOC0, Charging),
    {ok, #state { parent=Parent, ip5209=IP5209, 
		  voltage=V0, soc=SOC0, soc0=SOC0 }}.
		  
message_handler(State=#state{ip5209=IP5209,parent=Parent}) ->
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
	    %% FIXME: handle eremoteio, when ip5209 goes away,
	    %% when USB (power/gadget) is connected at the same
	    %% time as the PiSuer2 cabel is connected!
	    V1 = i2c_ip5209:read_voltage(IP5209),
	    SOC1 = i2c_ip5209:parse_voltage_level(V1),
	    Charging = i2c_ip5209:is_power_plugged_2led(IP5209),
	    SOC0 = if abs(SOC1 - State#state.soc0) > 1.0 ->
			   SOC1;
		      true ->
			   State#state.soc0
		   end,
	    publish(V1, SOC0, Charging),
	    {noreply, State#state{voltage=V1,soc=SOC1, soc0=SOC0 }}
    end.

%% note that we send charging before soc to (possibly) get an updated 
%% view of charge status.
publish(Voltage, Soc, Charging) ->
    xbus:pub(<<"mixmesh.battery.charging">>, Charging),
    xbus:pub(<<"mixmesh.battery.voltage">>, Voltage),
    xbus:pub(<<"mixmesh.battery.soc">>, Soc).


