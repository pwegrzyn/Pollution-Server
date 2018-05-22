-module(pollution_gen_server).
-behaviour(gen_server).
-compile(export_all).

% user interface
start_link() ->
    InitialMonitor = pollution:createMonitor(),
    gen_server:start_link(
        {local, pollution_gen_server},
        pollution_gen_server,
        InitialMonitor, []).

stop() ->
    gen_server:cast(pollution_gen_server, stop).

getMonitor() ->
    gen_server:call(pollution_gen_server, {getMonitor}).

getStationMean(StIden, Type) ->
    gen_server:call(pollution_gen_server, {getStationMean, StIden, Type}).

getDailyMean(Day, Type) ->
    gen_server:call(pollution_gen_server, {getDailyMean, Day, Type}).

getMaximumGrowthTime(Type) ->
    gen_server:call(pollution_gen_server, {getMaximumGrowthTime, Type}).

addStation(Name, Coords) ->
    gen_server:cast(pollution_gen_server, {addStation, Name, Coords}).

addValue(StIden, Time, Type, Val) ->
    gen_server:cast(pollution_gen_server, {addValue, StIden, Time, Type, Val}).

getOneValue(Type, Time, StIden) ->
    gen_server:call(pollution_gen_server, {getOneValue, Type, Time, StIden}).

crash() ->
    gen_server:cast(pollution_gen_server, {crash}).

% callbacks
init(InitialMonitor) ->
    {ok, InitialMonitor}.

terminate(Reason, Monitor) ->
    io:format("Server: exit with monitor: ~p~n", [Monitor]),
    Reason.

handle_call({getMonitor}, _From, Monitor) ->
    {reply, Monitor, Monitor};
handle_call({getOneValue, Type, Time, StIden}, _From, Monitor) ->
    Found = pollution:getOneValue(Type, Time, StIden),
    {reply, Found, Monitor};
handle_call({getStationMean, StIden, Type}, _From, Monitor) ->
    Found = pollution:getStationMean(Type, StIden, Monitor),
    {reply, Found, Monitor};
handle_call({getDailyMean, Day, Type}, _From, Monitor) ->
    Found = pollution:getDailyMean(Type, Day, Monitor),
    {reply, Found, Monitor};
handle_call({getMaximumGrowthTime, Type}, _From, Monitor) ->
    Found = pollution:getMaximumGrowthTime(Type, Monitor),
    {reply, Found, Monitor}.

handle_cast({addStation, Name, Coords}, Monitor) ->
    UpdatedMonitor = pollution:addStation(Name, Coords, Monitor),
    {noreply, UpdatedMonitor};
handle_cast({crash}, Monitor) ->
    Rip = 1/0,
    {noreply, Monitor};
handle_cast({addValue, StIden, Time, Type, Val}, Monitor) ->
    Res = pollution:addValue(StIden, Time, Type, Val, Monitor),
    case Res of
            {err, nonExistentStation} -> {noreply, Monitor};
            {err, measurementAlreadyExists} -> {noreply, Monitor};
            UpdatedMonitor -> {noreply, UpdatedMonitor}
    end;
handle_cast(stop, Monitor) ->
    {stop, normal, Monitor}.