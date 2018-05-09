-module(pollution_server).
-compile(export_all).
-include("monitor.hrl").

start() ->
    io:format("Spawning a new server instance...~n"),
    register(monitorServer, spawn_link(pollution_server, init, [])).

stop() ->
    io:format("Stopping the server...~n"),
    monitorServer ! {request, self(), stop}.

init() ->
    io:format("Server is starting...~n"),
    InitialMonitor = pollution:createMonitor(),
    loop(InitialMonitor).

 %% main server loop
 loop(Monitor) ->
    receive
        {request, Pid, addStation, Args} ->
            NewMonitor = 
                pollution:addStation(lists:nth(1, Args), lists:nth(2, Args),
                Monitor),
            Pid ! {reply, ok},
            loop(NewMonitor);
        {request, Pid, addValue, Args} ->
            case pollution:addValue(lists:nth(1, Args), lists:nth(2, Args),
            lists:nth(3, Args), lists:nth(4, Args), Monitor) of
                {err, ErrMsg} -> 
                    Pid ! {reply, {err, ErrMsg}},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {reply, ok},
                    loop(NewMonitor)
            end;
        {request, Pid, removeValue, Args} ->
            case pollution:removeValue(lists:nth(1, Args), lists:nth(2, Args),
            lists:nth(3, Args), Monitor) of
                {err, ErrMsg} -> 
                    Pid ! {reply, {err, ErrMsg}},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {reply, ok},
                    loop(NewMonitor)
            end;
        {request, Pid, getOneValue, Args} ->
            Value = 
                pollution:getOneValue(lists:nth(1, Args), lists:nth(2, Args),
                lists:nth(3, Args), Monitor),
            Pid ! {reply, Value},
            loop(Monitor);
        {request, Pid, getStationMean, Args} ->
            StationMean = 
                pollution:getStationMean(lists:nth(1, Args), lists:nth(2, Args),
                Monitor),
            Pid ! {reply, StationMean},
            loop(Monitor);
        {request, Pid, getDailyMean, Args} ->
            DailyMean = 
                pollution:getDailyMean(lists:nth(1, Args), lists:nth(2, Args),
                Monitor),
            Pid ! {reply, DailyMean},
            loop(Monitor);
        {request, Pid, getMaximumGrowthTime, Args} ->
            Found = 
                pollution:getMaximumGrowthTime(lists:nth(1, Args), Monitor),
            Pid ! {reply, Found},
            loop(Monitor);
        {request, Pid, getMonitor, _} ->
            Pid ! {reply, Monitor},
            loop(Monitor);
        {request, Pid, stop} ->
            Pid ! {reply, ok};
        {request, Pid, crash} ->
            1 / 0,
            Pid ! {reply, ok},
            loop(Monitor)
    end.

%% client

call(Message, Args) ->
    monitorServer ! {request, self(), Message, Args},
    receive
        {reply, Reply} -> Reply
    end.

addStation(Name, Coords) ->
    call(addStation, [Name, Coords]).

addValue(StIden, Time, Type, Val) ->
    call(addValue, [StIden, Time, Type, Val]).

removeValue(StIden, Time, Type) ->
    call(removeValue, [StIden, Time, Type]).

getOneValue(Type, Time, StIden) ->
    call(getOneValue, [Type, Time, StIden]).

getStationMean(Type, StIden) ->
    call(getDailyMean, [Type, StIden]).

getDailyMean(Type, Day) ->
    call(getDailyMean, [Type, Day]).

getMaximumGrowthTime(Type) ->
    call(getMaximumGrowthTime, [Type]).

getMonitor() ->
    call(getMonitor, []).

crash() ->
    call(crash, []).
