-module(pollution).
-compile(export_all).
-include("monitor.hrl").

createMonitor() -> 
    Monitor = #monitor{stations=[]},
    Monitor.

addStation(Name, Coords, OldMonitor) ->
    Stations = OldMonitor#monitor.stations,
    FindSame = 
        fun (#station{name=X, location=Y}) -> 
            (X == Name) or (Y == Coords) end,
    case lists:any(FindSame, Stations) of
        true -> 
            OldMonitor;
        false -> 
            NewStation = #station{name=Name, location=Coords, measurements=#{}},
            NewMonitor = #monitor{stations = [NewStation | Stations]},
            NewMonitor
    end.

findStatByCoords({X, Y},[#station{location={X, Y},name=Name, measurements=M}|_]) 
    -> {found, #station{location={X, Y}, name=Name, measurements=M}};
findStatByCoords({X, Y}, [_ | Ss]) 
    -> findStatByCoords({X, Y}, Ss);
findStatByCoords({_, _}, []) 
    -> {err, notFound}.

findStatByName(Name, [#station{location={X, Y}, name=Name, measurements=M}|_]) 
    -> {found, #station{location={X, Y}, name=Name, measurements=M}};
findStatByName(Name, [_ | Ss]) 
    -> findStatByName(Name, Ss);
findStatByName(_, []) 
    -> {err, notFound}.

getStation({X, Y}, Monitor) ->
    Stations = Monitor#monitor.stations,
    findStatByCoords({X, Y}, Stations);
getStation(Name, Monitor) ->
    Stations = Monitor#monitor.stations,
    findStatByName(Name, Stations).

addValue(StIden, Time, Type, Val, Monitor) ->
    Station = getStation(StIden, Monitor),
    case Station of
        {err, notFound} -> 
            {err, nonExistentStation};
        {err, _} -> 
            {err, unknowError};
        {found, Found} ->
            M = Found#station.measurements,
            case catch(maps:get({Time, Type}, M)) of
                {'EXIT', _} ->
                    NewM = M#{{Time, Type} => Val},
                    NewStation = Found#station{measurements=NewM},
                    NotEqual = fun (Rec) -> not (Rec =:= Found) end,
                    W_o_old = lists:filter(NotEqual, Monitor#monitor.stations),
                    NewMonitor = #monitor{stations=[NewStation | W_o_old]},
                    NewMonitor;
                _ ->
                    {err, measurementAlreadyExists}
            end
    end.

removeValue(StIden, Time, Type, Monitor) ->
    Station = getStation(StIden, Monitor),
    case Station of
        {err, notFound} -> 
            {err, nonExistentStation};
        {err, _} -> 
            {err, unknowError};
        {found, Found} ->
            M = Found#station.measurements,
            case catch(maps:get({Time, Type}, M)) of
                {'EXIT', _} ->
                    Monitor;
                _ ->
                    NotEqualMap = fun(K,_) -> not (K == {Time, Type}) end,
                    NewM = maps:filter(NotEqualMap, M),
                    NewStation = Found#station{measurements=NewM},
                    NotEqual = fun (Rec) -> not (Rec =:= Found) end,
                    W_o_old = lists:filter(NotEqual, Monitor#monitor.stations),
                    NewMonitor = #monitor{stations=[NewStation | W_o_old]},
                    NewMonitor
            end
    end.

getOneValue(Type, Time, StIden, Monitor) ->
    Station = getStation(StIden, Monitor),
    case Station of
        {err, notFound} -> 
            {err, nonExistentStation};
        {err, _} -> 
            {err, unknowError};
        {found, Found} ->
            M = Found#station.measurements,
            case catch(maps:get({Time, Type}, M)) of
                {'EXIT', _} ->
                    {err, noMeasurementFound};
                Val ->
                    Val
            end
    end.

getStationMean(Type, StIden, Monitor) ->
    Station = getStation(StIden, Monitor),
    case Station of
        {err, notFound} -> 
            {err, nonExistentStation};
        {err, _} -> 
            {err, unknowError};
        {found, Found} ->
            M = Found#station.measurements,
            CorrectType = fun({_,MType},_) -> MType == Type end,
            OnlyCorrect = maps:values(maps:filter(CorrectType, M)),
            lists:foldl(fun(X,Y)->X + Y end, 0, OnlyCorrect)/length(OnlyCorrect)
    end.

getDailyMean(Type, Day, Monitor) ->
    CorrectType = fun({{MDay, _}, MType},_) -> 
                  ((MDay == Day) and (MType == Type)) end,
    Summarize = fun(#station{measurements=M}) -> 
                {lists:foldl(fun(X,Y) -> X+Y end, 0, 
                maps:values(maps:filter(CorrectType, M))), 
                length(maps:values(maps:filter(CorrectType, M)))} end,
    Mapped = lists:map(Summarize, Monitor#monitor.stations),
    {ValSum, LenSum} = lists:foldl(fun({X1,Y1}, {X2,Y2}) -> 
                       {X1+X2,Y1+Y2} end, {0,0}, Mapped),
    ValSum / LenSum.

getMaximumGrowthTime(Type, Monitor) ->
    CorrectType = fun({_,MType},_) -> MType == Type end,
    M = lists:flatten(lists:map( fun(#station{measurements=M}) -> 
        maps:to_list(maps:filter(CorrectType, M)) end,
        Monitor#monitor.stations)),
    {Min, Max} = go(M, maps:new(), maps:new()),
    MaxGrowths = generateMaxGrowths(maps:keys(Min), Min, Max, maps:new()),
    fst(max2(maps:to_list(MaxGrowths))).
    
go([], Min, Max) ->
    {Min, Max};
go([H | T], Min, Max) ->
    NewMin = updateMin(H, Min),
    NewMax = updateMax(H, Max),
    go(T, NewMin, NewMax).

updateMax({{{Day, {Hour, _, _}}, _}, Value}, Max) ->
        case (maps:is_key({Hour, Day}, Max)) of
            true -> case (Value >= maps:get({Hour, Day}, Max)) of 
                        true -> maps:update({Hour, Day}, Value, Max);
                        false -> Max
                    end;
            false -> maps:put({Hour, Day}, Value, Max)
        end.

updateMin({{{Day, {Hour, _, _}}, _}, Value}, Min) ->
        case (maps:is_key({Hour, Day}, Min)) of
            true -> case (Value =< maps:get({Hour, Day}, Min)) of 
                        true -> maps:update({Hour, Day}, Value, Min);
                        false -> Min
                    end;
            false -> maps:put({Hour, Day}, Value, Min)
        end.

generateMaxGrowths([], _, _, Curr) -> Curr;
generateMaxGrowths(_, #{}, _, _) -> notFound;
generateMaxGrowths(_, _, #{}, _) -> notFound;
generateMaxGrowths([H | T], Min, Max, Curr) ->
    generateMaxGrowths(T, Min, Max, maps:put(H, 
        abs(maps:get(H, Max) - maps:get(H, Min)), Curr)).

max2([H|T]) -> max2(T, H).
max2([{Fst_H,Snd_H}|T],{_,Snd_MAX}) when Snd_H>Snd_MAX->max2(T, {Fst_H,Snd_H});
max2([_|T], Max) -> max2(T, Max);
max2([], Max) -> Max.

fst({X,_}) -> X.

snd({_,Y}) -> Y.