-module(pollution_server_test).
-include_lib("eunit/include/eunit.hrl").
-compil(export_all).
-include("monitor.hrl").

start_test() ->
    true = pollution_server:start().

addStation_test() ->
    ok = pollution_server:addStation("First", {12,23}).

addValue_test() ->
    {err, nonExistentStation} = 
        pollution_server:addValue("Second", erlang:localtime(), "PM10", 100).

addValue_2_test() ->
    ok = 
        pollution_server:addValue("First", {{2018,4,9},{22,45,0}}, "PM10", 100).

removeValue_test() ->
    {err, nonExistentStation} = 
        pollution_server:removeValue("Second", erlang:localtime(), "PM10").

getOneValue_test() ->
    100 =
        pollution_server:getOneValue("PM10", {{2018,4,9},{22,45,0}}, "First").

getStationMean_test() ->
    100 =
        pollution_server:getOneValue("PM10", "First").

getDailyMean_test() ->
    100 =
        pollution_server:getOneValue("PM10", {2018,4,9}).