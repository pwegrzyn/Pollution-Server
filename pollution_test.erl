-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-compil(export_all).
-include("monitor.hrl").

sample_monitor() ->
    #monitor{stations=
        [#station{name="First", location={42,32}, measurements=#{}},
         #station{name="Second", location={1,1}, measurements=#{}},
         #station{name="Third", location={42.3,32.0}, measurements=#{}},
         #station{name="Fifth", location={42.2,32}, measurements=#{}},
         #station{name="Sixth", location={42,15}, measurements=#{}}]}.

sample_monitor2() ->
    #monitor{stations=
        [#station{name="First", location={42,32}, measurements=#{}},
         #station{name="Sixth", location={42,15}, measurements=#{}}]}.

create_monitor_test() ->
    #monitor{stations=[]} = pollution:createMonitor().

add_station_test() ->
    #monitor{stations=
        [#station{name="First", location={42,32}, measurements=#{}}]} 
        = pollution:addStation("First", {42,32}, pollution:createMonitor()).

add_station2_test() ->
    #monitor{stations=
        [#station{name="First", location={42,32}, measurements=#{}},
         #station{name="Second", location={1,1}, measurements=#{}},
         #station{name="Third", location={42.3,32.0}, measurements=#{}},
         #station{name="Fifth", location={42.2,32}, measurements=#{}},
         #station{name="Sixth", location={42,15}, measurements=#{}}]}
         = pollution:addStation("Fifth", {11,12}, sample_monitor()).

add_station3_test() ->
    #monitor{stations=
        [#station{name="First", location={42,32}, measurements=#{}},
         #station{name="Second", location={1,1}, measurements=#{}},
         #station{name="Third", location={42.3,32.0}, measurements=#{}},
         #station{name="Fifth", location={42.2,32}, measurements=#{}},
         #station{name="Sixth", location={42,15}, measurements=#{}}]}
         = pollution:addStation("Seventh", {42,15}, sample_monitor()).

get_station_test() ->
    {found, #station{name="Second", location={1,1}, measurements=#{}}}
    = pollution:getStation({1,1}, sample_monitor()).

get_station2_test() ->
    {found, #station{name="Second", location={1,1}, measurements=#{}}}
    = pollution:getStation("Second", sample_monitor()).

get_station3_test() ->
    {err, notFound} = pollution:getStation("Tenth", sample_monitor()).

get_station4_test() ->
    {err, notFound} = pollution:getStation({100,1}, sample_monitor()).

add_value_test() ->
    #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} := 143}},
         #station{name="First", location={42,32}, measurements=#{}}]}
         = pollution:addValue("Sixth", {{2018,4,9},{22,45,0}}, "PM10", 143, 
            sample_monitor2()).

add_value2_test() ->
    {err, measurementAlreadyExists} = 
    pollution:addValue("Sixth", {{2018,4,9},{22,45,0}}, "PM10", 19, 
        #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} => 143}},
         #station{name="First", location={42,32}, measurements=#{}}]}).

remove_value_test() ->
    #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=#{}},
         #station{name="First", location={42,32}, measurements=#{}}]}
     = pollution:removeValue("Sixth", {{2018,4,9},{22,45,0}},
        "PM10", #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} => 143}},
         #station{name="First", location={42,32}, measurements=#{}}]}).

get_one_value_test() ->
    143 = pollution:getOneValue("PM10", {{2018,4,9},{22,45,0}}, "Sixth",
    #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} => 143}},
         #station{name="First", location={42,32}, measurements=#{}}]}).

get_station_mean_test() ->
    150.0 = pollution:getStationMean("PM10", "Sixth",
        #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} => 200,
              {{{2018,3,9},{23,30,12}}, "PM10"} => 100,
              {{{2017,4,10},{12,31,31}}, "PM5"} => 500}},
         #station{name="First", location={42,32}, measurements=#{}}]}).

get_daily_mean_test() ->
    100.0 = pollution:getDailyMean("PM10", {2018,4,9},
        #monitor{stations=
        [#station{name="Sixth", location={42,15}, measurements=
            #{{{{2018,4,9},{22,45,0}}, "PM10"} => 200,
              {{{2018,4,9},{23,30,12}}, "PM10"} => 0,
              {{{2017,4,10},{12,31,31}}, "PM10"} => 500}},
         #station{name="First", location={42,32}, measurements=#{}}]}).