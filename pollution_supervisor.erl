-module(pollution_supervisor).
-behaviour(supervisor).
-compile(export_all).

% user interface
start_link() ->
    supervisor:start_link({local, pollution_supervisor}, pollution_supervisor, []).

% callbacks
init(_InitialValue) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => pollution_server,
                    start => {pollution_gen_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [pollution_gen_server]}],
    {ok, {SupFlags, ChildSpecs}}.