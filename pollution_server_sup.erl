-module(pollution_server_sup).
-compile(export_all).

init() ->
    pollution_server:start(),
    process_flag(trap_exit, true),
    receive
        {'EXIT', _Worker, normal} -> handle_normal();
        {'EXIT', _Worker, _Reason} -> handle_crash()
    end.

handle_crash() ->
    io:format("The pollution server had crashed. Restarting...~n"),
    init().

handle_normal() ->
    io:format("The pollution server has exited normally.~n"),
    ok.

start_supervisor() ->
    spawn(pollution_server_sup, init, []).