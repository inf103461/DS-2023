-module(test).
-export([run/1]).

run(RunFor) ->
    Server = server:start(),
    Client1 = client:start(100, "Client(100)", Server),
    Client2 = client:start(1000, "Client(1000)", Server),
    Client3 = client:start(5000, "Client(5000)", Server),

    %Observer = observe(3000, [Server]),
    Observer = observe(3000, [Server, Client1, Client2, Client3]),
    timer:sleep(RunFor * 1000), % Convert seconds to milliseconds

    mylog:log("~n", []),
    Observer!stop,
    Client1!stop,
    Client2!stop,
    Client3!stop,
    Server!stop,
    ok.
    

observe(Interval, Pids) -> spawn(fun() -> observe_loop(Interval, Pids) end).
observe_loop(Interval, Pids) ->
    receive
        stop -> ok
    after Interval ->
        mylog:log("~n", []),
        callShow(Pids),
        observe_loop(Interval, Pids)
    end.


callShow([]) -> ok;
callShow([Pid | Tail]) ->
    Pid!show,
    callShow(Tail).