-module(test).
-export([test/0]).

-define(COUNTER_LOG_INTERVAL, 3000).

test() ->
    Counter = startCounter(),
    Counter ! show,

    P1 = bully:start("Proc 1", [], Counter),
    P2 = bully:start("Proc 2", [], Counter),
    P3 = bully:start("Proc 3", [], Counter),
    P4 = bully:start("Proc 4", [], Counter),

    P1 ! {newProcs, [P2, P3, P4]},
    P2 ! {newProcs, [P1, P3, P4]},
    P3 ! {newProcs, [P1, P2, P4]},
    P4 ! {newProcs, [P1, P2, P3]},
    timer:sleep(10000),

    Counter ! show,
    P4 ! stop,
    timer:sleep(10000),

    Counter ! show,
    P3 ! stop,
    timer:sleep(10000),

    Counter ! show,
    P2 ! stop,
    timer:sleep(10000),

    Counter ! show,
    P1 ! stop,
    Counter ! stop,
    ok.



startCounter() -> spawn(fun() -> counter(0) end).

counter(Count) ->
    receive
        message -> counter(Count + 1);

        show ->
            log:log(self(), "Counter", "~p messages sent", [Count]),
            counter(Count);

        stop -> log:log(self(), "Counter", "stop", [])
    end.

