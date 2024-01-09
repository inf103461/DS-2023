-module(clock).
-export([start/1, get/1, tick/2, test_clock/0]).

-define(MAX_INT, 999999999).

% Logging
log_enabled() -> true.

log(Format, Args) ->
    case log_enabled() of
        true -> io:format(Format, Args);
        false -> ok
    end.


start(Interval) ->
    log("~p,   MAIN: Starting clock(~p)~n", [self(), Interval]),
    spawn(fun() -> clock(0, Interval, 0) end).


clock(Time, Interval, PauseInterval) ->
    log("~p,  CLOCK: New clock cycle, Time: ~p~n", [self(), Time]),
    receive
        {set, Value} ->
            log("~p,  CLOCK: {set, ~p}~n", [self(), Value]),
            clock(Value, Interval, PauseInterval);

        {get, Pid} ->
            log("~p,  CLOCK: {get, ~p}~n", [self(), Pid]),
            Pid ! {clock, Time};

        pause ->
            log("~p,  CLOCK: pause~n", [self()]),
            clock(Time, ?MAX_INT, Interval);

        resume ->
            log("~p,  CLOCK: resume~n", [self()]),
            clock(Time, PauseInterval, 0);

        stop ->
            log("~p,  CLOCK: stop~n", [self()]),
            ok;
        tick ->
            log("~p,  CLOCK: received tick~n", [self()]),
            clock(Time + Interval, Interval, PauseInterval)
    after Interval -> clock(Time + Interval, Interval, PauseInterval)
    end.

get(Pid) ->
    log("~p, GETTER: {get, ~p}~n", [self(), Pid]),
    Pid ! {get, self()},
    receive
        {clock, Time} -> Time
    end.

% Disadvantage: Runtime of code in receive-block influences clock time.
% Also, receiving in the middle of a cycle will immediately cause a new
% cycle with the same time value to start, increasing time of the new
% cycle by the remainder of the last.
tick(Interval, Pid) ->
    log("~p,   TICK: sending tick~n", [self()]),
    Pid ! tick,
    timer:sleep(Interval),
    tick(Interval, Pid).



test_clock() ->
    Clock = start(1000),
    timer:sleep(5500),
    Clock!pause,
    timer:sleep(5500),
    Clock!resume,
    timer:sleep(5500),
    Clock!{set, 1000000000},
    timer:sleep(5500),
    Clock!stop,
    ok.