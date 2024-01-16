-module(clock).
-export([start/1, get/1, tick/2, test_all/0, test_clock/0, test_get/0, test_tick/0]).

% Logging
log_enabled() -> true.

-define(PID_LENGTH, 9).
-define(SOURCE_LENGTH, 5).

% Formatted logging
log(Pid, Source, Entry, Args) ->
    case log_enabled() of
        true ->
            PidStr = io_lib:format("~p", [Pid]),
            PidStrPad = pad_string(PidStr, ?PID_LENGTH - string:length(PidStr)),
            SourceStr = pad_string(Source, ?SOURCE_LENGTH - string:length(Source)),

            io:format(PidStrPad ++ ", " ++ SourceStr ++ ": " ++ Entry ++ "~n", Args);
        false -> ok
    end.

pad_string(String, Amount) when Amount > 0 -> pad_string(" " ++ String, Amount - 1);
pad_string(String, _) -> String.


% Unformatted logging
log(Format, Args) ->
    case log_enabled() of
        true -> io:format(Format, Args);
        false -> ok
    end.


% Tick config
enable_tick(Enabled) when is_boolean(Enabled) ->
    erlang:put(enable_tick, Enabled).

tick_enabled() ->
    case erlang:get(enable_tick) of
        undefined -> false; % Default value
        Value -> Value
    end.


% Main code
start(Interval) ->
    log(self(), "MAIN", "Starting clock(~p)", [Interval]),
    case tick_enabled() of
        false ->
            Clock = spawn(fun() -> clock(0, Interval, false) end),
            {Clock, undefined};
        true -> 
            Clock = spawn(fun() -> clock_ticked(0, false) end),
            log(self(), "MAIN", "Starting ticker(~p)", [Interval]),
            Ticker = spawn(fun() -> tick(Interval, Clock) end),
            {Clock, Ticker}
    end.


clock(Time, Interval, Paused) ->
    log(self(), "CLOCK", "New clock cycle, Time: ~p", [Time]),
    receive
        {set, Value} ->
            log(self(), "CLOCK", "{set, ~p}", [Value]),
            clock(Value, Interval, Paused);

        {get, Pid} ->
            log(self(), "CLOCK", "{get, ~p}", [Pid]),
            Pid ! {clock, Time},
            clock(Time, Interval, Paused);

        pause ->
            log(self(), "CLOCK", "pause", []),
            clock(Time, Interval, true);

        resume ->
            log(self(), "CLOCK", "resume", []),
            clock(Time, Interval, false);

        stop ->
            log(self(), "CLOCK", "stop", []),
            ok

    after Interval ->
        case Paused of
            true -> clock(Time, Interval, Paused);
            false -> clock(Time + Interval, Interval, Paused)
        end
    end.


get(Pid) ->
    log(self(), "MAIN", "{get, ~p}", [Pid]),
    Pid ! {get, self()},
    receive
        {clock, Time} -> log(self(), "MAIN", "Received time: ~p", [Time]),
        Time
    end.

% Disadvantage: Runtime of code in receive-block influences clock time.
% Also, receiving in the middle of a cycle will immediately cause a new
% cycle with the same time value to start, increasing time of the new
% cycle by the remainder of the last.
tick(Interval, Pid) ->
    log(self(), "TICKER", "sending tick", []),
    Pid ! {tick, Interval},
    receive
        stop ->
            log(self(), "TICKER", "stop", []),
            ok
    after Interval -> tick(Interval, Pid)
    end.

clock_ticked(Time, Paused) ->
        log(self(), "CLOCK", "New clock cycle, Time: ~p", [Time]),
    receive
        {set, Value} ->
            log(self(), "CLOCK", "{set, ~p}", [Value]),
            clock_ticked(Value, Paused);

        {get, Pid} ->
            log(self(), "CLOCK", "{get, ~p}", [Pid]),
            Pid ! {clock, Time},
            clock_ticked(Time, Paused);

        pause ->
            log(self(), "CLOCK", "pause", []),
            clock_ticked(Time, true);

        resume ->
            log(self(), "CLOCK", "resume", []),
            clock_ticked(Time, false);

        stop ->
            log(self(), "CLOCK", "stop", []),
            ok;

        {tick, TimeSinceLast} ->
            log(self(), "CLOCK", "received tick", []),
            case Paused of
                true -> clock_ticked(Time, Paused);
                false -> clock_ticked(Time + TimeSinceLast, Paused)
            end
    end.


% Testing (make sure logging is enabled)
-define(TEST_CLOCK_INTERVAL, 1000).
-define(TEST_DEFAULT_WAIT, 3500).
-define(TEST_WAIT_INBETWEEN, 2000).

test_all() -> 
    log("~nRUNNING ALL TESTS: ~n", []),
    
    log("====================== ~n~n", []),
    log("RUNNING CLOCK TESTS: ~n", []),
    log("================================================= ~n", []),
    test_clock(),
    timer:sleep(?TEST_WAIT_INBETWEEN),

    log("================================================= ~n~n", []),
    log("RUNNING GET TESTS: ~n", []),
    log("================================================= ~n", []),
    test_get(),
    timer:sleep(?TEST_WAIT_INBETWEEN),

    log("================================================= ~n~n", []),
    log("RUNNING TICK TESTS: ~n", []),
    log("================================================= ~n", []),
    test_tick(),
    timer:sleep(?TEST_WAIT_INBETWEEN),

    log("================================================= ~n", []),
    enable_tick(false),
    ok.

test_clock() ->
    enable_tick(false),
    {Clock, _} = start(?TEST_CLOCK_INTERVAL),
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!pause,
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!resume,
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!{set, 10000000},
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!stop,
    ok.

test_get() ->
    enable_tick(false),
    {Clock, _} = start(?TEST_CLOCK_INTERVAL),
    timer:sleep(?TEST_DEFAULT_WAIT),
    Time = clock:get(Clock), % specify module because get() is ambiguous
    log(self(), "MAIN", "Extracted time: ~p", [Time]),
    timer:sleep(?TEST_DEFAULT_WAIT),
    Clock!stop,
    ok.

test_tick() ->
    enable_tick(true),
    {Clock, Ticker} = start(?TEST_CLOCK_INTERVAL),
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!pause,
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!resume,
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!{set, 10000000},
    timer:sleep(?TEST_DEFAULT_WAIT),

    Clock!stop,
    Ticker!stop,
    ok.