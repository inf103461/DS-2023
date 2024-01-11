-module(server).
-export([start/0, tick/2, now/0, micros_to_tuple/1]).


-define(DEFAULT_INTERVAL, 50). % In milliseconds

% Main code
start() ->
    Server = spawn(fun() -> clock(server:now(), undefined) end),
    Ticker = spawn(fun() -> tick(?DEFAULT_INTERVAL, Server) end),
    Server!{ticker, Ticker},
    Server.

tick(Interval, Pid) ->
    receive
        stop -> ok
    after Interval ->
        Pid ! {tick, Interval * 1000}, % Convert to millis
        tick(Interval, Pid)
    end.

clock(Time, Ticker) ->
    receive
        {get, Pid} ->
            Pid ! {time, server:now(), Time, Time}, % t2 and t3 are always the same in this simplistic example, calling now() would be more apropriate
            clock(Time, Ticker);

        show ->
            {_, Sec, Micro} = micros_to_tuple(Time),
            mylog:log(self(), "Server", "~p Sec, ~p Microseconds", [Sec, Micro]),
            clock(Time, Ticker);

        {ticker, NewTicker} -> clock(Time, NewTicker);

        stop ->
            mylog:log(self(), "Server", "stop", []),
            Ticker!stop,
            ok;

        {tick, Delta} ->
            clock(Time + Delta, Ticker)
    end.



-define(MILL, 1000000).
% Returns current time in microseconds
now() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    (MegaSecs * ?MILL * ?MILL) + (Secs * ?MILL) + MicroSecs.


micros_to_tuple(Input) ->
    Rounded = round(Input),
    Mega = Rounded div (?MILL * ?MILL),
    Sec = (Rounded div ?MILL) rem ?MILL,
    Micro = Rounded rem ?MILL,
    {Mega, Sec, Micro}.
