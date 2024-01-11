-module(client).
-export([start/3]).

-define(SELF_ADJUST_INTERVAL, 4000000). % In microseconds

% Main code
start(Interval, Name, ServerPid) ->
    Time = server:now(),
    Client = spawn(fun() -> clock(Time, ServerPid, undefined, Name, 0) end),
    Ticker = spawn(fun() -> server:tick(Interval, Client) end),
    Client!{ticker, Ticker},
    Client.

clock(Time, ServerPid, Ticker, Name, AdjustTimer) ->
    case AdjustTimer >= ?SELF_ADJUST_INTERVAL of
        true -> adjust(ServerPid, Ticker, Name);
        false -> 
            receive
                adjust -> adjust(ServerPid, Ticker, Name);

                show ->
                    {_, Sec, Micro} = server:micros_to_tuple(Time),
                    mylog:log(self(), Name, "~p Sec, ~p Microseconds", [Sec, Micro]),
                    clock(Time, ServerPid, Ticker, Name, AdjustTimer);

                {ticker, NewTicker} -> clock(Time, ServerPid, NewTicker, Name, AdjustTimer);

                stop ->
                    mylog:log(self(), Name, "stop", []),
                    Ticker!stop,
                    ok;

                {tick, Delta} -> clock(Time + Delta, ServerPid, Ticker, Name, AdjustTimer + Delta)
            end
    end.


adjust(ServerPid, Ticker, Name) ->
    T1 = server:now(),
    ServerPid ! {get, self()},
    receive
        {time, C_UTC, T2, T3} ->
            NewTime = C_UTC + ((T2 - T1) + (server:now() - T3)) / 2,
            {_, Sec, Micro} = server:micros_to_tuple(NewTime),
            mylog:log(self(), Name, "Adjusting time to: ~p Sec, ~p Microseconds", [Sec, Micro]),
            clock(NewTime, ServerPid, Ticker, Name, 0)
    end.