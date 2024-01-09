-module(ex8).
-export([filter/1]).

filter(I) ->
    spawn(fun() -> loop(I, undefined) end).


loop(I, Pid) ->
    io:format("~p: Received message: {I: ~p, Sender: ~p}~n", [self(), I, Pid]),
    receive
        {set_sender, NewPid} ->
            io:format("~p: {set_sender, ~p}~n", [self(), NewPid]),
            loop(I, NewPid);
        {filter, Msg} ->
            io:format("~p: {filter, ~p}~n", [self(), Msg]),
            case Pid of
                undefined ->
                    io:format("~p: ERROR: NO PID SET~n", [self()]),
                    ok;  % Do nothing if Pid is undefined
                _ when Msg rem I == 0 -> 
                    io:format("~p: MESSAGE SUCCESSFULLY SENT TO ~p~n", [self(), Pid]),
                    Pid ! {filter, Msg};
                _ ->
                    io:format("~p: ERROR: UNKNOWN PID~n", [self()]),
                    ok  % Handle other cases if needed
            end,
            loop(I, Pid)
            %if Msg rem I == 0 andalso Pid =/= undefined -> Pid ! {filter, Msg} end,
    end.
