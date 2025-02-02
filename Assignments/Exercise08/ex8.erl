-module(ex8).
-export([
    echo/0,
    filter/1,
    collector/0,
    test_echo/0,
    test_filter/0,
    test_collector/0,
    test_pipeline/0
]).

log_enabled() -> false.

log(Format, Args) ->
    case log_enabled() of
        true -> io:format(Format, Args);
        false -> ok
    end.


echo() ->
    receive
        stop ->
            log("~p: Stopped echo process~n", [self()]),
            ok;
        Msg ->
            log("~p: Echo received message: ~p~n", [self(), Msg]),
            io:format("Echo: ~p\n",[Msg]), echo()
    end.


% 1.
filter(I) -> spawn(fun() -> filter_loop(I, undefined, 0) end).


filter_loop(I, Pid, Count) ->
    log("~p: Filter received message: {I: ~p, Sender: ~p}~n", [self(), I, Pid]),
    receive
        {set_sender, NewPid} ->
            log("~p: {set_sender, ~p}~n", [self(), NewPid]),
            filter_loop(I, NewPid, Count);

        {filter, Msg} ->
            log("~p: {filter, ~p}~n", [self(), Msg]),
            case Pid of
                undefined ->
                    log("~p: ERROR: NO PID SET~n", [self()]);
                _ when Count rem I == (I - 1) -> 
                    log("~p: MESSAGE SUCCESSFULLY SENT TO ~p, Msg = ~p, I = ~p~n", [self(), Pid, Msg, I]),
                    Pid ! {filter, Msg};
                _ ->
                    log("~p: MESSAGE NOT SENT, Msg = ~p, I = ~p~n", [self(), Msg, I])
            end,
            filter_loop(I, Pid, Count + 1);

        stop ->
            log("~p: Stopped filter process: {I: ~p, Sender: ~p}~n", [self(), I, Pid]),
            ok
    end.

% 2.
collector() -> spawn(fun() -> collector_loop([], undefined) end).


collector_loop(List, Pid) ->
    log("~p: Collector received message: {Sender: ~p}~n", [self(), Pid]),
    receive
        {set_sender, NewPid} ->
            log("~p: {set_sender, ~p}~n", [self(), NewPid]),
            collector_loop(List, NewPid);

        reset ->
            log("~p: reset~n", [self()]),
            collector_loop([], Pid);

        {filter, Msg} ->
            log("~p: {filter: ~p}~n", [self(), Msg]),
            NewList = List ++ [Msg],
            Pid ! {filter, NewList},
            collector_loop(NewList, Pid);

        stop -> 
            log("~p: Stopped collector process: {Sender: ~p}~n", [self(), Pid]),
            ok
    end.


test_echo() ->
    
    Echo = spawn(?MODULE, echo,[]),
    
    P2 = Echo,
 
    P2!{filter,120},
    P2!{filter,109},
    P2!{filter,150},
    P2!{filter,101},
    P2!{filter,155},
    P2!{filter,114},
    P2!{filter,189},
    P2!{filter,114},
    P2!{filter,27},
    P2!{filter,121},
    P2!{filter,68},
    P2!{filter,32},
    P2!{filter,198},
    P2!{filter,99},
    P2!{filter,33},
    P2!{filter,104},
    P2!{filter,164},
    P2!{filter,114},
    P2!{filter,212},
    P2!{filter,105},
    P2!{filter,194},
    P2!{filter,115},
    P2!{filter,24},
    P2!{filter,116},
    P2!{filter,148},
    P2!{filter,109},
    P2!{filter,173},
    P2!{filter,97},
    P2!{filter,8},
    P2!{filter,115},
    P2!{filter,191},
    P2!{filter,33},

    ok.

test_filter() ->
    F1 = filter(2),
    F2 = filter(5),
    F3 = filter(1),

    F1!{filter, 0},

    F1!{set_sender, F2},
    F2!{set_sender, F3},

    F1!{filter, 0},
    F1!{filter, 1},
    F1!{filter, 2},
    F1!{filter, 3},
    F1!{filter, 4},
    F1!{filter, 5},
    F1!{filter, 6},
    F1!{filter, 7},
    F1!{filter, 8},
    F1!{filter, 9},
    F1!{filter, 10},
    F1!{filter, 11},

    ok.

test_collector() -> 

    C1 = collector(),
    E1 = spawn(?MODULE, echo,[]),

    C1!{set_sender, E1},

    C1!{filter, 1},
    C1!{filter, b},
    C1!{filter, 3},

    C1!reset,

    C1!{filter, 1},
    C1!{filter, b},
    C1!{filter, 3},

    ok.

% 3.
test_pipeline() ->

    P2 = filter(2),
    C1 = collector(),
    E1 = spawn(?MODULE, echo,[]),

    P2!{set_sender, C1},
    C1!{set_sender, E1},

    P2!{filter,120},
    P2!{filter,109},
    P2!{filter,150},
    P2!{filter,101},
    P2!{filter,155},
    P2!{filter,114},
    P2!{filter,189},
    P2!{filter,114},
    P2!{filter,27},
    P2!{filter,121},
    P2!{filter,68},
    P2!{filter,32},
    P2!{filter,198},
    P2!{filter,99},
    P2!{filter,33},
    P2!{filter,104},
    P2!{filter,164},
    P2!{filter,114},
    P2!{filter,212},
    P2!{filter,105},
    P2!{filter,194},
    P2!{filter,115},
    P2!{filter,24},
    P2!{filter,116},
    P2!{filter,148},
    P2!{filter,109},
    P2!{filter,173},
    P2!{filter,97},
    P2!{filter,8},
    P2!{filter,115},
    P2!{filter,191},
    P2!{filter,33},

    ok.
