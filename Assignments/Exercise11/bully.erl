-module(bully).
-export([start/3]).

-define(BOSS_PING_INTERVAL, 3000).


start(Name, Others, Counter) -> spawn(fun() -> initBully(Name, Others, Counter) end).

% Initialize process-local varibales and start main loop
initBully(Name, Others, Counter) ->
    put(name, Name),
    put(counter, Counter),
    bully(Others, undefined, false).

% Main loop
bully(Others, Boss, IsElectionRunning) ->
    receive
        {Pid, election} ->
            message(Pid, {{self(), election}, self()}),
            case IsElectionRunning of
                true ->
                    log:log(self(), get(name), "Election already in progress, not started by: ~p", [Pid]),
                    bully(Others, Boss, IsElectionRunning);
                false ->
                    log:log(self(), get(name), "Election started by: ~p", [Pid]),
                    election(Others, Boss)
                end;

        {Pid, boss} ->
            log:log(self(), get(name), "New boss: ~p", [Pid]),
            bully(Others, Pid, IsElectionRunning);

        {Pid, ping} ->
            message(Pid, {{self(), ping}, ok}),
            bully(Others, Boss, IsElectionRunning);

        {newProcs, NewProcs} ->
            bully(addWithoutDups(NewProcs, Others), Boss, IsElectionRunning);

        {removeProc, Pid} ->
            NewOthers = [X || X <- Others, X =/= Pid],
            % ======================================================
            % Switch comments here to demonstrate ping system
            % ======================================================
            case Pid == Boss of
                false -> bully(NewOthers, Boss, IsElectionRunning);
                true -> election(NewOthers, Boss)
            end;
            % bully(NewOthers, Boss, IsElectionRunning);
            % ======================================================

        stop ->
            log:log(self(), get(name), "stop", []),
            lists:map(fun(Pid) -> message(Pid, {removeProc, self()}) end, Others)

    after ?BOSS_PING_INTERVAL ->
        Self = self(),
        case Boss of
            Self ->
                log:log(self(), get(name), "Is boss already", []),
                bully(Others, Boss, false);
            undefined -> election(Others, Boss);
            _ ->
                Response = rpc(Boss, ping),
                case Response of
                    unreachable ->
                        log:log(self(), get(name), "Boss unreachable: ~p", [Boss]),
                        BossRemoved = [X || X <- Others, X =/= Boss],
                        % Remove boss from other processes as well
                        lists:map(fun(Pid) -> message(Pid, {removeProc, Boss}) end, BossRemoved),
                        election(BossRemoved, Boss);
                    _ ->
                        log:log(self(), get(name), "Received ping from ~p (boss), ~p", [Boss, Response]),
                        bully(Others, Boss, false)
                end
        end
    end.


% Starts a new election for this process
election(Others, Boss) -> 
    Highers = lists:filter(fun(PID) -> PID > self() end, Others),
    log:log(self(), get(name), "Started election, messaging: ~p", [Highers]),
    Rivals = rpcList(Highers, election),
    log:log(self(), get(name), "Received rivals list: ~p", [Rivals]),
    case Rivals of
        [] ->
            log:log(self(), get(name), "Elected as boss", []),
            lists:map(fun(Pid) -> message(Pid, {self(), boss}) end, Others),
            bully(Others, self(), true);
        _ ->
            log:log(self(), get(name), "Not elected", []),
            bully(Others, Boss, true)       
    end.


% Adds all itmes of the first list to the second one, does not add duplicates
addWithoutDups([], AddTo) -> AddTo;
addWithoutDups([Curr | Tail], AddTo) ->
    case lists:member(Curr, AddTo) of
        false -> addWithoutDups(Tail, AddTo ++ [Curr]);
        true -> addWithoutDups(Tail, AddTo)
    end.


% Sends a syncronized rpc style message to a list of processes
rpcList([], _) -> [];
rpcList([Pid | Rest], Request) ->
    Response = rpc(Pid, Request),
    case Response of
        unreachable -> rpcList(Rest, Request);
        _ -> [Response | rpcList(Rest, Request)]
    end.


% Sends a syncronized rpc style message and returns the response
rpc(Pid, Request) -> 
    message(Pid, {self(), Request}),
    Pattern = {Pid, Request}, % Make sure that response can be uniquely identified within a process
    receive 
        {Pattern, Response} ->
            Response
        after 250 -> 
            unreachable
    end. 
 
% Added message function to send messages to counter as well
message(Pid, Message) ->
    Pid ! Message,
    get(counter) ! message.