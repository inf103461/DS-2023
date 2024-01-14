-module(bully).
-export([]).

-define(BOSS_PING_INTERVAL, 5000).


start(Name, Others) ->
    put(name, Name),
    spawn(fun() -> bully(Others, undefined) end).

bully(Others, Boss) ->
    receive
        {Pid, election} ->
            log:log(self(), get(name), "Election started by: ~p", [Pid]),
            Pid ! self(),
            election(Others);

        {Pid, boss} ->
            log:log(self(), get(name), "New boss: ~p", [Pid]),
            bully(Others, Pid);

        {Pid, ping} -> Pid ! ok;

        {newProcs, NewProcs} -> bully(Others ++ NewProcs, Boss);

        {removeProc, Pid} ->
            NewOthers = [X || X <- Others, X =/= Pid],
            case Pid == Boss of
                false -> bully(NewOthers, Boss);
                true -> election(NewOthers)
            end;

        stop ->
            log:log(self(), get(name), "stop", []),
            rpcList(Others, {removeProc, self()})

    after ?BOSS_PING_INTERVAL ->
        case Boss of
            self() -> bully(Others, Boss);
            undefined -> election(Others);
            _ ->
                Response = rpc(Boss, ping),
                case Response of
                    unreachable ->
                        BossRemoved = [X || X <- Others, X =/= Boss],
                        election(BossRemoved);
                    _ -> bully(Others, Boss)
                end
        end
    end.


election(Others) -> 
    Highers = lists:filter(fun(PID) -> PID > self() end, Others),
    log:log(self(), get(name), "Started election, messaging: ~p", [Highers]),
    Rivals = rpcList(Highers, election),
    case Rivals of
        [] -> boss(Others);
        _ -> bully(Others, undefined);
    end.


boss(Others) ->
    log:log(self(), get(name), "Elected as boss", []),
    rpcList(Others, boss),
    bully(Others, self()).


rpcList([], _) -> [];
rpcList([Pid | Rest], Request) ->
    Response = rpc(Pid, Request),
    case Response of
        unreachable -> rpcList(Rest, Request);
        _ -> Response ++ rpcList(Rest, Request)
    end.


rpc(Pid, Request) -> 
    Pid ! {self(), Request}, 
    receive 
        {Pid, Response} -> 
            Response
        after 250 -> 
            unreachable
    end. 
 