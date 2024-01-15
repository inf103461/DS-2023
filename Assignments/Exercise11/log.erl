-module(log).
-export([log/4, log/2]).

-define(PID_LENGTH, 9).
-define(SOURCE_LENGTH, 7).

% Formatted logging
log(Pid, Source, Entry, Args) ->
    PidStr = io_lib:format("~p", [Pid]),
    PidPad = pad_string(PidStr, ?PID_LENGTH - string:length(PidStr)),
    SourcePad = pad_string(Source, ?SOURCE_LENGTH - string:length(Source)),

    io:format(logtime() ++ ", " ++ PidPad ++ ", " ++ SourcePad ++ ": " ++ Entry ++ "~n", Args).

pad_string(String, Amount) when Amount > 0 -> pad_string(" " ++ String, Amount - 1);
pad_string(String, _) -> String.

logtime() ->
    {_, Secs, _} = erlang:timestamp(),
    io_lib:format("~p", [(Secs rem 1000)]).


% Unformatted logging
log(Format, Args) -> io:format(Format, Args).