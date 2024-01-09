-module(ex7).
-export([convert/2, maxitem/1, diff/3, verify_diff/0]).


% Constants
-define(CM_TO_INCH, 0.3937007874015748).
-define(INCH_TO_CM, 2.54).
-define(MIN_INTEGER, -9223372036854775808).

% a)
% Convert between inches anc centimeters
convert(Amount, Unit) ->
    case Unit of
        inch    -> {cm, Amount * ?INCH_TO_CM};
        cm      -> {inch, Amount * ?CM_TO_INCH}
    end.


% b/c)
% Find the maximum value in a list of integers
maxitem([]) -> 0;
maxitem(List) -> maxitem2(List, ?MIN_INTEGER).


% Helper function for maxitem() to allow for recursion
maxitem2(([Elem | Tail]), Max) when Elem > Max ->
    io:format("Current element: ~B, Current max: ~B~n", [Elem, Max]),
    maxitem2(Tail, Elem);

maxitem2(([_ | Tail]), Max) ->
    io:format("Ignoring element, Current max still: ~B~n", [Max]),
    maxitem2(Tail, Max);

maxitem2([], Max) ->
    io:format("Reached end of the list. Final max: ~B~n", [Max]),
    Max.



% d)

% Calculates the differentail of F at point X using environment H
diff(F, X, H) -> (F(X + H) - F(X - H)) / (2*H).

% Verification function for diff() function
test_fun1(X) -> 2*X*X*X - 12*X + 3.
verify_diff() ->
    X = 3,
    H = 1.0e-10,
    Result = diff(fun test_fun1/1, X, H),
    io:format("Result: ~p~n", [Result]).
