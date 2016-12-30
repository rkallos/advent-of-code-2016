-module(day18).
-export([part1/1, part2/1]).
-compile([export_all]).

part1(FirstRow) ->
    do(FirstRow, 40, 0).

part2(FirstRow) ->
    do(FirstRow, 400000, 0).

do(_, 0, Safe) -> Safe;
do(Row, Remaining, Safe) ->
    SafeInRow = count_safe(Row),
    NextRow = transform(Row),
    do(NextRow, Remaining - 1, Safe + SafeInRow).

transform(Row) ->
    Input = [$.] ++ Row ++ [$.],
    transform(Input, []).

transform([_, _], Result) ->
    lists:reverse(Result);
transform(Input, Result) ->
    I = lists:sublist(Input, 3),
    Char = case I of
               "^^." -> $^;
               ".^^" -> $^;
               "..^" -> $^;
               "^.." -> $^;
               _ -> $.
           end,
    transform(tl(Input), [Char | Result]).


count_safe(String) ->
    count_safe(String, 0).

count_safe([], N) -> N;
count_safe([First | Rest], N) ->
    count_safe(Rest, N + is_safe(First)).

is_safe($^) -> 0;
is_safe($.) -> 1.
