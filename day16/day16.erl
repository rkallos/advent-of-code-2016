-module(day16).
-export([part1/1, part2/1]).
-compile([export_all]).

part1(Input) ->
    do(Input, 272).

part2(_Input) ->
    ok.

do(Input, Length) when length(Input) > Length ->
    checksum(lists:sublist(Input, Length)).
do(Input, Length) ->
    do(fill(Input), Length).

checksum(Input) when (length(Input) rem 2) =:= 1 ->
    Input;
checksum(Input) ->

fill(Input) ->
    Copy = reverse_and_negate(Input),
    Input ++ Copy.

reverse_and_negate(Input) ->
    reverse_and_negate(Input, []).
reverse_and_negate([], Result) -> Result;
reverse_and_negate([First | Rest], Result) ->
    Char = case First of
               $0 -> $1;
               $1 -> $0
           end,
    reverse_and_negate(Rest, [Char | Result]).
