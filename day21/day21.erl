-module(day21).
-export([part1/2, part2/2]).
-compile([export_all]).

part1(String, Input) ->
    ok.

part2(String, Input) ->
    ok.

rotate(Lst, right, N) ->
    rotate(Lst, left, length(Lst) - N);
rotate(Lst, left, N) ->
    {Fore, Aft} = lists:split(N, Lst),
    lists:reverse(lists:reverse(Fore) ++
                  lists:reverse(Aft)).

do_operation(_, Input, Tokens, Arg1, Arg2) ->
    Op = list_to_atom(hd(Tokens)),
    do_operation(Op, Input, tl(Tokens), Arg1, Arg2);
do_operation(rotate, Input, Tokens, Arg1, Arg2) ->
    case list_to_atom(hd(Tokens)) of
        based -> do_operation(rotate, Input, Tokens, lists:last(Tokens), Arg2);
        left -> rotate(Input, left,
