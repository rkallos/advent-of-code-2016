-module(day05).
-export([part1/1, part2/1]).

part1(String) ->
    crack_password(String, 8).

part2(String) ->
    crack_password2(String, 8).

crack_password(String, Length) ->
    crack_password(String, Length, 0, []).
crack_password(_String, 0, _Index, Result) ->
    lists:flatten(lists:map(fun(C) -> io_lib:format("~.16B", [C]) end,
                            lists:reverse(Result)));
crack_password(String, Length, Index, Result) ->
    Concat = string:concat(String, integer_to_list(Index)),
    case parse_hash(erlang:md5(Concat)) of
        false -> crack_password(String, Length, Index + 1, Result);
        Char -> io:format("Found character: ~.16B~n", [Char]),
                crack_password(String, Length - 1, Index + 1, [Char | Result])
    end.

crack_password2(String, Length) ->
    crack_password2(String, Length, Length, 0, maps:new()).
crack_password2(_String, _, 0, _Index, Result) ->
    Sorted = lists:sort(maps:to_list(Result)),
    [io:format("~.16B", [Ch]) || {_, Ch} <- Sorted],
    io:format("~n");
crack_password2(String, InitLength, Length, Index, Result) ->
    Concat = string:concat(String, integer_to_list(Index)),
    case parse_hash2(erlang:md5(Concat)) of
        false -> crack_password2(String, InitLength, Length, Index + 1, Result);
        {Pos, Char} -> case not(maps:is_key(Pos, Result)) andalso Pos < InitLength of
                           true ->
                               io:format("Found character: ~.16B for Pos ~p~n", [Char, Pos]),
                               crack_password2(String, InitLength, Length - 1, Index + 1,
                                               maps:put(Pos, Char, Result));
                           false ->
                               crack_password2(String, InitLength, Length, Index + 1, Result)
                       end
    end.

parse_hash(<<0:4, 0:4, 0:4, 0:4, 0:4, Char:4, _Rest/bitstring>>) ->
    Char;
parse_hash(_) -> false.

parse_hash2(<<0:4, 0:4, 0:4, 0:4, 0:4, Pos:4, Char:4, _Rest/bitstring>>) ->
    {Pos, Char};
parse_hash2(_) -> false.
