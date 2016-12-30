-module(day14).
-export([part1/1, part2/1]).
-compile([export_all]).

part1(String) ->
    Table = ets:new(memo, [set, {keypos,1}, public]),
    solve(String, 0, 64, Table, part1).

solve(_, Index, 0, _, _) ->
    Index - 1;
solve(String, Index, Remaining, Memo, Part) ->
    MD5 = md5(String, Index, Part),
    Sub = case check_three(MD5) of
              false -> 0;
              Char -> case check_hash(String, Index, Char, Memo) of
                          true -> 1;
                          false -> 0
                      end
          end,
    solve(String, Index + 1, Remaining - Sub, Memo, Part).

part2(String) ->
    Table = ets:new(memo, [set, {keypos,1}, public]),
    solve(String, 0, 64, Table, part2).

md5(String, Index) ->
    md5(io_lib:format("~s~b", [String, Index])).
md5(String, Index, part1) ->
    md5(io_lib:format("~s~b", [String, Index]));
md5(String, Index, part2) ->
    Str = io_lib:format("~s~b", [String, Index]),
    md5_2016(Str, 2016).
md5(String) ->
    md5_fmt(erlang:md5(String)).

md5_2016(Str, 0) -> md5(Str);
md5_2016(Str, Rem) ->
    md5_2016(md5(Str), Rem - 1).


md5_fmt(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).

check_three(String) ->
    case re:run(String, "(.)\\1\\1") of
        {match, [{Idx, _} | _]} -> lists:nth(Idx + 1, String);
        _ -> false
    end.

check_hash(String, Index, Char, Memo) ->
    Keys = [N || N <- lists:seq(Index + 1, Index + 1000)],
    lists:any(fun(Key) ->
                      Hash = case ets:lookup(Memo, Key) of
                                 [] -> Val = md5(String, Key),
                                       ets:insert(Memo, {Key, Val}),
                                       Val;
                                 [{Key, Val}] -> Val
                             end,
                      has_five(Hash, Char)
              end, Keys).

has_five(String, Char) ->
    Regex = string:chars(Char, 5),
    case re:run(String, Regex) of
        {match, _} -> true;
        _ -> false
    end.
