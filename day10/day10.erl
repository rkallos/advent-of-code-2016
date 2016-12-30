-module(day10).
-export([part1/1, part2/1]).
-compile([export_all]).

-record(state, {
          bots = [] :: #{integer() => {integer(), integer()}},
          gives = [] :: #{integer() => {integer(), integer()}}
         }).

part1(File) ->
    Lines = common:scan_and_parse(File, day10_lexer, day10_parser),
    {Gives, Values} = lists:partition(fun is_give/1, Lines),
    State = #state{bots = maps:new(), gives = maps:new()},
    State2 = populate_values(State, Values),
    State3 = populate_gives(State2, Gives),
    Target = {61, 17},
    find_bot(Target, State3),
    State3.

part2(_File) ->
    ok.

is_give({give, _, _, _}) -> true;
is_give(_) -> false.

populate_values(State = #state{bots = Bots}, []) ->
    NewBots = maps:fold(
                fun(K, V, Acc) ->
                        Val = list_to_tuple(lists:reverse(lists:sort(V))),
                        maps:put(K, Val, Acc)
                end, Bots, Bots),
    State#state{bots = flip_map(NewBots)};
populate_values(State = #state{bots = Bots},
               [{to, {value, Val}, {bot, Key}} | Rest]) ->
    Bot = [Val | maps:get(Key, Bots, [])],
    NewBots = maps:put(Key, Bot, Bots),
    populate_values(State#state{bots = NewBots}, Rest).

populate_gives(State, []) -> State;
populate_gives(State = #state{gives = Gives},
               [{give, {bot, From}, LoDest, HiDest} | Rest]) ->
    Give = {LoDest, HiDest},
    NewGives = maps:put(From, Give, Gives),
    populate_gives(State#state{gives = NewGives}, Rest).

find_bot(Target, State = #state{bots = Bots}) ->
    ok.

flip_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
                      maps:put(V, K, Acc)
              end, Map, Map).
