-module(day06).
-export([part1/1, part2/1]).

part1(Input) ->
    Lines = common:read_input(Input, "\n"),
    Length = length(hd(Lines)),
    Maps = lists:foldl(fun process_line/2,
                       create_maps(Length), Lines),
    [get_most_common(maps:get(Index, Maps)) || Index <- lists:seq(1, Length)].

part2(Input) ->
    Lines = common:read_input(Input, "\n"),
    Length = length(hd(Lines)),
    Maps = lists:foldl(fun process_line/2,
                       create_maps(Length), Lines),
    [get_least_common(maps:get(Index, Maps)) || Index <- lists:seq(1, Length)].


process_line(Line, Map) ->
    process_line(Line, 1, Map).
process_line([], _Index, Map) ->
    Map;
process_line([Char | Rest], Index, MapOfMaps) ->
    Map = maps:get(Index, MapOfMaps),
    Map2 = maps:update_with(Char, fun update_fun/1, 1, Map),
    process_line(Rest, Index+1, maps:update(Index, Map2, MapOfMaps)).

update_fun(Val) ->
    Val + 1.

create_maps(Length) ->
    create_maps(Length, maps:new()).
create_maps(0, Map) ->
    Map;
create_maps(Length, Map) ->
    create_maps(Length - 1, maps:put(Length, maps:new(), Map)).


get_most_common(Map) ->
    {Char, _} = hd(lists:sort(fun dec/2, maps:to_list(Map))),
    Char.

dec({_KeyA, ValA}, {_KeyB, ValB}) ->
    ValB < ValA.

get_least_common(Map) ->
    {Char, _} = hd(lists:sort(fun inc/2, maps:to_list(Map))),
    Char.

inc({_KeyA, ValA}, {_KeyB, ValB}) ->
    ValB > ValA.
