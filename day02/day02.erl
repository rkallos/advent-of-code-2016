-module(day02).
-export([part1/1, part2/1]).

part1(Input) ->
    Lines = common:read_input(Input, "\n"),
    [get_digit(Line) || Line <- Lines].

get_digit(Line) ->
    get_digit(Line, 5).
get_digit([], State) -> State;
get_digit([$U | Rest], State) ->
    case lists:member(State, [1,2,3]) of
        true -> get_digit(Rest, State);
        false -> get_digit(Rest, State - 3)
    end;
get_digit([$D | Rest], State) ->
    case lists:member(State, [7,8,9]) of
        true -> get_digit(Rest, State);
        false -> get_digit(Rest, State + 3)
    end;
get_digit([$L | Rest], State) ->
    case lists:member(State, [1,4,7]) of
        true -> get_digit(Rest, State);
        false -> get_digit(Rest, State - 1)
    end;
get_digit([$R | Rest], State) ->
    case lists:member(State, [3,6,9]) of
        true -> get_digit(Rest, State);
        false -> get_digit(Rest, State + 1)
    end.


part2(Input) ->
    Lines = common:read_input(Input, "\n"),
    {_, Code} = lists:foldl(fun get_digit2/2, {5, []}, Lines),
    [io:format("~.16B", [Digit]) || Digit <- Code],
    io:format("~n").


get_digit2([], {Num, Nums}) -> {Num, Nums ++ [Num]};
get_digit2([Dir | Rest], {State, Nums}) ->
    Map = part2_map(Dir),
    case maps:is_key(State, part2_map(Dir)) of
        true -> get_digit2(Rest, {maps:get(State, Map), Nums});
        false -> get_digit2(Rest, {State, Nums})
    end.

part2_map($U) ->
    #{3 => 1,
      6 => 2,
      7 => 3,
      8 => 4,
      10 => 6,
      11 => 7,
      12 => 8,
      13 => 11};
part2_map($D) -> reverse_map(part2_map($U));

part2_map($R) ->
    #{2 => 3,
      3 => 4,
      5 => 6,
      6 => 7,
      7 => 8,
      8 => 9,
      10 => 11,
      11 => 12};
part2_map($L) -> reverse_map(part2_map($R)).

reverse_map(Map) ->
    Tuples = maps:to_list(Map),
    Reversed = [{B, A} || {A, B} <- Tuples],
    maps:from_list(Reversed).
