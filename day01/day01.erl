-module(day01).
-export([part1/1, part2/1]).

part1(Input) ->
    Data = common:read_input(Input, ", \n"),
    parse(Data, {0, 0}, {0, 1}).

part2(Input) ->
    Data = common:read_input(Input, ", \n"),
    parse_and_remember(Data, {0, 0}, {0, 1}, []).

parse([[Dir | Digits] | Rest], {X, Y}, {Dx, Dy}) ->
    {Dx2, Dy2} = turn(Dir, {Dx, Dy}),
    {Num, _} = string:to_integer(Digits),
    {DistX, DistY} = {Num * Dx2, Num * Dy2},
    parse(Rest, {X + DistX, Y + DistY}, {Dx2, Dy2});
parse([], {X, Y}, _) -> {X, Y}.

turn($R, {X, Y}) ->
    case {X, Y} of
        {1, 0} -> {0, 1};
        {0, 1} -> {-1, 0};
        {-1, 0} -> {0, -1};
        {0, -1} -> {1, 0}
    end;
turn($L, {X, Y}) ->
    turn($R, {-X, -Y}).

parse_and_remember([[Dir | Digits] | Rest], {X, Y}, {Dx, Dy}, Hist) ->
    {Num, _} = string:to_integer(Digits),
    move(Num, Rest, {X, Y}, turn(Dir, {Dx, Dy}), Hist).

move(0, Rest, {X, Y}, {Dx, Dy}, Hist) ->
    parse_and_remember(Rest, {X, Y}, {Dx, Dy}, Hist);
move(Num, Rest, {X, Y}, {Dx, Dy}, Hist) ->
    {DistX, DistY} = {1 * Dx, 1 * Dy},
    case lists:member({X, Y}, Hist) of
        true -> {found, {X, Y}, Hist};
        _ -> move(Num-1, Rest, {X + DistX, Y + DistY}, {Dx, Dy}, [{X, Y} | Hist])
    end.
