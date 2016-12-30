-module(day15).

-export([part1/1, part2/1]).
-compile([export_all]).

part1(Input) ->
    Disks = [parse_line(Line) || Line <- common:read_input(Input, "\n")],
    solve(1, Disks).


part2(_Input) ->
    ok.


parse_line(Line) ->
    Format = "Disc #~d has ~d positions; at time=0, it is at position ~d.",
    {ok, [_, Positions, Init], []} = io_lib:fread(Format, Line),
    {Positions, Init}.


calculate(Time, Disks) ->
    [(Init + Time) rem Positions || {Positions, Init} <- Disks].


solve(Time, Disks) ->
    solve(Time, Disks, 1).

solve(Time, Disks, Inc) ->
    case solve2(Time, Disks, Inc) of
        ok -> Time;
        {NewDisks, NewInc} -> solve(Time + NewInc, NewDisks, NewInc)
    end.


solve2(_, [], _) -> ok;
solve2(Time, Disks, Inc) ->
    PositionsAtTime = calculate(Time, Disks),
    case PositionsAtTime of
        [0, _] -> {Pos, _} = hd(Disks),
                  solve2(Time + 1, tl(Disks), Inc * Pos);
        _ -> {Disks, Inc}
    end.
