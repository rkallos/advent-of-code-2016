-module(day19).
-export([part1/1, part2/1]).

part1(Elves) ->
    part1_solution(Elves, 2).
part2(_Elves) ->
    ok.

part1_solution(1, _) -> 0;
part1_solution(N, K) ->
    (part1_solution(N - 1, K) + K) rem N.
