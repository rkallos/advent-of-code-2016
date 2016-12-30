-module(day04).
-export([part1/1, part2/1]).

part1(Input) -> main(1, Input).
part2(Input) -> main(2, Input).

main(Part, Input) ->
    Lines = common:read_input(Input, "\n"),
    Pids = [spawn(worker_fun(Part, self(), Line)) || Line <- Lines],
    receive_loop(Part, length(Pids), 0).


receive_loop(1, 0, Sum) -> Sum;
receive_loop(1, N, Sum) ->
    receive
        Num -> receive_loop(1, N-1, Sum + Num)
    end;

receive_loop(2, 0, _) -> not_found;
receive_loop(2, N, State) ->
    receive
        not_here -> receive_loop(2, N-1, State);
        Id -> Id
    end.

worker_fun(1, Pid, Line) ->
    fun() ->
            Pid ! validate_room(Line)
    end;
worker_fun(2, Pid, Line) ->
    fun() -> {Name, Id} = decrypt_room(Line),
             io:format("{~s, ~p}~n", [Name, Id]),
             case lists:prefix("north", Name) of
                 true -> io:format("-----FOUND ROOM. ID = ~p-----~n", [Id]),
                         Pid ! Id;
                 false -> Pid ! not_here
             end
    end.

validate_room(Line) ->
    SplitLine = re:split(Line, "-|([0-9]+)|[\\[\\]]", [{return, list}, trim]),
    ParsedLine = lists:filter(fun ([]) -> false;
                                  (_)  -> true
                              end, SplitLine),
    {ParsedName, [Id, Checksum]} =
        lists:split(length(ParsedLine) - 2, ParsedLine),
    FlatName = lists:flatten(ParsedName),
    ParsedChecksum = make_checksum(FlatName, maps:new()),
    case Checksum =:= ParsedChecksum of
        true -> {IntId, []} = string:to_integer(Id),
                IntId;
        false -> 0
    end.

make_checksum([], Map) ->
    generate_checksum(Map);
make_checksum([Char | Rest], Map) ->
    make_checksum(Rest,
                  maps:update_with(Char, fun(Occurrences) -> Occurrences + 1 end,
                                   1, Map)).

generate_checksum(Map) ->
    Sorted = lists:sublist(lists:sort(fun sort_fun/2, maps:to_list(Map)), 5),
    [Char || {Char, _} <- Sorted].

sort_fun({CharA, OccurrencesA}, {CharB, OccurrencesB}) ->
    case OccurrencesA =:= OccurrencesB of
        true -> CharA < CharB;
        false -> OccurrencesA > OccurrencesB
    end.

decrypt_room(Line) ->
    SplitLine = re:split(Line, "([0-9]+)|[\\[\\]]", [{return, list}, trim]),
    ParsedLine = lists:filter(fun ([]) -> false;
                                  (_)  -> true
                              end, SplitLine),
    {ParsedName, [Id, _]} =
        lists:split(length(ParsedLine) - 2, ParsedLine),
    {IntId, []} = string:to_integer(Id),
    {shift(ParsedName, IntId), IntId}.

shift(S, Shift) when is_list(S) ->
    lists:map(fun(C) -> shift(C, Shift) end, S);
shift($-, _) -> $\s;
shift(C, Shift) when is_list(Shift) ->
    {Amount, []} = string:to_integer(Shift),
    shift(C, Amount);
shift(C, Shift) when C >= $a, C =< $z ->
    $a + (C - $a + Shift) rem 26;
shift(C, Shift) when C >= $A, C =< $Z ->
    $A + (C - $A + Shift) rem 26;
shift(C, _) -> C.
