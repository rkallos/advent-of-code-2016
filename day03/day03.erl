-module(day03).
-export([part1/1, part2/1]).

part1(Input) ->
    StringNumbers = common:read_input(Input, "\n "),
    Numbers = parse_numbers(StringNumbers),
    Triangles = common:make_groups_of(3, Numbers),
    count_possible_triangles(Triangles, 0).

parse_numbers(L) -> parse_numbers(L, []).
parse_numbers([], Result) -> lists:reverse(Result);
parse_numbers([Number | Rest], Result) ->
    case string:to_integer(Number) of
        {error, no_integer} -> parse_numbers(Rest, Result);
        {Num, []} -> parse_numbers(Rest, [Num | Result])
    end.

count_possible_triangles([], HowMany) -> HowMany;
count_possible_triangles([Sides | Lines], HowMany) ->
    case is_possible_triangle(Sides) of
        true   -> count_possible_triangles(Lines, HowMany+1);
        false  -> count_possible_triangles(Lines, HowMany)
    end.

is_possible_triangle(Sides) ->
    Max = lists:max(Sides),
    (lists:sum(Sides) - Max) > Max.

part2(Input) ->
    {ok, Bytes} = file:read_file(Input),
    StringNumbers = string:tokens(binary_to_list(Bytes), " \n"),
    Numbers = parse_numbers(StringNumbers),
    RowTriangles = common:make_groups_of(3, Numbers),
    RowTriples = common:make_groups_of(3, RowTriangles),
    Triangles = [make_column_based_triangles(Triple)
                 || Triple <- RowTriples],
    count_possible_triangles(lists:append(Triangles), 0).

make_column_based_triangles([[A1, A2, A3],
                             [B1, B2, B3],
                             [C1, C2, C3]]) ->
    [[A1, B1, C1],
     [A2, B2, C2],
     [A3, B3, C3]].
