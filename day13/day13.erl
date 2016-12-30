-module(day13).
-export([part1/2, part2/2]).

-record(grid,
        {rows = 1 :: pos_integer(),
         cols = 1 :: pos_integer()}).

-record(state,
        {grid = #grid{},
         target = {1, 1} :: {integer(), integer()},
         fav_num = 0 :: integer(),
         visited = [{1, 1}] :: list({integer(), integer()}),
         open_list = gb_trees:empty() :: gb_trees:tree()}).

part1(FavNum, Coord) ->
    State = #state{fav_num = FavNum, target = Coord},
    shortest_path(State).


part2(_Input, _Coord) ->
    ok.


popcount(N) ->
    popcount(N, 0).

popcount(0, Pop) -> Pop;

popcount(N, Pop) ->
    popcount(N div 2, Pop + (N rem 2)).


is_open_space({X, Y}, FavNum) ->
    Sum = ((X * X) + (3 * X) + (2 * X * Y) + Y + (Y * Y) + FavNum),
    popcount(Sum) rem 2 =:= 0.


shortest_path(#state{visited = [Coord | Visited], target = Coord}) ->
    {length(Visited), Visited};

shortest_path(State) ->
    Neighbors = get_neighbors(State),
    State2 = add_to_open_list(Neighbors, State),
    {{_, Path}, _, OpenList3} = gb_trees:take_smallest(State2#state.open_list),
    State3 = State2#state{open_list = OpenList3,
                          visited = Path},
    shortest_path(State3).


get_neighbors(State = #state{visited = [{X, Y} | _], fav_num = FavNum}) ->
    Neighbors = [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}],
    lists:filter(fun(Coord) ->
                         is_valid(Coord)
                             andalso is_open_space(Coord, FavNum)
                             andalso not(visited(Coord, State))
                 end, Neighbors).


is_valid({X, Y}) when X > 0 andalso Y > 0 ->
    true;
is_valid(_) -> false.


visited(Coord, #state{visited = Visited}) ->
    lists:member(Coord, Visited).


add_to_open_list(Neighbors, State = #state{open_list = OpenList,
                                           visited = Visited}) ->
    TotalVisited = length(Visited),
    NewOpenList = lists:foldl(
                    fun(Neighbor, OL) ->
                            gb_trees:insert({TotalVisited, [Neighbor | Visited]}, nil, OL)
                    end, OpenList, Neighbors),
    State#state{open_list = NewOpenList}.
