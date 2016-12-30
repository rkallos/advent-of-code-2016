-module(day08).
-export([solve/1]).

-record(screen, {
    rows :: pos_integer(),
    cols :: pos_integer(),
    array
}).

solve(File) -> solve(File, 6, 50).
solve(File, Rows, Cols) ->
    Lines = common:scan_and_parse(File, day08_lexer, day08_parser),
    BScreen = make_screen(Rows, Cols),
    AScreen = lists:foldl(fun do/2, BScreen, Lines),
    print_screen(AScreen),
    array:sparse_foldl(fun(_, $#, State) -> State + 1;
                          (_, _, State) -> State end,
                        0, AScreen#screen.array).

make_screen(Rows, Cols) ->
    Opts = [{fixed, true},
            {size, Rows * Cols},
            {default, $.}],
    Array = array:new(Opts),
    #screen{rows = Rows,
            cols = Cols,
            array = Array}.

print_screen(#screen{cols = Cols, array = Array}) ->
    Chars = array:foldr(fun(_, E, Acc) -> [E | Acc] end,
                        [], Array),
    Rows = common:make_groups_of(Cols, Chars),
    String = [[$\n | Row] || Row <- Rows],
    io:format("~s~n", [String]).

coord_to_idx({Row, Col}, #screen{rows = Rows, cols = Cols}) ->
    RowOffset = (Row * Cols) rem (Rows * Cols),
    ColOffset = Col rem Cols,
    RowOffset + ColOffset.

set_pixel({Row, Col}, Val, Screen) ->
    Idx = coord_to_idx({Row, Col}, Screen),
    set_pixel(Idx, Val, Screen);
set_pixel(Idx, Val, Screen = #screen{array = Array}) ->
    OutArray = array:set(Idx, Val, Array),
    Screen#screen{array = OutArray}.

set_pixels([], _, Screen) -> Screen;
set_pixels([Idx | Idxs], Val, Screen) ->
    NewScreen = set_pixel(Idx, Val, Screen),
    set_pixels(Idxs, Val, NewScreen).

do({rect, Cols, Rows}, Screen) ->
    make_rect(Screen, Cols, Rows);
do({rotate, col, Col, Amount}, Screen) ->
    col_shift(Screen, Col, Amount);
do({rotate, row, Row, Amount}, Screen) ->
    row_shift(Screen, Row, Amount).

make_rect(Screen, Cols, Rows) ->
    Coords = [{Row, Col} || Row <- lists:seq(0, Rows-1),
                            Col <- lists:seq(0, Cols-1)],
    Idxs = [coord_to_idx(Coord, Screen) || Coord <- Coords],
    lists:foldl(fun(Idx, OutScreen) ->
                        set_pixel(Idx, $#, OutScreen)
                end, Screen, Idxs).

col_shift(Screen, Column, Amount) ->
    Coords = get_col(Screen, Column),
    NewCoords = [fix_coord({Row + Amount, Col}, Screen) ||
                    {Row, Col} <- Coords],
    ClearedScreen = set_pixels(Coords, $., Screen),
    set_pixels(NewCoords, $#, ClearedScreen).

row_shift(Screen, Row, Amount) ->
    Coords = get_row(Screen, Row),
    NewCoords = [fix_coord({R, C + Amount}, Screen) ||
                    {R, C} <- Coords],
    ClearedScreen = set_pixels(Coords, $., Screen),
    set_pixels(NewCoords, $#, ClearedScreen).

get_col(Screen = #screen{rows = Rows}, Column) ->
    [{N, Column} || N <- lists:seq(0, Rows - 1),
                    is_set({N, Column}, Screen)].

get_row(Screen = #screen{cols = Cols}, Row) ->
    [{Row, N} || N <- lists:seq(0, Cols - 1),
                 is_set({Row, N}, Screen)].

fix_coord({Row, Col}, #screen{rows = Rows, cols = Cols}) ->
    {Row rem Rows, Col rem Cols}.

is_set({Row, Col}, Screen) ->
    Idx = coord_to_idx({Row, Col}, Screen),
    case array:get(Idx, Screen#screen.array) of
        $. -> false;
        _ -> true
    end.
