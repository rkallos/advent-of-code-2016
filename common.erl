-module(common).
-export([read_input/2,
         make_groups_of/2,
         scan_and_parse/3]).

read_input(File, Separators) ->
    {ok, Bytes} = file:read_file(File),
    string:tokens(binary_to_list(Bytes), Separators).

make_groups_of(N, List) -> make_groups_of(N, List, []).
make_groups_of(_, [], Result) -> lists:reverse(Result);
make_groups_of(N, List, Result) ->
    {Group, Rest} = lists:split(N, List),
    make_groups_of(N, Rest, [Group | Result]).

scan_and_parse(File, LexMod, ParseMod) ->
    [Input] = read_input(File, ""),
    {ok, Tokens, _} = LexMod:string(Input),
    {ok, Lines} = ParseMod:parse(Tokens),
    Lines.
