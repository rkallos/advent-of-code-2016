-module(day07).
-export([part1/1, part2/1]).

part1(Input) ->
    Lines = common:read_input(Input, "\n"),
    lists:foldl(fun supports_tls/2, 0, Lines).

part2(Input) ->
    Lines = common:read_input(Input, "\n"),
    lists:foldl(fun supports_ssl/2, 0, Lines).

supports_tls(Line, State) ->
    {Supernets, Hypernets} = parse_ip(Line),
    case check_for_abbas(Hypernets) of
        true -> State;
        false -> case check_for_abbas(Supernets) of
                     true -> State + 1;
                     false -> State
                 end
    end.

supports_ssl(Line, State) ->
    {Supernets, Hypernets} = parse_ip(Line),
    Babs = get_abas(Hypernets),
    case Babs of
        [] -> State;
        _ -> Abas = lists:map(fun swap_aba/1, get_abas(Supernets)),
               case lists:any(fun(Bab) -> lists:member(Bab, Abas) end,
                              Babs) of
                   false -> State;
                   true -> State + 1
               end
    end.

parse_ip(Ip) ->
    Split = string:tokens(Ip, "[]"),
    NumberedSplits = lists:zip(lists:seq(1,length(Split)), Split),
    {NumberedSupers, NumberedHypers} =
        lists:partition(fun({N, _}) -> N rem 2 =:= 1 end, NumberedSplits),
    Supers = [Super || {_, Super} <- NumberedSupers],
    Hypers = [Hyper || {_, Hyper} <- NumberedHypers],
    {Supers, Hypers}.

windows(Size, Input) ->
    windows(Size, Input, []).

windows(_, [], Result) -> lists:reverse(Result);
windows(Size, Input, Result) ->
    Window = lists:sublist(Input, Size),
    windows(Size, tl(Input), [Window | Result]).

check_for_abbas(Nets) ->
    lists:any(fun check_for_abba/1, Nets).

check_for_abba(Net) ->
    Windows = windows(4, Net),
    lists:any(fun is_abba/1, Windows).

is_abba([C1, C2, C2, C1]) when C1 /= C2 -> true;
is_abba(_) -> false.

get_abas([]) -> [];
get_abas([Net | Nets]) when is_list(Net) ->
    get_abas(Net) ++ get_abas(Nets);
get_abas(Net) ->
    Windows = windows(3, Net),
    lists:filter(fun is_aba/1, Windows).

is_aba([C1, C2, C1]) when C1 /= C2 -> true;
is_aba(_) -> false.

swap_aba([C1, C2, C1]) -> [C2, C1, C2].
