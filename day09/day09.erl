-module(day09).
-export([part1/1, part2/1]).
-compile([export_all]).

part1(File) ->
    Lines = common:read_input(File, "\n"),
    lists:sum([decompressed_length(Line, part1) || Line <- Lines]).

part2(File) ->
    Lines = common:read_input(File, "\n"),
    lists:sum([decompressed_length(Line, part2) || Line <- Lines]).

decompressed_length(In, Part) ->
    decompressed_length(In, 0, Part).
decompressed_length([], Length, _) ->
    Length;
decompressed_length([$( | Rest], Length, Part) ->
    {Subseq, Times, ToDrop} = parse_marker(Rest),
    DecompressedMarkerLength =
        case Part of
            part1 -> length(Subseq) * Times;
            part2 -> decompressed_length(Subseq, Part) * Times
        end,
    decompressed_length(lists:nthtail(ToDrop, Rest),
                        Length + DecompressedMarkerLength,
                        Part);
decompressed_length([_Char | Rest], Length, Part) ->
    decompressed_length(Rest, Length + 1, Part).

parse_marker(Marker) ->
    ClosingIndex = string:chr(Marker, $)),
    MarkerChars = string:sub_string(Marker, 1, ClosingIndex),
    Tokens = string:tokens(MarkerChars, "(x)"),
    [Chars, Times] = [list_to_integer(Token) || Token <- Tokens],
    Subseq = string:substr(Marker, ClosingIndex + 1, Chars),
    {Subseq, Times, ClosingIndex + Chars}.
