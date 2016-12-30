-module(day12).
-export([part1/1, part2/1]).

-record(state, {
          line = 1 :: integer(),
          registers = undefined :: map() | undefined,
          lines = [] :: list() | undefined
         }).

part1(File) ->
    Parsed = common:scan_and_parse(File, day12_lexer, day12_parser),
    State = #state{registers = #{a => 0, b => 0, c => 0, d => 0},
                   lines = Parsed},
    FinalState = eval(State),
    maps:get(a, FinalState#state.registers).

part2(File) ->
    Parsed = common:scan_and_parse(File, day12_lexer, day12_parser),
    State = #state{registers = #{a => 0, b => 0, c => 1, d => 0},
                   lines = Parsed},
    FinalState = eval(State),
    maps:get(a, FinalState#state.registers).

eval(State = #state{line = Line, lines = Lines}) ->
    Instruction = get_line(Line, Lines),
    case Instruction of
        eof -> State;
        Inst -> exec(Inst, State)
    end.

get_line(Line, Parsed) when Line > length(Parsed) ->
    eof;
get_line(Line, Parsed) ->
    lists:nth(Line, Parsed).

exec({cpy, _, {register, _, Src}, {register, _, Dest}},
     State = #state{line = Line, registers = Registers}) ->
    SrcValue = maps:get(Src, Registers),
    NewRegisters = maps:put(Dest, SrcValue, Registers),
    NewState = State#state{line = Line + 1,
                           registers = NewRegisters},
    eval(NewState);

exec({cpy, _, {integer, _, Val}, {register, _, Dest}},
     State = #state{line = Line, registers = Registers}) ->
    NewRegisters = maps:put(Dest, Val, Registers),
    NewState = State#state{line = Line + 1,
                           registers = NewRegisters},
    eval(NewState);

exec({jnz, _, {integer, _, 0}, {integer, _, _}},
     State = #state{line = Line}) ->
    eval(State#state{line = Line + 1});
exec({jnz, _, {integer, _, _}, {integer, _, Jmp}},
     State = #state{line = Line}) ->
    eval(State#state{line = Line + Jmp});
exec({jnz, _, {register, _, Reg}, {integer, _, Jmp}},
     State = #state{line = Line, registers = Registers}) ->
    NextLine = case maps:get(Reg, Registers) of
                   0 -> Line + 1;
                   _ -> Line + Jmp
               end,
    eval(State#state{line = NextLine});

exec({inc, _, {register, _, Reg}},
     State = #state{line = Line, registers = Registers}) ->
    Val = maps:get(Reg, Registers),
    NewRegisters = maps:put(Reg, Val + 1, Registers),
    eval(State#state{line = Line + 1,
                     registers = NewRegisters});
exec({dec, _, {register, _, Reg}},
     State = #state{line = Line, registers = Registers}) ->
    Val = maps:get(Reg, Registers),
    NewRegisters = maps:put(Reg, Val - 1, Registers),
    eval(State#state{line = Line + 1,
                     registers = NewRegisters}).
