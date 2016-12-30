-module(day23).
-export([part1/1, part2/1]).

-record(state, {
          line = 1 :: integer(),
          registers = undefined :: map() | undefined,
          lines = undefined :: map() | undefined
         }).

part1(File) ->
    Parsed = common:scan_and_parse(File, day23_lexer, day23_parser),
    State = #state{registers = #{a => 7, b => 0, c => 0, d => 0},
                   lines = Parsed},
    FinalState = eval(State),
    maps:get(a, FinalState#state.registers).

part2(File) ->
    Parsed = common:scan_and_parse(File, day23_lexer, day23_parser),
    State = #state{registers = #{a => 12, b => 0, c => 0, d => 0},
                   lines = Parsed},
    FinalState = eval(State),
    maps:get(a, FinalState#state.registers).

eval(State = #state{line = Line, lines = Lines}) ->
    Instruction = get_line(Line, Lines),
    case Instruction of
        eof -> State;
        Inst -> exec(Inst, State)
    end.

get_line(Line, Parsed) ->
    maps:get(Line, Parsed, eof).

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
exec({cpy, _, {integer, _, _}, {integer, _, _}},
     State = #state{line = Line}) ->
    eval(State#state{line = Line + 1});

exec({jnz, _, {integer, _, 0}, {integer, _, _}},
     State = #state{line = Line}) ->
    eval(State#state{line = Line + 1});
exec({jnz, _, {integer, _, _}, {integer, _, Jmp}},
     State = #state{line = Line}) ->
    eval(State#state{line = Line + Jmp});
exec({jnz, _, {integer, _, _}, {register, _, Reg}},
     State = #state{line = Line, registers = Registers}) ->
    eval(State#state{line = Line + maps:get(Reg, Registers)});
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
                     registers = NewRegisters});

exec({tgl, _, {register, _, Reg}},
     State = #state{line = Line, registers = Registers, lines = Lines}) ->
    Val = maps:get(Reg, Registers),
    NewLines = toggle(Line + Val, Lines),
    eval(State#state{line = Line + 1,
                     lines = NewLines}).

toggle(Line, Lines) ->
    case get_line(Line, Lines) of
        eof -> Lines;
        Inst -> maps:put(Line, toggle_instruction(Inst), Lines)
    end.

toggle_instruction({inc, N, Reg}) -> {dec, N, Reg};
toggle_instruction({_, N, Reg}) -> {inc, N, Reg};
toggle_instruction({jnz, N, One, Two}) -> {cpy, N, One, Two};
toggle_instruction({cpy, N, One, Two}) -> {jnz, N, One, Two}.
