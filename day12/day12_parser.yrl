Nonterminals program instruction argument.

Terminals unary binary integer register.

Rootsymbol program.

program -> instruction program : ['$1' | '$2'].
program -> instruction : ['$1'].

instruction -> unary register : {unwrap('$1'), line('$1'), '$2'}.
instruction -> binary argument argument : {unwrap('$1'), line('$1'), '$2', '$3'}.

argument -> integer  : '$1'.
argument -> register : '$1'.

Erlang code.
unwrap({_, _, Atom}) -> Atom.
line({_, Line, _}) -> Line.
