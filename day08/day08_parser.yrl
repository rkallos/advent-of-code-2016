Nonterminals program instruction dim.

Terminals rect rotate row col int.

Rootsymbol program.

program -> instruction program : ['$1' | '$2'].
program -> instruction : ['$1'].

instruction -> rect int int : {unwrap('$1'), unwrap('$2'), unwrap('$3')}.
instruction -> rotate dim int int : {unwrap('$1'), unwrap('$2'), unwrap('$3'), unwrap('$4')}.

dim -> row : '$1'.
dim -> col : '$1'.

Erlang code.
unwrap({Thing, _}) -> Thing;
unwrap({_, _, Thing}) -> Thing.
