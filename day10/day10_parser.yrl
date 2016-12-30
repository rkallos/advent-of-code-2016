Nonterminals program instruction numbered.

Terminals atom verb chip int.

Rootsymbol program.

program -> instruction program : ['$1' | '$2'].
program -> instruction : ['$1'].

instruction -> numbered verb chip numbered chip numbered :
  {give, '$1', '$4', '$6'}.

instruction -> numbered verb numbered :
  {to, '$1', '$3'}.

numbered -> atom int : {unwrap('$1'), unwrap('$2')}.

Erlang code.
unwrap({_, _, Thing}) -> Thing.
