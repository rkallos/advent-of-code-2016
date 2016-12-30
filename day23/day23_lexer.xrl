Definitions.

D = -?[0-9]
R = [a-d]
OneAry = (inc|dec|tgl)
TwoAry = (cpy|jnz)
WS = [\000-\s]

Rules.

{OneAry} : {token, {unary, TokenLine, list_to_atom(TokenChars)}}.
{TwoAry} : {token, {binary, TokenLine, list_to_atom(TokenChars)}}.
{D}+     : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{R}      : {token, {register, TokenLine, list_to_atom(TokenChars)}}.
{WS}+    : skip_token.

Erlang code.
