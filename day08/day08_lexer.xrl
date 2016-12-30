Definitions.

D = [0-9]
WS = ([\000-\s]|[bxy=])

Rules.

rect   : {token, {rect, TokenLine}}.
rotate : {token, {rotate, TokenLine}}.
row    : {token, {row, TokenLine}}.
column : {token, {col, TokenLine}}.
{D}+   : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{WS}+  : skip_token.

Erlang code.
