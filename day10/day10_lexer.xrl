Definitions.

Atom = (bot|value|output)
Verb = (gives|goes)
Chip = (high|low)
SkipWord = (to|and)
D = [0-9]
WS = [\000-\s]

Rules.

{Atom}     : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Verb}     : {token, {verb, TokenLine, list_to_atom(TokenChars)}}.
{Chip}     : {token, {chip, TokenLine, list_to_atom(TokenChars)}}.
{D}+       : {token, {int,  TokenLine, list_to_integer(TokenChars)}}.
{WS}+      : skip_token.
{SkipWord} : skip_token.

Erlang code.
