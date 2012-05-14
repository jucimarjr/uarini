%% Uarini Lexer

Definitions.

% Begin Keywords

Class = class
Constructor = constructor
DefAtributtes = def_atributtes
Import = import


% End Keywords

Digit				= [0-9]
Identifier			= [a-zA-Z_][a-zA-Z0-9_]*

StringLiteral		= "(\\\^.|\\.|[^\"])*"

% Symbols Begin

OpenParentheses		= \(
CloseParentheses	= \)
OpenBrackets		= \[
CloseBrackets		= \]
OpenKeys			= \{
CloseKeys			= \}
Dot					= \.
Comma				= ,
Semicolon			= ;

Type = -
BeginStmt = ->

% Symbols End



% Operator: one of
AttributionOp	= =
ComparatorOp	= (<|<=|==|>=|>|!=)
BooleanOp		= (\&\&|!|'\|''\|')
AddOp			= (\+|-)
MultOp			= (\*|/)
ModulusOp		= (\%)
IncrementOp		= (\+\+|--)

% End Operator

WhiteSpace	= [\r|\s|\n|\t|\f]

Rules.

{Comment}			: skip_token.
{EndOfLineComment}	: skip_token.

{Import}	: {token, {import,	TokenLine, list_to_atom(TokenChars)}}.
{Class}		: {token, {class,	TokenLine, list_to_atom(TokenChars)}}.
{ImportAll}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Dot}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Comma}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Semicolon}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{AttributionOp}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{Type}	: {token, {list_to_atom(TokenChars), TokenLine}}.

Erlang code.
