%% Uarini Lexer

Definitions.

% Begin Keywords

Class 			= -class
Constructor 		= -constructor
DefAtributtes 		= -def_atributtes
Export 			= -export

% End Keywords

Digit			= [0-9]
Identifier		= [a-zA-Z_][a-zA-Z0-9_]*

StringLiteral		= "(\\\^.|\\.|[^\"])*"

% Symbols Begin

OpenParentheses		= \(
CloseParentheses	= \)
OpenBrackets		= \[
CloseBrackets		= \]
OpenKeys		= \{
CloseKeys		= \}
Dot			= \.
Comma			= ,
Semicolon		= ;
Barra			= \/
BeginStmt 		= ->

% Symbols End



% Operator: one of
AttributionOp		= =
ComparatorOp		= (<|<=|==|>=|>|!=)
BooleanOp		= (\&\&|!|'\|''\|')
AddOp			= (\+|-)
MultOp			= (\*|/)
ModulusOp		= (\%)
IncrementOp		= (\+\+|--)
SendOp			= !

% End Operator

Digit			= [0-9]
Identifier		= [a-zA-Z_][a-zA-Z0-9_]*
WhiteSpace		= [\r|\s|\n|\t|\f]

Rules.

{Comment}		: skip_token.
{WhiteSpace}+		: skip_token.
{EndOfLineComment}	: skip_token.
{Identifier}		: {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{Import}		: {token, {import, TokenLine, list_to_atom(TokenChars)}}.
{Class}			: {token, {class, TokenLine, list_to_atom(TokenChars)}}.
{Constructor}		: {token, {constructor,	TokenLine, list_to_atom(TokenChars)}}.
{DefAtributtes}		: {token, {def_atributtes, TokenLine, list_to_atom(TokenChars)}}.
{Export}		: {token, {export, TokenLine, list_to_atom(TokenChars)}}.
{ImportAll}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Dot}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Comma}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Semicolon}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{AttributionOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{SendOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Barra}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{BeginStmt}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Digit}+		: {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Digit}+\.{Digit}+	: {token, {float, TokenLine, list_to_float(TokenChars)}}.
{Identifier}		: {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.

Erlang code.
