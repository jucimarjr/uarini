%% Uarini Lexer

%%-----------------------------------------------------------------------------
Definitions.

%% keywords
Class			= -class
Constructor		= -constructor
Export			= -export
Extends			= -extends
Import			= -import
NULL			= NULL

ClassIdentifier		= class
AttributesIdentifier	= attributes\.
MethodsIdentifier	= methods\.
%% symbols
Instance		= ::
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
BeginStm		= ->

% Operator: one of
AttributionOp		= =
ComparatorOp		= (<|<=|==|>=|>|!=)
BooleanOp		= (\&\&|!|'\|''\|')
AddOp			= (\+|-)
MultOp			= (\*|/)
ModulusOp		= 'rem'
SendOp			= !

%% misc
Digit			= [0-9]
Identifier		= [a-zA-Z_][a-zA-Z0-9_]*
StringLiteral		= "(\\\^.|\\.|[^\"])*"
WhiteSpace		= [\r|\s|\n|\t|\f]

%%-----------------------------------------------------------------------------
Rules.

{Class}		: {token, {'-class', TokenLine, list_to_atom(TokenChars)}}.
{Constructor}	: {token, {constructor, TokenLine, list_to_atom(TokenChars)}}.
{Export}	: {token, {export, TokenLine, list_to_atom(TokenChars)}}.
{Extends}	: {token, {extends, TokenLine, list_to_atom(TokenChars)}}.
{Import}	: {token, {import, TokenLine, list_to_atom(TokenChars)}}.
{NULL}		: {token, {null, TokenLine, list_to_atom(TokenChars)}}.

{ClassIdentifier}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{AttributesIdentifier}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{MethodsIdentifier}	: {token, {list_to_atom(TokenChars), TokenLine}}.

{Instance}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseParentheses}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseBrackets}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{OpenKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{CloseKeys}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Dot}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Comma}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{Semicolon}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{Barra}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{BeginStm}		: {token, {list_to_atom(TokenChars), TokenLine}}.

{AttributionOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{ComparatorOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{BooleanOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{AddOp}			: {token, {list_to_atom(TokenChars), TokenLine}}.
{MultOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{ModulusOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.
{SendOp}		: {token, {list_to_atom(TokenChars), TokenLine}}.

{Digit}+		
		: {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Digit}+\.{Digit}+	
		: {token, {float, TokenLine, list_to_float(TokenChars)}}.
{Identifier}		
		: {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{StringLiteral}		: build_text(text, TokenChars, TokenLine, TokenLen).
{WhiteSpace}+	: skip_token.


%%-----------------------------------------------------------------------------
Erlang code.

%%-----------------------------------------------------------------------------
%% reescreve o texto sem caracteres especiais
build_text(Type, Chars, Line, Length) ->
	Text = detect_special_char(lists:sublist(Chars, 2, Length - 2)),
	{token, {Type, Line, Text}}.

%%-----------------------------------------------------------------------------
%% detecta caracteres especiais
detect_special_char(Text) ->
	detect_special_char(Text, []).

detect_special_char([], Output) ->
	lists:reverse(Output);

detect_special_char([$\\, SpecialChar | Rest], Output) ->
	Char = case SpecialChar of
		$\\	-> $\\;
		$/	-> $/;
		$\" -> $\";
		$\' -> $\';
		$b	-> $\b;
		$d	-> $\d;
		$e	-> $\e;
		$f	-> $\f;
		$n	-> $\n;
		$r	-> $\r;
		$s	-> $\s;
		$t	-> $\t;
		$v	-> $\v;
		_	->
			throw({error,
				{"unrecognized special character: ", [$\\, SpecialChar]}})
	end,
	detect_special_char(Rest, [Char|Output]);

detect_special_char([Char|Rest], Output) ->
	detect_special_char(Rest, [Char|Output]).
