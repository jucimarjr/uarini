-module(uarini_build).
-export([build/0,get_tokens/1,get_ast/1]).

%%-----------------------------------------------------------------------------
%%Gera os analisadores lexico e sintatico e compila o compilador
build() ->
	leex:file(uarini_lexer),
%	yecc:file(uarini_parser),
	compile:file(uarini_lexer),
%	compile:file(uarini_parser),
	ok.


%%-----------------------------------------------------------------------------
%% Extrai a Java Abstract Syntax Tree de um arquivo .java
get_ast(ErlangClassFileName) ->
	Tokens = get_tokens(ErlangClassFileName),
	{ok, AST} = uarini_parser:parse(Tokens),
	AST.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo .java
get_tokens(ErlangClassFileName) ->
	{ok, FileContent} = file:read_file(ErlangClassFileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_lexer:string(Program),
	Tokens.
