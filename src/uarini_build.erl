-module(uarini_build).
-export([build/0,get_tokens/1,get_ast/1]).

%%-----------------------------------------------------------------------------
%%Gera os analisadores lexico e sintatico e compila o compilador
build() ->
	yecc:file(uarini_parse),
	compile:file(uarini_scan),
	compile:file(uarini_parse),
	ok.


%%-----------------------------------------------------------------------------
%% Extrai a Abstract Syntax Tree de um arquivo .cerl
get_ast(ErlangClassFileName) ->
	Tokens = get_tokens(ErlangClassFileName),
	{ok, AST} = uarini_parse:parse(Tokens),
	AST.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo
get_tokens(ErlangClassFileName) ->
	{ok, FileContent} = file:read_file(ErlangClassFileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_scan:string(Program),
	Tokens.
