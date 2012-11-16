%% autor: Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Test erl_scan and erl_compile to generate tokens and AST 

-module(test_scan_parse).
-author('emiliano.firmino@gmail.com').
-vsn(1.0).

-import(file, [read_file/1]).
-export([ast/1, src/1, ast_tokens/1, tokenize/1]).

% to try another scan & parse change these macros
-define(LEXER, erl_scan).
-define(PARSER, erl_parse).

ast(Tokens) ->
	?PARSER:abstract(Tokens).

src(SourceFileName) ->
	{ok, BinData} = read_file(SourceFileName),
	binary_to_list(BinData).

ast_tokens(AST) ->
	?PARSER:normalise(AST).

tokenize(Source) ->
	{ok, Tokens, _Lines} = ?LEXER:string(Source),
	Tokens.
