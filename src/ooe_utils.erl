%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Utilitários para o compilador

-module(ooe_utils).
-compile(export_all).

-include("../include/ooe_define.hrl").

%%-----------------------------------------------------------------------------
%% Compila vários códigos em OOE dependentes e gera .beam's correspondentes
compile(FileNameList) ->
	case catch(ooe:compile(FileNameList)) of
		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason]);

		error ->
			error;

		Result ->
			io:format("~nCompilation Result:~n"
						"~p", [Result])

		%% X ->
		%% 	io:format("*******UNEXPECTED ERROR!~n"),
		%% 	io:format("***Reason:~n~p", [X])
	end.

%%-----------------------------------------------------------------------------
%% Extrai a Erlang Abstract Syntax Tree do código-fonte
get_ast(FileName) ->
	AST = ast:get_ast(FileName),
	[{class, {_Line, {name, ClassName}, _ClassBody}}] = AST,
	ModuleName = helpers:lower_atom(ClassName),
	core:transform_oo_expr(AST, ModuleName, []).

%%-----------------------------------------------------------------------------
%% Imprime a árvore gerada pela análise sintática do compilador
print_urn_ast(FileName) ->
	io:format("Generating Syntax Analysis... "),

	Dir = filename:dirname(FileName),
	%FileName = filename:basename(FileName),
	case catch(ast:get_ast(FileName)) of
		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason]);
		{Dir, AST} ->
			io:format("done!~n"),

			io:format("Jaraki Syntax Tree from ~p:~n", [FileName]),
			io:format("~p~n", [AST])
	end.

%%-----------------------------------------------------------------------------
%% Imprime os tokens gerados pela análise léxica do compilador
print_tokens(FileName) ->
	io:format("Generating Lexical Analysis... "),

	Tokens = ast:get_tokens(FileName),
	io:format("done!~n"),

	io:format("Uarini Tokens from ~p:~n", [FileName]),
	io:format("~p~n", [Tokens]).

%%-----------------------------------------------------------------------------
%% Imprime a árvore sintática do código gerado
print_erl_ast(FileName) ->
	io:format("Generating Erlang Abstract Syntax Tree... "),

	case catch(get_erl_ast(FileName)) of
		{ok, ErlangAST} ->
			io:format("done!~n"),

			io:format("Erlang Abstract Syntax Tree from ~p:~n", [FileName]),
			io:format("~p~n", [ErlangAST]);

		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason])
	end.

%%-----------------------------------------------------------------------------
%% traduz expressões em ooerlang e gera árvore sintática do erlang resultante
get_erl_ast(FileName) ->
	AST = ast:get_urn_ast(FileName),
	[{class, {_Line, {name, ClassName}, _ClassBody}}] = AST,
	ModuleName = helpers:lower_atom(ClassName),
	core:transform_uast_to_east(AST, ModuleName, []).

%%-----------------------------------------------------------------------------
%% calcula o tempo de execução de uma funcao em microssegundos
get_runtime(Module, Func, N )->
	{ElapsedTime, R} = timer:tc(Module, Func, [N]),
	io:format("~p(~p): ~p [~p us] [~p s] ~n",
				[Func, N, R, ElapsedTime, ElapsedTime/1000000]).

get_runtime(Module, Func )->
	{ElapsedTime, R} = timer:tc(Module, Func, []),
	io:format("~p(~p): [~p us] [~p s] ~n",
				[Func, R, ElapsedTime, ElapsedTime/1000000]).
