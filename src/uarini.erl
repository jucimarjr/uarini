-module(uarini).
-export([compile/1, get_version/0]).

-include("../include/uarini_define.hrl").

compile({beam, CerlFileName}) ->
	{_, _, StartTime} = now(),

	ErlangFile = get_erl_file(CerlFileName),

	erl_tidy:file(ErlangFile,[{backups,false}]),

	compile:file(ErlangFile),

	{_, _, EndTime} = now(),
	ElapsedTime = EndTime - StartTime,
	io:format(
		"~p -> ~p [ Compile time: ~p us (~p s) ]~n",
		[
			filename:basename(CerlFileName),
			ErlangFile,
			ElapsedTime,
			ElapsedTime/1000000
		]
	);

%%-----------------------------------------------------------------------------
%% Interface com o usuario final. Compila vários arquivos cerl dependentes
compile(CerlFileName) ->
	{_, _, StartTime} = now(),
	ErlangFile = get_erl_file(CerlFileName),
	{_, _, EndTime} = now(),
	ElapsedTime = EndTime - StartTime,
	io:format(
		"~p -> ~p [ Compile time: ~p us (~p s) ]~n", [CerlFileName,
			ErlangFile,
			ElapsedTime, ElapsedTime/1000000]
	).

%%-----------------------------------------------------------------------------
%% gera um arquivo .erl de um .cerl
%% FUNÇÃO OBSOLETA, falta atualizar dependências
get_erl_file(CerlFileName) ->
	CerlAST = ast:get_cerl_ast(CerlFileName),

	ErlangModuleName= get_erl_modulename(CerlAST),

	ErlangFileName= get_erl_filename(ErlangModuleName),
	ErlangCode =
		core:transform_uast_to_east(CerlAST, ErlangModuleName),
	create_erl_file(ErlangCode, ErlangFileName),
	ErlangFileName.

%%-----------------------------------------------------------------------------
%% Mostra a versao, autores e ano do Uarini.
get_version() ->
	io:format("Uarini - A Cerl compiler for Erlang VM ~n"),
	io:format("Version: ~p~n", [?VSN]),
	io:format("Team: ~p~n", [?TEAM]).

%%-----------------------------------------------------------------------------
%% Extrai o nome do arquivo .erl a partir do cerl ast
get_erl_filename(ErlangModuleName) ->
	atom_to_list(ErlangModuleName) ++ ".erl".

%%-----------------------------------------------------------------------------
%% Extrai o nome do modulo erlang partir do cerl ast
%% o nome do modulo eh o nome da classe
get_erl_modulename(CerlAST) ->
	[{class,CerlClassName}|_] = CerlAST,
	list_to_atom(string:to_lower(atom_to_list(CerlClassName))).

%%-----------------------------------------------------------------------------
%% Cria o arquivo .erl no sistema de arquivos
create_erl_file(ErlangCode, ErlangFileName) ->
	{ok, WriteDescriptor} = file:open(ErlangFileName, [raw, write]),
	file:write(WriteDescriptor, ErlangCode),
	file:close(WriteDescriptor).
