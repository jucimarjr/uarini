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
compile(CerlFileNameList) ->
	{_, _, StartTime} = now(),

	ErlangFileList = get_erl_file_list(CerlFileNameList),
	{_, _, EndTime} = now(),
	ElapsedTime = EndTime - StartTime,
	io:format(
		"~p -> ~p [ Compile time: ~p us (~p s) ]~n",
		[[filename:basename(CerlFileName) || CerlFileName <- CerlFileNameList],
			ErlangFileList,
			ElapsedTime, ElapsedTime/1000000]
	).

%%-----------------------------------------------------------------------------
%% gera vários arquivos .erl de vários .cerl
get_erl_file_list(CerlFileNameList) ->
	CerlASTList = lists:map(fun ast:get_cerl_ast/1, CerlFileNameList),
	ClassesInfo = lists:map(fun ast:get_class_info/1, CerlASTList),

	get_erl_file_list(CerlASTList, ClassesInfo, []).

get_erl_file_list([], _, ErlangFileList) ->
	lists:reverse(ErlangFileList, []);
get_erl_file_list([CerlAST | Rest], ClassesInfo, ErlangFileList) ->
	ErlangModuleName= get_erl_modulename(CerlAST),

	ErlangFileName= get_erl_filename(ErlangModuleName),

	{ok, ErlangAST} =
		core:transform_uast_to_east(CerlAST, ErlangModuleName, ClassesInfo),

	create_erl_file(ErlangAST,ErlangFileName),

	get_erl_file_list(Rest, ClassesInfo, [ErlangFileName | ErlangFileList]).

%%-----------------------------------------------------------------------------
%% gera um arquivo .erl de um .cerl
%% FUNÇÃO OBSOLETA, falta atualizar dependências
get_erl_file(CerlFileName) ->
	CerlAST = ast:get_cerl_ast(CerlFileName),

	ErlangModuleName= get_erl_modulename(CerlAST),

	ErlangFileName= get_erl_filename(ErlangModuleName),
	{ok, ErlangAST} =
		core:transform_uast_to_east(CerlAST, ErlangModuleName, []),
	create_erl_file(ErlangAST,ErlangFileName),

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
create_erl_file(ErlangAST, ErlangFileName) ->
	ErlangCode = erl_prettypr:format(erl_syntax:form_list(ErlangAST)),
	{ok, WriteDescriptor} = file:open(ErlangFileName, [raw, write]),
	file:write(WriteDescriptor, ErlangCode),
	file:close(WriteDescriptor).
