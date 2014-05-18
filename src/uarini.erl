%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Módulo API para compilar arquivos uarini

-module(uarini).
-export([compile/1, get_version/0]).

-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Interface com o usuario final. Compila 1 arquivo uarini, gera o .beam
compile({beam, UariniFileName}) ->
	[ErlangFile] = get_erl_file_list([{UariniFileName, UariniFileName}]),

	erl_tidy:file(ErlangFile,[{backups,false}]),
	compile:file(ErlangFile);

%%-----------------------------------------------------------------------------
%% Interface com o usuario final. Compila vários arquivos uarini dependentes
compile(FileNameList) ->
	ErlangFileList = get_erl_file_list(FileNameList),

	io:format("\n"),
	Result =
	[ begin
		%% io:format("~ncompiling ~p~n", [ErlangFile]),
		case ErlangFile of
			{error, ClassName, Errors} ->
				uarini_errors:print_errors(ClassName, Errors),
				{error, ClassName};

			_ ->
				erl_tidy:file(ErlangFile, [{backups,false}, {quiet, true}]),
				compile:file(ErlangFile, [{outdir, filename:dirname(ErlangFile)},
										  verbose,report_errors,report_warnings])
		end
	  end
	  || ErlangFile <- ErlangFileList ],
	Result.

%%-----------------------------------------------------------------------------
%% gera varios .erl a partir de varios .cerl
%% antes da traducao, as informacoes de todas as classes sao obtidas
get_erl_file_list(FileNameList) ->
	ASTList = lists:map(fun ast:get_urn_forms/1, FileNameList),
	ClassesInfo = lists:map(fun ast:get_class_info/1, ASTList),

	ASTList2 = remove_interfaces(ASTList),

	get_erl_file_list(ASTList2, ClassesInfo, []).

get_erl_file_list([], _, ErlangFileList) ->
	lists:reverse(ErlangFileList, []);

get_erl_file_list([AST | Rest], ClassesInfo, ErlangFileList) ->
	ErlangModuleName = get_erl_modulename(AST),
	ErlangFileName = get_erl_filename(ErlangModuleName),

	case core:transform_uast_to_east(AST, ErlangModuleName, ClassesInfo) of
		{ok, ErlangAST} ->
			create_erl_file(ErlangAST, ErlangFileName),
			get_erl_file_list(Rest, ClassesInfo, [ErlangFileName | ErlangFileList]);

		{error, Errors} ->
			Element = {error, ErlangModuleName, Errors},
			get_erl_file_list(Rest, ClassesInfo, [Element | ErlangFileList])
	end.

%% remove da lista de AST a ser compilada as interfaces,
%% pois elas nao geram codigo, sao usadas apenas nas info das classes
remove_interfaces(ASTList) -> remove_interfaces(ASTList, []).

remove_interfaces([], NewASTList) -> lists:reverse(NewASTList);
remove_interfaces([AST | ASTList], NewASTList) ->
	case AST of
		[_File|[{attribute, _Line, interface, _InterName} | _ ]] ->
			remove_interfaces(ASTList, NewASTList);
		_ ->
			remove_interfaces(ASTList, [AST | NewASTList])
	end.

%%-----------------------------------------------------------------------------
%% Mostra a versao, autores e ano do ooErlang ( uarini ).
get_version() ->
	io:format("Uarini - A OOP extension for Erlang Compiler ~n"),
	io:format("Version: ~p~n", [?VSN]),
	io:format("Team: ~p~n", [?TEAM]),
	io:format("Year: ~p~n", [?YEAR]).

%%-----------------------------------------------------------------------------
%% Determina o nome do arquivo .erl
get_erl_filename(ErlangModuleName) ->
	"./" ++ atom_to_list(ErlangModuleName) ++ ".erl".

%%-----------------------------------------------------------------------------
%% Extrai o nome do modulo erlang partir dos forms do uarini
%% o nome do modulo Erlang gerado eh o nome da classe
get_erl_modulename(AST) ->
	[_File|[Class|_]] = AST,
	{attribute, _Line, class, ClassName} = Class,
	ClassName.

%%-----------------------------------------------------------------------------
%% Cria o arquivo .erl no sistema de arquivos
create_erl_file(ErlangAST, ErlangFileName) ->
	ErlangCode = erl_prettypr:format(erl_syntax:form_list(ErlangAST)),
	{ok, WriteDescriptor} = file:open(ErlangFileName, [raw, write]),
	file:write(WriteDescriptor, ErlangCode),
	file:close(WriteDescriptor).
