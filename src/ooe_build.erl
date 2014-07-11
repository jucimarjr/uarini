-module(ooe_build).
-export([build/0,get_tokens/1,get_ast/1,get_raw_tokens/1]).

%%-----------------------------------------------------------------------------
%%Gera os analisadores lexico e sintatico e compila o compilador
build() ->
	yecc:file(ooe_parse),
	compile:file(ooe_scan),
	compile:file(ooe_parse),
	ok.

%%-----------------------------------------------------------------------------
%% Extrai a Abstract Syntax Tree de um arquivo .cerl
get_ast(ErlangClassFileName) ->
	Tokens = ast:get_urn_tokens(ErlangClassFileName),
    TokenFormList = ast:get_urn_forms_tokens(Tokens),
    ParseResultList = [ooe_parse:parse(T) || T <- TokenFormList],
    filter_result(ParseResultList, []).


%%-----------------------------------------------------------------------------
%% Retorna a AST caso nao exista nenhum error no parse
filter_result([], ReverseForms) ->
	{ok, lists:reverse(ReverseForms)};
filter_result([{ok, F}|Result], RFs) ->
	filter_result(Result, [F|RFs]);
filter_result([{error,Msg}|_], _) ->
	{error, Msg}.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo
get_raw_tokens(ErlangClassFileName) ->
	%%{ok, Tokens} = aleppo:scan_file(ErlangClassFileName),
    %% Remove EOF token
    %%lists:sublist(Tokens, length(Tokens)-1).
    {ok, Source} = file:read_file(ErlangClassFileName),
    {ok, Tokens, _Lines} = ooe_scan:string( binary_to_list(Source) ),
    Tokens.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens preprocessados de um arquivo fonte
get_tokens(ErlangClassFileName) ->
    ast:get_urn_tokens(ErlangClassFileName).
