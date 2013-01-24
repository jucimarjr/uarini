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
    TokenFormList = split_forms(Tokens),
    ParseResultList = [uarini_parse:parse(T) || T <- TokenFormList],
    filter_result(ParseResultList, []).


%%-----------------------------------------------------------------------------
%% Retorna a AST caso nao exista nenhum error no parse
filter_result([], ReverseForms) ->
	{ok, lists:reverse(ReverseForms)};
filter_result([{ok, F}|Result], RFs) ->
	filter_result(Result, [F|RFs]);
filter_result([{error,Msg}|_], _) ->
	%erlang:error(Msg).
	{error, Msg}.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo
get_tokens(ErlangClassFileName) ->
	%%{ok, Tokens} = aleppo:scan_file(ErlangClassFileName),
    %% Remove EOF token
    %%lists:sublist(Tokens, length(Tokens)-1).
    {ok, Source} = file:read_file(ErlangClassFileName),
    {ok, Tokens, _Lines} = uarini_scan:string( binary_to_list(Source) ),
    Tokens.

%%-----------------------------------------------------------------------------
%% Particiona o fluxo de token em subfluxos terminados pelo token dot
split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], ReverseForms) ->
    FormList = lists:reverse(ReverseForms),
    FormList;
split_forms([], ReverseFormTokens, ReverseForms) ->
    Form = lists:reverse(ReverseFormTokens),
    FormList = lists:reverse([Form|ReverseForms]),
    FormList;
split_forms(
        [DotToken={dot,_}|Tokens],
        ReverseFormTokens,
        ReverseForms) ->
    Form = lists:reverse([DotToken|ReverseFormTokens]),
    split_forms(Tokens, [], [Form|ReverseForms]);
split_forms(
        [Token|Tokens],
        ReverseFormTokens,
        ReverseForms) ->
    split_forms(Tokens, [Token|ReverseFormTokens], ReverseForms).

