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
    [F|| {ok, F} <- ParseResultList].

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo
get_tokens(ErlangClassFileName) ->
	{ok, FileContent} = file:read_file(ErlangClassFileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_scan:string(Program),
	Tokens.

%%-----------------------------------------------------------------------------
%%
split_forms(Ts) ->
    split_forms(Ts, [], []).

split_forms([], [], Fs) ->
    lists:reverse(Fs);
split_forms([], F, Fs) ->
    lists:reverse([lists:reverse(F)|Fs]);
split_forms([T={dot,_}|Ts], F, Fs) ->
    split_forms(Ts, [], [lists:reverse([T|F])|Fs]);
split_forms([T|Ts], F, Fs) ->
    split_forms(Ts, [T|F], Fs).
