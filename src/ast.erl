-module(ast).
-export([get_cerl_tokens/1, get_cerl_forms/1]).

%%-----------------------------------------------------------------------------
%% Extrai a Uarini Abstract Syntax Tree de um arquivo .cerl
get_cerl_forms(CerlFileName) ->
	Tokens = get_cerl_tokens(CerlFileName),
	lists:map(
		fun(Ts) ->
			{ok, Form} = uarini_parse:parse(Ts),
			Form
		end,
		split_dots(Tokens)).

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo .cerl
get_cerl_tokens(CerlFileName) ->
	{ok, FileContent} = file:read_file(CerlFileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_scan:string(Program),
	Tokens.

%%-----------------------------------------------------------------------------
%% Quebra os forms de um fluxo de Tokens identificados por 'dot'
split_dots(Ts) ->
	split_dots(Ts, [], []).

split_dots([], [], Fs) ->
    lists:reverse(Fs);
split_dots([], F, Fs) ->
    lists:reverse([lists:reverse(F)|Fs]);
split_dots([T={dot,_}|Ts], F, Fs) ->
    split_dots(Ts, [], [lists:reverse([T|F])|Fs]);
split_dots([T|Ts], F, Fs) ->
    split_dots(Ts, [T|F], Fs).