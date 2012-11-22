%% autor: Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Test uarini_scan e uarini_parse

-module(test_uarini).
-author('emiliano.firmino@gmail.com').
-vsn(0.1).

-import(file, [read_file/1]).
-export([forms/1, source/1, split_dots/1, tokenize/1]).

% to try another scan & parse change these macros
-define(SCAN, uarini_scan).
-define(PARSE, uarini_parse).

forms(File) ->
    Source = source(File),
    Tokens = tokenize(Source),
    FrmTkn = split_dots(Tokens),
    lists:map(fun(T) -> {ok, F} = ?PARSE:parse(T), F end, FrmTkn).

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

source(FileName) ->
    {ok, Bin} = read_file(FileName),
    binary_to_list(Bin).

tokenize(Source) ->
    {ok, Tokens, _Lines} = ?SCAN:string(Source),
    Tokens.
