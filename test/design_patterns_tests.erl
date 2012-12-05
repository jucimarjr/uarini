
%% Autor      : Emiliano Carlos de Moraes Firmino ( emiliano.firmino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Descricao  : Unit tests of uarini_parse

-module(design_patterns_tests).
-author('emiliano.firmino@gmail.com').
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

source_filenames() ->
    filelib:wildcard("design_patterns/*/Uarini/*.cerl").

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

check_list([]) ->
    ok;
check_list([ok|L]) ->
    check_list(L);
check_list(_) ->
    error.

merge_list(L1,L2) ->
    merge_list(L1,L2,[]).

merge_list([],[],R) ->
    lists:reverse(R);
merge_list([H1|L1],[H2|L2],R) ->
    merge_list(L1,L2,[{H1,H2}|R]).

scan_file(Filename) ->
    {ok, BinData} = file:read_file(Filename),
    {ok, Ts, _Ls} = uarini_scan:string(binary_to_list(BinData)),
    Ts.

parse_test() ->
    Fs = source_filenames(),
    CC = fun(Filename) ->
            FTL = split_forms(scan_file(Filename)),
            Parse = fun(Ts) -> {R,_} = uarini_parse:parse(Ts), R end,
            Forms = lists:map(Parse, FTL),
            {check_list(Forms), Filename}
         end,
    L = lists:map(CC, Fs).

