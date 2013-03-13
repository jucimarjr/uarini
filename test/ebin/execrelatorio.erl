-module(execrelatorio).

-export([funcao_aux/1, main/0, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {execrelatorio, ObjectID}.

funcao_aux(Rel) ->
    io:format("~p~n",
	      [(erlang:element(1, Rel)):getSql(erlang:element(2,
							      Rel))]).

main() ->
    RelatorioCli = relatorioclientes:new_(),
    RelatorioPro = relatorioprodutos:new_(),
    funcao_aux(RelatorioCli),
    funcao_aux(RelatorioPro).
