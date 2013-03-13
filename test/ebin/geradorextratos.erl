-module(geradorextratos).

-export([geradorConta/2, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {geradorextratos, ObjectID}.

geradorConta(ObjectID, Conta) ->
    io:format("Saldo atual: ~p~n",
	      [(erlang:element(1, Conta)):getSaldo(erlang:element(2,
								  Conta))]).
