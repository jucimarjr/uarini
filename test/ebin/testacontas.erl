-module(testacontas).

-export([main/0, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {testacontas, ObjectID}.

main() ->
    CC = contacorrente:new(0),
    (erlang:element(1, CC)):depositar(erlang:element(2, CC),
				      1200),
    (erlang:element(1, CC)):sacar(erlang:element(2, CC),
				  300),
    CP = contapoupanca:new(0),
    (erlang:element(1, CP)):depositar(erlang:element(2, CP),
				      500),
    (erlang:element(1, CP)):sacar(erlang:element(2, CP),
				  25),
    Gerador = geradorextratos:new_(),
    (erlang:element(1,
		    Gerador)):geradorConta(erlang:element(2, Gerador), CC),
    (erlang:element(1,
		    Gerador)):geradorConta(erlang:element(2, Gerador), CP).
