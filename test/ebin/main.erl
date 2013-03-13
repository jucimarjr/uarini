-module(main).

-export([main/0, new_/0]).

new_() -> ObjectID = ooe:new([]), {main, ObjectID}.

main() ->
    F1 = quadrado:new(4),
    F2 = circulo:new(2),
    io:format("Area da Figura 1 eh: ~p~n",
	      [(erlang:element(1, F1)):calcularArea(erlang:element(2,
								   F1))]),
    io:format("Area da Figura 2 eh: ~p~n",
	      [(erlang:element(1, F2)):calcularArea(erlang:element(2,
								   F2))]).
