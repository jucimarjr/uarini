-module(subtracao).

-export([calcular/3, new_/0]).

new_() -> ObjectID = ooe:new([]), {subtracao, ObjectID}.

calcular(ObjectID, X, Y) -> X - Y.
