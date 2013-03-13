-module(operacaomatematica).

-export([calcular/3, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {operacaomatematica, ObjectID}.

calcular(ObjectID, X, Y) -> null.
