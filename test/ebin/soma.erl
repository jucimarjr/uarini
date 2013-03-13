-module(soma).

-export([calcular/3, new_/0]).

new_() -> ObjectID = ooe:new([]), {soma, ObjectID}.

calcular(ObjectID, X, Y) -> X + Y.
