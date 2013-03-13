-module(quadrado).

-export([calcularArea/1, new/1]).

-implements(figura).

new(Lado) ->
    ObjectID = ooe:new([{'Lado', []}]),
    ooe:update_attr(ObjectID, 'Lado', Lado),
    {quadrado, ObjectID}.

calcularArea(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Lado') *
      ooe:lookup_attr(ObjectID, 'Lado').
