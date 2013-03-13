-module(circulo).

-export([calcularArea/1, new/1]).

-implements(figura).

new(Raio) ->
    ObjectID = ooe:new([{'Raio', []}]),
    ooe:update_attr(ObjectID, 'Raio', Raio),
    {circulo, ObjectID}.

calcularArea(ObjectID) ->
    3.14 * ooe:lookup_attr(ObjectID, 'Raio') *
      ooe:lookup_attr(ObjectID, 'Raio').
