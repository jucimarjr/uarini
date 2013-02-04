-module(circulo).

-export([calcularArea/1, constructor/1]).

-implements(figura).

constructor(Raio) ->
    ObjectID = ooe:new(['Raio']),
    ooe:update_attr(ObjectID, 'Raio', Raio),
    {circulo, ObjectID}.

calcularArea(ObjectID) ->
    Area = 3.14 * Raio * Raio, Area.

