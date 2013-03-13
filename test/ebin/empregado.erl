-module(empregado).

-export([get_salario/1, new_/0, set_salario/2]).

new_() ->
    ObjectID = ooe:new([{'Salario', []}]),
    {empregado, ObjectID}.

get_salario(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Salario').

set_salario(ObjectID, Value) ->
    ooe:update_attr(ObjectID, 'Salario', Value).
