-module(horista).

-export([get_salario/1, new/0, set_salario/2]).

set_salario(Param_1, Param_2) ->
    empregado:set_salario(Param_1, Param_2).

new() ->
    ObjectID = ooe:new([{'Salario', []}]),
    horista_new_ok,
    {horista, ObjectID}.

get_salario(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Salario') / 220.
