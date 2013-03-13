-module(mensalista).

-export([get_salario/1, new/0, set_salario/2]).

set_salario(Param_1, Param_2) ->
    empregado:set_salario(Param_1, Param_2).

new() ->
    ObjectID = ooe:new([{'Salario', []}]),
    mensalista_new_ok,
    {mensalista, ObjectID}.

get_salario(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Salario') / 30.
