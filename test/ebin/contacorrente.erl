-module(contacorrente).

-export([depositar/2, getSaldo/1, new/1, sacar/2]).

-implements(conta).

new(ValorInicial) ->
    ObjectID = ooe:new([{'Saldo', []}]),
    ooe:update_attr(ObjectID, 'Saldo', ValorInicial),
    {contacorrente, ObjectID}.

depositar(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'Saldo',
		    ooe:lookup_attr(ObjectID, 'Saldo') + Valor - 4.5e-1).

getSaldo(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Saldo').

sacar(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'Saldo',
		    ooe:lookup_attr(ObjectID, 'Saldo') - Valor - 4.5e-1).
