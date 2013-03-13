-module(contapoupanca).

-export([depositar/2, getSaldo/1, new/1, sacar/2]).

-implements(conta).

new(ValorInicial) ->
    ObjectID = ooe:new([{'Saldo', []}]),
    ooe:update_attr(ObjectID, 'Saldo', ValorInicial),
    {contapoupanca, ObjectID}.

depositar(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'Saldo',
		    ooe:lookup_attr(ObjectID, 'Saldo') + Valor).

getSaldo(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'Saldo').

sacar(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'Saldo',
		    ooe:lookup_attr(ObjectID, 'Saldo') - Valor).
