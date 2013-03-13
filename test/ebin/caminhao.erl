-module(caminhao).

-export([calcularIpva/1, constructor/3, new_/1]).

new_(Param_1) -> ipva:new_(Param_1).

constructor(Ano, Placa, QtdEixos) ->
    ObjectID = ooe:new([{'Ano', []}, {'Placa', []},
			{'QtdEixos', []}, {'VlrBase', []}, {'QtdCavalos', []},
			{'QtdEixos', []}]),
    ooe:update_attr(ObjectID, 'Ano', Ano),
    ooe:update_attr(ObjectID, 'Placa', Placa),
    ooe:update_attr(ObjectID, 'QtdEixos', QtdEixos),
    ooe:update_attr(ObjectID, 'VlrBase',
		    1.19999999999999995559),
    ooe:update_attr(ObjectID, 'QtdCavalos', 150),
    {caminhao, ObjectID}.

calcularIpva(ObjectID) ->
    ooe:lookup_attr(ObjectID, 'VlrBase') *
      ooe:lookup_attr(ObjectID, 'QtdCavalos')
      * ooe:lookup_attr(ObjectID, 'QtdEixos').
