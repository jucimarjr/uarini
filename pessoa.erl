-module(pessoa).

-export([constructor/2, gastar/2, receber/2]).

constructor(Nome, Nasc) ->
    ObjectID = ooe:new(['Nome', 'Nascimento',
			'DinheiroNaCarteira']),
    ooe:update_attr(ObjectID, 'Nome', Nome),
    ooe:update_attr(ObjectID, 'Nascimento', Nasc),
    ooe:update_attr(ObjectID, 'DinheiroNaCarteira', 0),
    {pessoa, ObjectID}.

gastar(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'DinheiroNaCarteira',
		    ooe:lookup_attr(ObjectID, 'DinheiroNaCarteira') -
		      Valor).

receber(ObjectID, Valor) ->
    ooe:update_attr(ObjectID, 'DinheiroNaCarteira',
		    ooe:lookup_attr(ObjectID, 'DinheiroNaCarteira') +
		      Valor).
