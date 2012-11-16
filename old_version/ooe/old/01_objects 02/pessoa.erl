-module(pessoa, [Nome, Nascimento]).
-export([constructor/0]).
-export([gastar/2, receber/2, get_dinheiro/1]).

constructor() ->
	Key = key(),
	put({pessoa, nome, Key}, Nome),
	put({pessoa, nascimento, Key}, Nascimento),
	put({pessoa, dinheiroNaCarteira, Key}, 0),
	Key.

gastar(Valor, {Key, self}) ->
	put({pessoa, dinheiroNaCarteira, Key}, 
			get({pessoa, dinheiroNaCarteira, Key}) - Valor).	

receber(Valor, {Key, self}) ->
	put({pessoa, dinheiroNaCarteira, Key}, 
			get({pessoa, dinheiroNaCarteira, Key}) + Valor).

get_dinheiro({Key, self}) ->
	get({pessoa, dinheiroNaCarteira, Key}).

key() ->
	case get({pessoa, key}) of
		undefined ->
			put({pessoa, key}, 0),
			0;
		Key ->
			put({pessoa, key}, Key + 1),
			Key + 1
	end.
