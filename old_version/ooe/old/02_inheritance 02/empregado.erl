-module(empregado, []).
-export[].

constructor() ->
	Key = key(),
	Key.

getSalario({Key, self}) ->
	-1.

key() ->
	case get({empregado, key}) of
		undefined ->
			put({empregado, key}, 0),
			0;
		Key ->
			put({empregado, key}, Key + 1),
			Key + 1
	end.
