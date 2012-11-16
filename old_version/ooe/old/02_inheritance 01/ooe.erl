-module(ooe).

def_class(Class) ->
	put({class, Class}, Class).

def_constructor(Class, Constructor, ConstructorFunctio) ->
	put({constructor, Class, Constructor}, ConstructorFunction),
	ok.

constructor(Class, Constructor, Parameters) ->
	apply(get({constructor, Class, Constructor}), Parameters).

key(Class) ->
	case get({Class, key}) of
		undefined ->
			put({Class, key}, 0),
			0;
		Key ->
			put({Class, key}, Key + 1),
			Key + 1
	end.
