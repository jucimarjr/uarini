-module (ooe).
-compile([export_all]).

%%put methods

def_class(Class)->
	put({class, Class}, Class),
	ok.

def_superclass(Class, SuperClass)->
	put({superclass, Class}, SuperClass),
	ok.

def_import(Class, Import)->
	put({import, Class, Import}, Import),
	ok.

def_constructor(Class, Constructor, ConstructorFunction)->
	put({constructor, Class, Constructor}, ConstructorFunction),
	ok.

def_attribute(Class, Attribute)->
	put({attribute, Class, Attribute}, Attribute),
	ok.

def_method(Class, Method, MethodFunction)->
	put({method, Class, Method}, MethodFunction),
	ok.

%%get methods

constructor(Class, Constructor, Parameters) ->
	Key =
		case get(object) of
			undefined -> 
				put(object, 0),
				0;
			Number ->
				put(object, Number + 1),
				Number + 1  
		end,
	apply(get({constructor, Class, Constructor}), Parameters ++ [Key]),
	Key.

method(Class, Method, Parameters, Key) ->
	apply(get({method, Class, Method}), Parameters ++ [Key]).
