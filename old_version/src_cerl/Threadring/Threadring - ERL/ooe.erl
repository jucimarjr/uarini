-module (ooe).
-compile([export_all]).

def_class(Class)->
	put({class, Class}, Class),
	ok.

def_superclass(Class, SuperClass)->
	put({superclass, Class}, SuperClass),
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

constructor(Class, Constructor, Parameters) ->
	apply(get({constructor, Class, Constructor}), Parameters).
