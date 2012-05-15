-module (eoo).
-compile([export_all]).

def_superclass(Class,SuperClass)->
	put({Class,superclass},SuperClass),
	ok.

def_constructor(Class,ConstructorFunction)->
	put({Class,constructor},ConstructorFunction),
	ok.

def_atributte({Class,[Atributtes]})->
	put({Class,atributtes},[Atributtes]),
	ok.

def_methods({Class,[Methods]})->
	put({Class,methods},[Methods]),
	ok.

