-module (ooe).
-compile([export_all]).

def_class(Class)->
	put({class},Class),
	ok.

def_superclass(Class,SuperClass)->
	put({Class,superclass},SuperClass),
	ok.

def_constructor(Class,ConstructorFunction)->
	put({Class,constructor},ConstructorFunction),
	ok.

def_attribute({Class,[Attributes]})->
	put({Class,attributes},[Attributes]),
	ok.

def_methods({Class,[Methods]})->
	put({Class,methods},[Methods]),
	ok.
