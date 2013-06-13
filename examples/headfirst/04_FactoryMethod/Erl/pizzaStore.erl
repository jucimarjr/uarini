-module(pizzaStore).
-export([create_pizza/1, order_pizza/2]).

create_pizza(Item) -> 
	separate_module:function_implemented(Item).

order_pizza(Type, String) ->
	Pizza = create_pizza(Type),
	erlang:register(object_pizza, Pizza),

	io:format("--- Making a ~p ---~n", [get_name(object_pizza)]),
	separate_module2:prepare(object_pizza, String),
	separate_module2:bake(object_pizza, String),
	separate_module2:cut(object_pizza, String),
	separate_module2:box(object_pizza, String),
	object_pizza.
	
get_name(Object) ->
    AttrName = separate_module3:get_name_attribute(Object),
    AttrName.