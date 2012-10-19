-module(cor).
-export([constructor/4, get_b/1, get_g/1, get_r/1, set_b/2, set_g/2, set_r/2]).

constructor(ObjectID, R, G, B) ->	 
	put({var_object, ObjectID, r, private}, R),
	put({var_object, ObjectID, g, private}, G),
	put({var_object, ObjectID, b, private}, B),
	ObjectID.

get_r(ObjectID) ->
	get({var_object, ObjectID, r, private}).

get_g(ObjectID) ->
	get({var_object, ObjectID, g, private}).

get_b(ObjectID) ->
	get({var_object, ObjectID, b, private}).

set_r(ObjectID, R) ->
	put({var_object, ObjectID, r, private}, R + somar_r(ObjectID, 100)).

set_g(ObjectID, G) ->
	put({var_object, ObjectID, g, private}, G).

set_b(ObjectID, B) ->
	put({var_object, ObjectID, b, private}, B).

somar_r(ObjectID, Value) ->
	get({var_object, ObjectID, r, private}) + Value.
