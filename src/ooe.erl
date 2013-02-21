-module (ooe).
-compile([export_all]).

%% criação de objetos
new(AttrList) ->
	ODict = orddict:from_list(AttrList),
	spawn(ooe, obj_loop, [ODict]).

%% função que os processos de objeto rodam para guardar os valores dos attr
%% o nome da classe fica na propria variavel
obj_loop(ODict) ->
	receive
		{Sender, attr_lookup, AttrName} ->
			case orddict:find(AttrName, ODict) of
				{ok, AttrValue} ->
					Sender ! {self(), attr_lookup, {value, AttrValue}};
				error ->
					Msg = {error, attr_not_found, AttrName},
					Sender ! {self(), attr_lookup, Msg}
			end,
			obj_loop(ODict);

		{Sender, attr_update, AttrName, AttrValue} ->
			case orddict:find(AttrName, ODict) of
				{ok, _} ->
					Sender ! {self(), attr_update, ok},
					obj_loop(orddict:store(AttrName, AttrValue, ODict));

				error ->
					Msg = {error, attr_not_found, AttrName},
					Sender ! {self(), attr_update, Msg},
					obj_loop(ODict)
			end;

		{Sender, destroy} ->
			Sender ! {self(), destroy, ok},
			ok
	end.

%% API para consulta e alteração de campos
lookup_attr(ObjectID, AttrName) ->
	Self = self(),
	ObjectID ! {Self, attr_lookup, AttrName},
	receive
		{ObjectID, attr_lookup, {value, AttrValue}} ->
			AttrValue;

		{ObjectID, attr_lookup, {error, attr_not_found, Attr}} ->
			throw({attr_lookup, {error, attr_not_found, Attr}})
	end.

update_attr(ObjectID, AttrName, NewValue) ->
	Self = self(),
	ObjectID ! {Self, attr_update, AttrName, NewValue},
	receive
		{ObjectID, attr_update, ok} ->
			ok;

		{ObjectID, attr_update, {error, attr_not_found, AttrName}} ->
			throw({attr_update, {error, attr_not_found, AttrName}})
	end.

destroy(ObjectID) ->
	ObjectID ! {self(), destroy},
	receive
		{ObjectID, destroy, ok} -> ok
	end.
