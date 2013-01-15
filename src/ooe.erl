-module (ooe).
-compile([export_all]).

%% criação de objetos
new(AttrList) ->
	AttrList2 = [{AttrName, []} || AttrName <- AttrList],
	spawn(ooe, obj_loop, [AttrList2]).

%% função que os processos de objeto rodam para guardar os valores dos attr
%% o nome da classe fica na propria variavel
obj_loop(AttrList) ->
	receive
		{Sender, attr_lookup, AttrName} ->
			case orddict:find(AttrName, AttrList) of
				{ok, AttrValue} ->
					Sender ! {self(), attr_lookup, {value, AttrValue}};
				error ->
					Sender ! {self(), attr_lookup, {error, attr_not_found}}
			end,
			obj_loop(AttrList);

		{Sender, attr_update, AttrName, AttrValue} ->
			case orddict:find(AttrName, AttrList) of
				{ok, _} ->
					Sender ! {self(), attr_update, ok},
					obj_loop(orddict:store(AttrName, AttrValue, AttrList));

				error ->
					Sender ! {self(), attr_update, {error, attr_not_found}},
					obj_loop(AttrList)
			end;

		{Sender, destroy} ->
			Sender ! {self(), destroy, ok},
			ok
	end.

%% API para consulta e alteração de campos
lookup_attr(ObjectID, attr_lookup, AttrName) ->
	Self = self(),
	ObjectID ! {Self, attr_lookup, AttrName},
	receive
		{ObjectID, attr_lookup, {value, AttrValue}} ->
			AttrValue;

		{ObjectID, attr_lookup, {error, attr_not_found}} ->
			throw({attr_lookup, {error, attr_not_found}})
	end.

update_attr(ObjectID, AttrName, NewValue) ->
	Self = self(),
	ObjectID ! {Self, attr_update, AttrName, NewValue},
	receive
		{ObjectID, attr_update, ok} ->
			ok;

		{ObjectID, attr_update, {error, attr_not_found}} ->
			throw({attr_update, {error, attr_not_found}})
	end.
