-module (ooe).
-compile([export_all]).
-define(SERVER, ooe).

start() ->
	global:trans({?SERVER, ?SERVER},
		fun() ->
			case global:whereis_name(?SERVER) of
				undefined ->
					Pid = spawn(ooe, handle, []),
					global:register_name(?SERVER, Pid);
				_ ->
					ok
			end
		end).	

def_class(Class) ->
	global:send(?SERVER, {class, Class}),
	ok.
	
def_superclass(Class, SuperClass) ->
	global:send(?SERVER, {superclass, Class, SuperClass}),	
	ok.

def_import(Class, Import) ->
	global:send(?SERVER, {import, Class, Import}),
	ok.

def_constructor(Class, Constructor, ConstructorFunction) ->
	global:send(?SERVER, {constructor, Class, Constructor, ConstructorFunction}),
	ok.

def_attribute(Class, Attribute)->
	global:send(?SERVER, {attribute, Class, Attribute}),	
	ok.

def_method(Class, Method, MethodFunction)->
	global:send(?SERVER, {method, Class, Method, MethodFunction}),
	ok.

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
	global:send(?SERVER, {self(), constructor, Class, Constructor}),
	receive
		undefined -> 
			error;
		Function ->
			apply(Function, Parameters ++ [Key])
	after 
		1000 ->
			error
	end,	
	Key.

method(Class, Method, Parameters, Key) ->
	global:send(?SERVER, {self(), method, Class, Method}),
	receive
		undefined -> 
			error;
		Function ->
			apply(Function, Parameters ++ [Key])
	after 
		1000 ->
			error
	end.

handle() ->
	receive
		{class, Class} ->
			put({class, Class}, Class);			
		{superclass, Class, SuperClass} ->
			put({superclass, Class}, SuperClass);
		{import, Class, Import} ->
			put({import, Class, Import}, Import);
		{constructor, Class, Constructor, ConstructorFunction} ->
			put({constructor, Class, Constructor}, ConstructorFunction);
		{attribute, Class, Attribute} ->
			put({attribute, Class, Attribute}, Attribute);
		{method, Class, Method, MethodFunction} ->
			put({method, Class, Method}, MethodFunction);
		{From, constructor, Class, Constructor} ->
			From ! get({constructor, Class, Constructor});
		{From, method, Class, Method} ->
			From ! get({method, Class, Method})
	end,
	handle().
