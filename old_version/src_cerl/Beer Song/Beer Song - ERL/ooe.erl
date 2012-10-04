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
	{key, Key} = request({key}),
	{constructor, Function} = request({constructor, {self(), constructor, Class, Constructor}}),
	apply(Function, Parameters ++ [Key]),
	Key.
	
method(Class, Method, Parameters, Key) ->
	{method, Function} = request({method, {self(), method, Class, Method}}),
	apply(Function, Parameters ++ [Key]).	

request(Request) ->	
	case Request of
		{key} ->
			global:send(?SERVER, {self(), object}),
			key;
		{constructor, Constructor} ->
			global:send(?SERVER, Constructor),
			constructor;			
		{method, Method} ->
			global:send(?SERVER, Method),
			method		
	end,
	receive
		Return -> Return 
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
			Function = get({constructor, Class, Constructor}),
			From ! {constructor, Function};
		{From, method, Class, Method} ->
			Function = get({method, Class, Method}), 			
			From ! {method, Function};
		{From, object} ->
			case get(object) of
				undefined -> 
					put(object, 0),
					From ! {key, 0};
				Number ->
					NewNumber = Number + 1,
					put(object, NewNumber),
					From ! {key, get(object)} 
			end
	end,
	handle().
