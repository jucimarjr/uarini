-module(core).
-export([transform_uast_to_east/2]).
-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Converte o uast em erl.
transform_uast_to_east(CerlAST, ErlangModuleName) ->
	Module = "-module(" ++ atom_to_list(ErlangModuleName) ++ ").\n",
	[_Class, DefinitionList | _] = CerlAST,	
	OO = [match_definition(Definition)|| Definition <- DefinitionList],
	Attribute =
	case CerlAST of
		[_Class, _DefinitionList1, 
			{'class attributes.', AttributeList, '.'}, _Methods] ->
			transform_attribute_list(AttributeList);		
		_ -> 
			"\n"
	end,
	Method =
	case CerlAST of
		[_Class, _DefinitionList2, 
			_AttributeList, {'class methods.', MethodList, '.'}] ->
			transform_method_list(MethodList);		
		_ -> 
			"\n"
	end,	
	lists:flatten(Module ++ OO ++ Attribute ++ Method).

%%------------------------------------------------------------------------------
%% Pattern Match de Export
match_definition({export, ExportList}) ->
	transform_export_list(ExportList).

%%------------------------------------------------------------------------------
%% Transforma lista de export
transform_export_list([{Name, Arity} | Rest]) ->
	"-export([" ++ atom_to_list(Name) ++ 
	"/" ++ 
	integer_to_list(Arity) ++ 
	transform_export_rest(Rest).

transform_export_rest([{Name, Arity} | Rest]) ->
	", " ++ 
	atom_to_list(Name) ++ 
	"/" ++ 
	integer_to_list(Arity) ++ 
	transform_export_rest(Rest);
transform_export_rest([]) ->
	"]).\n".

%%------------------------------------------------------------------------------
%% Transforma lista de atributos
transform_attribute_list([{Name, Value}| Rest]) -> 
	"class_attributes()-> \n put({a, " ++ 
	atom_to_list(Name) ++ "}," ++ 
	resolve_param(Value) ++
	")" ++ 
	transform_attribute_rest(Rest).
transform_attribute_rest([{Name, Value} | Rest]) ->

	",\n put({a, " ++ 
	atom_to_list(Name) ++ "}," ++ 
	resolve_param(Value) ++
	")" ++ 
	transform_attribute_rest(Rest); 
transform_attribute_rest([]) ->
	".\n".

%%------------------------------------------------------------------------------
%% Transforma lista de métodos
transform_method_list([]) ->
	"\n";
transform_method_list([{Signature, MethodBody} | Rest]) ->
	{signature, MethodName, ParameterList} = Signature,
	Name = atom_to_list(MethodName),
	Parameter = "(" ++ resolve_parameter(ParameterList),
	Body = resolve_method_body(MethodBody), 	
	Name ++ Parameter ++ Body ++ transform_method_list(Rest).

%%------------------------------------------------------------------------------
%% Transforma lista de parâmetros
resolve_parameter([]) -> 
	") ->\n";
resolve_parameter([Parameter | Rest]) ->
	Param = resolve_param(Parameter),	
	case Rest of
		[]-> Param ++ resolve_parameter(Rest);
		_ -> Param ++ ", " ++ resolve_parameter(Rest)
	end.

resolve_param(Value) when is_integer(Value) -> integer_to_list(Value);
resolve_param(Value) when is_float(Value) -> float_to_list(Value);
resolve_param(Value) when is_atom(Value) -> 
	case string:to_lower(atom_to_list(Value)) of
		"null" 	-> "nil";
		_	-> atom_to_list(Value)
	end;
resolve_param(Value) when is_list(Value) -> Value;
resolve_param(Value) when is_tuple(Value) -> 
	case Value of
		{tuple, Tuple} -> 
			resolve_tuple(Tuple);
		{list, List} -> 
			resolve_list(List);
		{list, List, EndList} ->
			resolve_list(List) ++ " | " ++ resolve_param(EndList)
	end.

%%------------------------------------------------------------------------------
%% Transforma tupla
resolve_tuple(Tuple) when is_tuple(Tuple) -> "{}";
resolve_tuple([Element | Rest]) ->
	case Rest of
		[]-> "{" ++ resolve_param(Element) ++ "}";
		_ -> "{" ++ resolve_param(Element) ++ ", " ++ 
			resolve_tuple_rest(Rest)
	end.

resolve_tuple_rest([Element | Rest]) ->
	case Rest of
		[]-> resolve_param(Element) ++ "}";
		_ -> resolve_param(Element) ++ ", " ++ 
			resolve_tuple_rest(Rest)
	end.
	
%%------------------------------------------------------------------------------
%% Transforma lista
resolve_list(List) when is_tuple(List) -> "[]";
resolve_list([Element | Rest]) ->
	case Rest of
		[]-> "[" ++ resolve_param(Element) ++ "]";
		_ -> "[" ++ resolve_param(Element) ++ ", " ++ 
			resolve_list_rest(Rest)
	end.

resolve_list_rest([Element | Rest]) ->
	case Rest of
		[]-> resolve_param(Element) ++ "]";
		_ -> resolve_param(Element) ++ ", " ++ 
			resolve_list_rest(Rest)
	end.

%%------------------------------------------------------------------------------
%% Transforma corpo do método

resolve_method_body([Head | Rest]) ->
	ErlangStatment =
	case Head of
		{Name, {none}} -> atom_to_list(Name) ++ "()"; 
		_ -> "ok"
	end,
	case Rest of
		[]	-> ErlangStatment ++ ".\n";
		_ 	-> ErlangStatment ++ ",\n" ++ resolve_method_body(Rest)	
	end.
