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
		[_Class, _DefinitionList, 
			{'class attributes.', AttributeList, '.'}, _Methods] ->
			transform_attribute_list(AttributeList);		
		_ -> 
			"\n"
	end,	
	lists:flatten(Module ++ OO ++ Attribute).

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
	convert_value(Value) ++
	")" ++ 
	transform_attribute_rest(Rest).

transform_attribute_rest([{Name, Value} | Rest]) ->
	",\n put({a, " ++ 
	atom_to_list(Name) ++ "}," ++ 
	convert_value(Value) ++
	")" ++ 
	transform_attribute_rest(Rest); 
transform_attribute_rest([]) ->
	".\n".

convert_value(Value) when is_integer(Value) -> integer_to_list(Value);
convert_value(Value) when is_float(Value) -> float_to_list(Value);
convert_value(Value) when is_atom(Value) -> 
	case string:to_lower(atom_to_list(Value)) of
		"null" 	-> "nil";
		_	-> atom_to_list(Value)
	end;
convert_value(Value) when is_list(Value) -> Value.
