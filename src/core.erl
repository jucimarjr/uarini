-module(core).
-export([transform_uast_to_east/2]).
-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Converte o uast em erl.
transform_uast_to_east(CerlAST, ErlangModuleName) ->
	Module = "-module(" ++ atom_to_list(ErlangModuleName) ++ ").\n",
	[_Class, DefinitionList | _] = CerlAST,	
	OO = [match_definition(Definition)|| Definition <- DefinitionList],
	lists:flatten(Module ++ OO).

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
