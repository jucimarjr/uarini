-module(uarini_utils).
-compile(export_all).

-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Compila vários códigos em Uarini dependentes e gera .erl's correspondentes
compile(CerlFileNameList) ->
	case catch(uarini:compile(CerlFileNameList)) of
		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason]);
		{error, Errors} ->
			io:format("*******ERROR!~n"),
			io:format("***Reasons:\n"),
			uarini_exception:print_errors(Errors);
		ok ->
			ok;
		X ->
			io:format("*******UNEXPECTED ERROR!~n"),
			io:format("***Reason:~n~p", [X])
	end.

%%-----------------------------------------------------------------------------
%% Extrai a Erlang Abstract Syntax Tree de um arquivo .cerl
get_erl_ast(CerlFileName) ->
	CerlAST = ast:get_cerl_ast(CerlFileName),
	[{_Line, {class, ClassName}, _ClassBody}] = CerlAST,
	ModuleName = list_to_atom(string:to_lower(atom_to_list(ClassName))),
	core:transform_uast_to_east(CerlAST, ModuleName, []).

%%-----------------------------------------------------------------------------
%% Imprime a árvore do cerl gerada análise sintática do compilador
print_ast(CerlFileName) ->
	io:format("Generating Syntax Analysis... "),

	case catch(ast:get_cerl_ast(CerlFileName)) of
		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason]);
		CerlAST ->
			io:format("done!~n"),

			io:format("Uarini Syntax Tree from ~p:~n", [CerlFileName]),
			io:format("~p~n", [CerlAST])
	end.

%%-----------------------------------------------------------------------------
%% Imprime os tokens do cerl gerados pela análise léxica do compilador
print_tokens(CerlFileName) ->
	io:format("Generating Lexical Analysis... "),

	Tokens = ast:get_cerl_tokens(CerlFileName),
	io:format("done!~n"),

	io:format("Uarini Tokens from ~p:~n", [CerlFileName]),
	io:format("~p~n", [Tokens]).

%%-----------------------------------------------------------------------------
%% Transforma o código erlang em Abstract Syntax Tree
print_erl_ast(CerlFileName) ->
	io:format("Generating Erlang Abstract Syntax Tree... "),

	case catch(get_erl_ast(CerlFileName)) of
		{ok, ErlangAST} ->
			io:format("done!~n"),

			io:format("Erlang Abstract Syntax Tree from ~p:~n", [CerlFileName]),
			io:format("~p~n", [ErlangAST]);

		{'EXIT', Reason} ->
			io:format("*******ERROR!~n"),
			io:format("***Reason:~n~p", [Reason])
	end.

%%-----------------------------------------------------------------------------
%% Transforma o código erlang em Abstract Syntax Tree
print_erl_ast_from_erlang(ErlangFileName) ->
	io:format("Generating Erlang Abstract Syntax Tree from Erlang... "),

	ErlangAst =
		case epp:parse_file(ErlangFileName, [], []) of
			{ok, Tree} -> Tree;
			Error -> io:format("~p", [Error])
		end,
	io:format("done!~n"),

	io:format("Erlang Abstract Syntax Tree from ~p:~n", [ErlangFileName]),
	io:format("~p~n", [ErlangAst]).

%%-----------------------------------------------------------------------------
%% calcula o tempo de execução de uma funcao em microssegundos
get_runtime(Module, Func, N )->
	{ElapsedTime, R} = timer:tc(Module, Func, [N]),
	io:format("~p(~p): ~p [~p us] [~p s] ~n",
				[Func, N, R, ElapsedTime, ElapsedTime/1000000]).

get_runtime(Module, Func )->
	{ElapsedTime, R} = timer:tc(Module, Func, []),
	io:format("~p(~p): [~p us] [~p s] ~n",
				[Func, R, ElapsedTime, ElapsedTime/1000000]).
