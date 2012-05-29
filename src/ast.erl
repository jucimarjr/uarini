-module(ast).
-export([get_cerl_ast/1, get_cerl_tokens/1]).

%%-----------------------------------------------------------------------------
%% Extrai a Uarini Abstract Syntax Tree de um arquivo .cerl
get_cerl_ast(CerlFileName) ->
	Tokens = get_cerl_tokens(CerlFileName),
	{ok, UAST} = uarini_parser:parse(Tokens),
	UAST.

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo .cerl
get_cerl_tokens(CerlFileName) ->
	{ok, FileContent} = file:read_file(CerlFileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_lexer:string(Program),
	Tokens.
