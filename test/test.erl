-module(test).
-include_lib("eunit/include/eunit.hrl").

get_ast_all_test_() ->
	{
		"ooErlang getting .CERL and generating AST...",
		[get_ast(CerlFile) ||
			CerlFile <-  
				filelib:wildcard("examples/uarini/*/*.cerl") ++
				filelib:wildcard("examples/mpi/*/*.cerl")
		]
	}.

compile_tokenize_test_() ->
    {"ooErlang comparing preprocessor and scanner",
    [compare_raw_preprocessed_tokens(CerlFile) ||
		CerlFile <-
			filelib:wildcard("examples/uarini/*/*.cerl") ++
			filelib:wildcard("examples/mpi/*/*.cerl")
        ]}.

compile_01_objects_test_() ->
	?_assertEqual({[{ok,gerenciadorDePessoas},{ok,pessoa}]},{uarini:compile(filelib:wildcard("examples/uarini/01_objects/*.cerl"))}).

compile_pingping_test_() ->
	?_assertEqual({[{ok,pingping},{ok,proc}]},{uarini:compile(filelib:wildcard("examples/mpi/pingping/*.cerl"))}).

compare_raw_preprocessed_tokens(CerlFile) ->
    {CerlFile, ?_assertEqual(
        uarini_build:get_raw_tokens(CerlFile),
        uarini_build:get_tokens(CerlFile)
    )}.

get_ast(CerlFile) ->
	{
	CerlFile,
	?_assertEqual({CerlFile, ok},
		{CerlFile, element(1,
			uarini_build:get_ast(CerlFile))})
	}.


