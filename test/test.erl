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

compile_ooe_all_test_() ->
	{
		"ooErlang compiling .CERL and generating .ERL and .BEAM...",
		[compile_ooe(Dir) || Dir <- examples_list()]
	}.

compile_ooe(Dir) ->
{
	Dir,
	?_assertEqual({gen_ok_module(Dir)},{uarini:compile(filelib:wildcard(Dir))})
}.


%% examples_list() ->
%% 	[
%% 	 "examples/uarini/01_objects/*.cerl",
%% 	 "examples/uarini/02_inheritance/*.cerl",
%% 	 "examples/uarini/03_polymorphism/*.cerl",
%% 	 "examples/uarini/04_polymorphism/*.cerl",
%% 	 "examples/uarini/05_polymorphism/*.cerl",
%% 	 "examples/uarini/06_interface/*.cerl",
%% 	 "examples/uarini/07_interface/*.cerl",
%% 	 "examples/uarini/08_interface/*.cerl",
%% 	 "examples/uarini/09_polymorphism/*.cerl",
%% 	 "examples/uarini/10_interface/*.cerl",
%% 	 "examples/mpi/pingping/*.cerl",
%% 	 "examples/mpi/pingpong/*.cerl",
%% 	 "examples/mpi/threadring/*.cerl"
%% 	 ].

examples_list() ->
	[
	 "examples/uarini/01_objects/*.cerl",
	 "examples/uarini/02_inheritance/*.cerl",
 	 "examples/uarini/03_polymorphism/*.cerl",
 	 "examples/uarini/04_polymorphism/*.cerl",
	 "examples/uarini/05_polymorphism/*.cerl",
	 "examples/uarini/06_interface/*.cerl",
	 "examples/uarini/07_interface/*.cerl",
	 "examples/uarini/08_interface/*.cerl",
	 "examples/uarini/09_polymorphism/*.cerl",
	 "examples/uarini/10_interface/*.cerl",
	 "examples/mpi/pingping/*.cerl",
 	 "examples/mpi/pingpong/*.cerl",
 	 "examples/mpi/threadring/*.cerl"
	 ].

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


gen_ok_module( Dir ) ->
	lists:map( 
	  fun(Arg) -> {ok,list_to_atom(filename:basename(Arg,".cerl"))} end,
	  filelib:wildcard(Dir)
	).


