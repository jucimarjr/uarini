-module(test).
-include_lib("eunit/include/eunit.hrl").

%% compile_all_test_() ->
%% 	{
%% 		"Jaraki compiling test...",
%% 		[compile_java(CerlFile) || CerlFile <- filelib:wildcard("java_src/*.java")]
%% 	}.
%% 

%% compile_all_error_test_() ->
%% 	{
%% 		"Jaraki compiling .JAVA with errors...",
%% 		[compile_error(CerlFile) || CerlFile <-	filelib:wildcard("java_erro/*.java")]
%% 	}.
%% 
compile_all_beam_test_() ->
	{
		"Uarini compiling .CERL and generating .BEAM...",
		[compile_beam(CerlFile) ||
			CerlFile <-  
				filelib:wildcard("examples/uarini/*/*.cerl") ++
				filelib:wildcard("design_patterns/*/Uarini/*.cerl")
		]
	
		%%[compile_beam(CerlFile) ||
		%%	CerlFile <-  
		%%		filelib:wildcard("examples/uarini/*/*.cerl") ++
		%%		filelib:wildcard("design_patterns/*/Uarini/*.cerl")
		%%]
	
	}.

compile_beam(CerlFile) ->
	?_assertEqual({CerlFile, ok},
		{CerlFile, element(1,
			uarini_build:get_ast(CerlFile))}).

%% compile_error(CerlFile) ->
%% 	{filename:basename(CerlFile), [?_assertError(badarg, jaraki:compile({beam,CerlFile}))]}.
