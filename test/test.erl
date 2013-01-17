-module(test).
-include_lib("eunit/include/eunit.hrl").

compile_all_beam_test_() ->
	{
		"Uarini compiling .CERL and generating .BEAM...",
		[compile_beam(CerlFile) ||
			CerlFile <-  
				filelib:wildcard("examples/uarini/*/*.cerl") ++
				filelib:wildcard("examples/mpi/*/*.cerl")
		]
	
		%%[compile_beam(CerlFile) ||
		%%	CerlFile <-  
		%%		filelib:wildcard("examples/uarini/*/*.cerl") ++
		%%		filelib:wildcard("design_patterns/*/Uarini/*.cerl")
		%%]
	
	}.

compile_beam(CerlFile) ->
	{
	CerlFile,
	?_assertEqual({CerlFile, ok},
		{CerlFile, element(1,
			uarini_build:get_ast(CerlFile))})
	}.