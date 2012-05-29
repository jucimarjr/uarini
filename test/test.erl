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
		[compile_beam(CerlFile) || CerlFile <-	filelib:wildcard("src_cerl/*.cerl")]
	}.

compile_beam(CerlFile) ->
	{filename:basename(CerlFile), [?_assertEqual(ok, jaraki:compile({beam,CerlFile}))]}.

%% compile_error(CerlFile) ->
%% 	{filename:basename(CerlFile), [?_assertError(badarg, jaraki:compile({beam,CerlFile}))]}.
