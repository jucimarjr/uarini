-module(test).
-include_lib("eunit/include/eunit.hrl").

get_ast_all_test_() ->
	{
		"ooErlang getting .CERL and generating AST...",
		[get_ast(CerlFile) ||
			CerlFile <-  
				filelib:wildcard("examples/ooe/*/*.cerl") ++
				filelib:wildcard("examples/mpi/*/*.cerl")
		]
	}.

compile_tokenize_test_() ->
    {"ooErlang comparing preprocessor and scanner",
    [compare_raw_preprocessed_tokens(CerlFile) ||
		CerlFile <-
			filelib:wildcard("examples/ooe/*/*.cerl") ++
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
	?_assertEqual({gen_ok_module(Dir)},{ooec:compile(filelib:wildcard(Dir))})
}.


%% examples_list() ->
%% 	[
%% 	 "examples/ooe/01_objects/*.cerl",
%% 	 "examples/ooe/02_inheritance/*.cerl",
%% 	 "examples/ooe/03_polymorphism/*.cerl",
%% 	 "examples/ooe/04_polymorphism/*.cerl",
%% 	 "examples/ooe/05_polymorphism/*.cerl",
%% 	 "examples/ooe/06_interface/*.cerl",
%% 	 "examples/ooe/07_interface/*.cerl",
%% 	 "examples/ooe/08_interface/*.cerl",
%% 	 "examples/ooe/09_polymorphism/*.cerl",
%% 	 "examples/ooe/10_interface/*.cerl",
%% 	 "examples/mpi/pingping/*.cerl",
%% 	 "examples/mpi/pingpong/*.cerl",
%% 	 "examples/mpi/threadring/*.cerl"
%% 	 ].

examples_list() ->
	[
	 "examples/ooe/01_objects/*.cerl",
	 "examples/ooe/02_inheritance/*.cerl",
	 "examples/ooe/03_polymorphism/*.cerl",
	 "examples/ooe/04_polymorphism/*.cerl",
	 "examples/ooe/05_polymorphism/*.cerl",
	 "examples/ooe/06_interface/*.cerl",
	 "examples/ooe/07_interface/*.cerl",
	 "examples/ooe/08_interface/*.cerl",
	 "examples/ooe/09_polymorphism/*.cerl",
	 "examples/ooe/10_interface/*.cerl",
	 "examples/mpi/pingping/*.cerl",
 	 "examples/mpi/pingpong/*.cerl",
 	 "examples/mpi/threadring/*.cerl"
	 ].

compare_raw_preprocessed_tokens(CerlFile) ->
    RawTokens = ooe_build:get_raw_tokens(CerlFile),
    [_Dot|Tokens] =
        lists:dropwhile(
            fun(I) -> element(1, I) =/= dot end,
            ooe_build:get_tokens(CerlFile)),
    {CerlFile, ?_assertEqual(RawTokens, Tokens)}.

get_ast(CerlFile) ->
	{
	CerlFile,
	?_assertEqual({CerlFile, ok},
		{CerlFile, element(1,
			ooe_build:get_ast(CerlFile))})
	}.


gen_ok_module( Dir ) ->
	FileList = filelib:wildcard(Dir),
        ClassList = get_file_classes(FileList),
	lists:map( 
	  fun(Arg) -> {ok, list_to_atom(filename:basename(Arg,".cerl"))} end,
          ClassList
	).

get_file_classes(FileList) ->
	get_file_classes(FileList, []).

get_file_classes([], ClassList) ->
	lists:reverse(ClassList);
get_file_classes([File|FileList], ClassList) ->
	case file_is_class(File) of
		true -> get_file_classes(FileList, [File|ClassList]);
		false -> get_file_classes(FileList, ClassList)
        end.

file_is_class(FileName) ->
	case file:open(FileName, [read]) of
		{ok, Device} -> has_class(Device);
		_ -> false
	end.

has_class(Device) ->
	case file:read_line(Device) of
		{ok, Line} ->
			case lists:prefix("-class(", Line) of
				true -> true;
				false -> has_class(Device)
			end;
		_ ->
			file:close(Device),
			false
	end.
