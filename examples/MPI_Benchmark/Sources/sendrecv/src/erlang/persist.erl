-module(persist).

-export([createOrOpen_file/1]).

-import(medicoes, [print_header/1]).

-include("conf.hrl").

createOrOpen_file(OutPath) ->
    case file:open(OutPath, [read]) of
	{error, enoent} ->
	    case file:open(OutPath, [append]) of
		{ok, OutFile} ->
		    print_header(OutFile),
		    OutFile;

		{error, Why} ->
		    ?ERR_REPORT("Falha ao criar arquivo!", Why)
	    end;

	{ok, ReadOnlyFile} ->
	    file:close(ReadOnlyFile),

	    case file:open(OutPath, [append]) of
		{ok, OutFile} ->
		    OutFile;

		{error, Why} ->
		    ?ERR_REPORT("Falha ao criar arquivo!", Why)
	    end;

	{error, Why} ->
	    ?ERR_REPORT("Falha ao abrir arquivo!", Why)
    end.
