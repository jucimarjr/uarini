-module(persist).

-export([write_result/3]).

-import(medicoes, [bandwidth_calc/2]).

-include("conf.hrl").

write_result(alltoall, OutLocation, Args) ->
    Header = io_lib:format("~-10s\t ~-15s\t ~-15s\t ~-17s\t ~-17s\t ~-17s\t ~-17s\r\n",
			   ["#bytes", "#repetitions", "#processes",
			    "t_min[µsec]", "t_max[µsec]", "t_avg[µsec]", "spawn_time[µsec]"]),
    [Data, Rep, QtdProcs, TimeMin, TimeMax, TimeAvg, TimeSpawn] = Args,
    Result = io_lib:format("~-9w\t ~-14w\t ~-14w\t ~-16w\t ~-16w\t ~-16w\t ~-16w\r\n",
			   [size(Data), Rep, QtdProcs, trunc(TimeMin), trunc(TimeMax),
			    trunc(TimeAvg), trunc(TimeSpawn)]),
    createOrOpen_file(OutLocation, Header, Result);

write_result(peer, OutLocation, Args) ->
    Header = io_lib:format("~-10s\t ~-15s\t ~-20s\t ~-17s\t ~-10s\r\n",
			   ["#bytes", "#repetitions", "exec_time[µsec]", "MBytes/sec", "spawn_time"]),
    [Data, R, Time_exec, Time_spawn] = Args,

    MBps = bandwidth_calc(Data, Time_exec),

    Result = io_lib:format("~-9w\t ~-13w\t ~-18.1f\t ~-16f\t ~-16f\r\n",
			   [size(Data), R, Time_exec, Time_spawn, MBps]),
    createOrOpen_file(OutLocation, Header, Result).

createOrOpen_file(OutPath, Header, Result) ->
    case file:open(OutPath, [read]) of
	{error, enoent} ->
	    case file:open(OutPath, [append]) of
		{ok, OutFile} ->
		    io:format(OutFile, Header, []),
		    io:format(OutFile, Result, []);

		{error, Why} ->
		    ?ERR_REPORT("Falha ao criar arquivo!", Why)
	    end;

	{ok, ReadOnlyFile} ->
	    file:close(ReadOnlyFile),

	    case file:open(OutPath, [append]) of
		{ok, OutFile} ->
		    io:format(OutFile, Result, []);

		{error, Why} ->
		    ?ERR_REPORT("Falha ao criar arquivo!", Why)
	    end;

	{error, Why} ->
	    ?ERR_REPORT("Falha ao abrir arquivo!", Why)
    end.
