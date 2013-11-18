-module(medicoes).
-export([printResult/6, print_header/1, generate_data/1, time_microseg/0]).

printResult(Data, R, QtdProcs, Time_exec, Time_spawn, OutFile) ->
	MBps = bandwidth_calc(Data, Time_exec),
	FormatD = "~-9w\t ~-13w\t ~-13w\t ~-18.1f\t ~-16f\t ~-16f\r\n",
	io:format(OutFile, FormatD, [size(Data), R, QtdProcs, Time_exec, Time_spawn, MBps]).

print_header(File) ->
	FormatH = "~-10s\t ~-15s\t ~-15s\t ~-20s\t ~-17s\t ~-10s\r\n",
	Header = ["#bytes", "#repetitions", "#processes", "exec_time[µsec]", "spawn_time", "MBytes/sec"],
	io:format(File, FormatH, Header).

bandwidth_calc(Data, Time) ->
	Megabytes = (size(Data) / math:pow(2, 20)),
	Seconds = (Time * 1.0e-6),
	Megabytes / Seconds.

generate_data(Size) -> generate_data(Size, []).

generate_data(0, Bytes) ->
	list_to_binary(Bytes);
generate_data(Size, Bytes) ->
	generate_data(Size - 1, [1 | Bytes]).

time_microseg() ->
	{MS, S, US} = now(),
	(MS * 1.0e+12) + (S * 1.0e+6) + US.
