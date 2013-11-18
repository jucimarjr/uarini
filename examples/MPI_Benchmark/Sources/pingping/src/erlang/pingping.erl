-module(pingping).
-export([run/2, run/3]).
-include("conf.hrl").

run(DataSize, R) ->
	OutFileLocation = "../../docs/erlang/out_erl_pingping.txt",
	case file:open(OutFileLocation, [append]) of
		{error, Why} ->
			?ERR_REPORT("Falha ao criar arquivo!", Why);
		{ok, OutFile} ->
			run(DataSize, R, OutFile)
	end.

run(DataSize, R, OutFile) ->
	Data = generate_data(DataSize),
	Self = self(),
	SpawnStart = time_microseg(),
	P1 = spawn(fun() -> pingping(Data, Self, R) end),
	P2 = spawn(fun() -> pingping(Data, Self, R) end),
	SpawnEnd = time_microseg(),
	TimeStart = time_microseg(),
	P1 ! {init, self(), P2},
	P2 ! {init, self(), P1},
	finalize(P1),
	finalize(P2),
	TimeEnd = time_microseg(),
	TotalTime = TimeEnd - TimeStart,
	SpawnTime = SpawnEnd - SpawnStart,
	 printResult(Data, R, TotalTime, SpawnTime, OutFile).

pingping(_,Parent, 0) ->
	Parent ! {finish, self()};

pingping(Data, Parent, R) ->
	receive
		{init, Parent, Peer} ->
			Peer ! {self(), Data},
			pingping(Data, Parent, R-1);
		{Peer, Data} ->
			Peer ! {self(), Data},
			pingping(Data, Parent, R-1)
	end.

finalize(P1) ->
	receive
		{finish, P1} ->
			ok
	end.

printResult(Data, R, Time_exec, Time_spawn, OutFile) ->
	FormatH = "~-9s\t ~-13s\t ~-17s\t ~-11s\t ~-10s~n",
	Header = ["#bytes", "#repetitions", "exec_time[µsec]", "MBytes/sec", "spawn_time"],
	io:format(OutFile, FormatH, Header),
	MBps = bandwidth_calc(Data, Time_exec),
	FormatD = "~-9w\t ~-13w\t ~-17.2f\t ~-11.6f\t ~-15.2f~n",
	io:format(OutFile, FormatD, [size(Data), R, Time_exec, Time_spawn, MBps]).

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
