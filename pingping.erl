-module(pingping).

-export([constructor/0, run/2, run/3]).

constructor() ->
    ObjectID = ooe:new([]), {pingping, ObjectID}.

run(DataSize, R) ->
    OutFileLocation = "out_cerl_pingping.txt",
    case file:open(OutFileLocation, [append]) of
      {error, Why} ->
	  error_report("Falha ao criar arquivo!", Why);
      {ok, OutFile} -> run(DataSize, R, OutFile)
    end.

run(DataSize, R, OutFile) ->
    Data = generate_data(DataSize),
    Self = self(),
    SpawnStart = time_microseg(),
    P1 = procping:new(Data, Self, R),
    P2 = procping:new(Data, Self, R),
    SpawnEnd = time_microseg(),
    TimeStart = time_microseg(),
    (erlang:element(1, P1)):send(erlang:element(2, P1),
				 {init, self(),
				  ooe:lookup_attr(element(2, P2), 'Pid')}),
    (erlang:element(1, P2)):send(erlang:element(2, P2),
				 {init, self(),
				  ooe:lookup_attr(element(2, P1), 'Pid')}),
    finalize(ooe:lookup_attr(element(2, P1), 'Pid')),
    finalize(ooe:lookup_attr(element(2, P2), 'Pid')),
    TimeEnd = time_microseg(),
    TotalTime = TimeEnd - TimeStart,
    SpawnTime = SpawnEnd - SpawnStart,
    printResult(Data, R, TotalTime, SpawnTime, OutFile).

finalize(Pid) -> receive {finish, Pid} -> ok end.

printResult(Data, R, Time_exec, Time_spawn, OutFile) ->
    FormatH = "~-9s\t ~-13s\t ~-17s\t ~-11s\t ~-10s~n",
    Header = ["#bytes", "#repetitions", "exec_time[µsec]",
	      "MBytes/sec", "spawn_time"],
    io:format(OutFile, FormatH, Header),
    MBps = bandwidth_calc(Data, Time_exec),
    FormatD =
	"~-9w\t ~-13w\t ~-17.2f\t ~-11.6f\t ~-15.2f~n",
    io:format(OutFile, FormatD,
	      [size(Data), R, Time_exec, Time_spawn, MBps]).

bandwidth_calc(Data, Time) ->
    Megabytes = size(Data) / math:pow(2, 20),
    Seconds = Time * 9.99999999999999954748e-7,
    Megabytes / Seconds.

generate_data(Size) -> generate_data(Size, []).

generate_data(0, Bytes) -> list_to_binary(Bytes);
generate_data(Size, Bytes) ->
    generate_data(Size - 1, [1 | Bytes]).

time_microseg() ->
    {MS, S, US} = now(), MS * 1.0e+12 + S * 1.0e+6 + US.

error_report(Msg, Reason) ->
    io:format("%%%%%%%%%%%%%% ERRO %%%%%%%%%%%%%%\n" ++
		"Msg: ~w\n" ++ "Reason: ~w\n\n",
	      [Msg, Reason]).
