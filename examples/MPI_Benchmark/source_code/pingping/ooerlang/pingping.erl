-module(pingping).
-export([run/2, run/3]).

run(DataSize, R) ->
   OutFileLocation = "out_cerl_pingping.txt",

   case file:open(OutFileLocation, [append]) of
	{error, Why} ->
	    error_report("Falha ao criar arquivo!", Why);
   
	{ok, OutFile} ->
	    run(DataSize, R, OutFile)
    end.

run(DataSize, R, OutFile) ->
    Data = generate_data(DataSize),
    Self = self(),
    SpawnStart = time_microseg(),
    P1 = proc:new(Data, Self, R), % cria P1 = {proc, <0.35.0>}
	P2 = proc:new(Data, Self, R), % cria P2 = {proc, <0.36.0>}
    SpawnEnd = time_microseg(),
    TimeStart = time_microseg(),
	(erlang:element(1, P1)):send(element(2, P1), {init, self(), ooe:lookup_attr(element(2, P2), 'Pid')}), % proc:send(<0.35.0>, ...)
	(erlang:element(1, P2)):send(element(2, P2), {init, self(), ooe:lookup_attr(element(2, P1), 'Pid')}), % proc:send(<0.36.0>, ...)
	finalize(ooe:lookup_attr(element(2, P1), 'Pid')),
	finalize(ooe:lookup_attr(element(2, P2), 'Pid')),
    TimeEnd = time_microseg(),
	ooe:destroy(element(2, P1)),
	ooe:destroy(element(2, P2)),
    TotalTime = TimeEnd - TimeStart,
    SpawnTime = SpawnEnd - SpawnStart,
    printResult(Data, R, TotalTime, SpawnTime, OutFile).

%%-----------------------------------------------------------------------------
%% finalize( Pid )
%% espera termino da execucao do processo
%%
%% Pid		= o pid do processo
finalize(Pid) ->
	receive
		{finish, Pid} ->
			ok
	end.

%%-----------------------------------------------------------------------------
%% printResult( Data, R, Time, OutFile )
%% imprime o resultado
%% 
%% Data (binary) = os dados usados no teste
%% R             = quantidade de repetições
%% Time (µs)  = o tempo total do teste
%% OutFile    = o arquivo de saída

printResult(Data, R, Time_exec, Time_spawn, OutFile) ->
    FormatH = "~-9s\t ~-13s\t ~-17s\t ~-11s\t ~-10s~n",
    Header = ["#bytes", "#repetitions", "exec_time[µsec]", "MBytes/sec", "spawn_time"],
    io:format(OutFile, FormatH, Header),
    MBps = bandwidth_calc(Data, Time_exec),
    FormatD = "~-9w\t ~-13w\t ~-17.2f\t ~-11.6f\t ~-15.2f~n",
    io:format(OutFile, FormatD, [size(Data), R, Time_exec, Time_spawn, MBps]).

%%-----------------------------------------------------------------------------
%% bandwidth_calc(Data, Time)
%% calcula o tráfego na rede baseado no tamanho dos dados e quanto tempo levou 
%%
%% Data (binary) = os dados usados no teste
%% Time (µs)  = o tempo total do teste

bandwidth_calc(Data, Time) ->
    Megabytes = (size(Data) / math:pow(2, 20)),
    Seconds = (Time * 1.0e-6),
    Megabytes / Seconds.

%%-----------------------------------------------------------------------------
%% generate_data(Size)
%%   gera um dado de tamanho Size bytes
%%
%%   Size = integer

generate_data(Size) -> generate_data(Size, []).

generate_data(0, Bytes) ->
    list_to_binary(Bytes);
generate_data(Size, Bytes) ->
    generate_data(Size - 1, [1 | Bytes]).

%%-----------------------------------------------------------------------------
%% time_microseg()
%% captura o tempo atual em microsegundos
time_microseg() ->
    {MS, S, US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.

%%-----------------------------------------------------------------------------
%% error_report(Msg, Reason)
%% formata a msg de erro
error_report(Msg, Reason) ->
	io:format("%%%%%%%%%%%%%% ERRO %%%%%%%%%%%%%%\n" ++
				     "Msg: ~w\n" ++
				     "Reason: ~w\n\n", [Msg, Reason]).


