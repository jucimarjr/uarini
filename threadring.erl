-module(threadring).

-export([run/1]).

-include("conf.hrl").

-import(persist, [createOrOpen_file/1]).

-import(medicoes,
	[generate_data/1, printResult/6, time_microseg/0]).

run([DataSizeStr, RepStr, QtdProcsStr]) ->
    DataSize = list_to_integer(DataSizeStr),
    Rep = list_to_integer(RepStr),
    QtdProcs = list_to_integer(QtdProcsStr),
    Data = generate_data(DataSize),
    SpawnStart = time_microseg(),
    Second = create_procs(QtdProcs),
    SpawnEnd = time_microseg(),
    ExecStart = time_microseg(),
    sender_ring_node(Data, Rep, Second),
    ExecEnd = time_microseg(),
    TotalTime = ExecEnd - ExecStart,
    SpawnTime = SpawnEnd - SpawnStart,
    OutFile = createOrOpen_file("./out_erl_threadring.txt"),
    printResult(Data, Rep, QtdProcs, TotalTime, SpawnTime,
		OutFile),
    erlang:halt().

create_procs(QtdProcs) ->
    lists:foldl(fun (_Id, RightPeer) ->
			ObjProc = proc:new(RightPeer),
			ooe:lookup_attr(element(2, ObjProc), 'Pid')
		end,
		self(), lists:seq(QtdProcs, 2, -1)).

sender_ring_node(_, 0, _) -> ok;
sender_ring_node(Data, Rep, Second) ->
    Second ! Data,
    receive
      Data -> sender_ring_node(Data, Rep - 1, Second)
    end.
