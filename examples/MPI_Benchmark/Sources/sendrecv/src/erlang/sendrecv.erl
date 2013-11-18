-module(sendrecv).
-export([run/1, ring_node/1]).

-include("conf.hrl").
-import(persist, [createOrOpen_file/1]).
-import(medicoes, [printResult/6, generate_data/1, time_microseg/0]).

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

	OutFile = createOrOpen_file(?OUT_PATH),
	printResult(Data, Rep, QtdProcs, TotalTime, SpawnTime, OutFile),
	erlang:halt().

create_procs(QtdProcs) ->
	lists:foldl(
	fun(_Id, RightPeer) -> spawn(threadring, ring_node, [RightPeer]) end, 
		self(), 
		lists:seq(QtdProcs, 2, -1)).

sender_ring_node(_,0,_) -> ok;
sender_ring_node(Data, Rep, Second) ->
	Second ! Data,
	receive
		Data ->
			sender_ring_node(Data, Rep-1, Second)
	end.

ring_node(RightPeer) ->
	receive
		Data ->
			RightPeer ! Data,
			ring_node(RightPeer)
	end.
