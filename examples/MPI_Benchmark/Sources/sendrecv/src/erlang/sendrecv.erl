-module(sendrecv).
-export([run/1, ring_node/4]).

-include("conf.hrl").
-import(persist, [createOrOpen_file/1]).
-import(medicoes, [printResult/6, generate_data/1, time_microseg/0]).

run([DataSizeStr, RepStr, QtdProcsStr]) ->
	
	DataSize = list_to_integer(DataSizeStr),
	Rep = list_to_integer(RepStr),
	QtdProcs = list_to_integer(QtdProcsStr),
	
	Data = generate_data(DataSize),		
	
	SpawnStart = time_microseg(),
	{ListProc, SpawnEnd} = create_procs(QtdProcs, Rep, Data),
	
	ExecStart = time_microseg(),
	start_ring(ListProc),
	ExecEnd = finalize(QtdProcs),
	
	TotalTime = ExecEnd - ExecStart,
	SpawnTime = SpawnEnd - SpawnStart,
	
	OutFile = createOrOpen_file(?OUT_PATH),	
	printResult(Data, Rep, QtdProcs, TotalTime, SpawnTime, OutFile),
	erlang:halt().

finalize(0) ->
	time_microseg();
finalize(NProc) ->
	receive		
		ok ->
			finalize(NProc - 1)
	end.

create_procs(QtdProcs, Rep, Data) ->
	ListProc = create(QtdProcs, []),
	SpawnEnd = time_microseg(),	
	connect_node(ListProc, Rep, Data),
	{ListProc, SpawnEnd}.

create(0, ListProc) -> 
	ListProc;	
create(N, ListProc) ->
	create(N - 1, [spawn(sendrecv, ring_node, [0, 0, 0, 0]) | ListProc]).

connect_node([H | Rest], Rep, Data) ->
	connect_node(H, [H | Rest], Rep, Data).

connect_node(Proc,[First | [Second | []]], Rep, Data) ->
	First ! {Second, Rep, self(), Data},
	Second ! {Proc, Rep, self(), Data};
connect_node(Proc,[First | [Second | Rest]], Rep, Data) ->
	First ! {Second, Rep, self(), Data},
	connect_node(Proc,[Second | Rest], Rep, Data).

start_ring([H | []]) ->
	H ! start;
start_ring([H | Rest]) ->
	H ! start,
	start_ring(Rest).

ring_node(RightPeer, Rep, Parent, Data) ->
	receive
		{Second, R, Par, D} ->
			ring_node(Second, R, Par, D);
		start ->
			RightPeer ! {Data, self()},	
			ring_node(RightPeer, Rep, Parent, Data);
		{Data, Pid} when (Pid =:= self()) ->			
			case Rep of
				0 -> 
					Parent ! ok,
					ring_node(RightPeer, Rep, Parent, Data);
				_ ->
					RightPeer ! {Data, Pid},
					ring_node(RightPeer, Rep - 1, Parent, Data)
			end;
		{Data, Pid} ->
			RightPeer ! {Data, Pid},
			ring_node(RightPeer, Rep, Parent, Data)			
	end.
