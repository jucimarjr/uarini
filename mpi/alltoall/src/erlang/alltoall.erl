-module(alltoall).
-export([run/3]).

-include("conf.hrl").
-import(persist, [write_result/3]).
-import(medicoes, [generate_data/1, time_microseg/0]).

run(DataSize, Rep, QtdProcs) ->
    Data = generate_data(DataSize),

    SpawnStart = time_microseg(),
    Procs_list = create_procs(QtdProcs, Data),
    SpawnEnd = time_microseg(),

    SpawnTime = SpawnEnd - SpawnStart,

    ExecStart = time_microseg(),
    lists:foreach(fun(X) -> X ! {init, Rep, Procs_list, self()} end, Procs_list),
    EndTime_list = finalize(QtdProcs),

    Time_list = [ EndTime - ExecStart || EndTime <- EndTime_list ],

    TimeMin = lists:min(Time_list),
    TimeMax = lists:max(Time_list),
    TimeAvg = lists:sum(Time_list) / length(Time_list),

    write_result(alltoall, ?OUT_PATH, [Data, Rep, QtdProcs, TimeMin, TimeMax, TimeAvg, SpawnTime]),
    erlang:halt().

create_procs(QtdProcs, Data) -> create_procs(QtdProcs, Data, []).
create_procs(0, _, Procs_list) -> Procs_list;
create_procs(QtdProcs, D, L) ->
    create_procs(QtdProcs-1, D, [spawn(fun() -> wait_init(D) end) | L]).

finalize(QtdProcs) -> finalize(QtdProcs, []).
finalize(0, EndTime_list) -> EndTime_list;
finalize(QtdProcs, EndTime_list) ->
    receive
	{done, _From} ->
	    ExecEnd = time_microseg(),
	    finalize(QtdProcs-1, [ExecEnd | EndTime_list])
    end.

wait_init(Data) ->
    receive
	{init, Rep, Procs_list, Parent} ->
	    alltoall(Rep, Procs_list, Data),
	    Parent ! {done, self()}
    end.

alltoall(0, _, _) -> done;
alltoall(Rep, Plist, Data) ->
    scatter(Plist, Data),
    _RecvData_list = gather(Plist),
    alltoall(Rep-1, Plist, Data).

scatter(Plist, Data) ->
    lists:foreach(fun(P) -> P ! {msg, self(), Data} end, Plist).

gather(Plist) -> gather(Plist, []).

gather([], RecvData_list) -> RecvData_list;
gather([From| Plist], Dlist) ->
    receive
	{msg, From, Data} ->
	    gather(Plist, [Data| Dlist])
    end.
