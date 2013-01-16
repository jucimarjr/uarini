-define(ERR_REPORT(Msg, Reason), io:format("%%%%%%%%%%%%%% ERRO %%%%%%%%%%%%%%\n" ++
				     "Msg: ~p\n" ++
				     "Reason: ~p\n\n", [Msg, Reason])).
-define(OUT_PATH, "../../docs/erlang/out_erl_alltoall.txt").
