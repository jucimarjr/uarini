%-define(OUT_FILE, "erl_out.txt").
-define(ERR_REPORT(Msg, Reason), io:format("%%%%%%%%%%%%%% ERRO %%%%%%%%%%%%%%\n" ++
				     "Msg: ~s\n" ++
				     "Reason: ~s\n\n", [Msg, Reason])).
-define(OUT_PATH, "out_erl_threadring.txt").
