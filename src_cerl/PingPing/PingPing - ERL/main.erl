-method(main).
-export([new/2, pingping/2]).

run(R, D) ->
	P1 = def_constructor({Ping, [pingping, [D]]}),
	P2 = ooe:def_constructor({Ping, [pingping, [D]]}),
	P1 ! {init, P2, R},
    P2 ! {init, P1, R}.
