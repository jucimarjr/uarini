-module(pingpong).
-export([run/2]).

run(R, D) ->
    Pong = spawn(fun() -> pong() end),
    ping(R, D, Pong).
    

ping(0, _, _) ->
    done;
ping(R, D, Pong) ->
    Pong ! {self(), D},
    receive
        {Pong, D} ->
            ping(R - 1, D, Pong)
    end.

pong() ->
    receive
        {From, D} ->
            From ! {self(), D},
            pong()
    end.
