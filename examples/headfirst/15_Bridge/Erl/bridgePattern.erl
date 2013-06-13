-module(bridgePattern).
-export([main/0]).

main() ->
	Vehicle1 = car:new(produce:new(), assemble:new()),
	Vehicle1 ! {self(), manufacture},
	
	Vehicle2 = bike:new(produce:new(), assemble:new()),
	Vehicle2 ! {self(), manufacture}.