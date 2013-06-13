-module(miniDuckSimulator).
-export([main/0]).

main() ->
	Mallard = mallardDuck:new(),
	Mallard ! {self(), display},
	Mallard ! {self(), performQuack},
	Mallard ! {self(), performFly},
	Mallard ! {self(), swim}.