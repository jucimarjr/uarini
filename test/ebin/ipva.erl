-module(ipva).

-export([calcularIpva/1, new_/0]).

new_() ->
    ObjectID = ooe:new([{'QtdCavalos', []},
			{'QtdEixos', []}]),
    ooe:update_attr(ObjectID, 'QtdCavalos', 150),
    {ipva, ObjectID}.

calcularIpva(ObjectID) -> 0.
