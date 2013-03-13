-module(relatorioclientes).

-export([getSql/1, new_/0]).

-implements(irelatorio).

new_() ->
    ObjectID = ooe:new([]), {relatorioclientes, ObjectID}.

getSql(ObjectID) -> "select * from clientes".
