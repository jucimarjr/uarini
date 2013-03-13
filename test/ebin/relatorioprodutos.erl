-module(relatorioprodutos).

-export([getSql/1, new_/0]).

-implements(irelatorio).

new_() ->
    ObjectID = ooe:new([]), {relatorioprodutos, ObjectID}.

getSql(ObjectID) -> "select * from produtos".
