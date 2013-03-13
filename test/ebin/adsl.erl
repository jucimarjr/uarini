-module(adsl).

-export([conectar/1, new_/0]).

-implements(conexao).

new_() -> ObjectID = ooe:new([]), {adsl, ObjectID}.

conectar(ObjectID) -> "Adsl conectado...".

