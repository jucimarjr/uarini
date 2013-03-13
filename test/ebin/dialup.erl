-module(dialup).

-export([conectar/1, new_/0]).

-implements(conexao).

new_() -> ObjectID = ooe:new([]), {dialup, ObjectID}.

conectar(ObjectID) -> "Modem discando...".

