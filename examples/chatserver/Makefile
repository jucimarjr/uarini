OOE_LIB=../../../ebin/
EBIN_DIR=ebin
ERL=erl -noshell -pa $(OOE_LIB) -s init stop

.PHONY: clean compile

# This is the default task
compile:
	@ mkdir --parents $(EBIN_DIR)
	@ erlc -o ebin include/server_util.erl
	@ cd $(EBIN_DIR);	\
		$(ERL) -eval 'ooec:compile(["../src/chat_client.cerl","../src/chat_server.cerl"]).'

clean:
	@ rm -rf $(EBIN_DIR)
