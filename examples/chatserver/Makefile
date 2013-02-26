UARINI_LIB=../../../ebin/
EBIN_DIR=ebin
ERL=erl -noshell -pa $(UARINI_LIB) -s init stop

.PHONY: clean compile

# This is the default task
compile:
	erlc -o ebin include/ooe.erl include/server_util.erl
	cd $(EBIN_DIR);	\
		$(ERL) -eval 'uarini:compile(["../src/chat_client.cerl","../src/chat_server.cerl"]).'

clean:
	rm -f $(EBIN_DIR)/*.beam $(EBIN_DIR)/*.erl
