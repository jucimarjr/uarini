SRC_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
TEST_EBIN_DIR=test/ebin
UARINI_SRC_DIR = src_cerl
ERLC=erlc -I $(INCLUDE_DIR)
ERL=erl -noshell -pa $(EBIN_DIR) -pa $(TEST_EBIN_DIR) 

.PHONY: clean debug

# This is the default task
compile: ebin/uarini.beam src/uarini_lexer.erl src/uarini_parser.erl test/ebin/test.beam 

test:   compile
	@ clear
	@ echo EUnit testing...
	@ rm -rf $(TEST_EBIN_DIR)/
	@ mkdir -p $(TEST_EBIN_DIR)
	@ $(ERLC) -o $(TEST_EBIN_DIR) test/test.erl
	@ $(ERL) -eval 'eunit:test("test/ebin", [verbose]), halt().'
	@ mv *.erl $(TEST_EBIN_DIR)
	@ mv *.beam $(TEST_EBIN_DIR)

# This is the task when you intend to debug
debug: ERLC += +debug_info
debug: compile

ebin/uarini.beam: $(INCLUDE_DIR)/*.hrl src/*.erl
	@ echo Compiling Erlang source...
	@ mkdir -p $(EBIN_DIR)
	@ $(ERLC) -o $(EBIN_DIR) src/*.erl
	@ echo

$(TEST_EBIN_DIR)/test.beam : test/*.erl
	@ echo Compiling Eunits tests...
	@ rm -rf $(TEST_EBIN_DIR)/
	@ mkdir -p $(TEST_EBIN_DIR)
	@ $(ERLC) -o $(TEST_EBIN_DIR) test/test.erl
	@ echo  

src/uarini_lexer.erl: src/uarini_lexer.xrl
	@ echo Compiling Lexer ...
	@ $(ERL) -eval 'leex:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	@ $(ERLC) -o $(EBIN_DIR) $@
	@ echo

src/uarini_parser.erl: src/uarini_parser.yrl
	@ echo Compiling Parser ...
	@ $(ERL) -eval 'yecc:file("$<", [{verbose, true}]), halt().'
	@ mkdir -p $(EBIN_DIR)
	@ $(ERLC) -o $(EBIN_DIR) $@
	@ echo

clean:
	@ echo Cleaning...
	rm -f erl_crash.dump
	rm -f *.erl
	rm -f $(SRC_DIR)/uarini_lexer.erl
	rm -f $(SRC_DIR)/uarini_parser.erl
	rm -rf $(EBIN_DIR)/
	rm -rf $(TEST_EBIN_DIR)/
	find . -name  *.*~ -print0 | xargs -0 rm
	find . -name  *~ -print0 | xargs -0 rm
	find . -name  *.beam -print0 | xargs -0 rm
	find . -name  *.class -print0 | xargs -0 rm
	@ echo
