SRC_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
TEST_EBIN_DIR=test/ebin
UARINI_SRC_DIR = src_cerl
ERLC=erlc -I $(INCLUDE_DIR)
ERL=erl -noshell -pa $(EBIN_DIR) -pa $(TEST_EBIN_DIR) 

.PHONY: clean debug

# This is the default task
compile: ebin/ooec.beam src/ooe_scan.erl src/ooe_parse.erl test/ebin/test.beam

test:   compile
	@ echo EUnit testing...
	@ rm -rf $(TEST_EBIN_DIR)/
	@ mkdir -p $(TEST_EBIN_DIR)
	@ $(ERLC) -o $(TEST_EBIN_DIR) test/*.erl
	@ $(ERL) -pa $(EBIN_DIR) -pa $(TEST_EBIN_DIR) \
		-eval 'eunit:test([test, ooe_scan_tests, ooe_parse_tests], [verbose]), halt().'
	@ mv *.beam $(TEST_EBIN_DIR)/
	@ mv *.erl $(TEST_EBIN_DIR)/
	@ cp $(EBIN_DIR)/ooe.beam $(TEST_EBIN_DIR)/

# This is the task when you intend to debug
debug: ERLC += +debug_info
debug: compile

ebin/ooec.beam: $(INCLUDE_DIR)/*.hrl src/*.erl src/ooe_parse.erl src/aleppo_parser.erl
	@ mkdir -p $(EBIN_DIR)
	@ echo Compiling Erlang source...
	@ $(ERLC) -o $(EBIN_DIR) src/*.erl
	@ echo

$(TEST_EBIN_DIR)/test.beam : test/*.erl
	@ echo Compiling Eunits tests...
	@ rm -rf $(TEST_EBIN_DIR)/
	@ mkdir -p $(TEST_EBIN_DIR)
	@ $(ERLC) -o $(TEST_EBIN_DIR) test/test.erl
	@ echo  

src/ooe_parse.erl: src/ooe_parse.yrl
	@ echo Compiling Parser ...
	@ $(ERL) -eval 'yecc:file("src/ooe_parse.yrl", [{verbose, true}]), halt().'
	@ mkdir -p $(EBIN_DIR)
	@ $(ERLC) -o $(EBIN_DIR) $@
	@ echo

src/aleppo_parser.erl: src/aleppo_parser.yrl
	@ echo Compiling Aleppo Parser ...
	@ $(ERL) -eval 'yecc:file("src/aleppo_parser.yrl", [{verbose, true}]), halt().'
	@ mkdir -p $(EBIN_DIR)
	@ $(ERLC) -o $(EBIN_DIR) $@
	@ echo

clean:
	@ echo Cleaning...
	rm -f erl_crash.dump
	rm -f *.erl
	rm -f *.beam
	rm -f $(SRC_DIR)/ooe_parse.erl
	rm -f $(SRC_DIR)/aleppo_parser.erl
	rm -rf $(EBIN_DIR)/
	rm -rf $(TEST_EBIN_DIR)/
	@ echo
