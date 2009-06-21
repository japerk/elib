ERL=erl
RUN_TEST=run_test

all: src

src: FORCE
	@$(ERL) -pa ebin -make


clean:
	rm -f ebin/*.beam
	rm -f test/*.beam

check: src
	@dialyzer -q -r elib/ -I include/ \
		-I $(ERL_LIBS)/test_server*/include/ \
		-I $(ERL_LIBS)/common_test*/include/

test: test.spec src
	@$(RUN_TEST) -pa $(PWD)/ebin -spec test.spec
	@rm variables-ct*

test.spec: test.spec.in
	@cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

FORCE:
