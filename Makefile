ERL=erl

all: src

src: FORCE
	@$(ERL) -pa ebin -make

check: src
	@dialyzer -q -r . -I include/ \
		-I $(ERL_LIBS)/test_server*/include/ \
		-I $(ERL_LIBS)/common_test*/include/

test: test.spec src FORCE
	@run_test -pa $(PWD)/ebin -spec test.spec
	@rm variables-ct*

test.spec: test.spec.in
	@cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

clean:
	rm -f ebin/*.beam
	rm -f test/*.beam
	rm test.spec

docs:
	erl -noshell -run edoc_run application elib '"."' '"."'

FORCE:
