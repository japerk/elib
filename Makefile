ERL=erl

all: src

src: FORCE
	@$(ERL) -pa ebin -make

plt:
	@dialyzer --build_plt --plt .plt -q -r . -I include/

check: src
	@dialyzer --check_plt --plt .plt -q -r . -I include/ \
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

doc: FORCE
	@erl -noshell -run edoc_run application elib '"."' '[{new, true}]'

FORCE:
