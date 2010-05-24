ERL=erl
ERL_LIBS=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

all: src

src: FORCE
	@$(ERL) -make

plt:
	@dialyzer --build_plt --output_plt .plt -q -r . -I include/

check: src
	@dialyzer --check_plt --plt .plt -q -r . -I include/ \
		-I $(ERL_LIBS)/test_server*/include/ \
		-I $(ERL_LIBS)/common_test*/include/

test: test.spec src
	@run_test -pa $(PWD)/ebin -pa $(PWD)/../osmos-0.0.1/src -spec test.spec
	@rm variables-ct*

test.spec: test.spec.in
	@cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

clean:
	rm -f */*.beam
	rm -f test.spec

doc: FORCE
	@erl -noshell -run edoc_run application elib '"."' '[{new, true}]'

FORCE:
