.PHONY: compile test dialyzer clean

REBAR = ./rebar
APPS = erts kernel stdlib

compile:
	@$(REBAR) compile

test: compile get-test-deps
	@$(REBAR) -C rebar.test.config eqc

get-test-deps:
	@$(REBAR) -C rebar.test.config get-deps

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps $(APPS) --output_plt $@

clean:
	@$(REBAR) clean
	@$(REBAR) -C rebar.test.config delete-deps
