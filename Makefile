.PHONY: deps docs

all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps

app.config:
	@cp app.config.orig app.config

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@./rebar skip_deps=true doc

test: test-eunit 

test-eunit:
	@./rebar eunit skip_deps=true

test-ct:
	@./rebar ct skip_deps=true verbose=1 suites=hub_ct

PLT_NAME=.evk_dialyzer.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt .evk_dialyzer.plt \
		--apps kernel stdlib sasl lager || true

dialyze: $(PLT_NAME)
	@dialyzer apps/evk/ebin --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wrace_conditions -Wunderspecs

#  http://erlang.org/pipermail/erlang-questions/2010-October/053853.html
d1: $(PLT_NAME)
	@dialyzer apps/evk/ebin --plt $(PLT_NAME) --no_native --no_spec \
		-Werror_handling -Wrace_conditions -Wunderspecs

NODE_NAME=evk@localhost
COOKIE=evk

run: check-deps app.config
	ERL_LIBS=deps:apps erl +MBas gf +MRas gf +Mim true -sname $(NODE_NAME) -setcookie $(COOKIE) -boot start_sasl -config app.config +P 2000000 -s evk

attach:
	erl -sname premsh@localhost -remsh $(NODE_NAME) -setcookie $(COOKIE)
