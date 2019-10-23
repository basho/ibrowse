PROJECT=ibrowse
PLT_APPS=erts kernel stdlib ssl crypto public_key
TEST_ERLC_OPTS=-pa ../ibrowse/ebin

.PHONY: test

include erlang.mk

test:
	rebar3 eunit
