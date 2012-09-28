all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile

clean:
	rebar clean

check:
	rebar eunit skip_deps=true

.PHONY: all get-deps compile clean check
