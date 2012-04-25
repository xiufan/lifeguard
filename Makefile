all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

rel: compile
	./rebar generate -f

test: compile
	./rebar eunit apps=lifeguard

.PHONY: all compile clean deps
