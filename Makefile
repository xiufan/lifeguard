all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

.PHONY: all compile clean deps
