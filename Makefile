
DIR := $(shell pwd)
REBAR = ./rebar
# ERL = erl -pa ebin

all: get-deps compile

console:
	erl \
		-sname owl_router \
		-pa ebin \
		-pa deps/*/ebin

clean:
	$(REBAR) clean

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

compile-skip_deps:
	$(REBAR) skip_deps=true compile

