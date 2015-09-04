
DIR := $(shell pwd)
REBAR = ./rebar
# ERL = erl -pa ebin

all: get-deps compile

console:
	$(RM) ./test.sock
	erl \
		-sname owl_router \
		-pa ebin \
		-pa deps/*/ebin \
		\
		-s owl_shell_kickstart

connect:
	rlwrap socat - UNIX-CONNECT:test.sock

clean:
	$(REBAR) clean

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

compile-skip_deps:
	$(REBAR) skip_deps=true compile

