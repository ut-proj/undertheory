PROJ = undermidi
PRIV = ./priv
PWD = $(shell pwd)

default: build

build:
	@rebar3 compile

check:
	-@rebar3 lfe clean
	@rebar3 lfe compile
	@rebar3 xref
	-@rebar3 dialyzer
	@rebar3 as test lfe ltest

recheck: rebuild check

rebuild: clean-all build

clean-all: clean
	@rm -rf _build rebar.lock

.PHONY: default run build clean-all

clean:
	@rebar3 clean

