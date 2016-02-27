BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar

.PHONY: deps

all: compile

compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
deps:
	$(REBAR) get-deps
clean: cleantest
	$(REBAR) clean
cleantest:
	rm -rf .eunit/*
test: cleantest
	$(REBAR)  skip_deps=true eunit
