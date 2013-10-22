# Makefile

COOKIE=cookie
NODE_NAME=worker$(PID)@127.0.0.1

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
DISFLAGS= -setcookie $(COOKIE) -name $(NODE_NAME)
DEPS_PLT=$(CURDIR)/.deps_plt

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

DIALYZER=$(shell which dialyzer)
ifeq ($(DIALYZER),)
$(error "dialyzer not available on this system")
endif

.PHONY: all compile doc clean test dialyzer shell distclean rebuild

# =============================================================================
# Rules to build the system
# =============================================================================

all: deps compile

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc: all
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit

test: compile dialyzer eunit

$(DEPS_PLT):
	@echo Building local erts plt at $(DEPS_PLT)
	@echo
	$(DIALYZER) --output_plt $(DEPS_PLT) --build_plt \
		--apps erts kernel stdlib -r deps

dialyzer: $(DEPS_PLT)
	$(DIALYZER) --fullpath --plt $(DEPS_PLT) \
	     -I include -Wrace_conditions -r ./ebin

shell: deps compile
	@$(ERL) $(ERLFLAGS) $(DISFLAGS)

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	- rm -f $(CURDIR)/*.dump
	- rm -rf $(CURDIR)/log
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps
	- rm -rf $(CURDIR)/doc

rebuild: distclean deps compile dialyzer test

release:
	$(REBAR) generate
