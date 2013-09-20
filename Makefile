# Makefile

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
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
	pandoc README.md -o README.pdf
	mv README.pdf priv/
	cd priv && pdflatex edfs.tex
	rm -f priv/*.aux priv/*.log priv/*.acn priv/*.ist priv/*.out \
		  priv/*.toc priv/*.glo priv/*.gls priv/*.glg priv/*.gls \
		  priv/*.glg priv/*.alg priv/*.acr

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
	@$(ERL) $(ERLFLAGS)

clean: 
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	- rm -f $(CURDIR)/*.dump
	- rm -f priv/*.aux priv/*.log priv/*.acn priv/*.ist priv/*.out \
		 priv/*.toc priv/*.glo priv/*.gls priv/*.glg priv/*.gls \
		 priv/*.glg priv/*.alg priv/*.acr
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps
	- rm -rf $(CURDIR)/log
	- rm -rf $(CURDIR)/doc
	- rm -f $(CURDIR)/priv/*.pdf

rebuild: distclean deps compile dialyzer test

release:
	$(REBAR) generate
