REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) ct

xref:
	@$(REBAR) xref

.PHONY: test dialyzer clean xref

dialyzer:
	@$(REBAR) dialyzer
