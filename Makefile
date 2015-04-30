OTP_RELEASE := $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTP_ROOT := $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(code:root_dir()), halt().')
OTP_PLT := .otp_$(OTP_RELEASE).plt
DEPS_PLT := .deps.plt

REBAR ?= $(CURDIR)/rebar
REBAR_URL ?= https://github.com/rebar/rebar/releases/download/2.5.1/rebar
define get_rebar
	curl -s -L -o $(REBAR) $(REBAR_URL) || rm $(REBAR)
	chmod +x $(REBAR)
endef
export REBAR

RELX ?= $(CURDIR)/relx
RELX_URL ?= https://github.com/erlware/relx/releases/download/v1.0.4/relx
define get_relx
	curl -s -L -o $(RELX) $(RELX_URL) || rm $(RELX)
	chmod +x $(RELX)
endef
export RELX

RLX_PRV_CMD ?= $(CURDIR)/rlx_prv_cmd.beam
RLX_PRV_CMD_URL ?= https://github.com/erlangninja/rlx_prv_cmd/releases/download/0.1.0/rlx_prv_cmd.beam
define get_rlx_prv_cmd
	curl -s -L -o $(RLX_PRV_CMD) $(RLX_PRV_CMD_URL) || rm $(RLX_PRV_CMD)
endef
export RLX_PRV_CMD

-include env.mak

all: test

$(RLX_PRV_CMD):
	@$(call get_rlx_prv_cmd)

$(RELX): $(RLX_PRV_CMD)
	@$(call get_relx)

$(REBAR):
	@$(call get_rebar)

deps: $(REBAR)
	@$(REBAR) --quiet get-deps

docs: $(REBAR)
	@$(REBAR) --quiet get-deps compile doc

compile: deps
	@$(REBAR) --quiet compile

test: compile
	@ERL_AFLAGS="-args_file etc/vm.args -config etc/test.config" $(REBAR) --quiet eunit skip_deps=true

release: compile $(RELX)
	@rm -rf rel
	@$(RELX) -c relx.config -o rel $(RELX_OPTS) release tar

clean: $(REBAR)
	@rm -rf rel .rebar .eunit *.dump dialyzer.log $(DEPS_PLT)
	@$(REBAR) --quiet clean

distclean:
	@rm -rf ebin deps log rel .rebar .eunit *.dump dialyzer.log $(DEPS_PLT) $(REBAR) $(RELX) $(RLX_PRV_CMD)

$(OTP_PLT):
	@dialyzer --verbose --build_plt --output_plt $(OTP_PLT) --apps $(OTP_ROOT)/lib

$(DEPS_PLT): compile
	@dialyzer --verbose --build_plt --output_plt $(DEPS_PLT) --apps deps

dialyzer: $(OTP_PLT) $(DEPS_PLT)
	@$(REBAR) --quiet compile skip_deps=true
	@dialyzer --no_check_plt -Wno_match -Wno_return --plts $(OTP_PLT) $(DEPS_PLT) -- ebin | tee dialyzer.log

console:
	@$(REBAR) --quiet compile skip_deps=true
	@erl -args_file etc/vm.args -config etc/sys.config -pa ebin -pa deps/*/ebin $(ERL_OPTS) -boot start_sasl
