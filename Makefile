# Copyright (c) 2014, Erik Hedenström <erik@hedenstroem.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

OTP_RELEASE := $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTP_ROOT := $(shell export PATH="$(PATH)"; erl -noshell -eval 'io:format(code:root_dir()), halt().')
OTP_PLT := .otp_$(OTP_RELEASE).plt
DEPS_PLT := .deps.plt

REBAR ?= $(CURDIR)/rebar
REBAR_URL ?= https://github.com/rebar/rebar/releases/download/2.2.0/rebar
define get_rebar
	curl -s -L -o $(REBAR) $(REBAR_URL) || rm $(REBAR)
	chmod +x $(REBAR)
endef
export REBAR

RELX ?= $(CURDIR)/relx
RELX_URL ?= https://github.com/erlware/relx/releases/download/v0.6.0/relx
define get_relx
	curl -s -L -o $(RELX) $(RELX_URL) || rm $(RELX)
	chmod +x $(RELX)
endef
export RELX

-include env.mak

all: release

$(RELX):
	@$(call get_relx)

$(REBAR):
	@$(call get_rebar)

deps: $(REBAR)
	@$(REBAR) get-deps

docs: $(REBAR)
	@$(REBAR) get-deps compile doc

compile: deps
	@$(REBAR) compile

release: compile $(RELX)
	@rm -rf rel
	@$(RELX) -c relx.config -o rel $(RELX_OPTS) release tar

test: compile
	@$(REBAR) eunit skip_deps=true

clean: $(REBAR)
	@rm -rf rel .rebar .eunit *.dump dialyzer.log $(DEPS_PLT)
	@$(REBAR) clean

distclean:
	@rm -rf ebin deps rel .rebar .eunit *.dump dialyzer.log $(DEPS_PLT) $(REBAR) $(RELX)

$(OTP_PLT):
	@dialyzer --verbose --build_plt --output_plt $(OTP_PLT) --apps $(OTP_ROOT)/lib

$(DEPS_PLT): $(REBAR)
	@$(REBAR) get-deps compile
	@dialyzer --verbose --build_plt --output_plt $(DEPS_PLT) --apps deps

dialyzer: $(OTP_PLT) $(DEPS_PLT)
	@$(REBAR) compile skip_deps=true
	@dialyzer --no_check_plt -Wno_match -Wno_return --plts $(OTP_PLT) $(DEPS_PLT) -- ebin | tee dialyzer.log

console:
	@$(REBAR) compile skip_deps=true
	@erl -pa ebin -pa deps/*/ebin $(ERL_OPTS) -boot start_sasl
