PROJECT = emqx_ct_helpers
PROJECT_DESCRIPTION = EMQ X Common Test Helpers

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

DEPS = meck proper
dep_meck = git-emqx https://github.com/emqx/meck.git 0.8.9
dep_proper = hex-emqx 1.3.0

BUILD_DEPS = emqx
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)

ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

rebar-deps:
	rebar3 get-deps

rebar-clean:
	@rebar3 clean

rebar-compile: rebar-deps
	rebar3 compile

rebar-ct:
	rebar3 ct -v

rebar-eunit:
	@rebar3 eunit

rebar-xref:
	@rebar3 xref
