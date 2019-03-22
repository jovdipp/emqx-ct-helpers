PROJECT = emqx_ct_helpers
PROJECT_DESCRIPTION = EMQ X Common Test Helpers
PROJECT_VERSION = 0.1.1

BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)
DEPS = meck proper
dep_meck = git https://github.com/emqx/meck.git
dep_proper = git https://github.com/proper-testing/proper.git

BUILD_DEPS = emqx
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)

include erlang.mk
