PROJECT = emqx_ct_helpers
PROJECT_DESCRIPTION = EMQ X Common Test Helpers
PROJECT_VERSION = 0.1.0

BUILD_DEPS = emqx
dep_emqx = git https://github.com/emqtt/emqttd emqx30

include erlang.mk
