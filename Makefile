PROJECT = emqx_ct_helpers
PROJECT_DESCRIPTION = EMQ X Common Test Helpers
PROJECT_VERSION = 0.1.0

BUILD_DEPS = emqx
dep_emqx = git https://github.com/emqtt/emqttd emqx30

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, emqx_ct_transform}'

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, emqx_ct_transform}'

include erlang.mk
