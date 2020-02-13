PROJECT = rabbitmq_webhooks
PROJECT_DESCRIPTION = Webhooks plugin for Rabbit
PROJECT_MOD = rabbit_webhooks_app

define PROJECT_ENV
[
    {username, <<"guest">>},
    {password, <<"guest">>},
    {virtual_host, <<"/">>},
    {webhooks, []}
  ]
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, ["3.8.0"]}
endef

DEPS = rabbit_common rabbit amqp_client dlhttpc rabbitmq_management
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
