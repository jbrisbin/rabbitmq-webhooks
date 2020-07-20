FROM elixir:1.10.4 as base

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# adds deps for erlang and rabbitmq
RUN apt-get update -yq && \
    apt-get install build-essential rsync zip -yq --no-install-recommends && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# set app basepath
ENV HOME=/opt/

# copy package.json files
COPY . $HOME/rabbitmq-webhooks/

# change workgin dir
WORKDIR $HOME/rabbitmq-webhooks/

# pulls changes, compile code and build all production stuff
RUN make && make dist

# start new image for lower size
FROM rabbitmq:3.8.2-management

# change workgin dir
WORKDIR $RABBITMQ_HOME

# copy production complied plugin to the new rabbitmq image
COPY --from=base /opt/rabbitmq-webhooks/plugins/dispcount-*.ez plugins
COPY --from=base /opt/rabbitmq-webhooks/plugins/dlhttpc-*.ez plugins
COPY --from=base /opt/rabbitmq-webhooks/plugins/rabbitmq_webhooks-*.ez plugins

RUN rabbitmq-plugins enable rabbitmq_webhooks
