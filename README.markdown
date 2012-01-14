## RabbitMQ Webhooks Plugin

This plugin provides a "webhook" functionality to a RabbitMQ broker. 
Any message processed by this plugin will be forwarded to the URL 
you configure, using the method you give it. 

Tested against RabbitMQ versions up to 2.7.

### Changes

* 0.14 - Lots better error handling and a Ruby script for generating config files
* 0.13 - Updated for use with the new plugin system in RabbitMQ 2.7
* 0.12 - Updated for use with RabbitMQ 2.3.0, now uses rebar for build
* 0.11 - Updated for use with RabbitMQ 2.2.0
* 0.9 - Incorporated patch from @cameronharris for OTP R13 compatibility, Makefile tweak
* 0.8 - Added error handling for request so bad URLs don't crash broker, fix for no message headers
* 0.7 - Added send window functionality for sending webhook requests only during specified time windows
* 0.6 - Added max_send config param for limiting how many outgoing HTTP requests happen
* 0.5 - Use RabbitMQ's worker_pool for sending requests to handle massive dumps of messages
* 0.4 - Accept more than just 200 status code for CouchDB
* 0.3 - Asynchronous HTTP send, URL and method overrideable per-message.
* 0.2 - URLs can be patterns and headers that start with "X-" get passed to REST URL.
* 0.1 - Synchronous HTTP send, no URL patterns. Rough draft.


### Install from Zip

Download the .tar.gz file from from the downloads section:

[http://github.com/jbrisbin/rabbitmq-webhooks/downloads](http://github.com/jbrisbin/rabbitmq-webhooks/downloads)

		cd $RABBITMQ_HOME
		mkdir plugins
		cd plugins
		tar -zxvf ~/rabbit_webhooks-0.x.tar.gz

You should now have two .ez files in your plugins directory:

		lhttpc.ez
		rabbit_webhooks.ez

In 2.7, you'll have to enable the plugins to get them to work:

		rabbitmq-plugins enable rabbit_webhooks

To configure your broker, download the `gen_config` script from the source tree and run it, pointing 
to a YAML file that contains your configuration (discussed below).

Copy the output of that generation to your RabbitMQ server config file (should be some place like: 
`/etc/rabbitmq/rabbitmq.config`).

Start your broker and you should see output similar to what's discussed in the "Installing" section.

### Install from Source

The build process for the webhooks plugin has changed. It now uses rebar to build.

		git clone https://github.com/jbrisbin/rabbitmq-webhooks.git
		cd rabbitmq-webhooks
		make
		make package

You can now install the three .ez files required:

		cp deps/amqp_client.ez $RABBITMQ_HOME/plugins
		cp deps/lhttpc.ez $RABBITMQ_HOME/plugins
		cp dist/rabbit_webhooks.ez $RABBITMQ_HOME/plugins

When you start the broker, you should see (at the top):

		... plugins activated:
		* lhttpc-1.2.5
		* rabbit_webhooks-0.14

and when the server is started:

		Configuring Webhooks...done

Logging is done to the server log file.

### What can I use this for?

If you configure a webhook to bind to exchange "test" with routing key 
"#", any messages published with that exchange and routing key will be 
automatically sent to an HTTP URL based on pre-configured parameters, or 
by specifying overrides in the message properties and headers.

This would allow you, for example, to drop JSON data into messages in an 
AMQP queue which get sent to a REST URL via POST (or PUT or DELETE, etc...). 

Clients with no access to a CouchDB server could send batches of updates 
through RabbitMQ. The webhooks plugin then HTTP POSTs those messages to the 
CouchDB server.

If the message is successfully POST/PUT/DELETE'd to the URL, it is ACK'd 
from the queue. If there was an error, the message is NOT ACK'd and stays in 
the queue for possible later delivery. There's probably a better way to handle 
this. I'm open for suggestions! :)

### Example Configuration

As of v0.14, there is a Ruby script you can use to transalte a YAML config 
file into the more complex and finicky Erlang config file. It will generate 
the correct atoms for you to include in your system `rabbitmq.config` file.

An example YAML file will look like this (with the bare minimum left uncommented,
everything commented out is optional and the values shown are the defaults):

		--- # Test YAML file for driving config file generation.

		# Broker configuration
		username          : guest
		virtual_host      : /

		# Use a YAML alias to reference this one exchange for all configs.
		exchange: &webhook_exchange
		  name            : webhooks
		  # type            : topic
		  # auto_delete     : true
		  # durable         : false

		# Webhooks configurations
		webhooks:
		  - 
		    name            : webhook1 # Name should be unique within the config file
		    url             : http://localhost:8000/rest
		    # method          : post # get | post | put | delete
		    exchange        : *webhook_exchange
		    queue:
		      name          : webhook1 # Best to have the queue name match the config name
		    #   auto_delete   : true
		    # routing_key     : "#"
		    # max_send:
		    #   frequency     : 5
		    #   per           : second # second | minute | hour | day | week
		    # send_if:
		    #   between: 
		    #     start_hour  : 8 # 24-hour time
		    #     start_min   : 0
		    #     end_hour    : 17  # 24-hour time
		    #     end_min     : 0

If you want to configure it manually, an example Erlang config file is included in `priv/`:

		[
			{rabbit_webhooks, [
		    {username, <<"guest">>},
		    {virtual_host, <<"/">>},
				{webhooks, [
					{test_one, [
						{url, "http://localhost:8000/rest"},
						{method, post},
						{exchange, [
							{exchange, &lt;&lt;"webhooks.test"&gt;&gt;},
							{type, &lt;&lt;"topic"&gt;&gt;},
							{auto_delete, true},
							{durable, false}
						]},
						{queue, [
							{queue, &lt;&lt;"webhooks.test.q"&gt;&gt;},
							{auto_delete, true}
						]},
						{routing_key, &lt;&lt;"#"&gt;&gt;},
						{max_send, {5, second}},
						{send_if, [{between, {13, 24}, {13, 25}}]}
					]}
				]}
			]}
		].

### TODO

Lots and lots still to do:

* Make message sending more robust, including SSL support, authentication, 
  and several other "would be nice to have"s.
* Expose various statii to the RabbitMQ console.

### License

Licensed under the Mozilla Public License:

[http://www.rabbitmq.com/mpl.html](http://www.rabbitmq.com/mpl.html)