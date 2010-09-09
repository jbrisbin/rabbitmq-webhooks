## RabbitMQ Webhooks Plugin

This plugin provides a "webhook" functionality to a RabbitMQ broker. 
Any message processed by this plugin will be forwarded to the URL 
you configure, using the method you give it. 

### Changes

<pre><code>0.8 - Added error handling for request so bad URLs don't crash broker, fix for no message headers
0.7 - Added send window functionality for sending webhook requests only during specified time windows
0.6 - Added max_send config param for limiting how many outgoing HTTP requests happen
0.5 - Use RabbitMQ's worker_pool for sending requests to handle massive dumps of messages
0.4 - Accept more than just 200 status code for CouchDB
0.3 - Asynchronous HTTP send, URL and method overrideable per-message.
0.2 - URLs can be patterns and headers that start with "X-" get passed to REST URL.
0.1 - Synchronous HTTP send, no URL patterns. Rough draft.
</code></pre>

### Be Careful!

If you misconfigure the webhooks plugin it will likely kill your broker. On the TODO list is robust error handling that will take care of this for you. But keep in mind that this is still fairly untested code. It works fine, it's just not very forgiving.

### Install from Zip

Download the .tar.gz file from from the downloads section:

[http://github.com/jbrisbin/rabbitmq-webhooks/downloads](http://github.com/jbrisbin/rabbitmq-webhooks/downloads)

<pre><code>cd $RABBITMQ_HOME
mkdir plugins
cd plugins
tar -zxvf ~/rabbit_webhooks-0.x.tar.gz
</code></pre>

You should now have three .ez files in your plugins directory:

<pre><code>amqp_client.ez
lhttpc.ez
rabbit_webhooks.ez
</code></pre>

Start your broker and you should see output similar to what's discussed in the "Installing" section.

### Install from Source

You first need to check out and build the public umbrella:

[http://www.rabbitmq.com/plugin-development.html#getting-started](http://www.rabbitmq.com/plugin-development.html#getting-started)

First, build rabbitmq-server and rabbitmq-erlang-client, then build the 
plugin:

<pre><code>
cd ~/src/rabbitmq-public-umbrella
git clone git://github.com/jbrisbin/rabbitmq-webhooks.git
cd rabbitmq-webhooks
make
</code></pre>

### Installing

To install in the broker that was built from source, copy the required 
.ez files to the "plugins" directory:

<pre><code>
cd ../rabbitmq-server
mkdir plugins
cp ../rabbitmq-webhooks/dist/amqp_client.ez plugins
cp ../rabbitmq-webhooks/dist/lhttpc.ez plugins
cp ../rabbitmq-webhooks/dist/rabbit_webhooks.ez plugins
make run
</code></pre>

You should see (at the top):

<pre><code>
3 plugins activated:
* amqp_client
* lhttpc
* rabbit_webhooks	
</code></pre>

and (when the server is started):

<pre><code>
Configuring Webhooks...done
</code></pre>

Logging is done to the server log file.

### Why?

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

An example rabbit.config file is included. Here it is:

<pre><code>[
	{rabbit_webhooks, [
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
</code></pre>

### TODO

Lots and lots still to do:

* Add error handling so the gen_server doesn't get terminated when the 
  URL is not alive or returns Really Bad Errors.
* Make message sending more robust, including SSL support, authentication, 
  and several other "would be nice to have"s.
* Expose various statii to the RabbitMQ console.

### License

Licensed under the Mozilla Public License:

[http://www.rabbitmq.com/mpl.html](http://www.rabbitmq.com/mpl.html)