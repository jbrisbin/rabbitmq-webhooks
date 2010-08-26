## RabbitMQ Webhooks Plugin ##

This plugin provides a "webhook" functionality to a RabbitMQ broker. 
Any message processed by this plugin will be forwarded to the URL 
you configure, using the method you give it. 

#### For example: ####

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

#### Example Configuration ####

An example rabbit.config file is included. Here it is:

<pre><code>
	[
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
					{routing_key, &lt;&lt;"#"&gt;&gt;}
				]}
			]}
		]}
	].
</code></pre>

#### TODO ####

Lots and lots still to do:

* Make the URL a pattern rather than a string, to allow for parameter 
  substitution within the URL.
* Add error handling so the gen_server doesn't get terminated when the 
  URL is not alive or returns Really Bad Errors.
* Make message sending more robust, including SSL support, authentication, 
  and several other "would be nice to have"s.
* Expose various statii to the RabbitMQ console.

#### License ####

Licensed under the Mozilla Public License:
http://www.rabbitmq.com/mpl.html