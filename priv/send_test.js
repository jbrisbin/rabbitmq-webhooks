var amqp = require("amqp");

var conn = amqp.createConnection();
conn.addListener("ready", function() {
	var x = conn.exchange("webhooks.test", {type: "topic", durable: false, autoDelete: true});
	x.publish("test", {test: "value"});
	
	setTimeout(function() { conn.end(); }, 500);
});