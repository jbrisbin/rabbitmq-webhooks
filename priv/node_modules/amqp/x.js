amqp = require('./amqp');
sys = require('sys');

inspect = sys.inspect;
puts = sys.puts;
var creds = { host:     process.env['AMQP_HOST']      || 'localhost'
            , login:    process.env['AMQP_LOGIN']     || 'guest'
            , password: process.env['AMQP_PASSWORD']  || 'guest'
            , vhost:    process.env['AMQP_VHOST']     || '/'
            };

connection = amqp.createConnection(creds);
connection.addListener('ready', function () {
  sys.debug("got ready event");
});
connection.addListener('error', function () {
  sys.debug("got error event");
});
connection.addListener('close', function () {
  sys.debug("got close event");
});
