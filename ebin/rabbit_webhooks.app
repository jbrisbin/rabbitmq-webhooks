{application, rabbit_webhooks, [
  {description, "Rabbit Webhooks"},
  {vsn, "0.10"},
  {modules, [
    rabbit_webhooks,
    rabbit_webhooks_app,
    rabbit_webhooks_sup
  ]},
  {registered, []},
  {mod, {rabbit_webhooks_app, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client, lhttpc]}
]}.
