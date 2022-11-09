{application, store, [
  {description, "Store app"},
  {vsn, "0.0.1"},
  {registered, [store, balancer_serv, number_serv, other_serv]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {store, []}},
  {env, []}
]}.
