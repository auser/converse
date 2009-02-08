{application, tcp_server,
 [
  {description, "Demo TCP server"},
  {vsn, "1.0"},
  {id, "tcp_server"},
  {modules,      [converse_listener, tcp_app_fsm]},
  {registered,   [tcp_server_sup, converse_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {converse, []}},
  {env, []}
 ]
}.