{application, converse_app,
 [
  {description, "Converse server"},
  {vsn, "1.0"},
  {id, "converse"},
  {modules,      [converse_listener, tcp_app_fsm]},
  {registered,   [tcp_server_sup, converse_listener]},
  {applications, [kernel, stdlib, sasl]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {converse, []}},
  {env, []}
 ]
}.