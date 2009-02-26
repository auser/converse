{application, converse_app,
 [
  {description, "Converse server"},
  {vsn, "1.0"},
  {id, "converse"},
  {modules,      [converse_listener, converse_tcp, converse_packet]},
  {registered,   []},
  {applications, [kernel, stdlib, sasl, crypto]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {converse, []}},
  {env, []}
 ]
}.