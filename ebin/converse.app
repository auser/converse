{application, converse,
 [
  {description, "Converse TCP Server"},
  {vsn, "1.0"},
  {id, "converse"},
  {modules,      [converse_tcp]},
  {registered,   [converse_supervisor, converse_through_tcp]},
  {applications, [kernel, stdlib]},
  {mod, {converse_app, []}},
  {env, []}
 ]
}.