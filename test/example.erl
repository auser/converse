% talker_router
talker_router:start_link().
talker_router:get_all_connections().
talker_router:register_connection({0,0,0,1}, 5001, "pid", "socket").
talker_router:register_connection_with_info({0,0,0,2}, 5001, "pid", "socket", {{name, "fred"}}).
talker_router:register_connection_with_info({0,0,0,3}, 5001, "pid", "socket", {{name, "bob"}}).
talker_router:register_connection({0,0,0,4}, 5001, "pid", "socket").
talker_router:get_all_connections().
talker_router:unregister_connection({0,0,0,2}, 5001).
talker_router:unregister_connection({0,0,0,1}, 5001).
talker_router:get_all_connections().

% talker
talker:send({{10,47,90,162}, 5001}, {hi}).
