% talker_router
talker_router:start_link().
talker_router:get_all_connections().
talker_router:register_connection({0,0,0,1}, 5001, {name, "fred"}).
talker_router:get_all_connections().

% talker
talker:start_link().
talker:send({{10,47,90,162}, 5001}, {hi}).