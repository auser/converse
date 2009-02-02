% converse_router
converse_router:start_link().
converse_router:get_all_connections().
converse_router:register_connection({0,0,0,1}, 5001, {name, "fred"}).
converse_router:get_all_connections().

% converse
converse:start_link().
converse:send({{10,47,90,162}, 5001}, {hi}).