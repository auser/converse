% Start it up!
erl -pa ./ebin -sname node0
converse:start_link( 5001, self() ).
converse:start_link_for_testing( 5001 ).

% Sending a message
converse:send({{10,211,55,2}, 5001}, {hi}).
converse:send({{10,211,55,2}, 5001}, {deliver, "hi"}).

converse:send({{0,0,0,0}, 5001}, {deliver, "hi"}).