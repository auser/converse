% Start it up!
erl -pa ./ebin -sname node0
converse_supervisor:start_in_shell_for_testing( 5001, self() ).

% Sending a message
converse:send({{10,211,55,2}, 5001}, {hi}).
converse:send({{10,211,55,2}, 5001}, {deliver, "hi"}).