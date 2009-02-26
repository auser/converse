layers:start([converse, converse_test_app], [{port, 22002}]).
converse:send_message("0.0.0.0", {data, "TEsting for replies"}).
converse:send_message("0.0.0.0", {data, "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."}).

converse:start(normal, []).
converse:send_message("0.0.0.0", "hey").
converse:send_message("0.0.0.0", 12345).
converse:cast_message("0.0.0.0", "hey").

f().
{ok, Sock} = gen_tcp:connect({0,0,0,0}, 22002, [binary]).
gen_tcp:send(Sock, term_to_binary({data, "TEsting for replies"})).
receive {tcp, S, M} -> M after 1000 -> no_response end.

receive
  {reply, Reply} -> io:format("Received ~p~n", [Reply])
  after 1000 -> ok
end.

{ok, Sock} = gen_tcp:connect({10,45,10,228}, 22002, [binary]).

converse:send({0,0,0,0}, {"hi"}).

layers:start([converse, layers_test_app], []).
converse:send({0,0,0,0}, {"hi"}).
converse:send({0,0,0,1}, {"hi"}).