-define (TCP_SEND(Socket, Data), 
  fun() ->
    DataToSend = converse_socket:encode(Data),
    case gen_tcp:send(Socket, DataToSend) of
      ok -> ok;
      {error, close} -> ok;
      {error, econnaborted} -> ok;
      Else ->
        io:format("Error in TCP_SEND: ~p~n", [Else]),
        Else
    end
  end()).