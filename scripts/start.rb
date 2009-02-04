require "rubygems"
require "skelerl"

erlang do  
  options :path => "./ebin"
  
  with_node(:node0, :stop => false, :boot => "start_sasl") do
    converse.start test_app, receive_function, port, 1234
  end
  
end