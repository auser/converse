require "rubygems"
require "skelerl"

erlang do  
  testing true
  options :path => "./ebin"
  
  with_node(:node0, :stop => false, :boot => "start_sasl") do
    converse.start test_app, layers_receive, port, 1234
  end
  
end