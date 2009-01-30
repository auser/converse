require "rubygems"
require "skelerl"

erlang do  
  options :path => "/../ebin"
  
  with_node(:node0, :stop => false) do
    talker:start_link
  end
  
  with_node(:node1) do
    talker:start_link
  end
  
end