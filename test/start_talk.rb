require "rubygems"
require "skelerl"

erlang do
  testing true
  
  options :path => "./ebin"
  
  with_node(:node0, :stop => false) do
    converse:start_link
  end
  
  with_node(:node1) do
    converse:start_link
  end
  
end