require "rubygems"
require "skelerl"

erlang do
  testing true
  
  options :path => "./ebin"
  
  with_node(:node0, :stop => false) do
    converse:start_in_shell_for_testing
  end
  
  with_node(:node1) do
    converse:start_in_shell_for_testing
  end
  
end