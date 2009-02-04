require "rubygems"
require "skelerl"

erlang do
  testing true
  
  options :path => "./ebin"
  
  with_node(:node0, :stop => false) do
    test_converse_app:start
  end
  
end