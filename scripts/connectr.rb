require "rubygems"
require "ruberl"

class TestClass < Ruberl::Base
end

puts TestClass.new("localhost", 7899).messenger_send!("hi")