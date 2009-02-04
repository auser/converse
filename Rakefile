require "rubygems"
require "skelerl"

task :launch_server => [:compile] do
  cmd = "ruby #{File.dirname(__FILE__)}/scripts/start.rb"
  puts %x[#{cmd}]
end