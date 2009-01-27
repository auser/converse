#!/usr/bin/env ruby
require "fileutils"

dirs = %w(deps doc ebin include priv scripts src support)
files = %w(LICENSE Makefile README Rakefile)

dirs.each {|d| FileUtils.mkdir_p d unless ::File.directory? d}
files.each {|d| FileUtils.touch d unless ::File.file? d}