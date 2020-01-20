# -*- ruby -*-

require "rubygems"
require "hoe"

Hoe.plugin :seattlerb
Hoe.plugin :isolate
Hoe.plugin :rdoc

Hoe.spec "assert_scanner" do
  developer "Ryan Davis", "ryand-ruby@zenspider.com"

  dependency "ruby_parser",    "~> 3.13"
  dependency "ruby2ruby",      "~> 2.4"
  dependency "path_expander",  "~> 1.0"
  dependency "sexp_processor", "~> 4.12"
  dependency "graph",          "~> 2.9"

  dependency "minitest", "~> 5.0", :dev

  license "MIT"
end

task :autotest => :isolate do
  sh "autotest"
end

task :graph => :isolate do
  $: << "lib"
  require "assert_scanner"

  AssertScanner.graph
end

task :parse => :isolate do
  require "ruby_parser"
  require "pp"

  rp  = RubyParser.new
  src = ENV["R"]

  pp rp.parse src
end

task :debug => :isolate do
  $: << "lib"
  require "assert_scanner"

  path = ENV["F"] || "stdin"
  ruby = ENV["R"] || File.read(ENV["F"])

  scanner = AssertScanner.new
  scanner.process RubyParser.new.process(ruby, path)

  puts scanner.count
end

# vim: syntax=ruby
