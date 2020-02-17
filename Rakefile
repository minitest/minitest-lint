require "rubygems"
require "hoe"

Hoe.plugin :seattlerb
Hoe.plugin :isolate
Hoe.plugin :rdoc

Hoe.spec "minitest-lint" do
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

task :list => :isolate do
  $: << "lib"
  require "minitest_lint/assert_scanner"

  MinitestLint::AssertScanner.list
end

task :print => :isolate do
  cmds = ["rake list PAGES=1",
          "cupsfilter -i text/plain -o landscape -o lpi=5 -o cpi=8",
          "/usr/libexec/cups/filter/cgpdftopdf 1 1 1 1 number-up=4 > text.pdf"]

  sh cmds.join " | "
  sh "open text.pdf"
end

task :graph => :isolate do
  $: << "lib"
  require "minitest_lint/assert_scanner"

  File.open "assert.dot", "w" do |f|
    MinitestLint::AssertScanner.graph f
  end
  sh "open assert.dot"
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
  require "minitest_lint/assert_scanner"

  path = ENV["F"] || "stdin"
  ruby = ENV["R"] || File.read(ENV["F"])

  scanner = MinitestLint::AssertScanner.new
  scanner.process RubyParser.new.process(ruby, path)

  puts scanner.count
end

def shell cmd
  sh "#{ENV["SHELL"]} -c %p" % cmd
end

task :sort => :isolate do
  shell "diff -u <(./bin/mt_lint --raw) <(./bin/mt_lint --list | grep .)"
  sh "grepsort -u '^ +def.test_' test/test_assert_scanner.rb"
  sh "./test/test_assert_scanner.rb"
end

# vim: syntax=ruby
