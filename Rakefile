# -*- ruby -*-

require "rubygems"
require "hoe"

Hoe.plugin :isolate
Hoe.plugin :seattlerb
Hoe.plugin :rdoc

Hoe.spec "assert_scanner" do
  developer "Ryan Davis", "ryand-ruby@zenspider.com"

  dependency "ruby_parser",    "~> 3.13"
  dependency "ruby2ruby",      "~> 2.4"
  dependency "path_expander",  "~> 1.0"
  dependency "sexp_processor", "~> 4.12"

  dependency "minitest", "~> 5.0", :dev

  license "MIT"
end

# vim: syntax=ruby
