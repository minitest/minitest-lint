$v ||= false
$d ||= false

class MinitestLint
  VERSION = "1.0.0"

  ######################################################################
  # Runners

  def self.run args = ARGV
    require_relative "minitest_lint/assert_scanner"
    MinitestLint::AssertScanner.run args
  end
end
