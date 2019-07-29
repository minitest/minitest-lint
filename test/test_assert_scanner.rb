require "minitest/autorun"
require "assert_scanner"

$v = true # enables "redundant message" phase

class TestAssertScanner < Minitest::Test
  def test_sanity
    flunk "write tests or I will kneecap you"
  end
end
