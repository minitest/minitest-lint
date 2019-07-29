require "minitest/autorun"
require "assert_scanner"

$v = true # enables "redundant message" phase

class TestAssertScanner < Minitest::Test
  def test_sanity
    ruby = %(assert [1, 2, 3].include?(b) == true, "is b in 1..3?")
    sexp = RubyParser.new.process ruby
    scan = AssertScanner.new

    scan.analyze_assert sexp

    exp = {
      s(:call, nil, :assert,
        s(:call,
          s(:call, s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)), :include?,
            s(:call, nil, :b)),
          :==,
          s(:true))) => "redundant message?",
      s(:call, nil, :assert_equal,
        s(:call, s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)), :include?,
          s(:call, nil, :b)),
        s(:true)) => "assert_equal exp, act",
      s(:call, nil, :assert_equal,
        s(:true),
        s(:call, s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)), :include?,
          s(:call, nil, :b))) => "assert_equal exp, act",
      s(:call, nil, :assert_includes,
        s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)),
        s(:lit, :include?),
        s(:call, nil, :b)) => "assert_includes obj, val",
    }

    assert_equal exp, scan.io

    exp = [
      "  assert(([1, 2, 3].include?(b) == true))   # redundant message?",
      "  assert_equal([1, 2, 3].include?(b), true) # assert_equal exp, act",
      "  assert_equal(true, [1, 2, 3].include?(b)) # assert_equal exp, act",
      "  assert_includes([1, 2, 3], :include?, b)  # assert_includes obj, val",
    ]

    assert_equal exp, scan.out
  end
end
