require "minitest/autorun"
require "assert_scanner"

$v = true # enables "redundant message" phase

class TestAssertScanner < Minitest::Test
  make_my_diffs_pretty!

  def self.todo msg
    define_method "test_#{msg}" do
      skip "not yet"
    end
  end

  def c(msg, *args); s(:call, nil, msg, *args);  end
  def a(*args);      c(:assert, *args);          end
  def r(*args);      c(:refute, *args);          end
  def e(l,m,*r);     s(:call, c(:_, l), m, *r);  end
  def aeq(*args);    c(:assert_equal, *args);    end
  def ain(*args);    c(:assert_includes, *args); end
  def blk(*a);       s(:iter, c(:_), 0, *a);     end
  def bm(*a, m, r);  s(:call, blk(*a), m, r);    end
  def meq(l,r);      e(l, :must_equal,    r);    end
  def lit(x);        s(:lit, x);                 end
  def mbe(l, m, *r); e(l, :must_be, lit(m), *r); end

  def assert_pattern scanner, from, msg = nil, to = nil
    pattern = AssertScanner.const_get scanner
    scan = AssertScanner.new
    proc = AssertScanner.assertions[pattern]

    scan.instance_exec from, &proc

    assert_operator pattern, :===, from
    assert_match pattern, from

    if msg && to
      exp = {to => msg}

      assert_equal exp, scan.io
    else
      assert_empty scan.io
    end
  end

  def assert_re scanner, msg, from, to
    assert_pattern scanner, from, msg, to
  end

  def assert_re_done scanner, from
    assert_pattern scanner, from
  end

  def assert_nothing sexp
    scan = AssertScanner.new

    scan.analyze_assert sexp

    assert_empty scan.io
  end

  ######################################################################
  # Sanity Test:

  def test_000_sanity
    ruby = %(assert [1, 2, 3].include?(b) == true, "is b in 1..3?")
    sexp = RubyParser.new.process ruby
    scan = AssertScanner.new

    scan.analyze_assert sexp

    lhs = s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3))
    rhs = c(:b)
    inc = s(:call, lhs, :include?, rhs)

    exp = {
      c(:assert, s(:call, inc, :==, s(:true)))          => "redundant message?",
      c(:assert_equal, inc, s(:true))                   => "assert_equal exp, act",
      c(:assert_equal, s(:true), inc)                   => "assert_equal lit, act",
      c(:assert_operator, lhs, s(:lit, :include?), rhs) => "assert_operator obj, :msg, val",
      c(:assert_includes, lhs, rhs)                     => "assert_includes obj, val",
    }

    assert_equal exp, scan.io

    exp = [
      #  assert([1, 2, 3].include?(b) == true, "is b in 1..3?") # original
      "  assert(([1, 2, 3].include?(b) == true))   # redundant message?",
      "  assert_equal([1, 2, 3].include?(b), true) # assert_equal exp, act",
      "  assert_equal(true, [1, 2, 3].include?(b)) # assert_equal lit, act",
      "  assert_operator([1, 2, 3], :include?, b)  # assert_operator obj, :msg, val",
      "  assert_includes([1, 2, 3], b)             # assert_includes obj, val",
    ]

    assert_equal exp, scan.out
  end

  ######################################################################
  # Positive Assertions

  todo :assert_match
  todo :assert_output
  todo :assert_path_exists
  todo :assert_raises
  todo :assert_respond_to
  todo :assert_same
  todo :assert_silent
  todo :assert_throws

  def test_assert
    assert_re(:RE_PLAIN,
              "Try to not use plain assert",
              a(:whatever),
              # =>
              a(:whatever))
  end

  def test_assert__msg
    assert_re(:RE_MSG,
              "redundant message?",
              a(:lhs, s(:str, "message")),
              # =>
              a(:lhs))
  end

  def test_assert__not
    assert_re(:RE_NOT,
              "refute obj",
              a(s(:call, :lhs, :!)),
              # =>
              r(:lhs))
  end

  def test_assert_empty
    assert_re(:RE_EMPTY,
              "assert_empty obj",
              a(s(:call, :lhs, :empty?)),
              # =>
              c(:assert_empty, :lhs))
  end

  def test_assert_equal
    assert_re(:RE_EQUAL,
              "assert_equal exp, act",
              a(s(:call, :lhs, :==, :rhs)),
              # =>
              aeq(:lhs, :rhs))
  end

  def test_assert_equal__empty
    assert_re(:RE_EQ_EMPTY,
              "assert_empty obj",
              aeq(lit(0), s(:call, :whatever, :length)),
              # =>
              c(:assert_empty, :whatever))
  end

  def test_assert_equal__empty_array
    assert_re(:RE_EQ_EMPTY_LIT,
              "assert_empty obj",
              aeq(s(:array), :lhs),
              # =>
              c(:assert_empty, :lhs))
  end

  def test_assert_equal__empty_hash
    assert_re(:RE_EQ_EMPTY_LIT,
              "assert_empty obj",
              aeq(s(:hash), :lhs),
              # =>
              c(:assert_empty, :lhs))
  end

  def test_assert_equal__lhs_str
    long = "string " * 100
    short = long[0, 20]

    assert_re(:RE_EQ_LHS_STR,
              "assert_includes str, 'substr'",
              aeq(s(:str, long), :rhs),
              # =>
              ain(:rhs, s(:str, short)))
  end

  def test_assert_equal__msg
    assert_re(:RE_EQ_MSG,
              "redundant message?",
              aeq(:lhs, :rhs, :msg),
              # =>
              aeq(:lhs, :rhs))
  end

  def test_assert_equal__nil
    assert_re(:RE_EQ_NIL,
              "assert_nil obj",
              aeq(s(:nil), :whatever),
              # =>
              c(:assert_nil, :whatever))
  end

  def test_assert_equal__oper
    assert_re(:RE_EQ_OPER,
              "assert_operator obj, :msg, val",
              aeq(s(:true), s(:call, :obj, :msg, :rhs)),
              # =>
              c(:assert_operator, :obj, lit(:msg), :rhs))
  end

  def test_assert_equal__pred
    assert_re(:RE_EQ_PRED,
              "assert_predicate obj, :pred?",
              aeq(s(:true), s(:call, :obj, :msg)),
              # =>
              c(:assert_predicate, :obj, lit(:msg)))
  end

  def test_assert_equal__rhs_lit
    assert_re(:RE_EQ_RHS_LIT,
              "assert_equal lit, act",
              aeq(:act, lit(:val)),
              # =>
              aeq(lit(:val), :act))
  end

  def test_assert_equal__rhs_ntf__false
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal lit, act",
              aeq(:act, s(:false)),
              # =>
              aeq(s(:false), :act))
  end

  def test_assert_equal__rhs_ntf__nil
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal lit, act",
              aeq(:act, s(:nil)),
              # =>
              aeq(s(:nil), :act))
  end

  def test_assert_equal__rhs_ntf__true_true
    assert_nothing aeq(s(:true), s(:true))
  end

  def test_assert_equal__rhs_ntf__lit_true
    assert_nothing aeq(s(:lit, 42), s(:true))
  end

  def test_assert_equal__rhs_ntf__true
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal lit, act",
              aeq(:act, s(:true)),
              # =>
              aeq(s(:true), :act))
  end

  def test_assert_equal__rhs_str
    assert_re(:RE_EQ_RHS_STR,
              "assert_equal lit, act",
              aeq(:act, s(:str, "str")),
              # =>
              aeq(s(:str, "str"), :act))
  end

  def test_assert_in_delta
    assert_re(:RE_IN_DELTA,
              "assert_in_epsilon float_lit, act",
              c(:assert_in_delta, :lhs, :rhs),
              # =>
              c(:assert_in_epsilon, :lhs, :rhs))
  end

  def test_assert_in_epsilon
    assert_re(:RE_IN_EPSILON,
              "assert_in_epsilon float_lit, act",
              aeq(s(:lit, 6.28), :rhs),
              # =>
              c(:assert_in_epsilon, s(:lit, 6.28), :rhs))
  end

  def test_assert_includes
    assert_re(:RE_INCL,
              "assert_includes obj, val",
              a(s(:call, :lhs, :include?, :rhs)),
              # =>
              ain(:lhs, :rhs))
  end

  def test_assert_instance_of
    assert_re(:RE_INSTANCE_OF,
              "assert_instance_of cls, obj",
              a(s(:call, :obj, :instance_of?, :cls)),
              # =>
              c(:assert_instance_of, :cls, :obj))
  end

  def test_assert_is_a
    assert_re(:RE_IS_A,
              "assert_kind_of mod, obj",
              a(s(:call, :obj, :is_a?, :mod)),
              # =>
              c(:assert_kind_of, :mod, :obj))
  end

  def test_assert_kind_of
    assert_re(:RE_KIND_OF,
              "assert_kind_of mod, obj",
              a(s(:call, :obj, :kind_of?, :mod)),
              # =>
              c(:assert_kind_of, :mod, :obj))
  end

  def test_assert_nequal
    assert_re(:RE_NEQUAL,
              "refute_equal exp, act",
              a(s(:call, :lhs, :!=, :rhs)),
              # =>
              c(:refute_equal, :lhs, :rhs))
  end

  def test_assert_operator
    assert_re(:RE_OPER,
              "assert_operator obj, :msg, val",
              a(s(:call, :lhs, :msg, :rhs)),
              # =>
              c(:assert_operator, :lhs, lit(:msg), :rhs))
  end

  def test_assert_operator__incl
    assert_re(:RE_OP_INCL,
              "assert_includes obj, val",
              c(:assert_operator, :lhs, lit(:include?), :rhs),
              # =>
              ain(:lhs, :rhs))
  end

  def test_assert_predicate
    assert_re(:RE_PRED,
              "assert_predicate obj, :pred?",
              a(s(:call, :lhs, :pred?)),
              # =>
              c(:assert_predicate, :lhs, lit(:pred?)))
  end

  ######################################################################
  # Negative Assertions

  todo :refute_in_delta
  todo :refute_in_epsilon
  todo :refute_match
  todo :refute_nil
  todo :refute_operator
  todo :refute_path_exists
  todo :refute_predicate
  todo :refute_respond_to
  todo :refute_same

  def test_refute
    assert_re(:RE_REF_PLAIN,
              "Try to not use plain refute",
              r(:whatever),
              # =>
              r(:whatever))
  end

  def test_refute__msg
    assert_re(:RE_REF_MSG,
              "redundant message?",
              r(:test, s(:str, "msg")),
              # =>
              r(:test))
  end

  def test_refute__not
    assert_re(:RE_REF_NOT,
              "assert obj",
              r(s(:call, :lhs, :!)),
              # =>
              a(:lhs))
  end

  def test_refute_empty
    assert_re(:RE_REF_EMPTY,
              "refute_empty val",
              r(s(:call, :lhs, :empty?)),
              # =>
              c(:refute_empty, :lhs))
  end

  def test_refute_equal
    assert_re(:RE_REF_EQUAL,
              "refute_equal exp, act",
              r(s(:call, :lhs, :==, :rhs)),
              # =>
              c(:refute_equal, :lhs, :rhs))
  end

  def test_refute_nequal
    assert_re(:RE_REF_NEQUAL,
              "assert_equal exp, act",
              r(s(:call, :lhs, :!=, :rhs)),
              # =>
              c(:assert_equal, :lhs, :rhs))
  end

  def test_refute_includes
    assert_re(:RE_REF_INCL,
              "refute_includes obj, val",
              r(s(:call, :lhs, :include?, :rhs)),
              # =>
              c(:refute_includes, :lhs, :rhs))
  end

  def test_refute_instance_of
    assert_re(:RE_REF_INSTANCE_OF,
              "refute_instance_of cls, obj",
              r(s(:call, :obj, :instance_of?, :cls)),
              # =>
              c(:refute_instance_of, :cls, :obj))
  end

  def test_refute_is_a
    assert_re(:RE_REF_IS_A,
              "refute_kind_of mod, obj",
              r(s(:call, :obj, :is_a?, :mod)),
              # =>
              c(:refute_kind_of, :mod, :obj))
  end

  def test_refute_kind_of
    assert_re(:RE_REF_KIND_OF,
              "refute_kind_of mod, obj",
              r(s(:call, :obj, :kind_of?, :mod)),
              # =>
              c(:refute_kind_of, :mod, :obj))
  end

  def test_refute_oper
    assert_re(:RE_REF_OPER,
              "refute_operator obj, :msg, val",
              r(s(:call, :lhs, :msg, :rhs)),
              # =>
              c(:refute_operator, :lhs, lit(:msg), :rhs))
  end

  def test_refute_pred
    assert_re(:RE_REF_PRED,
              "refute_predicate obj, :pred?",
              r(s(:call, :lhs, :msg)),
              # =>
              c(:refute_predicate, :lhs, lit(:msg)))
  end

  ######################################################################
  # Positive Expectations

  todo :must_equal
  todo :must_equal_true
  todo :must_equal_false
  todo :must_equal_pred
  todo :must_equal_oper

  todo :must_equal_big_string
  todo :must_equal_assert_equal?
  todo :must_equal_lhs_str
  todo :must_equal_rhs_lit
  todo :must_equal_rhs_str

  todo :must_be__nil
  todo :must_be__empty

  todo :must_include__include
  todo :must_include__key

  todo :must_be_close_to
  todo :must_be_same_as
  todo :must_be_silent
  todo :must_be_within_epsilon
  todo :must_match
  todo :must_output
  todo :must_output__empty
  todo :must_raise
  todo :must_respond_to
  todo :must_throw

  todo :path_must_exist

  def test_must___plain
    assert_re(:RE_MUST_PLAIN,
              "_(obj).must_<something> val",
              s(:call, :lhs, :must_equal, :rhs),
              # =>
              meq(:lhs, :rhs))
  end

  def test_must___plain_good
    assert_re_done(:RE_MUST_GOOD, # TODO: GOOD -> DONE|STOP ?
                   e(:lhs, :must_xxx, :rhs))
  end

  def test_must__plain_block_good
    assert_re_done(:RE_MUST_BLOCK_GOOD,
                   bm(:lhs, :must_xxx, :rhs))
  end

  def test_must__plain_expect
    assert_re(:RE_MUST_OTHER,
              "_(obj).must_<something> val",
              s(:call, c(:expect, :act), :must_equal, :exp),
              # =>
              meq(:act, :exp))
  end

  def test_must__plain_value
    assert_re(:RE_MUST_OTHER,
              "_(obj).must_<something> val",
              s(:call, c(:value, :act), :must_equal, :exp),
              # =>
              meq(:act, :exp))
  end

  def test_must_be__op
    assert_re(:RE_MUST_BE_OPER,
              "_(obj).must_be :msg, val",
              meq(s(:call, :lhs, :msg, :rhs), s(:true)),
              # =>
              mbe(:lhs, :msg, :rhs))
  end

  def test_must_be__pred
    assert_re(:RE_MUST_BE_PRED,
              "_(obj).must_be :pred?",
              meq(s(:call, :lhs, :pred?), s(:true)),
              # =>
              mbe(:lhs, :pred?))
  end

  def test_must_be_empty__array
    assert_re(:RE_MUST_BE_EMPTY_LIT,
              "_(obj).must_be_empty",
              meq(:lhs, s(:array)),
              # =>
              e(:lhs, :must_be_empty))
  end

  def test_must_be_empty__hash
    assert_re(:RE_MUST_BE_EMPTY_LIT,
              "_(obj).must_be_empty",
              meq(:lhs, s(:hash)),
              # =>
              e(:lhs, :must_be_empty))
  end

  def test_must_be_empty__length
    assert_re(:RE_MUST_BE_EMPTY,
              "_(obj).must_be_empty",
              meq(s(:call, :lhs, :length), lit(0)),
              # =>
              e(:lhs, :must_be_empty))
  end

  def test_must_be_empty__count
    assert_re(:RE_MUST_BE_EMPTY,
              "_(obj).must_be_empty",
              meq(s(:call, :lhs, :count), lit(0)),
              # =>
              e(:lhs, :must_be_empty))
  end

  def test_must_be_empty__size
    assert_re(:RE_MUST_BE_EMPTY,
              "_(obj).must_be_empty",
              meq(s(:call, :lhs, :size), lit(0)),
              # =>
              e(:lhs, :must_be_empty))
  end

  def test_must_be_instance_of
    assert_re(:RE_MUST_BE_INSTANCE_OF,
              "_(obj).must_be_instance_of cls",
              mbe(:lhs, :instance_of?, :rhs),
              # =>
              e(:lhs, :must_be_instance_of, :rhs))
  end

  def test_must_be_is_a
    assert_re(:RE_MUST_BE_IS_A,
              "_(obj).must_be_instance_of cls",
              mbe(:lhs, :is_a?, :rhs),
              # =>
              e(:lhs, :must_be_instance_of, :rhs))
  end

  def test_must_be_kind_of
    assert_re(:RE_MUST_BE_KIND_OF,
              "_(obj).must_be_kind_of mod",
              mbe(:lhs, :kind_of?, :rhs),
              # =>
              e(:lhs, :must_be_kind_of, :rhs))
  end

  def test_must_be_respond_to
    assert_re(:RE_MUST_BE_RESPOND_TO,
              "_(obj).must_respond_to val",
              mbe(:lhs, :respond_to?, :rhs),
              # =>
              e(:lhs, :must_respond_to, :rhs))
  end

  def test_must_equal_rhs_ntf__nil
    assert_re(:RE_MUST_EQ_NIL,
              "_(obj).must_be_nil",
              meq(:lhs, s(:nil)),
              # =>
              e(:lhs, :must_be_nil))
  end

  def test_must_include
    assert_re(:RE_MUST_INCLUDE,
              "_(obj).must_include val",
              meq(s(:call, :lhs, :include?, :rhs), s(:true)),
              # =>
              e(:lhs, :must_include, :rhs))
  end

  ######################################################################
  # Negative Expectations

  todo :wont_be_empty
  todo :wont_be__empty
  todo :wont_be__nil
  todo :wont_be__include
  todo :wont_be__key

  todo :wont_equal
  todo :wont_be_close_to
  todo :wont_be_within_epsilon
  todo :wont_be_instance_of
  todo :wont_be_kind_of
  todo :wont_match
  todo :wont_be_nil
  todo :wont_respond_to
  todo :wont_be_same_as
  todo :path_wont_exist

  # # TODO: make sure I'm picking up _/value/expect
  # # TODO: make sure I'm picking up _ { ... }.must/wont...
  #
  # _(lhs.size).wont_be(:>, 0) -> must_be_empty
  # _(lhs.size).must_be(:>, 0) -> wont_be_empty
  # _(lhs.length).must_be(:>=, 4) # TODO: warn about magic numbers?

  def test_wont_be__oper
    assert_re(:RE_WONT_BE_OPER,
              "_(obj).wont_be :msg, val",
              meq(s(:call, :lhs, :msg, :rhs), s(:false)),
              # =>
              e(:lhs, :wont_be, lit(:msg), :rhs))
  end

  def test_wont_be__pred
    assert_re(:RE_WONT_BE_PRED,
              "_(obj).wont_be :pred?",
              meq(s(:call, :lhs, :pred?), s(:false)),
              # =>
              e(:lhs, :wont_be, lit(:pred?)))
  end

  def test_wont_include
    assert_re(:RE_WONT_INCLUDE,
              "_(obj).wont_include val",
              meq(s(:call, :lhs, :include?, :rhs), s(:false)),
              # =>
              e(:lhs, :wont_include, :rhs))
  end
end

__END__
# TODO: some sort of auditing:

require "minitest/assertions"
require "minitest/expectations"

p Minitest::Assertions.public_instance_methods.grep(/assert|refute/)
p Minitest::Expectations.public_instance_methods.grep(/must|wont/)
