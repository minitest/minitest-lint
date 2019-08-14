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
      s(:call, nil, :assert_operator,
        s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)),
        s(:lit, :include?),
        s(:call, nil, :b)) => "assert_operator obj, msg, arg",
      s(:call, nil, :assert_includes,
        s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3)),
        s(:call, nil, :b)) => "assert_includes enum, val",
    }

    assert_equal exp, scan.io

    exp = [
      "  assert(([1, 2, 3].include?(b) == true))   # redundant message?",
      "  assert_equal([1, 2, 3].include?(b), true) # assert_equal exp, act",
      "  assert_equal(true, [1, 2, 3].include?(b)) # assert_equal exp, act",
      "  assert_operator([1, 2, 3], :include?, b)  # assert_operator obj, msg, arg",
      "  assert_includes([1, 2, 3], b)             # assert_includes enum, val",
    ]

    assert_equal exp, scan.out
  end

  def assert_re scanner, msg, from, to
    pattern = AssertScanner.const_get scanner
    scan = AssertScanner.new
    proc = AssertScanner.assertions[pattern]

    scan.instance_exec from, &proc

    assert_operator pattern, :===, from
    assert_match pattern, from

    exp = {to => msg}

    assert_equal exp, scan.io
  end

  def assert_re_done scanner, from
    pattern = AssertScanner.const_get scanner
    scan = AssertScanner.new
    proc = AssertScanner.assertions[pattern]

    scan.instance_exec from, &proc

    assert_operator pattern, :===, from
    assert_match pattern, from

    assert_empty scan.io
  end

  def am(msg, *args); s(:call, nil, msg, *args); end
  def a(*args);  am(:assert, *args);             end
  def r(*args);  am(:refute, *args);             end
  def ae(*args); am(:assert_equal, *args);       end
  def ai(*args); am(:assert_includes, *args);    end
  def e_(l,m,*r); s(:call, am(:_, l), m, *r);    end
  def eq(l,r);   e_(l, :must_equal,    r);       end
  def ee(l,r);   e_(l, :must_be_empty, r);       end
  def ep(*args); e_(l, :must_be,       r);       end

  def test_re_msg
    assert_re(:RE_MSG,
              "redundant message?",
              a(s(:lit, 42), s(:str, "message")),
              a(s(:lit, 42)))
  end

  def test_re_not
    assert_re(:RE_NOT,
              "refute not_cond",
              a(s(:call, s(:lit, 42), :!)),
              r(s(:lit, 42)))
  end

  def test_re_equal
    assert_re(:RE_EQUAL,
              "assert_equal exp, act",
              a(s(:call, :lhs, :==, :rhs)),
              ae(:lhs, :rhs))
  end

  def test_re_nequal
    assert_re(:RE_NEQUAL,
              "refute_equal exp, act",
              a(s(:call, :lhs, :!=, :rhs)),
              am(:refute_equal, :lhs, :rhs))
  end

  def test_re_incl
    assert_re(:RE_INCL,
              "assert_includes obj, val",
              a(s(:call, :lhs, :include?, :rhs)),
              ai(:lhs, :rhs))
  end

  def test_re_pred
    assert_re(:RE_PRED,
              "assert_predicate obj, msg",
              a(s(:call, :lhs, :pred?)),
              am(:assert_predicate, :lhs, s(:lit, :pred?)))
  end

  def test_re_oper
    assert_re(:RE_OPER,
              "assert_operator obj, msg, arg",
              a(s(:call, :lhs, :op, :rhs)),
              am(:assert_operator, :lhs, s(:lit, :op), :rhs))
  end

  def test_re_eq_msg
    assert_re(:RE_EQ_MSG,
              "redundant message?",
              ae(:lhs, :rhs, :msg),
              ae(:lhs, :rhs))
  end

  def test_re_eq_nil
    assert_re(:RE_EQ_NIL,
              "assert_nil obj",
              ae(s(:nil), :whatever),
              am(:assert_nil, :whatever))
  end

  def test_re_eq_pred
    assert_re(:RE_EQ_PRED,
              "assert_predicate obj, msg",
              ae(s(:true), s(:call, :obj, :msg)),
              am(:assert_predicate, :obj, s(:lit, :msg)))
  end

  def test_re_eq_oper
    assert_re(:RE_EQ_OPER,
              "assert_operator obj, msg, arg",
              ae(s(:true), s(:call, :obj, :msg, :rhs)),
              am(:assert_operator, :obj, s(:lit, :msg), :rhs))
  end

  def test_re_eq_lhs_str
    long = "string " * 100
    short = long[0, 20]

    assert_re(:RE_EQ_LHS_STR,
              "assert_includes actual, substr",
              ae(s(:str, long), :rhs),
              ai(:rhs, s(:str, short)))
  end

  def test_re_eq_rhs_lit
    assert_re(:RE_EQ_RHS_LIT,
              "assert_equal exp, act",
              ae(:lhs, s(:lit, :rhs)),
              ae(s(:lit, :rhs), :lhs))
  end

  def test_re_eq_rhs_str
    assert_re(:RE_EQ_RHS_STR,
              "assert_equal exp, act",
              ae(:lhs, s(:str, "str")),
              ae(s(:str, "str"), :lhs))
  end

  def test_re_eq_rhs_ntf__nil
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal exp, act",
              ae(:lhs, s(:nil)),
              ae(s(:nil), :lhs))
  end

  def test_re_eq_rhs_ntf__true
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal exp, act",
              ae(:lhs, s(:true)),
              ae(s(:true), :lhs))
  end

  def test_re_eq_rhs_ntf__false
    assert_re(:RE_EQ_RHS_NTF,
              "assert_equal exp, act",
              ae(:lhs, s(:false)),
              ae(s(:false), :lhs))
  end

  def test_re_eq_empty
    assert_re(:RE_EQ_EMPTY,
              "assert_empty",
              ae(s(:lit, 0), s(:call, :whatever, :length)),
              am(:assert_empty, :whatever))
  end

  def test_re_ref_msg
    assert_re(:RE_REF_MSG,
              "redundant message?",
              r(:test, s(:str, "msg")),
              r(:test))
  end

  def test_re_ref_not
    assert_re(:RE_REF_NOT,
              "assert cond",
              r(s(:call, s(:lit, 42), :!)),
              a(s(:lit, 42)))
  end

  def test_re_ref_incl
    assert_re(:RE_REF_INCL,
              "refute_includes obj, val",
              r(s(:call, :lhs, :include?, :rhs)),
              am(:refute_includes, :lhs, :rhs))
  end

  def test_re_ref_pred
    assert_re(:RE_REF_PRED,
              "refute_predicate obj, msg",
              r(s(:call, :lhs, :pred?)),
              am(:refute_predicate, :lhs, s(:lit, :pred?)))
  end

  def test_re_ref_oper
    assert_re(:RE_REF_OPER,
              "refute_operator obj, msg, arg",
              r(s(:call, :lhs, :op, :rhs)),
              am(:refute_operator, :lhs, s(:lit, :op), :rhs))
  end

  def test_re_op_incl
    assert_re(:RE_OP_INCL,
              "assert_includes enum, val",
              am(:assert_operator, :lhs, s(:lit, :include?), :rhs),
              ai(:lhs, :rhs))
  end

  def test_must_plain
    assert_re(:RE_MUST_PLAIN,
              "_(act).must_equal exp",
              s(:call,
                s(:call, s(:call, s(:call, nil, :a), :b), :c),
                :must_equal,
                s(:call, nil, :d)),
              # =>
              s(:call,
                s(:call, nil, :_,
                  s(:call, s(:call, s(:call, nil, :a), :b), :c)),
                :must_equal,
                s(:call, nil, :d)))
  end

  def test_must_plain_good
    assert_re_done(:RE_MUST_GOOD,
                   eq(s(:call, s(:call, s(:call, nil, :a), :b), :c),
                      s(:call, nil, :d)))
  end

  def test_must_plain_expect
    assert_re(:RE_MUST_OTHER,
              "_(act).must_equal exp",
              s(:call,
                s(:call, nil, :expect,
                  s(:call, s(:call, s(:call, nil, :a), :b), :c)),
                :must_equal,
                s(:call, nil, :d)),
              # =>
              eq(s(:call, s(:call, s(:call, nil, :a), :b), :c),
                 s(:call, nil, :d)))
  end

  def test_must_plain_value
    assert_re(:RE_MUST_OTHER,
              "_(act).must_equal exp",
              s(:call,
                s(:call, nil, :value,
                  s(:call, s(:call, s(:call, nil, :a), :b), :c)),
                :must_equal,
                s(:call, nil, :d)),
              # =>
              eq(s(:call, s(:call, s(:call, nil, :a), :b), :c),
                 s(:call, nil, :d)))
  end

  def test_must_equal_nil
    assert_re(:RE_MUST_EQ_NIL,
              "_(act).must_be_nil",
              eq(s(:call, s(:call, s(:call, nil, :a), :b), :c),
                 s(:nil)),
              # =>
              e_(s(:call, s(:call, s(:call, nil, :a), :b), :c),
                 :must_be_nil))
  end

  def test_re_plain
    assert_re(:RE_PLAIN,
              "Try to not use plain assert",
              a(:whatever),
              a(:whatever))
  end
end
