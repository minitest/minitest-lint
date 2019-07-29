require "ruby_parser"
require "sexp_processor"
require "path_expander"
require "ruby2ruby"

$v ||= false

class AssertScanner < SexpProcessor
  VERSION = "1.0.0"

  ############################################################
  # Patterns:

  def self.pat s
    Sexp::Matcher.parse s
  end

  def self.assert_pat test
    pat "(call nil assert #{test} ___)"
  end

  def self.refute_pat test
    pat "(call nil refute #{test} ___)"
  end

  def self.eq_pat lhs, rhs
    pat "(call nil assert_equal #{lhs} #{rhs} ___)"
  end

  RE_EQ_EMPTY   = eq_pat "(lit 0)", "(call _ [m length size count])"
  RE_EQ_INCL    = eq_pat "(true)",  "(call _ [m /include./] _)"
  RE_EQ_LHS_STR = eq_pat "(str _)", "_"
  RE_EQ_NIL     = eq_pat "nil",     "_"
  RE_EQ_OPER    = eq_pat "(true)",  "(call _ _ _)"
  RE_EQ_PRED    = eq_pat "(true)",  "(call _ _)"
  RE_EQ_RHS_LIT = eq_pat "_",       "(lit _)"
  RE_EQ_RHS_NTF = eq_pat "_",       "([atom])"
  RE_EQ_RHS_STR = eq_pat "_",       "(str _)"

  RE_INCL       = assert_pat "(call _ [m /include./] _)"
  RE_NOT        = s{ s(:call, nil, :assert, s(:call, _, :"!")) }
  RE_OPER       = assert_pat "(call _ _ _)"
  RE_PRED       = assert_pat "(call _ _)"
  RE_EQUAL      = s{ s(:call, nil, :assert, s(:call, _, :==, _), ___) }
  RE_NEQUAL     = s{ s(:call, nil, :assert, s(:call, _, :!=, _), ___) }

  RE_REF_INCL   = refute_pat "(call _ [m /include./] _)"
  RE_REF_NOT    = s{ s(:call, nil, :refute, s(:call, _, :"!")) }
  RE_REF_OPER   = refute_pat "(call _ _ _)"
  RE_REF_PRED   = refute_pat "(call _ _)"

  # assert(obj.size > 0) => refute_empty
  # assert obj.is_a? klass
  # lhs msg is count/length/size && rhs != 0 => refute_empty
  # lhs == binary call => refute_operator && rhs == false
  # assert_raises Exception do ... end
  # assert_equal "str", klass.name

  ############################################################

  def self.run args = ARGV
    expander = PathExpander.new args, "**/*.{rb,rake}"
    files = expander.process

    scanner = new
    scanner.scan(*files)

    puts scanner.count
  end

  attr_accessor :io, :rr, :count

  def initialize
    super

    self.io            = {}
    self.count         = 0
    self.require_empty = false
    self.rr            = Ruby2Ruby.new
    self.strict        = false
  end

  def scan(*files)
    files.each do |file|
      next unless file == '-' or File.readable? file

      ruby = file == '-' ? $stdin.read : File.binread(file)

      ast = RubyParser.new.process(ruby, file) rescue nil

      next unless ast

      process ast
    end
  end

  def process_call exp
    _, recv, msg, *args = exp

    case msg
    when /^(assert|refute|must|wont)/ then
      io["%s:%s:" % [exp.file, exp.line]] = nil
      io["  %s # %s" % [rr.process(exp), "original"]] = nil

      exp = process_assert exp
      output_all
      return exp
    end

    process recv

    args.each do |arg|
      process arg
    end

    exp
  end

  def process_assert exp
    _, _, msg, * = exp

    case msg
    when :assert_equal then
      analyze_assert_equal exp
    when :assert then
      analyze_assert exp
    when :refute then
      analyze_refute exp
    # when :must_equal then
    # when :must_include then
    # when :assert_includes then
    # when :assert_raises then
    # when :must_be_nil then
    # when :assert_kind_of then
    # when :must_match then
    # when :assert_nil then
    # when :must_be_kind_of then
    # when :must_raise then
    # when :assert_empty then
    # when :must_be_empty then
    # when :wont_include then
    # when :wont_be_nil then
    # when :refute_includes then
    # when :refute_nil then
    # when :assert_match then
    # when :must_be then
    # when :assert_cmp then
    # when :refute_cmp then
    # when :wont_match then
    # when :assert_respond_to then
    # when :assert_operator then
    # when :wont_equal then
    # when :must_respond_to then
    # when :must_output then
    # when :wont_be then
    # when :refute_empty then
    # when :refute_operator then
    # when :wont_be_empty then
    # when :must_throw then
    # when :refute_match then
    # when :assert_same then

    when /^(assert|refute|must|wont)/ then
      # w "unknown %s: %p" % [$1, msg]
    end

    exp
  end

  ############################################################
  # Analyzers

  def analyze_assert exp
    exp = handle_arity exp, 3

    loop do
      t, r, _, test, * = exp

      case exp
      when RE_NOT then
        _, recv, _ = test

        exp = s(t, r, :refute, recv)

        change exp, "refute not_cond"
        return analyze_refute exp
      when RE_EQUAL then
        _, lhs, _, rhs = test

        exp = s(t, r, :assert_equal, lhs, rhs)

        change exp, "assert_equal exp, act"
        return analyze_assert_equal exp
      when RE_NEQUAL then
        _, lhs, _, rhs = test

        exp = s(t, r, :refute_equal, lhs, rhs)

        change exp, "refute_equal exp, act"
        return analyze_refute_equal exp
      when RE_INCL then
        _, recv, msg, *rest = test
        exp = s(t, r, :assert_includes, recv, *rest)

        change exp, "assert_includes obj, val"
        break
      when RE_PRED then
        _, recv, msg = test
        exp = s(t, r, :assert_predicate, recv, s(:lit, msg))

        change exp, "assert_predicate obj, msg"
        break
      when RE_OPER then
        _, recv, msg, *rest = test
        exp = s(t, r, :assert_operator, recv, s(:lit, msg), *rest)

        change exp, "assert_operator obj, msg, arg"
        break
      else
        warn "You should probably NEVER use plain assert: %p" % [exp]
        break
      end
    end
  end

  def analyze_assert_equal exp
    exp = handle_arity exp, 4

    loop do
      t, r, m, lhs, rhs, * = exp

      case exp
      when RE_EQ_NIL then
        exp = s(t, r, :assert_nil, rhs)

        change exp, "assert_nil"
        break
      when RE_EQ_INCL then
        _, recv, msg, *rest = rhs
        exp = s(t, r, :assert_includes, recv, s(:lit, msg), *rest)

        change exp, "assert_includes obj, val"
        break
      when RE_EQ_PRED then
        _, recv, msg = rhs
        exp = s(t, r, :assert_predicate, recv, s(:lit, msg))

        change exp, "assert_predicate obj, msg"
        break
      when RE_EQ_OPER then
        _, recv, msg, *rest = rhs
        exp = s(t, r, :assert_operator, recv, s(:lit, msg), *rest)

        change exp, "assert_operator obj, msg, arg"
        break
      when RE_EQ_LHS_STR then
        _, str = lhs
        if str.length > 20 then
          lhs = s(:str, str[0, 20])
          exp = s(t, r, :assert_includes, lhs, rhs)
          change exp, "assert_includes substr, actual (or fixture)"
        end
        break
      when RE_EQ_RHS_LIT, RE_EQ_RHS_STR, RE_EQ_RHS_NTF then
        lhs, rhs = rhs, lhs
        exp = s(t, r, m, lhs, rhs)

        change exp, "assert_equal exp, act"
      when RE_EQ_EMPTY then
        rhs = rhs[1] # recv to remove .count
        exp = s(t, r, :assert_empty, rhs)

        change exp, "assert_empty"
        break
      else
        break
      end
    end
  end

  ############################################################
  # Negatives

  def analyze_refute exp
    exp = handle_arity exp, 3

    loop do
      t, r, _, test, * = exp

      case exp
      when RE_REF_NOT then
        _, recv, _ = test

        exp = s(t, r, :assert, recv)

        change exp, "refute not_cond"
        return process_assert exp
      when RE_REF_INCL then
        _, recv, msg, *rest = test
        exp = s(t, r, :refute_includes, recv, *rest)

        change exp, "refute_includes obj, val"
        break
      when RE_REF_PRED then
        _, recv, msg = test
        exp = s(t, r, :refute_predicate, recv, s(:lit, msg))

        change exp, "refute_predicate obj, msg"
        break
      when RE_REF_OPER then
        _, recv, msg, *rest = test
        exp = s(t, r, :refute_operator, recv, s(:lit, msg), *rest)

        change exp, "refute_operator obj, msg, arg"
        break
      else
        warn "You should probably NEVER use plain refute: %p" % [exp]
        break
      end
    end
  end

  ############################################################
  # Utilities:

  def handle_arity exp, arity
    msg = exp[arity+1]
    exp = exp[0..arity]

    change exp, "redundant message?" if msg if $v

    exp
  end

  ############################################################$
  # Output:

  def change exp, msg
    raise ArgumentError, "key already exists! %p in %p" % [exp, io] if io.key?(exp)
    io[exp] = msg
  end

  def out
    out = io.map { |(sexp, msg)|
      case sexp
      when Sexp then
        ruby = rr.process(sexp)
        [ruby.length, ruby, msg]
      else
        [0, sexp, nil]
      end
    }

    max = out.map(&:first).max
    fmt = "  %-#{max}s # %s"

    out.map { |(_, ruby, msg)|
      if msg then
        fmt % [ruby, msg]
      else
        ruby
      end
    }
  end

  def output_all
    if io.size > 2 then
      puts out
      puts
    end

    io.clear
  end
end
