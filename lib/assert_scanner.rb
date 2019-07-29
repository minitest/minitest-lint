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
    pat "(call nil assert #{test})"
  end

  def self.refute_pat test
    pat "(call nil refute #{test})"
  end

  def self.eq_pat lhs, rhs
    pat "(call nil assert_equal #{lhs} #{rhs})"
  end

  RE_EQ_MSG     = pat "(call nil assert_equal _ _ _)"
  RE_EQ_EMPTY   = eq_pat "(lit 0)", "(call _ [m length size count])"
  RE_EQ_INCL    = eq_pat "(true)",  "(call _ [m /include./] _)"
  RE_EQ_LHS_STR = eq_pat "(str _)", "_"
  RE_EQ_NIL     = eq_pat "nil",     "_"
  RE_EQ_OPER    = eq_pat "(true)",  "(call _ _ _)"
  RE_EQ_PRED    = eq_pat "(true)",  "(call _ _)"
  RE_EQ_RHS_LIT = eq_pat "_",       "(lit _)"
  RE_EQ_RHS_NTF = eq_pat "_",       "([atom])"
  RE_EQ_RHS_STR = eq_pat "_",       "(str _)"

  RE_MSG        = pat "(call nil assert _ _)"
  RE_INCL       = assert_pat "(call _ [m /include./] _)"
  RE_NOT        = s{ s(:call, nil, :assert, s(:call, _, :"!")) }
  RE_OPER       = assert_pat "(call _ _ _)"
  RE_PRED       = assert_pat "(call _ _)"
  RE_EQUAL      = s{ s(:call, nil, :assert, s(:call, _, :==, _), ___) }
  RE_NEQUAL     = s{ s(:call, nil, :assert, s(:call, _, :!=, _), ___) }
  RE_PLAIN      = assert_pat "_"

  RE_REF_MSG    = pat "(call nil refute _ _)"
  RE_REF_INCL   = refute_pat "(call _ [m /include./] _)"
  RE_REF_NOT    = s{ s(:call, nil, :refute, s(:call, _, :"!")) }
  RE_REF_OPER   = refute_pat "(call _ _ _)"
  RE_REF_PRED   = refute_pat "(call _ _)"

  RE_OP_INCL = pat "(call nil assert_operator _ (lit [m /include./]) _)"

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

  @assertions = {}

  def self.assertions
    @assertions
  end

  def self.reset_assertions
    @assertions.clear
  end

  def self.register_assert *matchers, &handler
    matchers.each do |matcher|
      assertions[matcher] = handler
    end
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

      exp = analyze_assert exp
      output_all
      return exp
    end

    process recv

    args.each do |arg|
      process arg
    end

    exp
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
    exp
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

  ############################################################
  # Analyzer

  def analyze_assert exp
    begin
      found = false

      self.class.assertions.each do |pat, blk|
        if pat === exp then
          found = true
          exp = self.instance_exec(exp, &blk)
          break # TODO: infinite loop on non-changing blocks
        end
      end
    end while found

    exp
  end

  ############################################################
  # Analyzer Blocks

  register_assert RE_MSG do |exp|
    handle_arity exp, 3
  end

  register_assert RE_NOT do |t, r, _, test|
    _, recv, _ = test

    exp = s(t, r, :refute, recv)

    change exp, "refute not_cond"
  end

  register_assert RE_EQUAL do |t, r, _, test|
    _, lhs, _, rhs = test

    exp = s(t, r, :assert_equal, lhs, rhs)

    change exp, "assert_equal exp, act"
  end

  register_assert RE_NEQUAL do |t, r, _, test|
    _, lhs, _, rhs = test

    exp = s(t, r, :refute_equal, lhs, rhs)

    change exp, "refute_equal exp, act"
  end

  register_assert RE_INCL do |t, r, _, test|
    _, recv, _, *rest = test
    exp = s(t, r, :assert_includes, recv, *rest)

    change exp, "assert_includes obj, val"
  end

  register_assert RE_PRED do |t, r, _, test|
    _, recv, msg = test
    exp = s(t, r, :assert_predicate, recv, s(:lit, msg))

    change exp, "assert_predicate obj, msg"
  end

  register_assert RE_OPER do |t, r, _, test|
    _, recv, msg, *rest = test
    exp = s(t, r, :assert_operator, recv, s(:lit, msg), *rest)

    change exp, "assert_operator obj, msg, arg"
  end

  register_assert RE_PLAIN do |exp|
    warn "You should probably NEVER use plain assert: %p" % [exp]
  end

  register_assert RE_EQ_MSG do |exp|
    handle_arity exp, 4
  end

  register_assert RE_EQ_NIL do |t, r, m, lhs, rhs|
    exp = s(t, r, :assert_nil, rhs)

    change exp, "assert_nil"
  end

  register_assert RE_EQ_INCL do |t, r, m, lhs, rhs|
    _, recv, _, obj = rhs

    exp = s(t, r, :assert_includes, recv, obj)

    change exp, "assert_includes enum, val"
  end

  register_assert RE_EQ_PRED do |t, r, m, lhs, rhs|
    _, recv, msg = rhs
    exp = s(t, r, :assert_predicate, recv, s(:lit, msg))

    change exp, "assert_predicate obj, msg"
  end

  register_assert RE_EQ_OPER do |t, r, m, lhs, rhs|
    _, recv, msg, *rest = rhs
    exp = s(t, r, :assert_operator, recv, s(:lit, msg), *rest)

    change exp, "assert_operator obj, msg, arg"
  end

  register_assert RE_EQ_LHS_STR do |exp|
    t, r, _, lhs, rhs, * = exp
    _, str = lhs

    if str.length > 20 then
      lhs = s(:str, str[0, 20])
      exp = s(t, r, :assert_includes, lhs, rhs)
      change exp, "assert_includes substr, actual (or fixture)"
    else
      exp
    end
  end

  register_assert RE_EQ_RHS_LIT, RE_EQ_RHS_STR, RE_EQ_RHS_NTF do |t, r, m, lhs, rhs|
    lhs, rhs = rhs, lhs
    exp = s(t, r, m, lhs, rhs)

    change exp, "assert_equal exp, act"
  end

  register_assert RE_EQ_EMPTY do |t, r, m, lhs, rhs|
    rhs = rhs[1] # recv to remove .count
    exp = s(t, r, :assert_empty, rhs)

    change exp, "assert_empty"
  end

  register_assert RE_REF_MSG do |exp|
    handle_arity exp, 3
  end

  register_assert RE_REF_NOT do |t, r, _, test|
    _, recv, _ = test

    exp = s(t, r, :assert, recv)

    change exp, "refute not_cond"
  end

  register_assert RE_REF_INCL do |t, r, _, test|
    _, recv, _, *rest = test
    exp = s(t, r, :refute_includes, recv, *rest)

    change exp, "refute_includes obj, val"
  end

  register_assert RE_REF_PRED do |t, r, _, test|
    _, recv, msg = test
    exp = s(t, r, :refute_predicate, recv, s(:lit, msg))

    change exp, "refute_predicate obj, msg"
  end

  register_assert RE_REF_OPER do |t, r, _, test|
    _, recv, msg, *rest = test
    exp = s(t, r, :refute_operator, recv, s(:lit, msg), *rest)

    change exp, "refute_operator obj, msg, arg"
  end

  register_assert RE_OP_INCL do |t, r, _, obj, _, val|
    exp = s(t, r, :assert_includes, obj, val)

    change exp, "assert_includes enum, val"
  end
end
