require "ruby_parser"
require "sexp_processor"
require "path_expander"
require "ruby2ruby"
require "minitest_lint"

$v ||= false
$d ||= false

class MinitestLint::AssertScanner < SexpProcessor
  VERSION = "1.0.0"

  ######################################################################
  # Runners

  def self.run args = ARGV
    # TODO: real option processing
    return graph    if args.include? "--graph"
    return list     if args.include? "--list"
    return raw_list if args.include? "--raw"

    scan args
  end

  def self.scan args
    expander = PathExpander.new args, "**/*.{rb,rake}"
    files = expander.process

    scanner = new
    scanner.scan(*files)

    puts scanner.count
  end

  def self.graph io = $stdout
    new.graph io
  end

  def self.list
    new.list
  end

  def self.raw_list
    new.raw_list
  end

  ######################################################################
  # Main Program:

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
      warn "# #{file}" if $v

      ruby = file == '-' ? $stdin.read : File.binread(file)

      ast = RubyParser.new.process(ruby, file) rescue nil

      next unless ast

      process ast
    end
  end

  def graph io = $stdout
    require "graph"

    g = Graph.new
    g.rotate
    g.boxes

    sg = {}
    sg.default = g

    ORDER.each do |name|
      c = g.cluster(name)
      c.label(name)
      sg[name] = c
    end

    doco = self.class.__doco

    doco.to_a.flatten.each do |name|
      where = name[/#{RE}|STOP|WARNING/]
      node = sg[where].node name

      case where
      when "assert", "must" then
        g.darkgreen << node
      when "refute", "wont" then
        g.red       << node
      when "STOP" then
        g.filled             << node
        g.darkgreen          << node
        g.fontcolor("white") << node
      when "WARNING" then
        g.filled        << node
        g.darkgoldenrod << node
      end
    end

    doco.each do |from, to|
      g[from][to]
    end

    io.puts g
  end

  def list pages = ENV["PAGES"]
    doco = self.class.__doco

    sep = pages ? "\cL" : "\n"

    puts doco
      .group_by { |a,b| ORDER.index a[RE] }
      .sort
      .map { |_, items|
        items.map { |from, to| "%-40s => %s\n" % [from, to] }.join
      }.join sep
  end

  def raw_list
    doco = self.class.__doco

    doco.each do |from, to|
      puts "%-40s => %s" % [from, to]
    end
  end

  ######################################################################
  # List Methods:

  def process_call exp
    _, recv, msg, *args = exp

    case msg
    when /^(?:#{RE})/ then
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

  def analyze_assert exp
    begin
      found = false

      d
      d { { :EXP => exp } }
      d { { :EXP => rr.process(exp) } }

      SCANNERS.each do |pat, blk|
        d { { :PAT => pat } } if $v

        if pat === exp then
          new_exp = self.instance_exec(exp, &blk)
          found = !!new_exp
          if found then
            exp = new_exp
            d { { :NEW => rr.process(exp) } }
          end
          break
        end
      end
    end while found

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
    elsif $v
      puts "question this:", out
      puts
    end

    io.clear
  end

  def d
    return unless $d
    o = yield if block_given?
    if o then
      p o
    else
      puts
    end
  end
end

require_relative "assert_scanner/scanners.rb"

# x.must_equal true
# x.must_y z
# _(x).must_y z
# _(x.msg y).must_equal true
# _(x)..msg y

# RE_WTF = parse "(call (call _ [not? [m /^_$/]] _) must_equal (true))"
# register_assert RE_WTF do |_, (t, lhs, msg, rhs), _, _|
#   exp = s(t, lhs, :must_be, s(:lit, msg), rhs)
#
#   change exp, "lhs.must_be pred rhs"
# end
#
# RE_WTF2 = parse "(call (call _ :_ (call _ _ _)) must_equal (true))"
# register_assert RE_WTF2 do |_, (_, _, _, (t, lhs, msg, rhs)), _, _|
#   exp = s(t, lhs, :must_be, s(:lit, msg), rhs)
#
#   change exp, "lhs.must_be pred rhs"
# end
#
# RE_MUST_BE_KIND_OF = parse "(call _ must_be (lit is_a?) _)"
# register_assert RE_MUST_BE_KIND_OF do |t, lhs, msg, msg2, rhs|
#   exp = s(t, lhs, :must_be_kind_of, rhs)
#
#   change exp, "lhs.must_be_kind_of rhs"
# end

# infect_an_assertion :assert_empty,       :must_be_empty,   :unary
# infect_an_assertion :assert_equal,       :must_equal
# infect_an_assertion :assert_in_delta,    :must_be_close_to
# infect_an_assertion :assert_in_epsilon,  :must_be_within_epsilon
# infect_an_assertion :assert_includes,    :must_include,    :reverse
# infect_an_assertion :assert_instance_of, :must_be_instance_of
# infect_an_assertion :assert_kind_of,     :must_be_kind_of
# infect_an_assertion :assert_match,       :must_match
# infect_an_assertion :assert_nil,         :must_be_nil,     :unary
# infect_an_assertion :assert_operator,    :must_be,         :reverse
# infect_an_assertion :assert_output,      :must_output,     :block
# infect_an_assertion :assert_raises,      :must_raise,      :block
# infect_an_assertion :assert_respond_to,  :must_respond_to, :reverse
# infect_an_assertion :assert_same,        :must_be_same_as
# infect_an_assertion :assert_silent,      :must_be_silent,  :block
# infect_an_assertion :assert_throws,      :must_throw,      :block
#
# infect_an_assertion :refute_empty,       :wont_be_empty,   :unary
# infect_an_assertion :refute_equal,       :wont_equal
# infect_an_assertion :refute_in_delta,    :wont_be_close_to
# infect_an_assertion :refute_in_epsilon,  :wont_be_within_epsilon
# infect_an_assertion :refute_includes,    :wont_include,    :reverse
# infect_an_assertion :refute_instance_of, :wont_be_instance_of
# infect_an_assertion :refute_kind_of,     :wont_be_kind_of
# infect_an_assertion :refute_match,       :wont_match
# infect_an_assertion :refute_nil,         :wont_be_nil,     :unary
# infect_an_assertion :refute_operator,    :wont_be,         :reverse
# infect_an_assertion :refute_respond_to,  :wont_respond_to, :reverse
# infect_an_assertion :refute_same,        :wont_be_same_as

# RE_MUST_EQ = parse "(call (call nil :_ _) must_equal _)"
# register_assert RE_MUST_PLAIN do |t, (_, _, m2, lhs), msg, rhs|
#   exp = s(t, s(:call, nil, m2, lhs), msg, rhs)
#
#   change exp, "_(act).#{msg} exp"
# end

# RE_MUST_INCL_WRAPPED2 = parse "(call (call nil [m value expect] _) must_include _)"
# register_assert RE_MUST_INCL_WRAPPED2 do |t, (_, _, _, lhs), msg, rhs|
#   exp = s(t, s(:call, nil, :_, lhs), msg, rhs)
#
#   change exp, "_(lhs).#{msg} rhs"
# end
#
# RE_MUST_INCL_WRAPPED = parse "(call (call nil :_ _) must_include _)"
# register_assert RE_MUST_INCL_WRAPPED do |t, lhs, msg, rhs|
#   # nothing to do
# end
