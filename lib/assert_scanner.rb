$:.unshift File.expand_path "~/Links/SP/lib"
$:.unshift File.expand_path "~/Links/G/lib"

require "ruby_parser"
require "sexp_processor"
require "path_expander"
require "ruby2ruby"

$v ||= false
$d ||= false

class AssertScanner < SexpProcessor
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

  def list
    doco = self.class.__doco

    doco
      .group_by { |a,b| ORDER.index a[RE] }
      .sort
      .each do |_, items|
        items.each do |from, to|
          puts "%-40s => %s" % [from, to]
        end
        puts
      end
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

require "assert_scanner/scanners.rb"
