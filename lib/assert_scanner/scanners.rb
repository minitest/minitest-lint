class AssertScanner
  # TODO: DECIDE! do the pattern names match the RHS or LHS?? I think LHS

  ORDER = %w{assert refute must wont}
  RE = Regexp.union(*ORDER) # TODO: rename?

  SCANNERS = {}

  ######################################################################
  # Doco Declarations:

  mc = (class << self; self; end)
  mc.attr_accessor :latest
  mc.attr_accessor :__doco

  self.latest = nil
  self.__doco = {}

  def self.doco from_to
    raise ArgumentError, "Already defined: #{from_to}" if
      from_to.keys.any? { |k| __doco.key? k }

    self.latest = from_to
    __doco.merge! from_to
  end

  def self.meta from_to
    self.latest = from_to
  end

  def self.latest_doco_to
    latest.values.first
  end

  ######################################################################
  # Pattern Declarations:

  def self.reset_scanners
    SCANNERS.clear
  end

  def self.register_assert *matchers, &handler
    raise "NO! %p" % [matchers] unless latest

    puts "BAD: %p" % [latest] if latest.size < matchers.size

    # TODO: register doco against the matcher so they can be looked up
    matchers.each do |matcher|
      if SCANNERS.key? matcher then
        warn "WARNING! Reassigning matcher! %p" % [matcher]
        warn "old: %s" % [SCANNERS[matcher].source_location]
        warn "new: %s" % [handler.source_location]
      end

      SCANNERS[matcher] = handler
    end

    self.latest = nil
  end

  def self.pattern patterns
    patterns.each do |name, value|
      const_set name, value
    end
  end

  def self.rewrite patterns, msg = latest_doco_to, &block
    self.pattern patterns

    register_assert(*patterns.values) do |exp|
      exp = self.instance_exec(exp, &block)
      change exp, msg if exp
    end
  end

  meta "old_assert(*args)" => "new_assert(*args)"
  def self.rename new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, *args|
      s(t, r, new_msg, *args)
    end
  end

  meta "old_assert obj.msg(*args)" => "new_assert obj, *args"
  def self.replace_call new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, (_, lhs, _, *rest)|
      s(t, r, new_msg, lhs, *rest)
    end
  end

  meta "old_assert [exp,] obj.msg(*args)" => "new_assert obj, :msg, *args"
  def self.unpack new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, *_, (_, recv, m, *rest)|
      s(t, r, new_msg, recv, s(:lit, m), *rest)
    end
  end

  meta "old_assert lhs, :msg, rhs" => "new_assert, lhs, rhs"
  def self.promote_oper new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, lhs, _msg, *rest|
      s(t, r, new_msg, lhs, *rest)
    end
  end

  meta "old_assert lhs, :msg, rhs" => "new_assert, rhs, lhs"
  def self.promote_oper_swap new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, lhs, _msg, *rest|
      s(t, r, new_msg, *rest, lhs)
    end
  end

  meta "old_assert lhs, :msg" => "new_assert, lhs"
  def self.promote_pred new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, lhs, _msg|
      s(t, r, new_msg, lhs)
    end
  end

  meta "old_assert _expected, obj" => "new_assert obj"
  def self.rename_and_drop new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, _lhs, rhs|
      s(t, r, new_msg, rhs)
    end
  end

  meta "assert lhs, rhs" => "assert rhs, lhs"
  def self.swap patterns, msg = latest_doco_to
    rewrite patterns do |t, r, m, lhs, rhs|
      s(t, r, m, rhs, lhs)
    end
  end

  ############################################################
  # Pattern Declaration Helpers:

  def self.parse str
    Sexp::Matcher.parse str
  end

  def self.pat *args
    parse "(call nil #{args.join " "})"
  end

  def self.assert_pat test
    pat :assert, test
  end

  def self.refute_pat test
    pat :refute, test
  end

  def self.eq_pat lhs, rhs
    pat :assert_equal, lhs, rhs
  end

  def self.pred l, m
    pat :assert_predicate, l, lit(m)
  end

  def self.a_oper l, m, r
    pat :assert_operator, l, "(lit #{m})", r
  end

  def self.r_eq_pat lhs, rhs
    pat :refute_equal, lhs, rhs
  end

  def self.r_oper l, m, r
    pat :refute_operator, l, "(lit #{m})", r
  end

  def self.r_pred l, m
    pat :refute_predicate, l, "(lit #{m})"
  end

  def self.lit x
    "(lit #{x})"
  end

  ############################################################
  # Pattern Helpers:

  def change exp, msg
    raise ArgumentError, "key already exists! %p in %p" % [exp, io] if io.key?(exp)
    io[exp] = msg
    self.count += 1
    exp
  end

  def handle_arity exp, arity
    exp, msg = exp[0..arity], exp[arity+1]

    change exp, "redundant message?" if msg if $v

    exp
  end

  ############################################################
  # Positive Assertions

  # TODO:
  # assert_equal "str", klass.name

  size_pat = "(call _ [m count length size])"

  # This must be first, to remove the redundancies right off
  doco "assert obj, msg" => "assert obj"
  pattern RE_MSG: assert_pat("_ _")
  register_assert RE_MSG do |exp|
    handle_arity exp, 3
  end

  doco "assert_equal exp, act, msg" => "assert_equal exp, act"
  pattern RE_EQ_MSG: pat(:assert_equal, "_ _ _")
  register_assert RE_EQ_MSG do |exp|
    handle_arity exp, 4
  end

  # This must be second, to flip to refute as soon as possible
  doco "assert ! obj" => "refute obj"
  replace_call(:refute,
               RE_NOT: assert_pat("(call _ !)"))

  doco "assert obj.pred?" => "assert_predicate obj, :pred?"
  unpack(:assert_predicate,
         RE_PRED: assert_pat("(call _ _)"))

  doco "assert obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack(:assert_operator,
         RE_OPER: assert_pat("(call _ _ _)"))

  doco "assert_equal true, obj.pred?" => "assert_predicate obj, :pred?"
  unpack(:assert_predicate,
         RE_EQ_PRED: eq_pat("(true)",  "(call _ _)"))

  doco "assert_equal true, obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack(:assert_operator,
         RE_EQ_OPER: eq_pat("(true)",  "(call _ _ _)"))

  doco "assert_equal false, obj.pred?" => "refute_predicate obj, :pred?"
  unpack(:refute_predicate,
         RE_NEQ_PRED: eq_pat("(false)",  "(call _ _)"))

  doco "assert_equal false, obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack(:refute_operator,
         RE_NEQ_OPER: eq_pat("(false)",  "(call _ _ _)"))

  NOT_LAS = "[- [any (lit _) (str _) ([atom])]]" # LAS = lit, atom, str

  doco("assert_equal act, lit"  => "assert_equal lit, act",
       "assert_equal act, str"  => "assert_equal str, act",
       "assert_equal act, atom" => "assert_equal atom, act")
  swap(RE_EQ_RHS_LIT: eq_pat(NOT_LAS, "(lit _)"),
       RE_EQ_RHS_STR: eq_pat(NOT_LAS, "(str _)"),
       RE_EQ_RHS_NTF: eq_pat(NOT_LAS, "([atom])"))

  doco "assert_equal nil, obj" => "assert_nil obj"
  rename_and_drop(:assert_nil,
                 RE_EQ_NIL: eq_pat("(:nil)", "_"))

  doco("assert_equal 0, obj.count"  => "assert_empty obj",
       "assert_equal 0, obj.length" => "assert_empty obj",
       "assert_equal 0, obj.size"   => "assert_empty obj")
  rewrite(RE_EQ_EMPTY: eq_pat(lit(0), size_pat)) do |t, r, _, _, (_, recv, _)|
    s(t, r, :assert_empty, recv)
  end

  doco("assert_equal [], obj" => "assert_empty obj",
       "assert_equal {}, obj" => "assert_empty obj")
  rename_and_drop(:assert_empty,
                  RE_EQ_EMPTY_LIT: eq_pat("([m array hash])", "_"))

  doco("assert_equal str, obj.class.name"  => "assert_instance_of cls, obj")
  rewrite(RE_EQ_CLASS_NAME: eq_pat("(str _)", "(call (call _ class) name)")) do |t, r, _, (_, lhs_str), (_, (_, rhs, _), _)|
    lhs = lhs_str.split(/::/).reduce(nil) { |m, n|
      n = n.to_sym
      m ? s(:colon2, m, n) : s(:const, n)
    }

    s(t, r, :assert_instance_of, lhs, rhs)
  end

  doco "assert_equal 'long str', str" => "assert_includes str, 'substr'"
  rewrite(RE_EQ_LHS_STR: eq_pat("(str _)", "_")) do |t, r, _, (_, str), rhs, *|
    next unless str && str.length > 20

    s(t, r, :assert_includes, rhs, s(:str, str[0, 20]))
  end

  doco("assert_equal float_lit, act"    => "assert_in_epsilon float_lit, act",
       "assert_in_delta float_lit, act" => "assert_in_epsilon float_lit, act")
  rename(:assert_in_epsilon,
         RE_EQ_FLOAT: pat(:assert_equal,    "(lit, [k Float])", "_"),
         RE_IN_DELTA: pat(:assert_in_delta, "_",                "_"))

  doco "assert_operator obj, :==, val" => "assert_equal exp, act"
  promote_oper(:assert_equal,
               RE_OPER_MATCH_EQ: a_oper("_", :==, "_"))

  doco "assert_operator obj, :!=, val" => "refute_equal exp, act"
  promote_oper(:refute_equal,
               RE_OPER_MATCH_NEQ: a_oper("_", :!=, "_"))

  doco("assert_operator obj, :===, val"    => "assert_match obj, val",
       "assert_operator obj, :=~, val"     => "assert_match obj, val",
       "assert_operator obj, :match, val"  => "assert_match obj, val",
       "assert_operator obj, :match?, val" => "assert_match obj, val")
  promote_oper(:assert_match,
               RE_OPER_MATCH_EQ3:       a_oper("_", :===, "_"),
               RE_OPER_MATCH_EQTILDE:   a_oper("_", :=~, "_"),
               RE_OPER_MATCH_MATCH:     a_oper("_", :match, "_"),
               RE_OPER_MATCH_MATCH_EH:  a_oper("_", :match?, "_"))

  doco "assert_operator obj, :!~, val"     => "refute_match obj, val"
  promote_oper(:refute_match,
               RE_OPER_MATCH_NOT_TILDE: a_oper("_", :!~, "_"))

  doco("assert_operator obj, :include?, val" => "assert_includes obj, val",
       "assert_operator obj, :key?, val"     => "assert_includes obj, val")
  promote_oper(:assert_includes,
               RE_OPER_INCLUDE: a_oper("_", :include?, "_"),
               RE_OPER_KEY:     a_oper("_", :key?, "_"))

  doco "assert_operator obj, :instance_of?, cls" => "assert_instance_of cls, obj"
  promote_oper_swap(:assert_instance_of,
                    RE_OPER_INSTANCE_OF: a_oper("_", :instance_of?, "_"))

  doco("assert_operator obj, :kind_of?, mod" => "assert_kind_of mod, obj",
       "assert_operator obj, :is_a?, mod"    => "assert_kind_of mod, obj")
  promote_oper_swap(:assert_kind_of,
                    RE_OPER_KIND_OF: a_oper("_", :kind_of?, "_"),
                    RE_OPER_IS_A:    a_oper("_", :is_a?, "_"))

  doco "assert_operator obj, :respond_to?, val" => "assert_respond_to obj, val"
  promote_oper(:assert_respond_to,
               RE_OPER_RESPOND_TO: a_oper("_", :respond_to?, "_"))

  doco "assert_operator obj, :equal?, val" => "assert_same obj, val"
  promote_oper(:assert_same,
               RE_OPER_SAME: a_oper("_", :equal?, "_"))

  doco "assert_operator File, :exist?, val" => "assert_path_exists val"
  rewrite(RE_OPER_FILE_EXIST: a_oper("(const :File)", :exist?, "_")) do |t, r, _, _, _, rhs|
    s(t, r, :assert_path_exists, rhs)
  end

  doco "assert_predicate obj, :empty?" => "assert_empty obj"
  promote_pred(:assert_empty,
               RE_PRED_EMPTY: pred("_", :empty?))

  doco "assert_predicate obj, :nil?" => "assert_nil obj"
  promote_pred(:assert_nil,
               RE_PRED_NIL: pred("_", :nil?))

  doco "assert obj" => "WARNING"
  RE_PLAIN = assert_pat "_"
  register_assert RE_PLAIN do |exp|
    io[exp] = "Try to not use plain assert"
    nil
  end

  ############################################################
  # Negative Assertions

  # TODO:
  # refute_nil
  # assert(obj.size > 0)                     => refute_empty
  # lhs msg is count/length/size && rhs != 0 => refute_empty
  # lhs == binary call && rhs == false       => refute_operator

  doco "refute obj, msg" => "refute obj"
  pattern RE_REF_MSG: refute_pat("_ _")
  register_assert RE_REF_MSG do |exp|
    handle_arity exp, 3
  end

  doco "refute_equal exp, act, msg" => "refute_equal exp, act"
  pattern RE_REF_EQ_MSG: pat(:refute_equal, "_ _ _")
  register_assert RE_REF_EQ_MSG do |exp|
    handle_arity exp, 4
  end

  doco "refute ! obj" => "assert obj"
  replace_call(:assert,
               RE_REF_NOT: refute_pat("(call _ !)"))

  doco "refute obj.pred?" => "refute_predicate obj, :pred?"
  unpack(:refute_predicate,
         RE_REF_PRED: refute_pat("(call _ _)"))

  doco "refute obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack(:refute_operator,
         RE_REF_OPER: refute_pat("(call _ _ _)"))

  doco "refute_equal true, obj.pred?" => "refute_predicate obj, :pred?"
  unpack(:refute_predicate,
         RE_REF_EQ_PRED: r_eq_pat("(true)",  "(call _ _)"))

  doco "refute_equal true, obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack(:refute_operator,
         RE_REF_EQ_OPER: r_eq_pat("(true)",  "(call _ _ _)"))

  doco "refute_equal false, obj.pred?" => "assert_predicate obj, :pred?"
  unpack(:assert_predicate,
         RE_REF_NEQ_PRED: r_eq_pat("(false)",  "(call _ _)"))

  doco "refute_equal false, obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack(:assert_operator,
         RE_REF_NEQ_OPER: r_eq_pat("(false)",  "(call _ _ _)"))

  doco("refute_equal act, lit"  => "refute_equal lit, act",
       "refute_equal act, str"  => "refute_equal str, act",
       "refute_equal act, atom" => "refute_equal atom, act")
  swap(RE_REF_EQ_RHS_LIT: r_eq_pat(NOT_LAS, "(lit _)"),
       RE_REF_EQ_RHS_STR: r_eq_pat(NOT_LAS, "(str _)"),
       RE_REF_EQ_RHS_NTF: r_eq_pat(NOT_LAS, "([atom])"))

  doco "refute_equal nil, obj" => "refute_nil obj"
  rename_and_drop(:refute_nil,
                 RE_REF_EQ_NIL: r_eq_pat("(:nil)", "_"))

  doco("refute_equal 0, obj.count"  => "refute_empty obj",
       "refute_equal 0, obj.length" => "refute_empty obj",
       "refute_equal 0, obj.size"   => "refute_empty obj")
  rewrite(RE_REF_EQ_EMPTY: r_eq_pat(lit(0), size_pat)) do |t, r, _, _, (_, recv, _)|
    s(t, r, :refute_empty, recv)
  end

  doco("refute_equal [], obj" => "refute_empty obj",
       "refute_equal {}, obj" => "refute_empty obj")
  rename_and_drop(:refute_empty,
                  RE_REF_EQ_EMPTY_LIT: r_eq_pat("([m array hash])", "_"))

  doco "refute_equal 'long str', str" => "refute_includes str, 'substr'"
  rewrite(RE_REF_EQ_LHS_STR: r_eq_pat("(str _)", "_")) do |t, r, _, (_, str), rhs, *|
    next unless str && str.length > 20

    s(t, r, :refute_includes, rhs, s(:str, str[0, 20]))
  end

  doco("refute_equal float_lit, act"    => "refute_in_epsilon float_lit, act",
       "refute_in_delta float_lit, act" => "refute_in_epsilon float_lit, act")
  rename(:refute_in_epsilon,
         RE_REF_EQ_FLOAT: pat(:refute_equal,    "(lit, [k Float])", "_"),
         RE_REF_IN_DELTA: pat(:refute_in_delta, "_",                "_"))

  doco "refute_operator obj, :==, val" => "refute_equal exp, act"
  promote_oper(:refute_equal,
               RE_REF_OPER_MATCH_EQ: r_oper("_", :==, "_"))

  doco "refute_operator obj, :!=, val" => "assert_equal exp, act"
  promote_oper(:assert_equal,
               RE_REF_OPER_MATCH_NEQ: r_oper("_", :!=, "_"))

  doco("refute_operator obj, :===, val"    => "refute_match obj, val",
       "refute_operator obj, :=~, val"     => "refute_match obj, val",
       "refute_operator obj, :match, val"  => "refute_match obj, val",
       "refute_operator obj, :match?, val" => "refute_match obj, val")
  promote_oper(:refute_match,
               RE_REF_OPER_MATCH_EQ3:       r_oper("_", :===, "_"),
               RE_REF_OPER_MATCH_EQTILDE:   r_oper("_", :=~, "_"),
               RE_REF_OPER_MATCH_MATCH:     r_oper("_", :match, "_"),
               RE_REF_OPER_MATCH_MATCH_EH:  r_oper("_", :match?, "_"))

  doco "refute_operator obj, :!~, val"     => "assert_match obj, val"
  promote_oper(:assert_match,
               RE_REF_OPER_MATCH_NOT_TILDE: r_oper("_", :!~, "_"))

  doco("refute_operator obj, :include?, val" => "refute_includes obj, val",
       "refute_operator obj, :key?, val"     => "refute_includes obj, val")
  promote_oper(:refute_includes,
               RE_REF_OPER_INCLUDE: r_oper("_", :include?, "_"),
               RE_REF_OPER_KEY:     r_oper("_", :key?, "_"))

  doco "refute_operator obj, :instance_of?, cls" => "refute_instance_of cls, obj"
  promote_oper_swap(:refute_instance_of,
                    RE_REF_OPER_INSTANCE_OF: r_oper("_", :instance_of?, "_"))

  doco("refute_operator obj, :kind_of?, mod" => "refute_kind_of mod, obj",
       "refute_operator obj, :is_a?, mod"    => "refute_kind_of mod, obj")
  promote_oper_swap(:refute_kind_of,
                    RE_REF_OPER_KIND_OF: r_oper("_", :kind_of?, "_"),
                    RE_REF_OPER_IS_A:    r_oper("_", :is_a?, "_"))

  doco "refute_operator obj, :respond_to?, val" => "refute_respond_to obj, val"
  promote_oper(:refute_respond_to,
               RE_REF_OPER_RESPOND_TO: r_oper("_", :respond_to?, "_"))

  doco "refute_operator obj, :equal?, val" => "refute_same obj, val"
  promote_oper(:refute_same,
               RE_REF_OPER_SAME: r_oper("_", :equal?, "_"))

  doco "refute_operator File, :exist?, val" => "refute_path_exists val"
  rewrite(RE_REF_OPER_FILE_EXIST: r_oper("(const :File)", :exist?, "_")) do |t, r, _, _, _, rhs|
    s(t, r, :refute_path_exists, rhs)
  end

  doco "refute_predicate val, :empty?" => "refute_empty val"
  promote_pred(:refute_empty,
               RE_REF_PRED_EMPTY: r_pred("_", :empty?))

  doco "refute_predicate val, :nil?" => "refute_nil val"
  promote_pred(:refute_nil,
               RE_REF_PRED_NIL: r_pred("_", :nil?))

  doco "refute obj" => "WARNING"
  RE_REF_PLAIN = refute_pat "_"
  register_assert RE_REF_PLAIN do |exp|
    io[exp] = "Try to not use plain refute"
    nil
  end

  ############################################################
  # Expectations Helpers

  def self.exp_rewrite patterns, &block
    rewrite patterns do |_, (_, _, _, lhs), msg, *rhs|
      self.instance_exec(lhs, msg, *rhs, &block)
    end
  end

  def self.must_pat lhs, msg, *rhs
    parse "(call (call nil :_ %s) %s %s)" % [lhs, msg, rhs.join(" ")]
  end

  def self.must_block_pat body, msg, rhs
    parse "(call (iter (call nil :_) 0 %s) %s %s)" % [body, msg, rhs]
  end

  def self.meq_pat lhs, rhs
    must_pat(lhs, :must_equal, rhs)
  end

  def self.mbe_pat lhs, *rhs
    must_pat(lhs, :must_be, *rhs)
  end

  def self.wbe_pat lhs, *rhs
    must_pat(lhs, :wont_be, *rhs)
  end

  def self.weq_pat lhs, rhs
    must_pat(lhs, :wont_equal, rhs)
  end

  def must lhs, msg, *rhs
    s(:call, s(:call, nil, :_, lhs), msg, *rhs)
  end

  ############################################################
  # Positive Expectations

  def self.declare_must_wont_be verb, pred, msg = pred, is_pred = false
    spec  = "be"
    const = "RE_#{verb}_#{spec}__#{pred}".delete("?").upcase.to_sym

    msg   = msg.to_s.delete("?")
    pat   = must_pat("_", :"#{verb}_#{spec}", lit(pred))
    lhs   = "_(obj).#{verb}_#{spec} :#{pred}"
    rhs   = "_(obj).#{verb}_#{msg}"

    unless is_pred then
      pat << parse("_")
      lhs << ", val"
      rhs << " val"
    end

    doco lhs => rhs
    exp_rewrite(const => pat) do |lhs, _, _, *rhs|
      must(lhs, :"#{verb}_#{msg}", *rhs)
    end
  end

  def self.declare_must_be pred, msg = pred, is_pred = false
    declare_must_wont_be "must", pred, msg, is_pred
  end

  def self.declare_wont_be pred, msg = pred, is_pred = false
    declare_must_wont_be "wont", pred, msg, is_pred
  end

  # TODO: rename all these to name RHS
  re_must_other        = parse("(call (call nil [m expect value] _) [m /^must/] ___)")
  block_under          = "(iter (call nil :_) 0 ___)"
  call_under           = "(call nil :_ ___)"
  not_underscore       = "[- [any #{call_under} #{block_under}]]"
  re_must_plain        = parse("(call #{not_underscore}             [m /^must/] ___)")
  re_must_size_zero    = meq_pat(size_pat, lit(0))
  re_must_be_oper      = meq_pat("(call _ _ _)", "(:true)")
  re_must_be_pred      = meq_pat("(call _ _)",   "(:true)")
  re_must_be_pred_f    = meq_pat("(call _ _)",   "(:false)")
  re_must_be_oper_f    = meq_pat("(call _ _ _)", "(:false)")
  re_must_be_empty_lit = meq_pat("_" ,           "([m array hash])")
  re_must_eq_float     = meq_pat("_",            "(lit [k Float])")

  # This must be first to immediately rewrite them to normal form
  doco("expect(obj).must_<something> val" => "_(obj).must_<something> val",
       "value(obj).must_<something> val"  => "_(obj).must_<something> val",
       "expect(obj).must_<something>"     => "_(obj).must_<something>",
       "value(obj).must_<something>"      => "_(obj).must_<something>")
  exp_rewrite(RE_MUST_OTHER: re_must_other) do |lhs, msg, *rhs|
    must lhs, msg, *rhs
  end

  # This must be second so it doesn't catch the above
  doco("obj.must_<something> val" => "_(obj).must_<something> val",
       "obj.must_<something>"     => "_(obj).must_<something>")
  rewrite(RE_MUST_PLAIN: re_must_plain) do |t, lhs, msg, *rhs|
    must lhs, msg, *rhs
  end

  doco "_(obj.pred?).must_equal true" => "_(obj).must_be :pred?"
  exp_rewrite(RE_MUST_BE_PRED: re_must_be_pred) do |(_, lhs, msg),|
    must(lhs, :must_be, s(:lit, msg))
  end

  doco "_(obj.msg(val)).must_equal true" => "_(obj).must_be :msg, val"
  exp_rewrite(RE_MUST_BE_OPER: re_must_be_oper) do |(_, lhs, msg, rhs),|
    next if msg == :[]

    must(lhs, :must_be, s(:lit, msg), rhs)
  end

  doco "_(obj.pred?).must_equal false" => "_(obj).wont_be :pred?"
  exp_rewrite(RE_MUST_BE_PRED_F: re_must_be_pred_f) do |(_, lhs, msg), _, _|
    must(lhs, :wont_be, s(:lit, msg))
  end

  doco "_(obj.msg(val)).must_equal false" => "_(obj).wont_be :msg, val"
  exp_rewrite(RE_MUST_BE_OPER_F: re_must_be_oper_f) do |(_, lhs, msg, rhs),|
    next if msg == :[]

    must(lhs, :wont_be, s(:lit, msg), rhs)
  end

  doco "_(obj).must_equal nil" => "_(obj).must_be_nil"
  exp_rewrite(RE_MUST_EQ_NIL: meq_pat("_", "(:nil)")) do |lhs,|
    must lhs, :must_be_nil
  end

  doco "_(obj).must_equal float_lit" => "_(obj).must_be_close_to float_lit"
  exp_rewrite(RE_MUST_EQ_FLOAT: re_must_eq_float) do |lhs, _, rhs|
    must(lhs, :must_be_close_to, rhs)
  end

  doco("_(obj.count).must_equal 0"  => "_(obj).must_be_empty",
       "_(obj.length).must_equal 0" => "_(obj).must_be_empty",
       "_(obj.size).must_equal 0"   => "_(obj).must_be_empty")
  exp_rewrite(RE_MUST_SIZE_ZERO: re_must_size_zero) do |(_, lhs, _), _, _|
    must(lhs, :must_be_empty)
  end

  doco("_(obj).must_equal([])" => "_(obj).must_be_empty",
       "_(obj).must_equal({})" => "_(obj).must_be_empty")
  exp_rewrite(RE_MUST_BE_EMPTY_LIT: re_must_be_empty_lit) do |lhs,|
    must(lhs, :must_be_empty)
  end

  # TODO: long strings

  declare_must_be :equal?, :be_same_as
  declare_must_be :empty?, :be_empty, :pred!
  declare_must_be :nil?,   :be_nil,   :pred!
  declare_must_be :include?
  declare_must_be :key?,         :include
  declare_must_be :instance_of?, :be_instance_of
  declare_must_be :kind_of?,     :be_kind_of
  declare_must_be :is_a?,        :be_kind_of
  declare_must_be :respond_to?

  doco("_(obj).must_be :===, val"    => "_(obj).must_match val",
       "_(obj).must_be :=~, val"     => "_(obj).must_match val",
       "_(obj).must_be :match, val"  => "_(obj).must_match val",
       "_(obj).must_be :match?, val" => "_(obj).must_match val")
  exp_rewrite(RE_MUST_MATCH_EQ3:      mbe_pat("_", "(lit :===)",    "_"),
              RE_MUST_MATCH_EQTILDE:  mbe_pat("_", "(lit :=~)",     "_"),
              RE_MUST_MATCH_MATCH:    mbe_pat("_", "(lit :match)",  "_"),
              RE_MUST_MATCH_MATCH_EH: mbe_pat("_", "(lit :match?)", "_")) do |lhs, _, _, rhs|
    must(lhs, :must_match, rhs)
  end

  doco("_(obj).must_be :==, val" => "_(obj).must_equal val")
  exp_rewrite(RE_MUST_BE__EQ: mbe_pat("_", lit(:==), "_")) do |lhs, _, _, rhs|
    must(lhs, :must_equal, rhs)
  end

  doco("_(obj).must_be :!=, val" => "_(obj).wont_equal val")
  exp_rewrite(RE_MUST_BE__NEQ: mbe_pat("_", lit(:!=), "_")) do |lhs, _, _, rhs|
    must(lhs, :wont_equal, rhs)
  end

  doco("_(obj).must_be :!~, val" => "_(obj).wont_match val")
  exp_rewrite(RE_MUST_MATCH_NOT_TILDE: mbe_pat("_", lit(:!~), "_")) do |lhs, _, _, rhs|
    must(lhs, :wont_match, rhs)
  end

  doco "_(File).must_be :exist?, val" => "_(val).path_must_exist"
  exp_rewrite(RE_MUST_BE_FILE_EXIST: mbe_pat("(const :File)", lit(:exist?), "_")) do |lhs, _, _, rhs|
    must(rhs, :path_must_exist)
  end

  ############################################################
  # Negative Expectations

  re_wont_other        = parse("(call (call nil [m expect value] _) [m /^wont/] ___)")
  re_wont_plain        = parse("(call #{not_underscore}             [m /^wont/] ___)")
  re_wont_be_oper      = weq_pat("(call _ _ _)", "(:true)")
  re_wont_be_oper_f    = weq_pat("(call _ _ _)", "(:false)")
  re_wont_be_pred      = weq_pat("(call _ _)",   "(:true)")
  re_wont_be_pred_f    = weq_pat("(call _ _)",   "(:false)")
  re_wont_eq_float     = weq_pat("_",            "(lit [k Float])")
  re_wont_size_zero    = weq_pat(size_pat, lit(0))
  re_wont_be_empty_lit = weq_pat("_" ,           "([m array hash])")

  # This must be first to immediately rewrite them to normal form
  doco("expect(obj).wont_<something> val" => "_(obj).wont_<something> val",
       "value(obj).wont_<something> val"  => "_(obj).wont_<something> val",
       "expect(obj).wont_<something>"     => "_(obj).wont_<something>",
       "value(obj).wont_<something>"      => "_(obj).wont_<something>")
  exp_rewrite(RE_WONT_OTHER: re_wont_other) do |lhs, msg, *rhs|
    must lhs, msg, *rhs
  end

  # This must be second so it doesn't catch the above
  doco("obj.wont_<something> val" => "_(obj).wont_<something> val",
       "obj.wont_<something>"     => "_(obj).wont_<something>")
  rewrite(RE_WONT_PLAIN: re_wont_plain) do |t, lhs, msg, *rhs|
    must lhs, msg, *rhs
  end

  doco "_(obj.pred?).wont_equal true" => "_(obj).wont_be :pred?"
  exp_rewrite(RE_WONT_BE_PRED: re_wont_be_pred) do |(_, lhs, msg),|
    must(lhs, :wont_be, s(:lit, msg))
  end

  doco "_(obj.msg(val)).wont_equal true" => "_(obj).wont_be :msg, val"
  exp_rewrite(RE_WONT_BE_OPER: re_wont_be_oper) do |(_, lhs, msg, rhs),|
    next if msg == :[]

    must(lhs, :wont_be, s(:lit, msg), rhs)
  end

  doco "_(obj.pred?).wont_equal false" => "_(obj).must_be :pred?"
  exp_rewrite(RE_WONT_BE_PRED_F: re_wont_be_pred_f) do |(_, lhs, msg), _, _|
    must(lhs, :must_be, s(:lit, msg))
  end

  doco "_(obj.msg(val)).wont_equal false" => "_(obj).must_be :msg, val"
  exp_rewrite(RE_WONT_BE_OPER_F: re_wont_be_oper_f) do |(_, lhs, msg, rhs),|
    next if msg == :[]

    must(lhs, :must_be, s(:lit, msg), rhs)
  end

  doco "_(obj).wont_equal nil" => "_(obj).wont_be_nil"
  exp_rewrite(RE_WONT_EQ_NIL: weq_pat("_", "(:nil)")) do |lhs, _, _|
    must(lhs, :wont_be_nil)
  end

  doco "_(obj).wont_equal float_lit" => "_(obj).wont_be_close_to float_lit"
  exp_rewrite(RE_WONT_EQ_FLOAT: re_wont_eq_float) do |lhs, _, rhs|
    must(lhs, :wont_be_close_to, rhs)
  end

  doco("_(obj.count).wont_equal 0"  => "_(obj).wont_be_empty",
       "_(obj.length).wont_equal 0" => "_(obj).wont_be_empty",
       "_(obj.size).wont_equal 0"   => "_(obj).wont_be_empty")
  exp_rewrite(RE_WONT_SIZE_ZERO: re_wont_size_zero) do |(_, lhs, _), _, _|
    must(lhs, :wont_be_empty)
  end

  doco("_(obj).wont_equal([])" => "_(obj).wont_be_empty",
       "_(obj).wont_equal({})" => "_(obj).wont_be_empty")
  exp_rewrite(RE_WONT_BE_EMPTY_LIT: re_wont_be_empty_lit) do |lhs,|
    must(lhs, :wont_be_empty)
  end

  declare_wont_be :equal?, :be_same_as
  declare_wont_be :empty?, :be_empty, :pred!
  declare_wont_be :nil?,   :be_nil,   :pred!
  declare_wont_be :include?
  declare_wont_be :key?,         :include
  declare_wont_be :instance_of?, :be_instance_of
  declare_wont_be :kind_of?,     :be_kind_of
  declare_wont_be :is_a?,        :be_kind_of
  declare_wont_be :respond_to?

  doco("_(obj).wont_be :===, val"    => "_(obj).wont_match val",
       "_(obj).wont_be :=~, val"     => "_(obj).wont_match val",
       "_(obj).wont_be :match, val"  => "_(obj).wont_match val",
       "_(obj).wont_be :match?, val" => "_(obj).wont_match val")
  exp_rewrite(RE_WONT_BE__EQ3:      wbe_pat("_", "(lit :===)",    "_"),
              RE_WONT_BE__EQTILDE:  wbe_pat("_", "(lit :=~)",     "_"),
              RE_WONT_BE__MATCH:    wbe_pat("_", "(lit :match)",  "_"),
              RE_WONT_BE__MATCH_EH: wbe_pat("_", "(lit :match?)", "_")) do |lhs, _, _, rhs|
    must(lhs, :wont_match, rhs)
  end

  doco("_(obj).wont_be :==, val" => "_(obj).wont_equal val")
  exp_rewrite(RE_WONT_BE__EQ: wbe_pat("_", lit(:==), "_")) do |lhs, _, _, rhs|
    must(lhs, :wont_equal, rhs)
  end

  doco("_(obj).wont_be :!=, val" => "_(obj).must_equal val")
  exp_rewrite(RE_WONT_BE__NEQ: wbe_pat("_", lit(:!=), "_")) do |lhs, _, _, rhs|
    must(lhs, :must_equal, rhs)
  end

  doco("_(obj).wont_be :!~, val" => "_(obj).must_match val")
  exp_rewrite(RE_WONT_MATCH_NOT_TILDE: wbe_pat("_", "(lit :!~)", "_")) do |lhs, _, _, rhs|
    must(lhs, :must_match, rhs)
  end

  doco "_(File).wont_be :exist?, val" => "_(val).path_wont_exist"
  exp_rewrite(RE_WONT_BE_FILE_EXIST: wbe_pat("(const :File)", lit(:exist?), "_")) do |lhs, _, _, rhs|
    must(rhs, :path_wont_exist)
  end
end
