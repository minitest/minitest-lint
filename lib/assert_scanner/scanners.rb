class AssertScanner
  # TODO: DECIDE! do the pattern names match the RHS or LHS?? I think RHS

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

  meta "old_assert obj.msg(*args)" => "new_assert obj, :msg, *args"
  def self.unpack_call new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, (_, recv, m, *rest)|
      s(t, r, new_msg, recv, s(:lit, m), *rest)
    end
  end

  meta "old_assert _expected, obj.msg(*args)" => "new_assert obj, :msg, *args"
  def self.unpack_and_drop new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, _m, _lhs, (_, recv, m, *rest)|
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

  # TODO: remove
  meta "old_assert lhs.msg(rhs)" => "new_assert rhs, lhs"
  def self.replace_and_swap new_msg, patterns, msg = latest_doco_to
    rewrite patterns do |t, r, m, (_, lhs, _, rhs)|
      s(t, r, new_msg, rhs, lhs)
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

  # TODO: audit usage
  def handle_arity exp, arity
    exp, msg = exp[0..arity], exp[arity+1]

    change exp, "redundant message?" if msg if $v

    exp
  end

  ############################################################
  # Positive Assertions

  # TODO:
  # assert_raises Exception do ... end
  # assert_equal "str", klass.name
  # assert_match
  # assert_raises
  # assert_respond_to
  # assert_same

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

  doco "assert exp == act" => "assert_equal exp, act"
  replace_call(:assert_equal,
               RE_EQUAL: assert_pat("(call _ == _)"))

  doco "assert exp != act" => "refute_equal exp, act"
  replace_call(:refute_equal,
               RE_EQUAL_NOT: assert_pat("(call _ != _)"))

  doco "assert obj.pred?" => "assert_predicate obj, :pred?"
  unpack_call(:assert_predicate,
              RE_PRED: assert_pat("(call _ _)"))

  doco "assert obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack_call(:assert_operator,
              RE_OPER: assert_pat("(call _ _ _)"))

  doco "assert_equal true, obj.pred?" => "assert_predicate obj, :pred?"
  unpack_and_drop(:assert_predicate,
                  RE_EQ_PRED: eq_pat("(true)",  "(call _ _)"))

  doco "assert_equal true, obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack_and_drop(:assert_operator,
                  RE_EQ_OPER: eq_pat("(true)",  "(call _ _ _)"))

  doco "assert_equal false, obj.pred?" => "refute_predicate obj, :pred?"
  unpack_and_drop(:refute_predicate,
                  RE_NEQ_PRED: eq_pat("(false)",  "(call _ _)"))

  doco "assert_equal false, obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack_and_drop(:refute_operator,
                  RE_NEQ_OPER: eq_pat("(false)",  "(call _ _ _)"))

  NOT_LAS = "[- [any (lit _) (str _) ([atom])]]" # LAS = lit, atom, str

  doco "assert_equal act, lit" => "assert_equal lit, act"
  swap(RE_EQ_RHS_LIT: eq_pat(NOT_LAS, "(lit _)"),
       RE_EQ_RHS_STR: eq_pat(NOT_LAS, "(str _)"),
       RE_EQ_RHS_NTF: eq_pat(NOT_LAS, "([atom])"))

  doco "assert_equal nil, obj" => "assert_nil obj"
  rename_and_drop(:assert_nil,
                 RE_EQ_NIL: eq_pat("(:nil)", "_"))

  doco("assert_equal float_lit, act"    => "assert_in_epsilon float_lit, act",
       "assert_in_delta float_lit, act" => "assert_in_epsilon float_lit, act")
  rename(:assert_in_epsilon,
         RE_EQ_FLOAT: pat(:assert_equal,    "(lit, [k Float])", "_"),
         RE_IN_DELTA: pat(:assert_in_delta, "_",                "_"))

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

  doco "assert_equal 'long str', str" => "assert_includes str, 'substr'"
  rewrite(RE_EQ_LHS_STR: eq_pat("(str _)", "_")) do |t, r, _, (_, str), rhs, *|
    next unless str && str.length > 20

    s(t, r, :assert_includes, rhs, s(:str, str[0, 20]))
  end

  doco "assert_operator obj, :include?, val" => "assert_includes obj, val"
  promote_oper(:assert_includes,
               RE_OPER_INCLUDE: a_oper("_", :include?, "_"))

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

  doco "assert_predicate obj, :empty?" => "assert_empty obj"
  promote_pred(:assert_empty,
               RE_PRED_EMPTY: pred("_", :empty?))

  doco "assert obj" => "WARNING"
  RE_PLAIN = assert_pat "_"
  register_assert RE_PLAIN do |exp|
    io[exp] = "Try to not use plain assert"
    nil
  end

  ############################################################
  # Negative Assertions

  # TODO:
  # refute_match
  # refute_nil
  # assert(obj.size > 0) => refute_empty
  # lhs msg is count/length/size && rhs != 0 => refute_empty
  # lhs == binary call => refute_operator && rhs == false

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

  # TODO: normalize doco terms val/obj/etc

  doco "refute exp == act" => "refute_equal exp, act"
  replace_call(:refute_equal,
               RE_REF_EQUAL: refute_pat("(call _ == _)"))

  doco "refute exp != act" => "assert_equal exp, act"
  replace_call(:assert_equal,
               RE_REF_EQUAL_NOT: refute_pat("(call _ != _)"))

  doco "refute obj.pred?" => "refute_predicate obj, :pred?"
  unpack_call(:refute_predicate,
              RE_REF_PRED: refute_pat("(call _ _)"))

  doco "refute obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack_call(:refute_operator,
              RE_REF_OPER: refute_pat("(call _ _ _)"))

  doco "refute_equal true, obj.pred?" => "refute_predicate obj, :pred?"
  unpack_and_drop(:refute_predicate,
                  RE_REF_EQ_PRED: r_eq_pat("(true)",  "(call _ _)"))

  doco "refute_equal true, obj.msg(val)" => "refute_operator obj, :msg, val"
  unpack_and_drop(:refute_operator,
                  RE_REF_EQ_OPER: r_eq_pat("(true)",  "(call _ _ _)"))

  doco "refute_equal false, obj.pred?" => "assert_predicate obj, :pred?"
  unpack_and_drop(:assert_predicate,
                  RE_REF_NEQ_PRED: r_eq_pat("(false)",  "(call _ _)"))

  doco "refute_equal false, obj.msg(val)" => "assert_operator obj, :msg, val"
  unpack_and_drop(:assert_operator,
                  RE_REF_NEQ_OPER: r_eq_pat("(false)",  "(call _ _ _)"))

  doco "refute_equal act, lit" => "refute_equal lit, act"
  swap(RE_REF_EQ_RHS_LIT: r_eq_pat(NOT_LAS, "(lit _)"),
       RE_REF_EQ_RHS_STR: r_eq_pat(NOT_LAS, "(str _)"),
       RE_REF_EQ_RHS_NTF: r_eq_pat(NOT_LAS, "([atom])"))

  doco "refute_equal nil, obj" => "refute_nil obj"
  rename_and_drop(:refute_nil,
                 RE_REF_EQ_NIL: r_eq_pat("(:nil)", "_"))

  doco("refute_equal float_lit, act"    => "refute_in_epsilon float_lit, act",
       "refute_in_delta float_lit, act" => "refute_in_epsilon float_lit, act")
  rename(:refute_in_epsilon,
         RE_REF_EQ_FLOAT: pat(:refute_equal,    "(lit, [k Float])", "_"),
         RE_REF_IN_DELTA: pat(:refute_in_delta, "_",                "_"))

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

  doco "refute_operator obj, :include?, val" => "refute_includes obj, val"
  promote_oper(:refute_includes,
               RE_REF_OPER_INCLUDE: r_oper("_", :include?, "_"))

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

  doco "refute_predicate val, :empty?" => "refute_empty val"
  promote_pred(:refute_empty,
               RE_REF_PRED_EMPTY: r_pred("_", :empty?))

  doco "refute obj" => "WARNING"
  RE_REF_PLAIN = refute_pat "_"
  register_assert RE_REF_PLAIN do |exp|
    io[exp] = "Try to not use plain refute"
    nil
  end

  ############################################################
  # Expectations Helpers

  def self.exp_rewrite patterns, &block
    rewrite patterns do |sexp|
      self.instance_exec(match(sexp), &block)
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

  def match exp
    _, (_, _, _, lhs), msg, *rhs = exp
    return lhs, msg, *rhs
  end

  def must lhs, msg, *rhs
    s(:call, s(:call, nil, :_, lhs), msg, *rhs)
  end

  ############################################################
  # Positive Expectations

  # TODO:
  # must_be
  # must_be_kind_of
  # must_equal
  # must_match
  # must_output
  # must_raise
  # must_respond_to
  # must_throw

  # TODO: rename all these to name RHS
  re_must_other        = parse("(call (call nil [m expect value] _) [m /^must/] ___)")
  re_must_plain        = parse("(call [- (call nil :_ ___)]         [m /^must/] ___)")
  re_must_size_zero    = meq_pat(size_pat, lit(0))
  re_must_be_oper      = meq_pat("(call _ _ _)", "(:true)")
  re_must_be_empty_lit = meq_pat("_" ,           "([m array hash])")
  re_must_be_pred      = meq_pat("(call _ _)",   "(:true)")
  re_must_be_pred_f    = meq_pat("(call _ _)",   "(:false)")
  re_must_be_oper_f    = meq_pat("(call _ _ _)", "(:false)")
  re_must_eq_float     = meq_pat("_",            "(lit [k Float])")
  re_must_be_include   = mbe_pat("_",            lit(:include?), "_")
  re_must_be__empty    = mbe_pat("_",            lit(:empty?))

  # This must be first to immediately rewrite them to normal form
  doco("expect(obj).must_<something> val" => "_(obj).must_<something> val",
       "value(obj).must_<something> val"  => "_(obj).must_<something> val",
       "expect(obj).must_<something>"     => "_(obj).must_<something>",
       "value(obj).must_<something>"      => "_(obj).must_<something>")
  exp_rewrite(RE_MUST_OTHER: re_must_other) do |lhs, msg, *rhs|
    must lhs, msg, *rhs
  end

  # TODO: arg vs no arg?
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

  doco "_(obj).must_be :empty?" => "_(obj).must_be_empty"
  exp_rewrite(RE_MUST_BE__EMPTY: re_must_be__empty) do |lhs,|
    must(lhs, :must_be_empty)
  end

  doco "_(obj).must_be :include?, val" => "_(obj).must_include val"
  exp_rewrite(RE_MUST_BE_INCLUDE: re_must_be_include) do |lhs, _, _, rhs|
    must(lhs, :must_include, rhs)
  end

  doco "_(obj).must_be :instance_of?, cls" => "_(obj).must_be_instance_of cls"
  exp_rewrite(RE_MUST_BE_INSTANCE_OF: mbe_pat("_", "(lit :instance_of?)", "_"),) do |lhs, _, _, rhs|
    must(lhs, :must_be_instance_of, rhs)
  end

  doco("_(obj).must_be :kind_of?, mod" => "_(obj).must_be_kind_of mod",
       "_(obj).must_be :is_a?, mod"    => "_(obj).must_be_kind_of mod")
  exp_rewrite(RE_MUST_BE_IS_A:    mbe_pat("_", "(lit :is_a?)",        "_"),
              RE_MUST_BE_KIND_OF: mbe_pat("_", "(lit kind_of?)", "_")) do |lhs, _, _, rhs|
    must(lhs, :must_be_kind_of, rhs)
  end

  doco "_(obj).must_be :respond_to?, val" => "_(obj).must_respond_to val"
  exp_rewrite(RE_MUST_BE_RESPOND_TO: mbe_pat("_", "(lit respond_to?)", "_")) do |lhs, _, _, rhs|
    must(lhs, :must_respond_to, rhs)
  end

  ############################################################
  # Negative Expectations

  # TODO:
  # wont_be
  # wont_be_empty
  # wont_be_nil
  # wont_equal
  # wont_include
  # wont_match

  def self.declare_wont_be pred, msg = pred
    msg = msg.to_s.delete("?")
    pat = must_pat("_", :wont_be, lit(pred), "_")

    doco "_(obj).wont_be :#{pred}, val" => "_(obj).wont_#{msg} val"
    exp_rewrite(:"RE_WONT_BE_#{msg.upcase}" => pat) do |lhs, _, _, rhs|
      must(lhs, :"wont_#{msg}", rhs)
    end
  end

  declare_wont_be :include?
  declare_wont_be :respond_to?
end
