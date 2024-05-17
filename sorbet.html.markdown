---
language: sorbet
filename: learnsorbet.rb
contributors:
  - ["Jeremy Kaplan", "https://jdkaplan.dev"]
---

Sorbet is a type checker for Ruby. It adds syntax for method signatures that
enable both static and runtime type checking.

The easiest way to see it in action is in the playground at
[sorbet.run](https://sorbet.run).

Try copying in one of the sections below! Each top-level `class` or `module`
is independent from the others.

```ruby
# Every file should have a "typed sigil" that tells Sorbet how strict to be
# during static type checking.
#
# Strictness levels (lax to strict):
#
# ignore: Sorbet won't even read the file. This means its contents are not
# visible during type checking. Avoid this.
#
# false: Sorbet will only report errors related to constant resolution. This is
# the default if no sigil is included.
#
# true: Sorbet will report all static type errors. This is the sweet spot of
# safety for effort.
#
# strict: Sorbet will require that all methods, constants, and instance
# variables have static types.
#
# strong: Sorbet will no longer allow anything to be T.untyped, even
# explicitly. Almost nothing satisfies this.

# typed: true

# Include the runtime type-checking library. This lets you write inline sigs
# and have them checked at runtime (instead of running Sorbet as RBI-only).
# These runtime checks happen even for files with `ignore` or `false` sigils.
require 'sorbet-runtime'

class BasicSigs
  # Bring in the type definition helpers. You'll almost always need this.
  extend T::Sig

  # Sigs are defined with `sig` and a block. Define the return value type with
  # `returns`.
  #
  # This method returns a value whose class is `String`. These are the most
  # common types, and Sorbet calls them "class types".
  sig { returns(String) }
  def greet
    'Hello, World!'
  end

  # Define parameter value types with `params`.
  sig { params(n: Integer).returns(String) }
  def greet_repeat(n)
    (1..n).map { greet }.join("\n")
  end

  # Define keyword parameters the same way.
  sig { params(n: Integer, sep: String).returns(String) }
  def greet_repeat_2(n, sep: "\n")
    (1..n).map { greet }.join(sep)
  end

  # Notice that positional/keyword and required/optional make no difference
  # here. They're all defined the same way in `params`.

  # For lots of parameters, it's nicer to use do..end and a multiline block
  # instead of curly braces.
  sig do
    params(
      str: String,
      num: Integer,
      sym: Symbol,
    ).returns(String)
  end
  def uhh(str:, num:, sym:)
    'What would you even do with these?'
  end

  # For a method whose return value is useless, use `void`.
  sig { params(name: String).void }
  def say_hello(name)
    puts "Hello, #{name}!"
  end

  # Splats! Also known as "rest parameters", "*args", "**kwargs", and others.
  #
  # Type the value that a _member_ of `args` or `kwargs` will have, not `args`
  # or `kwargs` itself.
  sig { params(args: Integer, kwargs: String).void }
  def no_op(*args, **kwargs)
    if kwargs[:op] == 'minus'
      args.each { |i| puts(i - 1) }
    else
      args.each { |i| puts(i + 1) }
    end
  end

  # Most initializers should be `void`.
  sig { params(name: String).void }
  def initialize(name:)
    # Instance variables must have annotated types to participate in static
    # type checking.

    # The value in `T.let` is checked statically and at runtime.
    @upname = T.let(name.upcase, String)

    # Sorbet can infer this one!
    @name = name
  end

  # Constants also need annotated types.
  SORBET = T.let('A delicious frozen treat', String)

  # Class variables too.
  @@the_answer = T.let(42, Integer)

  # Sorbet knows about the `attr_*` family.
  sig { returns(String) }
  attr_reader :upname

  sig { params(write_only: Integer).returns(Integer) }
  attr_writer :write_only

  # You say the reader part and Sorbet will say the writer part.
  sig { returns(String) }
  attr_accessor :name
end

module Debugging
  extend T::Sig

  # Sometimes it's helpful to know what type Sorbet has inferred for an
  # expression. Use `T.reveal_type` to make type-checking show a special error
  # with that information.
  #
  # This is most useful if you have Sorbet integrated into your editor so you
  # can see the result as soon as you save the file.

  sig { params(obj: Object).returns(String) }
  def debug(obj)
    T.reveal_type(obj) # Revealed type: Object
    repr = obj.inspect

    # Remember that Ruby methods can be called without arguments, so you can
    # save a couple characters!
    T.reveal_type repr # Revealed type: String

    "DEBUG: " + repr
  end
end

module StandardLibrary
  extend T::Sig
  # Sorbet provides some helpers for typing the Ruby standard library.

  # Use T::Boolean to catch both `true` and `false`.
  #
  # For the curious, this is equivalent to
  #
  #     T.type_alias { T.any(TrueClass, FalseClass) }
  #
  sig { params(str: String).returns(T::Boolean) }
  def confirmed?(str)
    str == 'yes'
  end

  # Remember that the value `nil` is an instance of NilClass.
  sig { params(val: NilClass).void }
  def only_nil(val:); end

  # To avoid modifying standard library classes, Sorbet provides wrappers to
  # support common generics.
  #
  # Here's the full list:
  # * T::Array
  # * T::Enumerable
  # * T::Enumerator
  # * T::Hash
  # * T::Range
  # * T::Set
  sig { params(config: T::Hash[Symbol, String]).returns(T::Array[String]) }
  def merge_values(config)
    keyset = [:old_key, :new_key]
    config.each_pair.flat_map do |key, value|
      keyset.include?(key) ? value : 'sensible default'
    end
  end

  # Sometimes (usually dependency injection), a method will accept a reference
  # to a class rather than an instance of the class. Use `T.class_of(Dep)` to
  # accept the `Dep` class itself (or something that inherits from it).
  class Dep; end

  sig { params(dep: T.class_of(Dep)).returns(Dep) }
  def dependency_injection(dep:)
    dep.new
  end

  # Blocks, procs, and lambdas, oh my! All of these are typed with `T.proc`.
  #
  # Limitations:
  # 1. All parameters are assumed to be required positional parameters.
  # 2. The only runtime check is that the value is a `Proc`. The argument types
  #    are only checked statically.
  sig do
    params(
      data: T::Array[String],
      blk: T.proc.params(val: String).returns(Integer),
    ).returns(Integer)
  end
  def count(data, &blk)
    data.sum(&blk)
  end

  sig { returns(Integer) }
  def count_usage
    count(["one", "two", "three"]) { |word| word.length + 1 }
  end

  # If the method takes an implicit block, Sorbet will infer `T.untyped` for
  # it. Use the explicit block syntax if the types are important.
  sig { params(str: String).returns(T.untyped) }
  def implicit_block(str)
    yield(str)
  end

  # If you're writing a DSL and will execute the block in a different context,
  # use `bind`.
  sig { params(num: Integer, blk: T.proc.bind(Integer).void).void }
  def number_fun(num, &blk)
    num.instance_eval(&blk)
  end

  sig { params(num: Integer).void }
  def number_fun_usage(num)
    number_fun(10) { puts digits.join }
  end

  # If the block doesn't take any parameters, don't include `params`.
  sig { params(blk: T.proc.returns(Integer)).returns(Integer) }
  def doubled_block(&blk)
    2 * blk.call
  end
end

module Combinators
  extend T::Sig
  # These methods let you define new types from existing types.

  # Use `T.any` when you have a value that can be one of many types. These are
  # sometimes known as "union types" or "sum types".
  sig { params(num: T.any(Integer, Float)).returns(Rational) }
  def hundreds(num)
    num.rationalize
  end

  # `T.nilable(Type)` is a convenient alias for `T.any(Type, NilClass)`.
  sig { params(val: T.nilable(String)).returns(Integer) }
  def strlen(val)
    val.nil? ? -1 : val.length
  end

  # Use `T.all` when you have a value that must satisfy multiple types. These
  # are sometimes known as "intersection types". They're most useful for
  # interfaces (described later), but can also describe helper modules.

  module Reversible
    extend T::Sig
    sig { void }
    def reverse
      # Pretend this is actually implemented
    end
  end

  module Sortable
    extend T::Sig
    sig { void }
    def sort
      # Pretend this is actually implemented
    end
  end

  class List
    include Reversible
    include Sortable
  end

  sig { params(list: T.all(Reversible, Sortable)).void }
  def rev_sort(list)
    # reverse from Reversible
    list.reverse
    # sort from Sortable
    list.sort
  end

  def rev_sort_usage
    rev_sort(List.new)
  end

  # Sometimes, actually spelling out the type every time becomes more confusing
  # than helpful. Use type aliases to make them easier to work with.
  JSONLiteral = T.type_alias { T.any(Float, String, T::Boolean, NilClass) }

  sig { params(val: JSONLiteral).returns(String) }
  def stringify(val)
    val.to_s
  end
end

module DataClasses
  extend T::Sig
  # Use `T::Struct` to create a new class with type-checked fields. It combines
  # the best parts of the standard Struct and OpenStruct, and then adds static
  # typing on top.
  #
  # Types constructed this way are sometimes known as "product types".

  class Matcher < T::Struct
    # Use `prop` to define a field with both a reader and writer.
    prop :count, Integer
    # Use `const` to only define the reader and skip the writer.
    const :pattern, Regexp
    # You can still set a default value with `default`.
    const :message, String, default: 'Found one!'

    # This is otherwise a normal class, so you can still define methods.

    # You'll still need to bring `sig` in if you want to use it though.
    extend T::Sig

    sig { void }
    def reset
      self.count = 0
    end
  end

  sig { params(text: String, matchers: T::Array[Matcher]).void }
  def awk(text, matchers)
    matchers.each(&:reset)
    text.lines.each do |line|
      matchers.each do |matcher|
        if matcher.pattern =~ line
          Kernel.puts matcher.message
          matcher.count += 1
        end
      end
    end
  end

  # Gotchas and limitations

  # 1. `const` fields are not truly immutable. They don't have a writer method,
  #    but may be changed in other ways.
  class ChangeMe < T::Struct
    const :list, T::Array[Integer]
  end

  sig { params(change_me: ChangeMe).returns(T::Boolean) }
  def whoops!(change_me)
    change_me = ChangeMe.new(list: [1, 2, 3, 4])
    change_me.list.reverse!
    change_me.list == [4, 3, 2, 1]
  end

  # 2. `T::Struct` inherits its equality method from `BasicObject`, which uses
  #    identity equality (also known as "reference equality").
  class Coordinate < T::Struct
    const :row, Integer
    const :col, Integer
  end

  sig { returns(T::Boolean) }
  def never_equal!
    p1 = Coordinate.new(row: 1, col: 2)
    p2 = Coordinate.new(row: 1, col: 2)
    p1 != p2
  end

  # Define your own `#==` method to check the fields, if that's what you want.
  class Position < T::Struct
    extend T::Sig

    const :x, Integer
    const :y, Integer

    sig { params(other: Object).returns(T::Boolean) }
    def ==(other)
      # There's a real implementation here:
      # https://github.com/tricycle/sorbet-struct-comparable
      true
    end
  end

  # Use `T::Enum` to define a fixed set of values that are easy to reference.
  # This is especially useful when you don't care what the values _are_ as much
  # as you care that the set of possibilities is closed and static.
  class Crayon < T::Enum
    extend T::Sig

    # Initialize members with `enums`.
    enums do
      # Define each member with `new`. Each of these is an instance of the
      # `Crayon` class.
      Red = new
      Orange = new
      Yellow = new
      Green = new
      Blue = new
      Violet = new
      Brown = new
      Black = new
      # The default value of the enum is its name in all-lowercase. To change
      # that, pass a value to `new`.
      Gray90 = new('light-gray')
    end

    sig { returns(String) }
    def to_hex
      case self
      when Red   then '#ff0000'
      when Green then '#00ff00'
      # ...
      else            '#ffffff'
      end
    end
  end

  sig { params(crayon: Crayon, path: T::Array[Position]).void }
  def draw(crayon:, path:)
    path.each do |pos|
      Kernel.puts "(#{pos.x}, #{pos.y}) = " + crayon.to_hex
    end
  end

  # To get all the values in the enum, use `.values`. For convenience there's
  # already a `#serialize` to get the enum string value.

  sig { returns(T::Array[String]) }
  def crayon_names
    Crayon.values.map(&:serialize)
  end

  # Use the "deserialize" family to go from string to enum value.

  sig { params(name: String).returns(T.nilable(Crayon)) }
  def crayon_from_name(name)
    if Crayon.has_serialized?(name)
      # If the value is not found, this will raise a `KeyError`.
      Crayon.deserialize(name)
    end

    # If the value is not found, this will return `nil`.
    Crayon.try_deserialize(name)
  end
end

module FlowSensitivity
  extend T::Sig
  # Sorbet understands Ruby's control flow constructs and uses that information
  # to get more accurate types when your code branches.

  # You'll see this most often when doing nil checks.
  sig { params(name: T.nilable(String)).returns(String) }
  def greet_loudly(name)
    if name.nil?
      'HELLO, YOU!'
    else
      # Sorbet knows that `name` must be a String here, so it's safe to call
      # `#upcase`.
      "HELLO, #{name.upcase}!"
    end
  end

  # The nils are a special case of refining `T.any`.
  sig { params(id: T.any(Integer, T::Array[Integer])).returns(T::Array[String]) }
  def database_lookup(id)
    if id.is_a?(Integer)
      # `ids` must be an Integer here.
      [id.to_s]
    else
      # `ids` must be a T::Array[Integer] here.
      id.map(&:to_s)
    end
  end

  # Sorbet recognizes these methods that narrow type definitions:
  # * is_a?
  # * kind_of?
  # * nil?
  # * Class#===
  # * Class#<
  # * block_given?
  #
  # Because they're so common, it also recognizes these Rails extensions:
  # * blank?
  # * present?
  #
  # Be careful to maintain Sorbet assumptions if you redefine these methods!

  # Have you ever written this line of code?
  #
  #     raise StandardError, "Can't happen"
  #
  # Sorbet can help you prove that statically (this is known as
  # "exhaustiveness") with `T.absurd`.  It's extra cool when combined with
  # `T::Enum`!

  class Size < T::Enum
    extend T::Sig

    enums do
      Byte = new('B')
      Kibibyte = new('KiB')
      Mebibyte = new('MiB')
      # "640K ought to be enough for anybody"
    end

    sig { returns(Integer) }
    def bytes
      case self
        when Byte     then 1 <<  0
        when Kibibyte then 1 << 10
        when Mebibyte then 1 << 20
        else
          # Sorbet knows you've checked all the cases, so there's no possible
          # value that `self` could have here.
          #
          # But if you _do_ get here somehow, this will raise at runtime.
          T.absurd(self)

          # If you're missing a case, Sorbet can even tell you which one it is!
      end
    end
  end

  # We're gonna need `puts` and `raise` for this next part.
  include Kernel

  # Sorbet knows that no code can execute after a `raise` statement because it
  # "never returns".
  sig { params(num: T.nilable(Integer)).returns(Integer) }
  def decrement(num)
    raise ArgumentError, '¯\_(ツ)_/¯' unless num

    num - 1
  end

  class CustomError < StandardError; end

  # You can annotate your own error-raising methods with `T.noreturn`.
  sig { params(message: String).returns(T.noreturn) }
  def oh_no(message = 'A bad thing happened')
    puts message
    raise CustomError, message
  end

  # Infinite loops also don't return.
  sig { returns(T.noreturn) }
  def loading
    loop do
      %q(-\|/).each_char do |c|
        print "\r#{c} reticulating splines..."
        sleep 1
      end
    end
  end

  # You may run into a situation where Sorbet "loses" your type refinement.
  # Remember that almost everything you do in Ruby is a method call that could
  # return a different value next time you call it. Sorbet doesn't assume that
  # any methods are pure (even those from `attr_reader` and `attr_accessor`).
  sig { returns(T.nilable(Integer)) }
  def answer
    rand > 0.5 ? 42 : nil
  end

  sig { void }
  def bad_typecheck
    if answer.nil?
      0
    else
      # But answer might return `nil` if we call it again!
      answer + 1
      # ^ Method + does not exist on NilClass component of T.nilable(Integer)
    end
  end

  sig { void }
  def good_typecheck
    ans = answer
    if ans.nil?
      0
    else
      # This time, Sorbet knows that `ans` is non-nil.
      ans + 1
    end
  end
end

module InheritancePatterns
  extend T::Sig

  # If you have a method that always returns the type of its receiver, use
  # `T.self_type`. This is common in fluent interfaces and DSLs.
  #
  # Warning: This feature is still experimental!
  class Logging
    extend T::Sig

    sig { returns(T.self_type) }
    def log
      pp self
      self
    end
  end

  class Data < Logging
    extend T::Sig

    sig { params(x: Integer, y: String).void }
    def initialize(x: 0, y: '')
      @x = x
      @y = y
    end

    # You don't _have_ to use `T.self_type` if there's only one relevant class.
    sig { params(x: Integer).returns(Data) }
    def setX(x)
      @x = x
      self
    end

    sig { params(y: String).returns(Data) }
    def setY(y)
      @y = y
      self
    end
  end

  # Ta-da!
  sig { params(data: Data).void }
  def chaining(data)
    data.setX(1).log.setY('a')
  end

  # If it's a class method (a.k.a. singleton method), use `T.attached_class`.
  #
  # No warning here. This one is stable!
  class Box
    extend T::Sig

    sig { params(contents: String, weight: Integer).void }
    def initialize(contents, weight)
      @contents = contents
      @weight = weight
    end

    sig { params(contents: String).returns(T.attached_class) }
    def self.pack(contents)
      new(contents, contents.chars.uniq.length)
    end
  end

  class CompanionCube < Box
    extend T::Sig

    sig { returns(String) }
    def pick_up
      "♥#{@contents}🤍"
    end
  end

  sig { returns(String) }
  def befriend
    CompanionCube.pack('').pick_up
  end

  # Sorbet has support for abstract classes and interfaces. It can check that
  # all the concrete classes and implementations actually define the required
  # methods with compatible signatures.

  # Here's an abstract class:

  class WorkflowStep
    extend T::Sig

    # Bring in the inheritance helpers.
    extend T::Helpers

    # Mark this class as abstract. This means it cannot be instantiated with
    # `.new`, but it can still be subclassed.
    abstract!

    sig { params(args: T::Array[String]).void }
    def run(args)
      pre_hook
      execute(args)
      post_hook
    end

    # This is an abstract method, which means it _must_ be implemented by
    # subclasses. Add a signature with `abstract` to an empty method to tell
    # Sorbet about it.
    #
    # If this implementation of the method actually gets called at runtime, it
    # will raise `NotImplementedError`.
    sig { abstract.params(args: T::Array[String]).void }
    def execute(args); end

    # The following non-abstract methods _can_ be implemented by subclasses,
    # but they're optional.

    sig { void }
    def pre_hook; end

    sig { void }
    def post_hook; end
  end

  class Configure < WorkflowStep
    extend T::Sig

    sig { void }
    def pre_hook
      puts 'Configuring...'
    end

    # To implement an abstract method, mark the signature with `override`.
    sig { override.params(args: T::Array[String]).void }
    def execute(args)
      # ...
    end
  end

  # And here's an interface:

  module Queue
    extend T::Sig

    # Bring in the inheritance helpers.
    extend T::Helpers

    # Mark this module as an interface. This adds the following restrictions:
    # 1. All of its methods must be abstract.
    # 2. It cannot have any private or protected methods.
    interface!

    sig { abstract.params(num: Integer).void }
    def push(num); end

    sig { abstract.returns(T.nilable(Integer)) }
    def pop; end
  end

  class PriorityQueue
    extend T::Sig

    # Include the interface to tell Sorbet that this class implements it.
    # Sorbet doesn't support implicitly implemented interfaces (also known as
    # "duck typing").
    include Queue

    sig { void }
    def initialize
      @items = T.let([], T::Array[Integer])
    end

    # Implement the Queue interface's abstract methods. Remember to use
    # `override`!

    sig { override.params(num: Integer).void }
    def push(num)
      @items << num
      @items.sort!
    end

    sig { override.returns(T.nilable(Integer)) }
    def pop
      @items.shift
    end
  end

  # If you use the `included` hook to get class methods from your modules,
  # you'll have to use `mixes_in_class_methods` to get them to type-check.

  module Mixin
    extend T::Helpers
    interface!

    module ClassMethods
      extend T::Sig

      sig { void }
      def whisk
        'fskfskfsk'
      end
    end

    mixes_in_class_methods(ClassMethods)
  end

  class EggBeater
    include Mixin
  end

  EggBeater.whisk # Meringue!
end

module EscapeHatches
  extend T::Sig

  # Ruby is a very dynamic language, and sometimes Sorbet can't infer the
  # properties you already know to be true. Although there are ways to rewrite
  # your code so Sorbet can prove safety, you can also choose to "break out" of
  # Sorbet using these "escape hatches".

  # Once you start using `T.nilable`, Sorbet will start telling you _all_ the
  # places you're not handling nils. Sometimes, you know a value can't be nil,
  # but it's not practical to fix the sigs so Sorbet can prove it. In that
  # case, you can use `T.must`.
  sig { params(maybe_str: T.nilable(String)).returns(String) }
  def no_nils_here(maybe_str)
    # If maybe_str _is_ actually nil, this will error at runtime.
    str = T.must(maybe_str)
    str.downcase
  end

  # More generally, if you know that a value must be a specific type, you can
  # use `T.cast`.
  sig do
    params(
      str_or_ary: T.any(String, T::Array[String]),
      idx_or_range: T.any(Integer, T::Range[Integer]),
    ).returns(T::Array[String])
  end
  def slice2(str_or_ary, idx_or_range)
    # Let's say that, for some reason, we want individual characters from
    # strings or sub-arrays from arrays. The other options are not allowed.
    if str_or_ary.is_a?(String)
      # Here, we know that `idx_or_range` must be a single index. If it's not,
      # this will error at runtime.
      idx = T.cast(idx_or_range, Integer)
      [str_or_ary.chars.fetch(idx)]
    else
      # Here, we know that `idx_or_range` must be a range. If it's not, this
      # will error at runtime.
      range = T.cast(idx_or_range, T::Range[Integer])
      str_or_ary.slice(range) || []
    end
  end

  # If you know that a method exists, but Sorbet doesn't, you can use
  # `T.unsafe` so Sorbet will let you call it. Although we tend to think of
  # this as being an "unsafe method call", `T.unsafe` is called on the receiver
  # rather than the whole expression.
  sig { params(count: Integer).returns(Date) }
  def the_future(count)
    # Let's say you've defined some extra date helpers that Sorbet can't find.
    # So `2.decades` is effectively `(2*10).years` from ActiveSupport.
    Date.today + T.unsafe(count).decades
  end

  # If this is a method on the implicit `self`, you'll have to make that
  # explicit to use `T.unsafe`.
  sig { params(count: Integer).returns(Date) }
  def the_past(count)
    # Let's say that metaprogramming defines a `now` helper method for
    # `Time.new`. Using it would normally look like this:
    #
    #     now - 1234
    #
    T.unsafe(self).now - 1234
  end

  # There's a special type in Sorbet called `T.untyped`. For any value of this
  # type, Sorbet will allow it to be used for any method argument and receive
  # any method call.

  sig { params(num: Integer, anything: T.untyped).returns(T.untyped) }
  def nothing_to_see_here(num, anything)
    anything.digits # Is it an Integer...
    anything.upcase # ... or a String?

    # Sorbet will not be able to infer anything about this return value because
    # it's untyped.
    BasicObject.new
  end

  def see_here
    # It's actually nil!  This will crash at runtime, but Sorbet allows it.
    nothing_to_see_here(1, nil)
  end

  # For a method without a sig, Sorbet infers the type of each argument and the
  # return value to be `T.untyped`.
end

# The following types are not officially documented but are still useful. They
# may be experimental, deprecated, or not supported.

module ValueSet
  extend T::Sig

  # A common pattern in Ruby is to have a method accept one value from a set of
  # options. Especially when starting out with Sorbet, it may not be practical
  # to refactor the code to use `T::Enum`. In this case, you can use `T.enum`.
  #
  # Note: Sorbet can't check this statically becuase it doesn't track the
  # values themselves.
  sig do
    params(
      data: T::Array[Numeric],
      shape: T.enum([:circle, :square, :triangle])
    ).void
  end
  def plot_points(data, shape: :circle)
    data.each_with_index do |y, x|
      Kernel.puts "#{x}: #{y}"
    end
  end
end

module Generics
  extend T::Sig

  # Generics are useful when you have a class whose method types change based
  # on the data it contains or a method whose method type changes based on what
  # its arguments are.

  # A generic method uses `type_parameters` to declare type variables and
  # `T.type_parameter` to refer back to them.
  sig do
    type_parameters(:element)
      .params(
        element: T.type_parameter(:element),
        count: Integer,
      ).returns(T::Array[T.type_parameter(:element)])
  end
  def repeat_value(element, count)
    count.times.each_with_object([]) do |elt, ary|
      ary << elt
    end
  end

  sig do
    type_parameters(:element)
      .params(
        count: Integer,
        block: T.proc.returns(T.type_parameter(:element)),
      ).returns(T::Array[T.type_parameter(:element)])
  end
  def repeat_cached(count, &block)
    elt = block.call
    ary = []
    count.times do
      ary << elt
    end
    ary
  end

  # A generic class uses `T::Generic.type_member` to define type variables that
  # can be like regular type names.
  class BidirectionalHash
    extend T::Sig
    extend T::Generic

    Left = type_member
    Right = type_member

    sig { void }
    def initialize
      @left_hash = T.let({}, T::Hash[Left, Right])
      @right_hash = T.let({}, T::Hash[Right, Left])
    end

    # Implement just enough to make the methods below work.

    sig { params(lkey: Left).returns(T::Boolean) }
    def lhas?(lkey)
      @left_hash.has_key?(lkey)
    end

    sig { params(rkey: Right).returns(T.nilable(Left)) }
    def rget(rkey)
      @right_hash[rkey]
    end
  end

  # To specialize a generic type, use brackets.
  sig do
    params(
      options: BidirectionalHash[Symbol, Integer],
      choice: T.any(Symbol, Integer),
    ).returns(T.nilable(String))
  end
  def lookup(options, choice)
    case choice
    when Symbol
      options.lhas?(choice) ? choice.to_s : nil
    when Integer
      options.rget(choice).to_s
    else
      T.absurd(choice)
    end
  end

  # To specialize through inheritance, re-declare the `type_member` with
  # `fixed`.
  class Options < BidirectionalHash
    Left = type_member(fixed: Symbol)
    Right = type_member(fixed: Integer)
  end

  sig do
    params(
      options: Options,
      choice: T.any(Symbol, Integer),
    ).returns(T.nilable(String))
  end
  def lookup2(options, choice)
    lookup(options, choice)
  end

  # There are other variance annotations you can add to `type_member`, but
  # they're rarely used.
end
```

## Additional resources

- [Official Documentation](https://sorbet.org/docs/overview)
- [sorbet.run](https://sorbet.run) - Playground
