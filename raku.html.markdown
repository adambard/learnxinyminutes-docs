---
category: language
language: Raku
filename: learnraku.raku
contributors:
    - ["vendethiel", "http://github.com/vendethiel"]
    - ["Samantha McVey", "https://cry.nu"]
---

Raku (formerly Perl 6) is a highly capable, feature-rich programming language
made for at least the next hundred years.

The primary Raku compiler is called [Rakudo](http://rakudo.org), which runs on
the JVM and the [MoarVM](http://moarvm.com).

Meta-note:

* Although the pound sign (`#`) is used for sentences and notes, Pod-styled
  comments (more below about them) are used whenever it's convenient.
* `# OUTPUT:` is used to represent the output of a command to any standard
   stream. If the output has a newline, it's represented by the `␤` symbol.
   The output is always enclosed by angle brackets (`«` and `»`).
* `#=>` represents the value of an expression, return value of a sub, etc.
   In some cases, the value is accompanied by a comment.
* Backticks are used to distinguish and highlight the language constructs
  from the text.

```perl6
####################################################
# 0. Comments
####################################################

# Single line comments start with a pound sign.

#`( Multiline comments use #` and a quoting construct.
  (), [], {}, 「」, etc, will work.
)

=for comment
Use the same syntax for multiline comments to embed comments.
for #`(each element in) @array {
    put #`(or print element) $_ #`(with newline);
}

# You can also use Pod-styled comments. For example:

=comment This is a comment that extends until an empty
newline is found.

=comment
The comment doesn't need to start in the same line as the directive.

=begin comment
This comment is multiline.

Empty newlines can exist here too!
=end comment

####################################################
# 1. Variables
####################################################

# In Raku, you declare a lexical variable using the `my` keyword:
my $variable;

# Raku has 3 basic types of variables: scalars, arrays, and hashes.

#
# 1.1 Scalars
#

# Scalars represent a single value. They start with the `$` sigil:
my $str = 'String';

# Double quotes allow for interpolation (which we'll see later):
my $str2 = "$str";

# Variable names can contain but not end with simple quotes and dashes,
# and can contain (and end with) underscores:
my $person's-belongings = 'towel'; # this works!

my $bool = True;             # `True` and `False` are Raku's boolean values.
my $inverse = !$bool;        # Invert a bool with the prefix `!` operator.
my $forced-bool = so $str;   # And you can use the prefix `so` operator
$forced-bool = ?$str;        # to turn its operand into a Bool. Or use `?`.

#
# 1.2 Arrays and Lists
#

# Arrays represent multiple values. An array variable starts with the `@`
# sigil. Unlike lists, from which arrays inherit, arrays are mutable.

my @array = 'a', 'b', 'c';
# equivalent to:
my @letters = <a b c>;
# In the previous statement, we use the quote-words (`<>`) term for array
# of words, delimited by space. Similar to perl's qw, or Ruby's %w.

@array = 1, 2, 4;

# Array indices start at 0. Here the third element is being accessed.
say @array[2]; # OUTPUT: «4␤»

say "Interpolate an array using []: @array[]";
# OUTPUT: «Interpolate an array using []: 1 2 3␤»

@array[0]    = -1;     # Assigning a new value to an array index
@array[0, 1] = 5, 6;   # Assigning multiple values

my @keys = 0, 2;
@array[@keys] = @letters; # Assignment using an array containing index values
say @array;               # OUTPUT: «a 6 b␤»

#
# 1.3 Hashes, or key-value Pairs.
#

# Hashes are pairs of keys and values. You can construct a `Pair` object
# using the syntax `key => value`. Hash tables are very fast for lookup,
# and are stored unordered. Keep in mind that keys get "flattened" in hash
# context, and any duplicated keys are deduplicated.
my %hash = 'a' => 1, 'b' => 2;

# Keys get auto-quoted when the fat comma (`=>`) is used. Trailing commas are
# okay.
%hash = a => 1, b => 2, ;

# Even though hashes are internally stored differently than arrays,
# Raku allows you to easily create a hash from an even numbered array:
%hash = <key1 value1 key2 value2>;          # Or:
%hash = "key1", "value1", "key2", "value2";

%hash = key1 => 'value1', key2 => 'value2'; # same result as above

# You can also use the "colon pair" syntax. This syntax is especially
# handy for named parameters that you'll see later.
%hash = :n(2),    # equivalent to `n => 2`
        :is-even, # equivalent to `:is-even(True)` or `is-even => True`
        :!is-odd, # equivalent to `:is-odd(False)` or `is-odd => False`
;
# The `:` (as in `:is-even`) and `:!` (as `:!is-odd`) constructs are known
# as the `True` and `False` shortcuts respectively.

# As demonstrated in the example below, you can use {} to get the value from a key.
# If it's a string without spaces, you can actually use the quote-words operator
# (`<>`). Since Raku doesn't have barewords, as Perl does, `{key1}` doesn't work
# though.
say %hash{'n'};     # OUTPUT: «2␤», gets value associated to key 'n'
say %hash<is-even>; # OUTPUT: «True␤», gets value associated to key 'is-even'

####################################################
# 2. Subroutines
####################################################

# Subroutines, or functions as most other languages call them, are
# created with the `sub` keyword.
sub say-hello { say "Hello, world" }

# You can provide (typed) arguments. If specified, the type will be checked
# at compile-time if possible, otherwise at runtime.
sub say-hello-to( Str $name ) {
    say "Hello, $name !";
}

# A sub returns the last value of the block. Similarly, the semicolon in
# the last expression can be omitted.
sub return-value { 5 }
say return-value;      # OUTPUT: «5␤»

sub return-empty { }
say return-empty;      # OUTPUT: «Nil␤»

# Some control flow structures produce a value, for instance `if`:
sub return-if {
	if True { "Truthy" }
}
say return-if;         # OUTPUT: «Truthy␤»

# Some don't, like `for`:
sub return-for {
    for 1, 2, 3 { 'Hi' }
}
say return-for;        # OUTPUT: «Nil␤»

# Positional arguments are required by default. To make them optional, use
# the `?` after the parameters' names.

# In the following example, the sub `with-optional` returns `(Any)` (Perl's
# null-like value) if no argument is passed. Otherwise, it returns its argument.
sub with-optional( $arg? ) {
    $arg;
}
with-optional;     # returns Any
with-optional();   # returns Any
with-optional(1);  # returns 1

# You can also give provide a default value when they're not passed. Doing
# this make said parameter optional. Required parameters must come before
# optional ones.

# In the sub `greeting`, the parameter `$type` is optional.
sub greeting( $name, $type = "Hello" ) {
  say "$type, $name!";
}

greeting("Althea");                 # OUTPUT: «Hello, Althea!␤»
greeting("Arthur", "Good morning"); # OUTPUT: «Good morning, Arthur!␤»

# You can also, by using a syntax akin to the one of hashes (yay unified syntax!),
# declared named parameters and thus pass named arguments to a subroutine.
# By default, named parameter are optional and will default to `Any`.
sub with-named( $normal-arg, :$named ) {
	say $normal-arg + $named;
}
with-named(1, named => 6); # OUTPUT: «7␤»

# There's one gotcha to be aware of, here: If you quote your key, Raku
# won't be able to see it at compile time, and you'll have a single `Pair`
# object as a positional parameter, which means the function subroutine
# `with-named(1, 'named' => 6);` fails.
with-named(2, :named(5));  # OUTPUT: «7␤»

# Similar to positional parameters, you can provide your named arguments with
# default values.
sub named-def( :$def = 5 ) {
    say $def;
}
named-def;            # OUTPUT: «5»
named-def(def => 15); # OUTPUT: «15»

# In order to make a named parameter mandatory, you can append `!` to the
# parameter. This is the inverse of `?`, which makes a required parameter
# optional.

sub with-mandatory-named( :$str! )  {
    say "$str!";
}
with-mandatory-named(str => "My String"); # OUTPUT: «My String!␤»
# with-mandatory-named;   # runtime error: "Required named parameter not passed"
# with-mandatory-named(3);# runtime error: "Too many positional parameters passed"

# If a sub takes a named boolean argument, you can use the same "short boolean"
# hash syntax we discussed earlier.
sub takes-a-bool( $name, :$bool ) {
    say "$name takes $bool";
}
takes-a-bool('config', :bool);  # OUTPUT: «config takes True␤»
takes-a-bool('config', :!bool); # OUTPUT: «config takes False␤»

# Since parenthesis can be omitted when calling a subroutine, you need to use
# `&` in order to distinguish between a call to a sub with no arguments and
# the code object.

# For instance, in this example we must use `&` to store the sub `say-hello`
# (i.e., the sub's code object) in a variable, not a subroutine call.
my &s = &say-hello;
my &other-s = sub { say "Anonymous function!" }

# A sub can have a "slurpy" parameter, or what one'd call a
# "doesn't-matter-how-many" parameter. This is Raku's way of supporting variadic
# functions. For this, you must use `*@` (slurpy) which will "take everything
# else". You can have as many parameters *before* a slurpy one, but not *after*.
sub as-many($head, *@rest) {
    @rest.join(' / ') ~ " !";
}
say as-many('Happy', 'Happy', 'Birthday');          # OUTPUT: «Happy / Birthday !␤»
say as-many('Happy', ['Happy', 'Birthday'], 'Day'); # OUTPUT: «Happy / Birthday / Day !␤»

# Note that the splat (the *) did not consume the parameter before it.

# There are other two variations of slurpy parameters in Raku. The previous one
# (namely, `*@`), known as flattened slurpy, flattens passed arguments. The other
# two are `**@` and `+@` known as unflattened slurpy and "single argument rule"
# slurpy respectively. The unflattened slurpy doesn't flatten its listy
# arguments (or Iterable ones).
sub b(**@arr) { @arr.perl.say };
b(['a', 'b', 'c']);             # OUTPUT: «[["a", "b", "c"],]»
b(1, $('d', 'e', 'f'), [2, 3]); # OUTPUT: «[1, ("d", "e", "f"), [2, 3]]»
b(1, [1, 2], ([3, 4], 5));      # OUTPUT: «[1, [1, 2], ([3, 4], 5)]␤»

# On the other hand, the "single argument rule" slurpy follows the "single argument
# rule" which dictates how to handle the slurpy argument based upon context and
# roughly states that if only a single argument is passed and that argument is
# Iterable, that argument is used to fill the slurpy parameter array. In any
# other case, `+@` works like `**@`.
sub c(+@arr) { @arr.perl.say };
c(['a', 'b', 'c']);             # OUTPUT: «["a", "b", "c"]␤»
c(1, $('d', 'e', 'f'), [2, 3]); # OUTPUT: «[1, ("d", "e", "f"), [2, 3]]␤»
c(1, [1, 2], ([3, 4], 5));      # OUTPUT: «[1, [1, 2], ([3, 4], 5)]␤»

# You can call a function with an array using the "argument list flattening"
# operator `|` (it's not actually the only role of this operator,
# but it's one of them).
sub concat3($a, $b, $c) {
    say "$a, $b, $c";
}
concat3(|@array); # OUTPUT: «a, b, c␤»
                  # `@array` got "flattened" as a part of the argument list

####################################################
# 3. Containers
####################################################

# In Raku, values are actually stored in "containers". The assignment
# operator asks the container on the left to store the value on its right.
# When passed around, containers are marked as immutable which means that,
# in a function, you'll get an error if you try to mutate one of your
# arguments. If you really need to, you can ask for a mutable container by
# using the `is rw` trait.
sub mutate( $n is rw ) {
    $n++; # postfix ++ operator increments its argument but returns its old value
}
my $m = 42;
mutate $m; #=> 42, the value is incremented but the old value is returned
say $m;    # OUTPUT: «43␤»

# This works because we are passing the container $m to the `mutate` sub.
# If we try to just pass a number instead of passing a variable, it won't work
# because there is no container being passed and integers are immutable by
# themselves:

# mutate 42; # Parameter '$n' expected a writable container, but got Int value

# Similar error would be obtained, if a bound variable is passed to
# to the subroutine. In Raku, you bind a value to a variable using the binding
# operator `:=`.
my $v := 50; # binding 50 to the variable $v
# mutate $v;   # Parameter '$n' expected a writable container, but got Int value

# If what you want is a copy instead, use the `is copy` trait which will
# cause the argument to be copied and allow you to modify the argument
# inside the routine without modifying the passed argument.

# A sub itself returns a container, which means it can be marked as `rw`.
# Alternatively, you can explicitly mark the returned container as mutable
# by using `return-rw` instead of `return`.
my $x = 42;
my $y = 45;
sub x-store is rw { $x }
sub y-store       { return-rw $y }

# In this case, the parentheses are mandatory or else Raku thinks that
# `x-store` and `y-store` are identifiers.
x-store() = 52;
y-store() *= 2;

say $x; # OUTPUT: «52␤»
say $y; # OUTPUT: «90␤»

####################################################
# 4.Control Flow Structures
####################################################

#
# 4.1 if/if-else/if-elsif-else/unless
#

# Before talking about `if`, we need to know which values are "truthy"
# (represent `True`), and which are "falsey" (represent `False`). Only these
# values are falsey: 0, (), {}, "", Nil, a type (like `Str`, `Int`, etc.) and
# of course, `False` itself. Any other value is truthy.
my $number = 5;
if $number < 5 {
    say "Number is less than 5"
}
elsif $number == 5 {
    say "Number is equal to 5"
}
else {
    say "Number is greater than 5"
}

unless False {
    say "It's not false!";
}

# `unless` is the equivalent of `if not (X)` which inverts the sense of a
# conditional statement. However, you cannot use `else` or `elsif` with it.

# As you can see, you don't need parentheses around conditions. However, you
# do need the curly braces around the "body" block. For example,
# `if (True) say 'It's true';` doesn't work.

# You can also use their statement modifier (postfix) versions:
say "Quite truthy" if True;      # OUTPUT: «Quite truthy␤»
say "Quite falsey" unless False; # OUTPUT: «Quite falsey␤»

# The ternary operator (`??..!!`) is structured as follows `condition ??
# expression1 !! expression2` and it returns expression1 if the condition is
# true. Otherwise, it returns expression2.
my $age = 30;
say $age > 18 ?? "You are an adult" !! "You are under 18";
# OUTPUT: «You are an adult␤»

#
# 4.2 with/with-else/with-orwith-else/without
#

# The `with` statement is like `if`, but it tests for definedness rather than
# truth, and it topicalizes on the condition, much like `given` which will
# be discussed later.
my $s = "raku";
with   $s.index("r") { say "Found a at $_"      }
orwith $s.index("k") { say "Found c at $_"      }
else                 { say "Didn't find r or k" }

# Similar to `unless` that checks un-truthiness, you can use `without` to
# check for undefined-ness.
my $input01;
without $input01 {
    say "No input given."
}
# OUTPUT: «No input given.␤»

# There are also statement modifier versions for both `with` and `without`.
my $input02 = 'Hello';
say $input02 with $input02;               # OUTPUT: «Hello␤»
say "No input given." without $input02;

#
# 4.3 given/when, or Raku's switch construct
#

=begin comment
`given...when` looks like other languages' `switch`, but is much more
powerful thanks to smart matching and Raku's "topic variable", `$_`.

The topic variable `$_ `contains the default argument of a block, a loop's
current iteration (unless explicitly named), etc.

`given` simply puts its argument into `$_` (like a block would do),
 and `when` compares it using the "smart matching" (`~~`) operator.

Since other Raku constructs use this variable (as said before, like `for`,
blocks, `with` statement etc), this means the powerful `when` is not only
applicable along with a `given`, but instead anywhere a `$_` exists.

=end comment

given "foo bar" {
    say $_;            # OUTPUT: «foo bar␤»

    # Don't worry about smart matching yet. Just know `when` uses it. This is
    # equivalent to `if $_ ~~ /foo/`.
    when /foo/ {
        say "Yay !";
    }

    # smart matching anything with `True` is `True`, i.e. (`$a ~~ True`)
    # so you can also put "normal" conditionals. For example, this `when` is
    # equivalent to this `if`: `if $_ ~~ ($_.chars > 50) {...}`
    # which means: `if $_.chars > 50 {...}`
    when $_.chars > 50 {
        say "Quite a long string !";
    }

    # same as `when *` (using the Whatever Star)
    default {
        say "Something else"
    }
}

#
# 4.4 Looping constructs
#

# The `loop` construct is an infinite loop if you don't pass it arguments, but
# can also be a C-style `for` loop:
loop {
    say "This is an infinite loop !";
    last;
}
# In the previous example, `last` breaks out of the loop very much
# like the `break` keyword in other languages.

# The `next` keyword skips to the next iteration, like `continue` in other
# languages. Note that you can also use postfix conditionals, loops, etc.
loop (my $i = 0; $i < 5; $i++) {
    next if $i == 3;
    say "This is a C-style for loop!";
}

# The `for` constructs iterates over a list of elements.
my @odd-array = 1, 3, 5, 7, 9;

# Accessing the array's elements with the topic variable $_.
for @odd-array {
    say "I've got $_ !";
}

# Accessing the array's elements with a "pointy block", `->`.
# Here each element is read-only.
for @odd-array -> $variable {
    say "I've got $variable !";
}

# Accessing the array's elements with a "doubly pointy block", `<->`.
# Here each element is read-write so mutating `$variable` mutates
# that element in the array.
for @odd-array <-> $variable {
    say "I've got $variable !";
}

# As we saw with `given`, a `for` loop's default "current iteration" variable
# is `$_`. That means you can use `when` in a `for`loop just like you were
# able to in a `given`.
for @odd-array {
    say "I've got $_";

    # This is also allowed. A dot call with no "topic" (receiver) is sent to
    # `$_` (topic variable) by default.
    .say;

    # This is equivalent to the above statement.
    $_.say;
}

for @odd-array {
    # You can...
    next if $_ == 3; # Skip to the next iteration (`continue` in C-like lang.)
    redo if $_ == 4; # Re-do iteration, keeping the same topic variable (`$_`)
    last if $_ == 5; # Or break out of loop (like `break` in C-like lang.)
}

# The "pointy block" syntax isn't specific to the `for` loop. It's just a way
# to express a block in Raku.
sub long-computation { "Finding factors of large primes" }
if long-computation() -> $result {
    say "The result is $result.";
}

####################################################
# 5. Operators
####################################################

=begin comment
Since Perl languages are very much operator-based languages, Raku
operators are actually just funny-looking subroutines, in syntactic
categories, like infix:<+> (addition) or prefix:<!> (bool not).

The categories are:
    - "prefix": before (like `!` in `!True`).
    - "postfix": after (like `++` in `$a++`).
    - "infix": in between (like `*` in `4 * 3`).
    - "circumfix": around (like `[`-`]` in `[1, 2]`).
    - "post-circumfix": around, after another term (like `{`-`}` in
                   `%hash{'key'}`)

The associativity and precedence list are explained below.

Alright, you're set to go!

=end comment

#
# 5.1 Equality Checking
#

# `==` is numeric comparison
say 3 == 4; # OUTPUT: «False␤»
say 3 != 4; # OUTPUT: «True␤»

# `eq` is string comparison
say 'a' eq 'b';  # OUTPUT: «False␤»
say 'a' ne 'b';  # OUTPUT: «True␤», not equal
say 'a' !eq 'b'; # OUTPUT: «True␤», same as above

# `eqv` is canonical equivalence (or "deep equality")
say (1, 2) eqv (1, 3); # OUTPUT: «False␤»
say (1, 2) eqv (1, 2); # OUTPUT: «True␤»
say Int === Int;       # OUTPUT: «True␤»

# `~~` is the smart match operator which aliases the left hand side to $_ and
# then evaluates the right hand side.
# Here are some common comparison semantics:

# String or numeric equality
say 'Foo' ~~ 'Foo'; # OUTPUT: «True␤», if strings are equal.
say 12.5 ~~ 12.50;  # OUTPUT: «True␤», if numbers are equal.

# Regex - For matching a regular expression against the left side.
# Returns a `Match` object, which evaluates as True if regexp matches.
my $obj = 'abc' ~~ /a/;
say $obj;       # OUTPUT: «｢a｣␤»
say $obj.WHAT;  # OUTPUT: «(Match)␤»

# Hashes
say 'key' ~~ %hash; # OUTPUT: «True␤», if key exists in hash.

# Type - Checks if left side "is of type" (can check superclasses and roles).
say 1 ~~ Int;       # OUTPUT: «True␤»

# Smart-matching against a boolean always returns that boolean (and will warn).
say 1 ~~ True;        # OUTPUT: «True␤», smartmatch against True always matches
say False.so ~~ True; # OUTPUT: «True␤», use .so for truthiness

# General syntax is `$arg ~~ &bool-returning-function;`. For a complete list
# of combinations, refer to the table at:
# https://docs.raku.org/language/operators#index-entry-smartmatch_operator

# Of course, you also use `<`, `<=`, `>`, `>=` for numeric comparison.
# Their string equivalent are also available: `lt`, `le`, `gt`, `ge`.
say 3 > 4;       # OUTPUT: «False␤»
say 3 >= 4;      # OUTPUT: «False␤»
say 3 < 4;       # OUTPUT: «True␤»
say 3 <= 4;      # OUTPUT: «True␤»
say 'a' gt 'b';  # OUTPUT: «False␤»
say 'a' ge 'b';  # OUTPUT: «False␤»
say 'a' lt 'b';  # OUTPUT: «True␤»
say 'a' le 'b';  # OUTPUT: «True␤»

#
# 5.2 Range constructor
#

say 3 .. 7;          # OUTPUT: «3..7␤»,   both included.
say 3 ..^ 7;         # OUTPUT: «3..^7␤»,  exclude right endpoint.
say 3 ^.. 7;         # OUTPUT: «3^..7␤»,  exclude left endpoint.
say 3 ^..^ 7;        # OUTPUT: «3^..^7␤», exclude both endpoints.

# The range 3 ^.. 7 is similar like 4 .. 7 when we only consider integers.
# But when we consider decimals:

say 3.5 ~~ 4 .. 7;	 # OUTPUT: «False␤»
say 3.5 ~~ 3 ^.. 7;	 # OUTPUT: «True␤»,

# This is because the range `3 ^.. 7` only excludes anything strictly
# equal to 3. Hence, it contains decimals greater than 3. This could
# mathematically be described as 3.5 ∈ (3,7] or in set notation,
# 3.5 ∈ { x | 3 < x ≤ 7 }.

say 3 ^.. 7 ~~ 4 .. 7; # OUTPUT: «False␤»

# This also works as a shortcut for `0..^N`:
say ^10;             # OUTPUT: «^10␤», which means 0..^10

# This also allows us to demonstrate that Raku has lazy/infinite arrays,
# using the Whatever Star:
my @natural = 1..*; # 1 to Infinite! Equivalent to `1..Inf`.

# You can pass ranges as subscripts and it'll return an array of results.
say @natural[^10]; # OUTPUT: «1 2 3 4 5 6 7 8 9 10␤», doesn't run out of memory!

# NOTE: when reading an infinite list, Raku will "reify" the elements
# it needs, then keep them in memory. They won't be calculated more than once.
# It also will never calculate more elements than that are needed.

# An array subscript can also be a closure. It'll be called with the array's
# length as the argument. The following two examples are equivalent:
say join(' ', @array[15..*]);            # OUTPUT: «15 16 17 18 19␤»
say join(' ', @array[-> $n { 15..$n }]); # OUTPUT: «15 16 17 18 19␤»

# NOTE: if you try to do either of those with an infinite array, you'll
# trigger an infinite loop (your program won't finish).

# You can use that in most places you'd expect, even when assigning to an array:
my @numbers = ^20;

# Here the numbers increase by 6, like an arithmetic sequence; more on the
# sequence (`...`) operator later.
my @seq =  3, 9 ... * > 95;  # 3 9 15 21 27 [...] 81 87 93 99;

# In this example, even though the sequence is infinite, only the 15
# needed values will be calculated.
@numbers[5..*] = 3, 9 ... *;
say @numbers; # OUTPUT: «0 1 2 3 4 3 9 15 21 [...] 81 87␤», only 20 values

#
# 5.3 and (&&), or (||)
#

# Here `and` calls `.Bool` on both 3 and 4 and gets `True` so it returns
# 4 since both are `True`.
say (3 and 4);     # OUTPUT: «4␤», which is truthy.
say (3 and 0);     # OUTPUT: «0␤»
say (0 and 4);     # OUTPUT: «0␤»

# Here `or` calls `.Bool` on `0` and `False` which are both `False`
# so it returns `False` since both are `False`.
say (0 or False); # OUTPUT: «False␤».

# Both `and` and `or` have tighter versions which also shortcut circuits.
# They're `&&` and `||` respectively.

# `&&` returns the first operand that evaluates to `False`. Otherwise,
# it returns the last operand.
my ($a, $b, $c, $d, $e) = 1, 0, False, True, 'pi';
say $a && $b && $c; # OUTPUT: «0␤», the first falsey value
say $a && $b && $c; # OUTPUT: «False␤», the first falsey value
say $a && $d && $e; # OUTPUT: «pi␤», last operand since everything before is truthy

# `||` returns the first argument that evaluates to `True`.
say $b || $a || $d; # OUTPUT: «1␤»
say $e || $d || $a; # OUTPUT: «pi␤»

# And because you're going to want them, you also have compound assignment
# operators:
$a *= 2;        # multiply and assignment. Equivalent to $a = $a * 2;
$b %%= 5;       # divisible by and assignment. Equivalent to $b = $b %% 2;
$c div= 3;      # return divisor and assignment. Equivalent to $c = $c div 3;
$d mod= 4;      # return remainder and assignment. Equivalent to $d = $d mod 4;
@array .= sort; # calls the `sort` method and assigns the result back

####################################################
# 6. More on subs!
####################################################

# As we said before, Raku has *really* powerful subs. We're going
# to see a few more key concepts that make them better than in any
# other language :-).

#
# 6.1 Unpacking!
#

# Unpacking is the ability to "extract" arrays and keys
# (AKA "destructuring"). It'll work in `my`s and in parameter lists.
my ($f, $g) = 1, 2;
say $f;                  # OUTPUT: «1␤»
my ($, $, $h) = 1, 2, 3; # keep the non-interesting values anonymous (`$`)
say $h;                  # OUTPUT: «3␤»

my ($head, *@tail) = 1, 2, 3; # Yes, it's the same as with "slurpy subs"
my (*@small) = 1;

sub unpack_array( @array [$fst, $snd] ) {
  say "My first is $fst, my second is $snd! All in all, I'm @array[].";
  # (^ remember the `[]` to interpolate the array)
}
unpack_array(@tail);
# OUTPUT: «My first is 2, my second is 3! All in all, I'm 2 3.␤»

# If you're not using the array itself, you can also keep it anonymous,
# much like a scalar:
sub first-of-array( @ [$fst] ) { $fst }
first-of-array(@small); #=> 1

# However calling `first-of-array(@tail);` will throw an error ("Too many
# positional parameters passed"), which means the `@tail` has too many
# elements.

# You can also use a slurpy parameter. You could keep `*@rest` anonymous
# Here, `@rest` is `(3,)`, since `$fst` holds the `2`. This results
# since the length (.elems) of `@rest` is 1.
sub slurp-in-array(@ [$fst, *@rest]) {
    say $fst + @rest.elems;
}
slurp-in-array(@tail); # OUTPUT: «3␤»

# You could even extract on a slurpy (but it's pretty useless ;-).)
sub fst(*@ [$fst]) { # or simply: `sub fst($fst) { ... }`
    say $fst;
}
fst(1);    # OUTPUT: «1␤»

# Calling `fst(1, 2);` will throw an error ("Too many positional parameters
# passed") though. After all, the `fst` sub declares only a single positional
# parameter.

# You can also destructure hashes (and classes, which you'll learn about later).
# The syntax is basically the same as
# `%hash-name (:key($variable-to-store-value-in))`.
# The hash can stay anonymous if you only need the values you extracted.

# In order to call the function, you must supply a hash wither created with
# curly braces or with `%()` (recommended). Alternatively, you can pass
# a variable that contains a hash.

sub key-of( % (:value($val), :qua($qua)) ) {
 	say "Got value $val, $qua time" ~~
        $qua == 1 ?? '' !! 's';
}

my %foo-once = %(value => 'foo', qua => 1);
key-of({value => 'foo', qua => 2});  # OUTPUT: «Got val foo, 2 times.␤»
key-of(%(value => 'foo', qua => 0)); # OUTPUT: «Got val foo, 0 times.␤»
key-of(%foo-once);                   # OUTPUT: «Got val foo, 1 time.␤»

# The last expression of a sub is returned automatically (though you may
# indicate explicitly by using the `return` keyword, of course):
sub next-index( $n ) {
 	$n + 1;
}
my $new-n = next-index(3); # $new-n is now 4

# This is true for everything, except for the looping constructs (due to
# performance reasons): there's no reason to build a list if we're just going to
# discard all the results. If you still want to build one, you can use the
# `do` statement prefix or the `gather` prefix, which we'll see later:

sub list-of( $n ) {
 	do for ^$n { $_ }
}
my @list3 = list-of(3); #=> (0, 1, 2)

#
# 6.2 Lambdas (or anonymous subroutines)
#

# You can create a lambda by using a pointy block (`-> {}`), a
# block (`{}`) or creating a `sub` without a name.

my &lambda1 = -> $argument {
	"The argument passed to this lambda is $argument"
}

my &lambda2 = {
	"The argument passed to this lambda is $_"
}

my &lambda3 = sub ($argument) {
	"The argument passed to this lambda is $argument"
}

# Both pointy blocks and blocks are pretty much the same thing, except that
# the former can take arguments, and that the latter can be mistaken as
# a hash by the parser. That being said, blocks can declare what's known
# as placeholders parameters through the twigils `$^` (for positional
# parameters) and `$:` (for named parameters). More on them later on.

my &mult = { $^numbers * $:times }
say mult 4, :times(6); #=> «24␤»

# Both pointy blocks and blocks are quite versatile when working with functions
# that accepts other functions such as `map`, `grep`, etc. For example,
# we add 3 to each value of an array using the `map` function with a lambda:
my @nums = 1..4;
my @res1 = map -> $v { $v + 3 }, @nums; # pointy block, explicit parameter
my @res2 = map { $_ + 3 },       @nums; # block using an implicit parameter
my @res3 = map { $^val + 3 },    @nums; # block with placeholder parameter

# A sub (`sub {}`) has different semantics than a block (`{}` or `-> {}`):
# A block doesn't have a "function context" (though it can have arguments),
# which means that if you return from it, you're going to return from the
# parent function.

# Compare:
sub is-in( @array, $elem ) {
   say map({ return True if $_ == $elem }, @array);
   say 'Hi';
}

# with:
sub truthy-array( @array ) {
    say map sub ($i) { $i ?? return True !! return False }, @array;
    say 'Hi';
}

# In the `is-in` sub, the block will `return` out of the `is-in` sub once the
# condition evaluates to `True`, the loop won't be run anymore and the
# following statement won't be executed. The last statement is only executed
# if the block never returns.

# On the contrary, the `truthy-array` sub will produce an array of `True` and
# `False`, which will printed, and always execute the last execute statement.
# Thus, the `return` only returns from the anonymous `sub`

# The `anon` declarator can be used to create an anonymous sub from a
# regular subroutine. The regular sub knows its name but its symbol is
# prevented from getting installed in the lexical scope, the method table
# and everywhere else.
my $anon-sum = anon sub summation(*@a) { [+] @a }
say $anon-sum.name;     # OUTPUT: «summation␤»
say $anon-sum(2, 3, 5); # OUTPUT: «10␤»
#say summation;         # Error: Undeclared routine: ...

# You can also use the Whatever Star to create an anonymous subroutine.
# (it'll stop at the furthest operator in the current expression).
# The following is the same as `{$_ + 3 }`, `-> { $a + 3 }`,
# `sub ($a) { $a + 3 }`, or even `{$^a + 3}` (more on this later).
my @arrayplus3v0 = map * + 3, @nums;

# The following is the same as `-> $a, $b { $a + $b + 3 }`,
# `sub ($a, $b) { $a + $b + 3 }`, or `{ $^a + $^b + 3 }` (more on this later).
my @arrayplus3v1 = map * + * + 3, @nums;

say (*/2)(4); # OUTPUT: «2␤», immediately execute the Whatever function created.
say ((*+3)/5)(5); # OUTPUT: «1.6␤», it works even in parens!

# But if you need to have more than one argument (`$_`) in a block (without
# wanting to resort to `-> {}`), you can also either `$^` and `$:` which
# declared placeholder parameters or self-declared positional/named parameters.
say map { $^a + $^b + 3 }, @nums;

# which is equivalent to the following which uses a `sub`:
map sub ($a, $b) { $a + $b + 3 }, @nums;

# Placeholder parameters are sorted lexicographically so the following two
# statements are equivalent:
say sort           { $^b <=> $^a }, @nums;
say sort -> $a, $b { $b  <=> $a  }, @nums;

#
# 6.3 Multiple Dispatch
#

# Raku can decide which variant of a `sub` to call based on the type of the
# arguments, or on arbitrary preconditions, like with a type or `where`:

# with types:
multi sub sayit( Int $n ) { # note the `multi` keyword here
    say "Number: $n";
}
multi sayit( Str $s ) {     # a multi is a `sub` by default
    say "String: $s";
}
sayit "foo"; # OUTPUT: «String: foo␤»
sayit 25;    # OUTPUT: «Number: 25␤»
sayit True;  # fails at *compile time* with "calling 'sayit' will never
             # work with arguments of types ..."

# with arbitrary preconditions (remember subsets?):
multi is-big(Int $n where * > 50) { "Yes!" }    # using a closure
multi is-big(Int $n where {$_ > 50}) { "Yes!" } # similar to above
multi is-big(Int $ where 10..50)  { "Quite." }  # Using smart-matching
multi is-big(Int $) { "No" }

subset Even of Int where * %% 2;
multi odd-or-even(Even) { "Even" } # Using the type. We don't name the argument.
multi odd-or-even($) { "Odd" }     # "everything else" hence the $ variable

# You can even dispatch based on the presence of positional and named arguments:
multi with-or-without-you($with) {
    say "I wish I could but I can't";
}
multi with-or-without-you(:$with) {
    say "I can live! Actually, I can't.";
}
multi with-or-without-you {
    say "Definitely can't live.";
}

# This is very, very useful for many purposes, like `MAIN` subs (covered
# later), and even the language itself uses it in several places.

# For example, the `is` trait is actually a `multi sub` named `trait_mod:<is>`,
# and it works off that. Thus, `is rw`, is simply a dispatch to a function with
# this signature `sub trait_mod:<is>(Routine $r, :$rw!) {}`

####################################################
# 7. About types...
####################################################

# Raku is gradually typed. This means you can specify the type of your
# variables/arguments/return types, or you can omit the type annotations in
# in which case they'll default to `Any`. Obviously you get access to a few
# base types, like `Int` and `Str`. The constructs for declaring types are
# `subset`, `class`, `role`, etc. which you'll see later.

# For now, let us examine `subset` which is a "sub-type" with additional
# checks. For example, "a very big integer is an `Int` that's greater than 500".
# You can specify the type you're subtyping (by default, `Any`), and add
# additional checks with the `where` clause.
subset VeryBigInteger of Int where * > 500;

# Or the set of the whole numbers:
subset WholeNumber of Int where * >= 0;
my WholeNumber $whole-six    = 6;  # OK
#my WholeNumber $nonwhole-one = -1; # Error: type check failed...

# Or the set of Positive Even Numbers whose Mod 5 is 1. Notice we're
# using the previously defined WholeNumber subset.
subset PENFO of WholeNumber where { $_ %% 2 and $_ mod 5 == 1 };
my PENFO $yes-penfo = 36;  # OK
#my PENFO $no-penfo  = 2;  # Error: type check failed...

####################################################
# 8. Scoping
####################################################

# In Raku, unlike many scripting languages, (such as Python, Ruby, PHP),
# you must declare your variables before using them. The `my` declarator
# we've used so far uses "lexical scoping". There are a few other declarators,
# (`our`, `state`, ..., ) which we'll see later. This is called
# "lexical scoping", where in inner blocks, you can access variables from
# outer blocks.

my $file_scoped = 'Foo';
sub outer {
    my $outer_scoped = 'Bar';
    sub inner {
        say "$file_scoped $outer_scoped";
    }
    &inner; # return the function
}
outer()();  # OUTPUT: «Foo Bar␤»

# As you can see, `$file_scoped` and `$outer_scoped` were captured.
# But if we were to try and use `$outer_scoped` outside the `outer` sub,
# the variable would be undefined (and you'd get a compile time error).

####################################################
# 9. Twigils
####################################################

# There are many special `twigils` (composed sigils) in Raku. Twigils
# define a variable's scope.
# The `*` and `?` twigils work on standard variables:
#     * for dynamic variables
#     ? for compile-time variables
#
# The `!` and the `.` twigils are used with Raku's objects:
#     ! for attributes (instance attribute)
#     . for methods (not really a variable)

#
# `*` twigil: Dynamic Scope
#

# These variables use the `*` twigil to mark dynamically-scoped variables.
# Dynamically-scoped variables are looked up through the caller, not through
# the outer scope.

my $*dyn_scoped_1 = 1;
my $*dyn_scoped_2 = 10;

sub say_dyn {
    say "$*dyn_scoped_1 $*dyn_scoped_2";
}

sub call_say_dyn {
    # Defines $*dyn_scoped_1 only for this sub.
    my $*dyn_scoped_1 = 25;

    # Will change the value of the file scoped variable.
    $*dyn_scoped_2 = 100;

    # $*dyn_scoped 1 and 2 will be looked for in the call.
    say_dyn();  # OUTPUT: «25 100␤»

    # The call to `say_dyn` uses the value of $*dyn_scoped_1 from inside
    # this sub's lexical scope even though the blocks aren't nested (they're
    # call-nested).
}
say_dyn();      # OUTPUT: «1 10␤»

# Uses $*dyn_scoped_1 as defined in `call_say_dyn` even though we are calling it
# from outside.
call_say_dyn(); # OUTPUT: «25 100␤»

# We changed the value of $*dyn_scoped_2 in `call_say_dyn` so now its
# value has changed.
say_dyn();      # OUTPUT: «1 100␤»

# TODO: Add information about remaining twigils

####################################################
# 10. Object Model
####################################################

# To call a method on an object, add a dot followed by the method name:
# `$object.method`

# Classes are declared with the `class` keyword. Attributes are declared
# with the `has` keyword, and methods declared with the `method` keyword.

# Every attribute that is private uses the `!` twigil. For example: `$!attr`.
# Immutable public attributes use the `.` twigil which creates a read-only
# method named after the attribute. In fact, declaring an attribute with `.`
# is equivalent to declaring the same attribute with `!` and then creating
# a read-only method with the attribute's name. However, this is done for us
# by Raku automatically. The easiest way to remember the `$.` twigil is
# by comparing it to how methods are called.

# Raku's object model ("SixModel") is very flexible, and allows you to
# dynamically add methods, change semantics, etc... Unfortunately, these will
# not all be covered here, and you should refer to:
# https://docs.raku.org/language/objects.html.

class Human {
    has Str $.name;           # `$.name` is immutable but with an accessor method.
    has Str $.bcountry;       # Use `$!bcountry` to modify it inside the class.
	has Str $.ccountry is rw; # This attribute can be modified from outside.
	has Int $!age = 0;        # A private attribute with default value.

	method birthday {
		$!age += 1; # Add a year to human's age
	}

	method get-age {
		return $!age;
	}

	# This method is private to the class. Note the `!` before the
	# method's name.
	method !do-decoration {
    	return "$!name born in $!bcountry and now lives in $!ccountry."
  	}

  	# This method is public, just like `birthday` and `get-age`.
  	method get-info {
        # Invoking a method on `self` inside the class.
        # Use `self!priv-method` for private method.
  		say self!do-decoration;

  		# Use `self.public-method` for public method.
  		say "Age: ", self.get-age;
  	}
};

# Create a new instance of Human class.
# NOTE: Only attributes declared with the `.` twigil can be set via the
# default constructor (more later on). This constructor only accepts named
# arguments.
my $person1 = Human.new(
	name     => "Jord",
	bcountry => "Togo",
	ccountry => "Togo"
);

# Make human 10 years old.
$person1.birthday for 1..10;

say $person1.name;     # OUTPUT: «Jord␤»
say $person1.bcountry; # OUTPUT: «Togo␤»
say $person1.ccountry; # OUTPUT: «Togo␤»
say $person1.get-age;  # OUTPUT: «10␤»

# This fails, because the `has $.bcountry`is immutable. Jord can't change
# his birthplace.
# $person1.bcountry = "Mali";

# This works because the `$.ccountry` is mutable (`is rw`). Now Jord's
# current country is France.
$person1.ccountry = "France";

# Calling methods on the instance objects.
$person1.birthday;      #=> 1
$person1.get-info;      #=> Jord born in Togo and now lives in France. Age: 10
# $person1.do-decoration; # This fails since the method `do-decoration` is private.

#
# 10.1 Object Inheritance
#

# Raku also has inheritance (along with multiple inheritance). While
# methods are inherited, submethods are not. Submethods are useful for
# object construction and destruction tasks, such as `BUILD`, or methods that
# must be overridden by subtypes. We will learn about `BUILD` later on.

class Parent {
  	has $.age;
 	has $.name;

  	# This submethod won't be inherited by the Child class.
  	submethod favorite-color {
    	say "My favorite color is Blue";
  	}

  	# This method is inherited
  	method talk { say "Hi, my name is $!name" }
}

# Inheritance uses the `is` keyword
class Child is Parent {
  	method talk { say "Goo goo ga ga" }
  	# This shadows Parent's `talk` method.
  	# This child hasn't learned to speak yet!
}

my Parent $Richard .= new(age => 40, name => 'Richard');
$Richard.favorite-color;  # OUTPUT: «My favorite color is Blue␤»
$Richard.talk;            # OUTPUT: «Hi, my name is Richard␤»
# $Richard is able to access the submethod and he knows how to say his name.

my Child $Madison .= new(age => 1, name => 'Madison');
$Madison.talk;            # OUTPUT: «Goo goo ga ga␤», due to the overridden method.
# $Madison.favorite-color # does not work since it is not inherited.

# When you use `my T $var`, `$var` starts off with `T` itself in it, so you can
# call `new` on it. (`.=` is just the dot-call and the assignment operator).
# Thus, `$a .= b` is the same as `$a = $a.b`. Also note that `BUILD` (the method
# called inside `new`) will set parent's properties too, so you can pass `val =>
# 5`.

#
# 10.2 Roles, or Mixins
#

# Roles are supported too (which are called Mixins in other languages)
role PrintableVal {
  	has $!counter = 0;
  	method print {
    	say $.val;
  	}
}

# you "apply" a role (or mixin) with the `does` keyword:
class Item does PrintableVal {
  	has $.val;

    =begin comment
    When `does`-ed, a `role` literally "mixes in" the class:
    the methods and attributes are put together, which means a class
    can access the private attributes/methods of its roles (but
    not the inverse!):
    =end comment
  	method access {
  		say $!counter++;
  	}

    =begin comment
    However, this: method print {} is ONLY valid when `print` isn't a `multi`
    with the same dispatch. This means a parent class can shadow a child class's
    `multi print() {}`, but it's an error if a role does)

    NOTE: You can use a role as a class (with `is ROLE`). In this case,
    methods will be shadowed, since the compiler will consider `ROLE`
    to be a class.
    =end comment
}

####################################################
# 11. Exceptions
####################################################

# Exceptions are built on top of classes, in the package `X` (like `X::IO`).
# In Raku, exceptions are automatically 'thrown':

# open 'foo';   # OUTPUT: «Failed to open file foo: no such file or directory␤»

# It will also print out what line the error was thrown at
# and other error info.

# You can throw an exception using `die`. Here it's been commented out to
# avoid stopping the program's execution:
# die 'Error!'; # OUTPUT: «Error!␤»

# Or more explicitly (commented out too):
# X::AdHoc.new(payload => 'Error!').throw; # OUTPUT: «Error!␤»

# In Raku, `orelse` is similar to the `or` operator, except it only matches
# undefined variables instead of anything evaluating as `False`.
# Undefined values include: `Nil`, `Mu` and `Failure` as well as `Int`, `Str`
# and other types that have not been initialized to any value yet.
# You can check if something is defined or not using the defined method:
my $uninitialized;
say $uninitialized.defined; # OUTPUT: «False␤»

# When using `orelse` it will disarm the exception and alias $_ to that
# failure. This will prevent it to being automatically handled and printing
# lots of scary error messages to the screen. We can use the `exception`
# method on the `$_` variable to access the exception
open 'foo' orelse say "Something happened {.exception}";

# This also works:
open 'foo' orelse say "Something happened $_";
# OUTPUT: «Something happened Failed to open file foo: no such file or directory␤»

# Both of those above work but in case we get an object from the left side
# that is not a failure we will probably get a warning. We see below how we
# can use try` and `CATCH` to be more specific with the exceptions we catch.

#
# 11.1 Using `try` and `CATCH`
#

# By using `try` and `CATCH` you can contain and handle exceptions without
# disrupting the rest of the program. The `try` block will set the last
# exception to the special variable `$!` (known as the error variable).
# NOTE: This has no relation to $!variables seen inside class definitions.

try open 'foo';
say "Well, I tried! $!" if defined $!;
# OUTPUT: «Well, I tried! Failed to open file foo: no such file or directory␤»

# Now, what if we want more control over handling the exception?
# Unlike many other languages, in Raku, you put the `CATCH` block *within*
# the block to `try`. Similar to how the `$_` variable was set when we
# 'disarmed' the exception with `orelse`, we also use `$_` in the CATCH block.
# NOTE: The `$!` variable is only set *after* the `try` block has caught an
# exception. By default, a `try` block has a `CATCH` block of its own that
# catches any exception (`CATCH { default {} }`).

try {
    my $a = (0 %% 0);
    CATCH {
        default { say "Something happened: $_" }
    }
}
# OUTPUT: «Something happened: Attempt to divide by zero using infix:<%%>␤»

# You can redefine it using `when`s (and `default`) to handle the exceptions
# you want to catch explicitly:

try {
  open 'foo';
    CATCH {
        # In the `CATCH` block, the exception is set to the $_ variable.
        when X::AdHoc {
            say "Error: $_"
        }
        when X::Numeric::DivideByZero {
            say "Error: $_";
        }

        =begin comment
        Any other exceptions will be re-raised, since we don't have a `default`.
        Basically, if a `when` matches (or there's a `default`), the
	    exception is marked as "handled" so as to prevent its re-throw
        from the `CATCH` block. You still can re-throw the exception
        (see below) by hand.
        =end comment
        default {
            say "Any other error: $_"
        }
  }
}
# OUTPUT: «Failed to open file /dir/foo: no such file or directory␤»

# There are also some subtleties to exceptions. Some Raku subs return a
# `Failure`, which is a wrapper around an `Exception` object which is
# "unthrown". They're not thrown until you try to use the variables containing
# them unless you call `.Bool`/`.defined` on them - then they're handled.
# (the `.handled` method is `rw`, so you can mark it as `False` back yourself)
# You can throw a `Failure` using `fail`. Note that if the pragma `use fatal`
# is on, `fail` will throw an exception (like `die`).

my $value = 0/0; # We're not trying to access the value, so no problem.
try {
    say 'Value: ', $value; # Trying to use the value
    CATCH {
        default {
            say "It threw because we tried to get the fail's value!"
        }
  }
}

# There is also another kind of exception: Control exceptions. Those are "good"
# exceptions, which happen when you change your program's flow, using operators
# like `return`, `next` or `last`. You can "catch" those with `CONTROL` (not 100%
# working in Rakudo yet).

####################################################
# 12. Packages
####################################################

# Packages are a way to reuse code. Packages are like "namespaces", and any
# element of the six model (`module`, `role`, `class`, `grammar`, `subset` and
# `enum`) are actually packages. (Packages are the lowest common denominator)
# Packages are important - especially as Perl is well-known for CPAN,
# the Comprehensive Perl Archive Network.

# You can use a module (bring its declarations into scope) with `use`:
use JSON::Tiny; # if you installed Rakudo* or Panda, you'll have this module
say from-json('[1]').perl; # OUTPUT: «[1]␤»

# You should not declare packages using the `package` keyword (unlike Perl).
# Instead, use `class Package::Name::Here;` to declare a class, or if you only
# want to export variables/subs, you can use `module` instead.

# If `Hello` doesn't exist yet, it'll just be a "stub", that can be redeclared
# as something else later.
module Hello::World { # bracketed form
    # declarations here
}

# The file-scoped form which extends until the end of the file. For
# instance, `unit module Parse::Text;` will extend until of the file.

# A grammar is a package, which you could `use`. You will learn more about
# grammars in the regex section.
grammar Parse::Text::Grammar {
}

# As said before, any part of the six model is also a package.
# Since `JSON::Tiny` uses its own `JSON::Tiny::Actions` class, you can use it:
my $actions = JSON::Tiny::Actions.new;

# We'll see how to export variables and subs in the next part.

####################################################
# 13. Declarators
####################################################

# In Raku, you get different behaviors based on how you declare a variable.
# You've already seen `my` and `has`, we'll now explore the others.

# `our` - these declarations happen at `INIT` time -- (see "Phasers" below).
# It's like `my`, but it also creates a package variable. All packagish
# things such as `class`, `role`, etc. are `our` by default.

module Var::Increment {
    # NOTE: `our`-declared variables cannot be typed.
    our $our-var = 1;
    my $my-var = 22;

    our sub Inc {
        our sub available { # If you try to make inner `sub`s `our`...
                            # ... Better know what you're doing (Don't !).
            say "Don't do that. Seriously. You'll get burned.";
        }

        my sub unavailable { # `sub`s are `my`-declared by default
            say "Can't access me from outside, I'm 'my'!";
        }
        say ++$our-var; # Increment the package variable and output its value
  }

}

say $Var::Increment::our-var; # OUTPUT: «1␤», this works!
say $Var::Increment::my-var;  # OUTPUT: «(Any)␤», this will not work!

say Var::Increment::Inc;  # OUTPUT: «2␤»
say Var::Increment::Inc;  # OUTPUT: «3␤», notice how the value of $our-var was retained.

# Var::Increment::unavailable;  # OUTPUT: «Could not find symbol '&unavailable'␤»

# `constant` - these declarations happen at `BEGIN` time. You can use
# the `constant` keyword to declare a compile-time variable/symbol:
constant Pi = 3.14;
constant $var = 1;

# And if you're wondering, yes, it can also contain infinite lists.
constant why-not = 5, 15 ... *;
say why-not[^5]; # OUTPUT: «5 15 25 35 45␤»

# `state` - these declarations happen at run time, but only once. State
# variables are only initialized one time. In other languages such as C
# they exist as `static` variables.
sub fixed-rand {
    state $val = rand;
    say $val;
}
fixed-rand for ^10; # will print the same number 10 times

# Note, however, that they exist separately in different enclosing contexts.
# If you declare a function with a `state` within a loop, it'll re-create the
# variable for each iteration of the loop. See:
for ^5 -> $a {
    sub foo {
        # This will be a different value for every value of `$a`
        state $val = rand;
    }
    for ^5 -> $b {
        # This will print the same value 5 times, but only 5. Next iteration
        # will re-run `rand`.
        say foo;
    }
}

####################################################
# 14. Phasers
####################################################

# Phasers in Raku are blocks that happen at determined points of time in
# your program. They are called phasers because they mark a change in the
# phase of a program.  For example, when the program is compiled, a for loop
# runs, you leave a block, or an exception gets thrown (The `CATCH` block is
# actually a phaser!). Some of them can be used for their return values,
# some of them can't (those that can have a "[*]" in the beginning of their
# explanation text). Let's have a look!

#
# 14.1 Compile-time phasers
#
BEGIN { say "[*] Runs at compile time, as soon as possible, only once" }
CHECK { say "[*] Runs at compile time, as late as possible, only once" }

#
# 14.2 Run-time phasers
#
INIT { say "[*] Runs at run time, as soon as possible, only once" }
END  { say "Runs at run time, as late as possible, only once" }

#
# 14.3 Block phasers
#
ENTER { say "[*] Runs every time you enter a block, repeats on loop blocks" }
LEAVE {
    say "Runs every time you leave a block, even when an exception
    happened. Repeats on loop blocks."
}

PRE {
    say "Asserts a precondition at every block entry,
        before ENTER (especially useful for loops)";
    say "If this block doesn't return a truthy value,
        an exception of type X::Phaser::PrePost is thrown.";
}

# Example (commented out):
for 0..2 {
    # PRE { $_ > 1 } # OUTPUT: «Precondition '{ $_ > 1 }' failed
}

POST {
    say "Asserts a postcondition at every block exit,
        after LEAVE (especially useful for loops)";
    say "If this block doesn't return a truthy value,
        an exception of type X::Phaser::PrePost is thrown, like PRE.";
}

# Example (commented out):
for 0..2 {
    # POST { $_ < 1 } # OUTPUT: «Postcondition '{ $_ < 1 }' failed
}

#
# 14.4 Block/exceptions phasers
#
{
    KEEP { say "Runs when you exit a block successfully
                (without throwing an exception)" }
    UNDO { say "Runs when you exit a block unsuccessfully
                (by throwing an exception)" }
}

#
# 14.5 Loop phasers
#
for ^5 {
  FIRST { say "[*] The first time the loop is run, before ENTER" }
  NEXT  { say "At loop continuation time, before LEAVE" }
  LAST  { say "At loop termination time, after LEAVE" }
}

#
# 14.6 Role/class phasers
#
COMPOSE {
    say "When a role is composed into a class. /!\ NOT YET IMPLEMENTED"
}

# They allow for cute tricks or clever code...:
say "This code took " ~ (time - CHECK time) ~ "s to compile";

# ... or clever organization:
class DB {
    method start-transaction { say "Starting transaction!" }
    method commit            { say "Committing transaction..." }
    method rollback          { say "Something went wrong. Rolling back!" }
}

sub do-db-stuff {
    my DB $db .= new;
  	$db.start-transaction; # start a new transaction
  	KEEP $db.commit;       # commit the transaction if all went well
  	UNDO $db.rollback;     # or rollback if all hell broke loose
}

do-db-stuff();

####################################################
# 15. Statement prefixes
####################################################

# Those act a bit like phasers: they affect the behavior of the following
# code. Though, they run in-line with the executable code, so they're in
# lowercase. (`try` and `start` are theoretically in that list, but explained
# elsewhere) NOTE: all of these (except start) don't need explicit curly
# braces `{` and `}`.

#
# 15.1 `do` - It runs a block or a statement as a term.
#

# Normally you cannot use a statement as a value (or "term"). `do` helps
# us do it. With `do`, an `if`, for example, becomes a term returning a value.
=for comment :reason<this fails since `if` is a statement>
my $value = if True { 1 }

# this works!
my $get-five = do if True { 5 }

#
# 15.1 `once` - makes sure a piece of code only runs once.
#
for ^5 {
	once say 1
};
# OUTPUT: «1␤», only prints ... once

# Similar to `state`, they're cloned per-scope.
for ^5 {
	sub { once say 1 }()
};
# OUTPUT: «1 1 1 1 1␤», prints once per lexical scope.

#
# 15.2 `gather` - co-routine thread.
#

# The `gather` constructs allows us to `take` several values from an array/list,
# much like `do`.
say gather for ^5 {
    take $_ * 3 - 1;
    take $_ * 3 + 1;
}
# OUTPUT: «-1 1 2 4 5 7 8 10 11 13␤»

say join ',', gather if False {
    take 1;
    take 2;
    take 3;
}
# Doesn't print anything.

#
# 15.3 `eager` - evaluates a statement eagerly (forces eager context).

# Don't try this at home. This will probably hang for a while (and might crash)
# so commented out.
# eager 1..*;

# But consider, this version which doesn't print anything
constant thricev0 = gather for ^3 { say take $_ };
# to:
constant thricev1 = eager gather for ^3 { say take $_ }; # OUTPUT: «0 1 2␤»

####################################################
# 16. Iterables
####################################################

# Iterables are objects that can be iterated over for things such as
# the `for` construct.

#
# 16.1 `flat` - flattens iterables.
#
say (1, 10, (20, 10) );      # OUTPUT: «(1 10 (20 10))␤»,  notice how nested
                             # lists are preserved
say (1, 10, (20, 10) ).flat; # OUTPUT: «(1 10 20 10)␤», now the iterable is flat

#
# 16.2 `lazy` - defers actual evaluation until value is fetched by forcing lazy context.
#
my @lazy-array = (1..100).lazy;
say @lazy-array.is-lazy; # OUTPUT: «True␤», check for laziness with the `is-lazy` method.

say @lazy-array;         # OUTPUT: «[...]␤», List has not been iterated on!

# This works and will only do as much work as is needed.
for @lazy-array { .print };

# (**TODO** explain that gather/take and map are all lazy)

#
# 16.3 `sink` - an `eager` that discards the results by forcing sink context.
#
constant nilthingie = sink for ^3 { .say } #=> 0 1 2
say nilthingie.perl;                       # OUTPUT: «Nil␤»

#
# 16.4 `quietly` - suppresses warnings in blocks.
#
quietly { warn 'This is a warning!' }; # No output

####################################################
# 17. More operators thingies!
####################################################

# Everybody loves operators! Let's get more of them.

# The precedence list can be found here:
# https://docs.raku.org/language/operators#Operator_Precedence
# But first, we need a little explanation about associativity:

#
# 17.1 Binary operators
#

my ($p, $q, $r) = (1, 2, 3);

# Given some binary operator § (not a Raku-supported operator), then:

# $p § $q § $r; # with a left-associative  §, this is ($p § $q) § $r
# $p § $q § $r; # with a right-associative §, this is $p § ($q § $r)
# $p § $q § $r; # with a non-associative   §, this is illegal
# $p § $q § $r; # with a chain-associative §, this is ($p § $q) and ($q § $r)§
# $p § $q § $r; # with a list-associative  §, this is `infix:<>`

#
# 17.2 Unary operators
#

# Given some unary operator § (not a Raku-supported operator), then:
# §$p§ # with left-associative  §, this is (§$p)§
# §$p§ # with right-associative §, this is §($p§)
# §$p§ # with non-associative   §, this is illegal

#
# 17.3 Create your own operators!
#

# Okay, you've been reading all of that, so you might want to try something
# more exciting?! I'll tell you a little secret (or not-so-secret):
# In Raku, all operators are actually just funny-looking subroutines.

# You can declare an operator just like you declare a sub. In the following
# example, `prefix` refers to the operator categories (prefix, infix, postfix,
# circumfix, and post-circumfix).
sub prefix:<win>( $winner ) {
	say "$winner Won!";
}
win "The King"; # OUTPUT: «The King Won!␤»

# you can still call the sub with its "full name":
say prefix:<!>(True);      # OUTPUT: «False␤»
prefix:<win>("The Queen"); # OUTPUT: «The Queen Won!␤»

sub postfix:<!>( Int $n ) {
    [*] 2..$n; # using the reduce meta-operator... See below ;-)!
}
say 5!; # OUTPUT: «120␤»

# Postfix operators ('after') have to come *directly* after the term.
# No whitespace. You can use parentheses to disambiguate, i.e. `(5!)!`

sub infix:<times>( Int $n, Block $r ) { # infix ('between')
    for ^$n {
        # You need the explicit parentheses to call the function in `$r`,
        # else you'd be referring at the code object itself, like with `&r`.
        $r();
    }
}
3 times -> { say "hello" }; # OUTPUT: «hello␤hello␤hello␤»

# It's recommended to put spaces around your infix operator calls.

# For circumfix and post-circumfix ones
multi circumfix:<[ ]>( Int $n ) {
    $n ** $n
}
say [5]; # OUTPUT: «3125␤»

# Circumfix means 'around'. Again, no whitespace.

multi postcircumfix:<{ }>( Str $s, Int $idx ) {
    $s.substr($idx, 1);
}
say "abc"{1}; # OUTPUT: «b␤», after the term `"abc"`, and around the index (1)

# Post-circumfix is 'after a term, around something'

# This really means a lot -- because everything in Raku uses this.
# For example, to delete a key from a hash, you use the `:delete` adverb
# (a simple named argument underneath). For instance, the following statements
# are equivalent.
my %person-stans =
    'Giorno Giovanna'  => 'Gold Experience',
    'Bruno Bucciarati' => 'Sticky Fingers';
my $key = 'Bruno Bucciarati';
%person-stans{$key}:delete;
postcircumfix:<{ }>( %person-stans, 'Giorno Giovanna', :delete );
# (you can call operators like this)

# It's *all* using the same building blocks! Syntactic categories
# (prefix infix ...), named arguments (adverbs), ..., etc. used to build
# the language - are available to you. Obviously, you're advised against
# making an operator out of *everything* -- with great power comes great
# responsibility.

#
# 17.4 Meta operators!
#

# Oh boy, get ready!. Get ready, because we're delving deep into the rabbit's
# hole, and you probably won't want to go back to other languages after
# reading this. (I'm guessing you don't want to go back at this point but
# let's continue, for the journey is long and enjoyable!).

# Meta-operators, as their name suggests, are *composed* operators. Basically,
# they're operators that act on another operators.

# The reduce meta-operator is a prefix meta-operator that takes a binary
# function and one or many lists. If it doesn't get passed any argument,
# it either returns a "default value" for this operator (a meaningless value)
# or `Any` if there's none (examples below). Otherwise, it pops an element
# from the list(s) one at a time, and applies the binary function to the last
# result (or the first element of a list) and the popped element.

# To sum a list, you could use the reduce meta-operator with `+`, i.e.:
say [+] 1, 2, 3; # OUTPUT: «6␤», equivalent to (1+2)+3.

# To multiply a list
say [*] 1..5; # OUTPUT: «120␤», equivalent to ((((1*2)*3)*4)*5).

# You can reduce with any operator, not just with mathematical ones.
# For example, you could reduce with `//` to get first defined element
# of a list:
say [//] Nil, Any, False, 1, 5;  # OUTPUT: «False␤»
                                 # (Falsey, but still defined)
# Or with relational operators, i.e., `>` to check elements of a list
# are ordered accordingly:
say [>] 234, 156, 6, 3, -20; # OUTPUT: «True␤»

# Default value examples:
say [*] (); # OUTPUT: «1␤», empty product
say [+] (); # OUTPUT: «0␤», empty sum
say [//];   # OUTPUT: «(Any)␤»
            # There's no "default value" for `//`.

# You can also use it with a function you made up,
# You can also surround  using double brackets:
sub add($a, $b) { $a + $b }
say [[&add]] 1, 2, 3; # OUTPUT: «6␤»

# The zip meta-operator is an infix meta-operator that also can be used as a
# "normal" operator. It takes an optional binary function (by default, it
# just creates a pair), and will pop one value off of each array and call
# its binary function on these until it runs out of elements. It returns an
# array with all of these new elements.
say (1, 2) Z (3, 4); # OUTPUT: «((1, 3), (2, 4))␤»
say 1..3 Z+ 4..6;    # OUTPUT: «(5, 7, 9)␤»

# Since `Z` is list-associative (see the list above), you can use it on more
# than one list.
(True, False) Z|| (False, False) Z|| (False, False); # (True, False)

# And, as it turns out, you can also use the reduce meta-operator with it:
[Z||] (True, False), (False, False), (False, False); # (True, False)

# And to end the operator list:

# The sequence operator (`...`) is one of Raku's most powerful features:
# It's composed by the list (which might include a closure) you want Raku to
# deduce from on the left and a value (or either a predicate or a Whatever Star
# for a lazy infinite list) on the right that states when to stop.

# Basic arithmetic sequence
my @listv0 = 1, 2, 3...10;

# This dies because Raku can't figure out the end
# my @list = 1, 3, 6...10;

# As with ranges, you can exclude the last element (the iteration ends when
# the predicate matches).
my @listv1 = 1, 2, 3...^10;

# You can use a predicate (with the Whatever Star).
my @listv2 = 1, 3, 9...* > 30;

# Equivalent to the example above but using a block here.
my @listv3 = 1, 3, 9 ... { $_ > 30 };

# Lazy infinite list of fibonacci sequence, computed using a closure!
my @fibv0 = 1, 1, *+* ... *;

# Equivalent to the above example but using a pointy block.
my @fibv1 = 1, 1, -> $a, $b { $a + $b } ... *;

# Equivalent to the above example but using a block with placeholder parameters.
my @fibv2 = 1, 1, { $^a + $^b } ... *;

# In the examples with explicit parameters (i.e., $a and $b), $a and $b
# will always take the previous values, meaning that for the Fibonacci sequence,
# they'll start with $a = 1 and $b = 1 (values we set by hand), then $a = 1
# and $b = 2 (result from previous $a + $b), and so on.

# In the example we use a range as an index to access the sequence. However,
# it's worth noting that for ranges, once reified, elements aren't re-calculated.
# That's why, for instance, `@primes[^100]` will take a long time the first
# time you print it but then it will be instantaneous.
say @fibv0[^10]; # OUTPUT: «1 1 2 3 5 8 13 21 34 55␤»

####################################################
# 18. Regular Expressions
####################################################

# I'm sure a lot of you have been waiting for this one. Well, now that you know
# a good deal of Raku already, we can get started. First off, you'll have to
# forget about "PCRE regexps" (perl-compatible regexps).

# IMPORTANT: Don't skip them because you know PCRE. They're different. Some
# things are the same (like `?`, `+`, and `*`), but sometimes the semantics
# change (`|`). Make sure you read carefully, because you might trip over a
# new behavior.

# Raku has many features related to RegExps. After all, Rakudo parses itself.
# We're first going to look at the syntax itself, then talk about grammars
# (PEG-like), differences between `token`, `regex` and `rule` declarators,
# and some more. Side note: you still have access to PCRE regexps using the
# `:P5` modifier which we won't be discussing this in this tutorial, though.

# In essence, Raku natively implements PEG ("Parsing Expression Grammars").
# The pecking order for ambiguous parses is determined by a multi-level
# tie-breaking test:
#  - Longest token matching: `foo\s+` beats `foo` (by 2 or more positions)
#  - Longest literal prefix: `food\w*` beats `foo\w*` (by 1)
#  - Declaration from most-derived to less derived grammars
#    (grammars are actually classes)
#  - Earliest declaration wins
say so 'a' ~~ /a/;   # OUTPUT: «True␤»
say so 'a' ~~ / a /; # OUTPUT: «True␤», more readable with some spaces!

# In all our examples, we're going to use the smart-matching operator against
# a regexp. We're converting the result using `so` to a Boolean value because,
# in fact, it's returning a `Match` object. They know how to respond to list
# indexing, hash indexing, and return the matched string. The results of the
# match are available in the `$/` variable (implicitly lexically-scoped). You
# can also use the capture variables which start at 0: `$0`, `$1', `$2`...

# You can also note that `~~` does not perform start/end checking, meaning
# the regexp can be matched with just one character of the string. We'll
# explain later how you can do it.

# In Raku, you can have any alphanumeric as a literal, everything else has
# to be escaped by using a backslash or quotes.
say so 'a|b' ~~ / a '|' b /; # OUTPUT: «True␤», it wouldn't mean the same
                             # thing if `|` wasn't escaped.
say so 'a|b' ~~ / a \| b /;  # OUTPUT: «True␤», another way to escape it.

# The whitespace in a regex is actually not significant, unless you use the
# `:s` (`:sigspace`, significant space) adverb.
say so 'a b c' ~~ / a  b  c /; #=> `False`, space is not significant here!
say so 'a b c' ~~ /:s a b c /; #=> `True`, we added the modifier `:s` here.

# If we use only one space between strings in a regex, Raku will warn us
# about space being not signicant in the regex:
say so 'a b c' ~~ / a b c /;   # OUTPUT: «False␤»
say so 'a b c' ~~ / a  b  c /; # OUTPUT: «False»

# NOTE: Please use quotes or `:s` (`:sigspace`) modifier (or, to suppress this
# warning, omit the space, or otherwise change the spacing). To fix this and make
# the spaces less ambiguous, either use at least two spaces between strings
# or use the `:s` adverb.

# As we saw before, we can embed the `:s` inside the slash delimiters, but we
# can also put it outside of them if we specify `m` for 'match':
say so 'a b c' ~~ m:s/a  b  c/; # OUTPUT: «True␤»

# By using `m` to specify 'match', we can also use other delimiters:
say so 'abc' ~~ m{a  b  c};     # OUTPUT: «True␤»
say so 'abc' ~~ m[a  b  c];     # OUTPUT: «True␤»

# `m/.../` is equivalent to `/.../`:
say 'raku' ~~ m/raku/; # OUTPUT: «True␤»
say 'raku' ~~ /raku/;  # OUTPUT: «True␤»

# Use the `:i` adverb to specify case insensitivity:
say so 'ABC' ~~ m:i{a  b  c};   # OUTPUT: «True␤»

# However, whitespace is important as for how modifiers are applied
# (which you'll see just below) ...

#
# 18.1 Quantifiers - `?`, `+`, `*` and `**`.
#

# `?` - zero or one match
say so 'ac' ~~ / a  b  c /;   # OUTPUT: «False␤»
say so 'ac' ~~ / a  b?  c /;  # OUTPUT: «True␤», the "b" matched 0 times.
say so 'abc' ~~ / a  b?  c /; # OUTPUT: «True␤», the "b" matched 1 time.

# ... As you read before, whitespace is important because it determines which
# part of the regex is the target of the modifier:
say so 'def' ~~ / a  b  c? /; # OUTPUT: «False␤», only the "c" is optional
say so 'def' ~~ / a  b?  c /; # OUTPUT: «False␤», whitespace is not significant
say so 'def' ~~ / 'abc'? /;   # OUTPUT: «True␤»,  the whole "abc" group is optional

# Here (and below) the quantifier applies only to the "b"

# `+` - one or more matches
say so 'ac' ~~ / a  b+  c /;     # OUTPUT: «False␤», `+` wants at least one 'b'
say so 'abc' ~~ / a  b+  c /;    # OUTPUT: «True␤», one is enough
say so 'abbbbc' ~~ / a  b+  c /; # OUTPUT: «True␤», matched 4 "b"s

# `*` - zero or more matches
say so 'ac' ~~ / a  b*  c /;     # OUTPUT: «True␤», they're all optional
say so 'abc' ~~ / a  b*  c /;    # OUTPUT: «True␤»
say so 'abbbbc' ~~ / a  b*  c /; # OUTPUT: «True␤»
say so 'aec' ~~ / a  b*  c /;    # OUTPUT: «False␤», "b"(s) are optional, not replaceable.

# `**` - (Unbound) Quantifier
# If you squint hard enough, you might understand why exponentiation is used
# for quantity.
say so 'abc' ~~ / a  b**1  c /;         # OUTPUT: «True␤», exactly one time
say so 'abc' ~~ / a  b**1..3  c /;      # OUTPUT: «True␤», one to three times
say so 'abbbc' ~~ / a  b**1..3  c /;    # OUTPUT: «True␤»
say so 'abbbbbbc' ~~ / a  b**1..3  c /; # OUTPUT: «Fals␤», too much
say so 'abbbbbbc' ~~ / a  b**3..*  c /; # OUTPUT: «True␤», infinite ranges are ok

#
# 18.2 `<[]>` - Character classes
#

# Character classes are the equivalent of PCRE's `[]` classes, but they use a
# more raku-ish syntax:
say 'fooa' ~~ / f <[ o a ]>+ /;  # OUTPUT: «fooa␤»

# You can use ranges (`..`):
say 'aeiou' ~~ / a <[ e..w ]> /; # OUTPUT: «ae␤»

# Just like in normal regexes, if you want to use a special character, escape
# it (the last one is escaping a space which would be equivalent to using
# ' '):
say 'he-he !' ~~ / 'he-' <[ a..z \! \  ]> + /; # OUTPUT: «he-he !␤»

# You'll get a warning if you put duplicate names (which has the nice effect
# of catching the raw quoting):
'he he' ~~ / <[ h e ' ' ]> /;
# Warns "Repeated character (') unexpectedly found in character class"

# You can also negate character classes... (`<-[]>` equivalent to `[^]` in PCRE)
say so 'foo' ~~ / <-[ f o ]> + /; # OUTPUT: «False␤»

# ... and compose them:
# any letter except "f" and "o"
say so 'foo' ~~ / <[ a..z ] - [ f o ]> + /;   # OUTPUT: «False␤»

# no letter except "f" and "o"
say so 'foo' ~~ / <-[ a..z ] + [ f o ]> + /;  # OUTPUT: «True␤»

# the + doesn't replace the left part
say so 'foo!' ~~ / <-[ a..z ] + [ f o ]> + /; # OUTPUT: «True␤»

#
# 18.3 Grouping and capturing
#

# Group: you can group parts of your regexp with `[]`. Unlike PCRE's `(?:)`,
# these groups are *not* captured.
say so 'abc' ~~ / a [ b ] c /;  # OUTPUT: «True␤», the grouping does nothing
say so 'foo012012bar' ~~ / foo [ '01' <[0..9]> ] + bar /; # OUTPUT: «True␤»

# The previous line returns `True`. The regex matches "012" one or more time
# (achieved by the the `+` applied to the group).

# But this does not go far enough, because we can't actually get back what
# we matched.

# Capture: The results of a regexp can be *captured* by using parentheses.
say so 'fooABCABCbar' ~~ / foo ( 'A' <[A..Z]> 'C' ) + bar /; # OUTPUT: «True␤»
# (using `so` here, see `$/` below)

# So, starting with the grouping explanations. As we said before, our `Match`
# object is stored inside the `$/` variable:
say $/;    # Will either print the matched object or `Nil` if nothing matched.

# As we also said before, it has array indexing:
say $/[0]; # OUTPUT: «｢ABC｣ ｢ABC｣␤»,

# The corner brackets (｢..｣) represent (and are) `Match` objects. In the
# previous example, we have an array of them.

say $0;    # The same as above.

# Our capture is `$0` because it's the first and only one capture in the
# regexp. You might be wondering why it's an array, and the answer is simple:
# Some captures (indexed using `$0`, `$/[0]` or a named one) will be an array
# if and only if they can have more than one element. Thus any capture with
# `*`, `+` and `**` (whatever the operands), but not with `?`.
# Let's use examples to see that:

# NOTE: We quoted A B C to demonstrate that the whitespace between them isn't
# significant. If we want the whitespace to *be* significant there, we can use the
# `:sigspace` modifier.
say so 'fooABCbar' ~~ / foo ( "A" "B" "C" )? bar /; # OUTPUT: «True␤»
say $/[0];   # OUTPUT: «｢ABC｣␤»
say $0.WHAT; # OUTPUT: «(Match)␤»
             # There can't be more than one, so it's only a single match object.

say so 'foobar' ~~ / foo ( "A" "B" "C" )? bar /;    # OUTPUT: «True␤»
say $0.WHAT; # OUTPUT: «(Any)␤», this capture did not match, so it's empty.

say so 'foobar' ~~ / foo ( "A" "B" "C" ) ** 0..1 bar /; #=> OUTPUT: «True␤»
say $0.WHAT; # OUTPUT: «(Array)␤», A specific quantifier will always capture
             # an Array, be a range or a specific value (even 1).

# The captures are indexed per nesting. This means a group in a group will be
# nested under its parent group: `$/[0][0]`, for this code:
'hello-~-world' ~~ / ( 'hello' ( <[ \- \~ ]> + ) ) 'world' /;
say $/[0].Str;    # OUTPUT: «hello~␤»
say $/[0][0].Str; # OUTPUT: «~␤»

# This stems from a very simple fact: `$/` does not contain strings, integers
# or arrays, it only contains `Match` objects. These contain the `.list`, `.hash`
# and `.Str` methods but you can also just use `match<key>` for hash access
# and `match[idx]` for array access.

# In the following example, we can see `$_` is a list of `Match` objects.
# Each of them contain a wealth of information: where the match started/ended,
# the "ast" (see actions later), etc. You'll see named capture below with
# grammars.
say $/[0].list.perl; # OUTPUT: «(Match.new(...),).list␤»

# Alternation - the `or` of regexes
# WARNING: They are DIFFERENT from PCRE regexps.
say so 'abc' ~~ / a [ b | y ] c /; # OUTPUT: «True␤», Either "b" or "y".
say so 'ayc' ~~ / a [ b | y ] c /; # OUTPUT: «True␤», Obviously enough...

# The difference between this `|` and the one you're used to is
# LTM ("Longest Token Matching") strategy. This means that the engine will
# always try to match as much as possible in the string.
say 'foo' ~~ / fo | foo /; # OUTPUT: «foo», instead of `fo`, because it's longer.

# To decide which part is the "longest", it first splits the regex in two parts:
#
#     * The "declarative prefix" (the part that can be statically analyzed)
#     which includes alternations (`|`), conjunctions (`&`), sub-rule calls (not
#     yet introduced), literals, characters classes and quantifiers.
#
#     * The "procedural part" includes everything else: back-references,
#     code assertions, and other things that can't traditionally be represented
#     by normal regexps.

# Then, all the alternatives are tried at once, and the longest wins.

# Examples:
# DECLARATIVE | PROCEDURAL
/ 'foo' \d+     [ <subrule1> || <subrule2> ] /;

# DECLARATIVE (nested groups are not a problem)
/ \s* [ \w & b ] [ c | d ] /;

# However, closures and recursion (of named regexes) are procedural.
# There are also more complicated rules, like specificity (literals win
# over character classes).

# NOTE: The alternation in which all the branches are tried in order
# until the first one matches still exists, but is now spelled `||`.
say 'foo' ~~ / fo || foo /; # OUTPUT: «fo␤», in this case.

####################################################
# 19. Extra: the MAIN subroutine
####################################################

# The `MAIN` subroutine is called when you run a Raku file directly. It's
# very powerful, because Raku actually parses the arguments and pass them
# as such to the sub. It also handles named argument (`--foo`) and will even
# go as far as to autogenerate a `--help` flag.

sub MAIN($name) {
    say "Hello, $name!";
}
# Supposing the code above is in file named cli.raku, then running in the command
# line (e.g., $ raku cli.raku) produces:
# Usage:
# cli.raku <name>

# And since MAIN is a regular Raku sub, you can have multi-dispatch:
# (using a `Bool` for the named argument so that we can do `--replace`
# instead of `--replace=1`. The presence of `--replace` indicates truthness
# while its absence falseness). For example:

    # convert to IO object to check the file exists
    =begin comment
    subset File of Str where *.IO.d;

    multi MAIN('add', $key, $value, Bool :$replace) { ... }
    multi MAIN('remove', $key) { ... }
    multi MAIN('import', File, Str :$as) { ... } # omitting parameter name
    =end comment

# Thus $ raku cli.raku produces:
# Usage:
#   cli.raku [--replace] add <key> <value>
#   cli.raku remove <key>
#   cli.raku [--as=<Str>] import <File>

# As you can see, this is *very* powerful. It even went as far as to show inline
# the constants (the type is only displayed if the argument is `$`/is named).

####################################################
# 20. APPENDIX A:
####################################################

# It's assumed by now you know the Raku basics. This section is just here to
# list some common operations, but which are not in the "main part" of the
# tutorial to avoid bloating it up.

#
# 20.1 Operators
#

# Sort comparison - they return one value of the `Order` enum: `Less`, `Same`
# and `More` (which numerify to -1, 0 or +1 respectively).
say 1 <=> 4;     # OUTPUT: «More␤»,   sort comparison for numerics
say 'a' leg 'b'; # OUTPUT: «Lessre␤», sort comparison for string
say 1 eqv 1;     # OUTPUT: «Truere␤», sort comparison using eqv semantics
say 1 eqv 1.0;   # OUTPUT: «False␤»

# Generic ordering
say 3 before 4;    # OUTPUT: «True␤»
say 'b' after 'a'; # OUTPUT: «True␤»

# Short-circuit default operator - similar to `or` and `||`, but instead
# returns the first *defined* value:
say Any // Nil // 0 // 5;        # OUTPUT: «0␤»

# Short-circuit exclusive or (XOR) - returns `True` if one (and only one) of
# its arguments is true
say True ^^ False;               # OUTPUT: «True␤»

# Flip flops. These operators (`ff` and `fff`, equivalent to P5's `..`
# and `...`) are operators that take two predicates to test: They are `False`
# until their left side returns `True`, then are `True` until their right
# side returns `True`. Similar to ranges, you can exclude the iteration when
# it become `True`/`False` by using `^` on either side. Let's start with an
# example :

for <well met young hero we shall meet later> {
    # by default, `ff`/`fff` smart-match (`~~`) against `$_`:
    if 'met' ^ff 'meet' { # Won't enter the if for "met"
        .say              # (explained in details below).
    }

    if rand == 0 ff rand == 1 { # compare variables other than `$_`
        say "This ... probably will never run ...";
    }
}

# This will print "young hero we shall meet" (excluding "met"): the flip-flop
# will start returning `True` when it first encounters "met" (but will still
# return `False` for "met" itself, due to the leading `^` on `ff`), until it
# sees "meet", which is when it'll start returning `False`.

# The difference between `ff` (awk-style) and `fff` (sed-style) is that `ff`
# will test its right side right when its left side changes to `True`, and can
# get back to `False` right away (*except* it'll be `True` for the iteration
# that matched) while `fff` will wait for the next iteration to try its right
# side, once its left side changed:

# The output is due to the right-hand-side being tested directly (and returning
# `True`). "B"s are printed since it matched that time (it just went back to
# `False` right away).
.say if 'B' ff 'B' for <A B C B A>; # OUTPUT: «B B␤»,

# In this case the right-hand-side wasn't tested until `$_` became "C"
# (and thus did not match instantly).
.say if 'B' fff 'B' for <A B C B A>; #=> «B C B␤»,

# A flip-flop can change state as many times as needed:
for <test start print it stop not printing start print again stop not anymore> {
    # exclude both "start" and "stop",
    .say if $_ eq 'start' ^ff^ $_ eq 'stop'; # OUTPUT: «print it print again␤»
}

# You might also use a Whatever Star, which is equivalent to `True` for the
# left side or `False` for the right, as shown in this example.
# NOTE: the parenthesis are superfluous here (sometimes called "superstitious
# parentheses"). Once the flip-flop reaches a number greater than 50, it'll
# never go back to `False`.
for (1, 3, 60, 3, 40, 60) {
    .say if $_ > 50 ff *;  # OUTPUT: «60␤3␤40␤60␤»
}

# You can also use this property to create an `if` that'll not go through the
# first time. In this case, the flip-flop is `True` and never goes back to
# `False`, but the `^` makes it *not run* on the first iteration
for <a b c> { .say if * ^ff *; }  # OUTPUT: «b␤c␤»

# The `===` operator, which uses `.WHICH` on the objects to be compared, is
# the value identity operator whereas the `=:=` operator, which uses `VAR()` on
# the objects to compare them, is the container identity operator.
```

If you want to go further and learn more about Raku, you can:

- Read the [Raku Docs](https://docs.raku.org/). This is a great
resource on Raku. If you are looking for something, use the search bar.
This will give you a dropdown menu of all the pages referencing your search
term (Much better than using Google to find Raku documents!).

- Read the [Raku Advent Calendar](https://rakuadventcalendar.wordpress.com/). This
is a great source of Raku snippets and explanations. If the docs don't
describe something well enough, you may find more detailed information here.
This information may be a bit older but there are many great examples and
explanations. Posts stopped at the end of 2015 when the language was declared
stable and `Raku v6.c` was released.

- Come along on `#raku` at [`irc.libera.chat`](https://web.libera.chat/?channel=#raku). The folks here are
always helpful.

- Check the [source of Raku's functions and
classes](https://github.com/rakudo/rakudo/tree/master/src/core.c). Rakudo is
mainly written in Raku (with a lot of NQP, "Not Quite Perl", a Raku subset
easier to implement and optimize).

- Read [the language design documents](https://design.raku.org/). They explain
Raku from an implementor point-of-view, but it's still very interesting.
