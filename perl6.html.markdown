---
name: perl6
category: language
language: perl6
filename: learnperl6.pl
contributors:
    - ["Nami-Doc", "http://github.com/Nami-Doc"]
---

Perl 6 is a highly capable, feature-rich programming language made for the upcoming hundred years.

Perl 6 runs on [the Parrot VM](http://parrot.org/), the JVM and [the MoarVM](http://moarvm.com).

Meta-note : the triple pound signs are here to denote headlines, double paragraphs, single notes.
`#=>` represents the output of a command.

```perl
# Single line comment start with a pound

#`(
  Multiline comments use #` and a quoting construct. (), [], {}, 「」, etc, will work.
)

### Variables

# In Perl 6, you declare a lexical variable using `my`
a
# Perl 6 has 4 variable types :

## - Scalars. They represent a single value. They start with a `$`

my $str = 'String';
my $str2 = "String"; # double quotes allow for interpolation

# variable names can contain but not end with simple quotes and dashes, and can contain (and end with) underscores :
# my $weird'variable-name_ = 5; # works !

my $bool = True; # `True` and `False` are Perl 6's boolean
my $inverse = !$bool; # You can invert a bool with the prefix `!` operator
my $forced-bool = so $str; # And you can use the prefix `so` operator which turns its operand into a Bool

## - Arrays. They represent multiple values. Their name start with `@`.

my @array = 1, 2, 3;
my @array = 'a', 'b', 'c';
# equivalent to :
my @array = <a b c>; # array of words, delimited by space. similar to perl5's qw, or Ruby's %w

say @array[2]; # Array indices start at 0 -- This is the third element

say "Interpolate an array using [] : @array[]"; #=> Interpolate an array using [] : a b c

## - Hashes. Key-Value Pairs.
# Hashes are actually arrays of Pairs (`Key => Value`),
#  except they get "flattened", removing duplicated keys.
my %hash = 1 => 2,
           3 => 4;
my %hash = autoquoted => "key", # keys *can* get auto-quoted
            "some other" => "value", # trailing commas are okay
            ;
my %hash = <key1 value1 key2 value2>; # you can also create a hash from an even-numbered array
my %hash = key1 => 'value1', key2 => 'value2'; # same as this

# You can also use the "colon pair" syntax: (especially handy for named parameters that you'll see later)
my %hash = :w(1), # equivalent to `w => 1`
           # this is useful for the `True` shortcut:
           :truey, # equivalent to `:truey(True)`, or `truey => True`
           # and for the `False` one:
           :!falsey, # equivalent to `:falsey(False)`, or `falsey => False`
           ;

say %hash{'key1'}; # You can use {} to get the value from a key
say %hash<key2>; # if it's a string, you can actually use <>

## - Subs (subroutines, or functions in most other languages). Stored in variable, they use `&`
sub say-hello { say "Hello, world" }

sub say-hello-to(Str $name) { # you can provide the type of an argument
                              # and it'll be checked at compile-time

    say "Hello, $name !";
}

# since you can omit parenthesis to call a function with no arguments,
#  you need "&" in the name to capture `say-hello`
my &s = &say-hello;
my &other-s = sub { say "anonymous function !" }

# A sub can have a "slurpy" parameter, or "doesn't-matter-how-many"
sub as-many($head, *@rest) { # the `*@` slurpy will basically "take everything else".
                             # Note: you can have parameters *before* (like here) a slurpy one,
                             # but not *after*.
  say @rest.join(' / ') ~ " !";
}
say as-many('Happy', 'Happy', 'Birthday'); #=> Happy Birthday !
                                           # Note that the splat did not consume the parameter before.

## You can call a function with an array using the "argument list flattening" operator `|`
#  (it's not actually the only feature of the operator, but it's one of them)
sub concat3($a, $b, $c) {
  say "$a, $b, $c";
}
concat3(|@array); #=> a, b, c
                  # `@array` got "flattened" as a part of the argument list

## It can also have optional arguments:
sub with-optional($arg?) { # the "?" marks the argument optional
  say "I might return `(Any)` if I don't have an argument passed, or I'll return my argument";
  $arg;
}
with-optional; # returns Any
with-optional(); # returns Any
with-optional(1); # returns 1

## You can also give them a default value when they're not passed:
sub hello-to($name = "World") {
  say "Hello, $name !";
}
hello-to; #=> Hello, World !
hello-to(); #=> Hello, World !
hello-to('You'); #=> Hello, You !

## You can also, by using a syntax akin to the one of hashes (yay unification !),
##  pass *named* arguments to a `sub`.
sub with-named($normal-arg, :$named) {
  say $normal-arg + $named;
}
with-named(1, named => 6); #=> 7
# There's one gotcha to be aware of, here:
# If you quote your key, Perl 6 won't be able to see it as compile time,
#  and you'll have a single Pair object as a positional paramater.

with-named(2, :named(5)); #=> 7
with-named(3, :4named); #=> 7
                        # (special colon pair syntax for numbers, mainly useful for `:2nd` etc)

with-named(3); # warns, because we tried to use the undefined $named in a `+`:
               # by default, named arguments are *optional*

# To make a named argument mandatory, you can use `?`'s inverse, `!`
sub with-mandatory-named(:$str!)  {
  say "$named !";
}
with-mandatory-named(str => "My String"); #=> My String !
with-mandatory-named; # run time error: "Required named parameter not passed" 
with-mandatory-named(3); # run time error: "Too many positional parameters passed"

## If a sub takes a named boolean argument ...
sub takes-a-bool($name, :$bool) {
  say "$name takes $bool";
}
# ... you can use the same "short boolean" hash syntax:
takes-a-bool('config', :bool); # config takes True
takes-a-bool('config', :!bool); # config takes False
# or you can use the "adverb" form:
takes-a-bool('config'):bool; #=> config takes True
takes-a-bool('config'):!bool; #=> config takes False
# You'll learn to love (or maybe hate, eh) that syntax later.


## You can also provide your named arguments with defaults:
sub named-def(:$def = 5) {
  say $def;
}
named-def; #=> 5
named-def(:10def); #=> 10
named-def(def => 15); #=> 15

# -- Note: we're going to learn *more* on subs really soon,
#  but we need to grasp a few more things to understand their real power. Ready?

### Containers
# In Perl 6, values are actually stored in "containers".
# the assignment operator asks the container on the left to store the value on its right
# When passed around, containers are marked as immutable. Which means that, in a function,
#  you'll get an error if you try to mutate one of your argument.
# If you really need to, you can ask for a mutable container using `is rw` :
sub mutate($n is rw) {
  $n++;
  say "\$n is now $n !";
}

# If what you want is a copy instead, use `is copy`.

# A sub itself returns a container, which means it can be marked as rw :
my $x = 42;
sub mod() is rw { $x }
mod() = 52; # in this case, the parentheses are mandatory (else Perl 6 thinks it's a "term")
say $x; #=> 52


### Control Flow Structures

# You don't need to put parenthesis around the condition,
# but that also means you always have to use brackets (`{ }`) for their body :

## Conditionals

# - `if`
if True {
  say "It's true !";
}

unless False {
  say "It's not false !";
}

# You can also use their postfix versions, with the keyword after:
say "Quite truthy" if True;

# if (true) say; # This doesn't work !

# - Ternary conditional, "?? !!" (like `x ? y : z` in some other languages)
my $a = $condition ?? $value-if-true !! $value-if-false;

# - `given`-`when` looks like other languages `switch`, but it's much more powerful thanks to smart matching,
# and thanks to Perl 6's "topic variable", $_.
# This variable contains the default argument of a block,
#  a loop's current iteration (unless explicitly named), etc.
# Given simply puts its argument into `$_` (like a block would do),
#  and `when` uses it using the "smart matching" operator.
# Since other Perl 6 constructs use this variable (as said before, like `for`, blocks, etc),
#  this means the powerful `when` is not only applicable along with a `given`,
#  but instead anywhere a `$_` exists.
given "foo bar" {
  when /foo/ { # you'll read about the smart-matching operator below -- just know `when` uses it
               # this is equivalent to `if $_ ~~ /foo/`
    say "Yay !";
  }
  when $_.chars > 50 { # smart matching anything with True (`$a ~~ True`) is True,
                       # so you can also put "normal" conditionals.
    say "Quite a long string !";
  }
  default { # same as `when *` (using the Whatever Star)
    say "Something else"
  }
}

## Looping constructs

# - `loop` is an infinite loop if you don't pass it arguments, but can also be a c-style `for` :
loop {
  say "This is an infinite loop !";
  last; # last breaks out of the loop, like the `break` keyword in other languages
}

loop (my $i = 0; $i < 5; $i++) {
  next if $i == 3; # `next` skips to the next iteration, like `continue` in other languages.
                   # Notice that you can also use postfix conditionals, loops, etc.
  say "This is a C-style for loop !";
}

# - `for` - Passes through an array
for @array -> $variable {
  say "I've found $variable !";
}

# As we saw with given, for's default "current iteration" variable is `$_`.
# That means you can use `when` in a `for` just like you were in a when.
for @array {
  say "I've got $_";
  
  .say; # This is also allowed.
        # A dot call with no "topic" (receiver) is sent to `$_` by default
  $_.say; # the above and this are equivalent.
}

for @array {
  next if $_ == 3; # you can skip to the next iteration (like `continue` in C-like languages)
  redo if $_ == 4; # you can re-do the iteration, keeping the same topic variable (`$_`)
  last if $_ == 5; # you can also break out of a loop (like `break` in C-like languages)
}

# Note - the "lambda" `->` syntax isn't reserved to `for` :
if long-computation() -> $result {
  say "The result is $result";
}

### Operators

## Since Perl languages are very much operator-based languages
## Perl 6 operators are actually just funny-looking subroutines, in syntactic categories,
##  like infix:<+> (addition) or prefix:<!> (bool not)

## The categories are :
# - "prefix" : before (like `!` in `!True`).
# - "postfix" : after (like `++` in `$a++`).
# - "infix" : in between (like `*` in `4 * 3`).
# - "circumfix" : around (like `[`-`]` in `[1, 2]`).
# - "post-circumfix" : around, after another term (like `{`-`}` in `%hash{'key'}`)

## The associativity and precedence list are explained below.

# Alright, you're set to go !

## * Equality Checking

# - `==` is numeric comparison
3 == 4; # False
3 != 4; # True

# - `eq` is string comparison
'a' eq 'b';
'a' ne 'b'; # not equal
'a' !eq 'b'; # same as above

# - `eqv` is canonical equivalence (or "deep equality")
(1, 2) eqv (1, 3);

# - `~~` is smart matching
# for a complete combinations list, use this table : http://perlcabal.org/syn/S03.html#Smart_matching
'a' ~~ /a/; # true if matches regexp
'key' ~~ %hash; # true if key exists in hash
$arg ~~ &bool-returning-function; # true if the function, passed `$arg` as an argument, returns True
1 ~~ Int; # "is of type"
1 ~~ True; # smart-matching against a boolean always returns that boolean (and will warn).

# - `===` is value identity and uses `.WHICH` on the objects to compare them
# - `=:=` is container identity and uses `VAR()` on the objects to compare them

# You also, of course, have `<`, `<=`, `>`, `>=`.
# Their string equivalent are also avaiable : `lt`, `le`, `gt`, `ge`.
3 > 4;

## * Range constructors
3 .. 7; # 3 to 7, both included
# `^` on either side them exclusive on that side :
3 ^..^ 7; # 3 to 7, not included (basically `4 .. 6`)
# this also works as a shortcut for `0..^N`
^10; # means 0..^10

# This also allows us to demonstrate that Perl 6 has lazy arrays, using the Whatever Star :
my @array = 1..*; # 1 to Infinite !
say @array[^10]; # you can pass arrays as subscripts and it'll return an array of results
                 # this will print "1 2 3 4 5 6 7 8 9 10" (and not run out of memory !)
# Note : when reading an infinite list, Perl 6 will "reify" the elements it needs, then keep them in memory
# They won't be calculated more than once.
                 
# Warning, though: if you try this example in the REPL and juste put `1..*`,
# Perl 6 will be forced to try and evaluate the whole array (to print it),
# so you'll end with an infinite loop.

## * And, Or
3 && 4; # True. Calls `.Bool` on `3`
0 || False; # False. Calls `.Bool` on `0`

## Short-circuit (and tight) versions of the above
$a && $b && $c; # returns the first argument that evaluates to False, or the last argument
$a || $b;

### More on subs !
# As we said before, Perl 6 has *really* powerful subs.
# We're going to see a few more key concepts that make them better than in any other language :-).

## Unpacking ! It's the ability to "extract" arrays and keys. It'll work in `my`s and parameters.
my ($a, $b) = 1, 2;
say $a; #=> 1
my ($, $, $c) = 1, 2, 3; # keep the non-interesting anonymous
say $c; #=> 3

my ($head, *@tail) = 1, 2, 3; # Yes, it's the same as with "slurpy subs"
my (*@small) = 1;

sub foo(@array [$fst, $snd]) {
  say "My first is $fst, my second is $snd ! All in all, I'm @array[].";
  # (^ remember the `[]` to interpolate the array)
}
foo(@tail); #=> My first is 2, my second is 3 ! All in all, I'm 1 2


# If you're not using the array itself, you can also keep it anonymous, much like a scalar:
sub first-of-array(@ [$fst]) { $fst }
first-of-array(@small); #=> 1
first-of-array(@tail); # errors with "Too many positional parameters passed" (the array is too big)

# You can also use a slurp ...
sub slurp-in-array(@ [$fst, *@rest]) { # you could decide to keep `*@rest` anonymous
  say $fst + @rest.elems;
}
slurp-in-array(@tail); #=> 3

# You could even extract on a slurpy (but it's pretty useless ;-).)
sub fst(*@ [$fst]) { # or simply : `sub fst($fst) { ... }`
  say $fst;
}
fst(1); #=> 1
fst(1, 2); # errors with "Too many positional parameters passed"

# You can also destructure hashes (and classes, which you'll learn about later !)
# The syntax is basically `%hash-name (:key($variable-to-store-value-in))`.
# The hash can stay anonymous if you only need the values you extracted.
sub key-of(% (:value($val), :qua($qua))) {
  say "Got val $val, $qua times.";
}

# Then call it with a hash: (you need to keep the brackets for it to be a hash)
key-of({value => 1});
#key-of(%hash); # the same (for an equivalent `%hash`)

## The last expression of a sub is returned automatically (though you may use the `return` keyword, of course):
sub next-index($n) {
  $n + 1;
}
my $new-n = next-index(3); # $new-n is now 4
# This is true for everything, except for the looping constructs (due to performance reasons):
#  there's no purpose in building a list if we're just going to discard all the results.
# If you still want to build one, you can use the `do` prefix: (or the `gather` prefix, which we'll see later)
sub list-of($n) {
  do for ^$n { # note the use of the range-to prefix operator `^` (`0..^N`)
    $_ # current loop iteration
  }
}
my @list3 = list-of(3); #=> (0, 1, 2)

## You can create a lambda with `-> {}` ("pointy block") or `{}` ("block")
my &lambda = -> $argument { "The argument passed to this lambda is $argument" }
# `-> {}` and `{}` are pretty much the same thing, except that the former can take arguments,
#  and that the latter can be mistaken as a hash by the parser.

# We can, for example, add 3 to each value of an array using map:
my @arrayplus3 = map({ $_ + 3 }, @array); # $_ is the implicit argument

# a sub (`sub {}`) has different semantics than a block (`{}` or `-> {}`):
# a block doesn't have a "function context" (though it can have arguments), which means that if you
#  return from it, you're going to return from the parent function, compare:
sub is-in(@array, $elem) {
  # this will `return` out of the `is-in` sub
  # once the condition evaluated to True, the loop won't be run anymore
  map({ return True if $_ == $elem }, @array);
}
sub truthy-array(@array) {
  # this will produce an array of `True` and `False`:
  # (you can also say `anon sub` for "anonymous subroutine")
  map(sub { if $_ { return True } else { return False } }, @array);
  # ^ the `return` only returns from the anonymous `sub`
}

# You can also use the "whatever star" to create an anonymous function
# (it'll stop at the furthest operator in the current expression)
my @arrayplus3 = map(*+3, @array); # `*+3` is the same as `{ $_ + 3 }`
my @arrayplus3 = map(*+*+3, @array); # also works. Same as `-> $a, $b { $a + $b + 3 }`
say (*/2)(4); #=> 2
              # Immediatly execute the function Whatever created.
say ((*+3)/5)(5); #=> 1.6
                  # works even in parens !

# but if you need to have more than one argument (`$_`) in a block (without wanting to resort to `-> {}`),
#  you can also use the implicit argument syntax, `$^` :
map({ $^a + $^b + 3 }, @array); # same as the above

# Note : those are sorted lexicographically. `{ $^b / $^a }` is like `-> $a, b { $b / $a }`

## Multiple Dispatch
# Perl 6 can decide which variant of a `sub` to call based on the type of the arguments,
# or on arbitrary preconditions, like with a type or a `where`:

# with types
multi sub sayit(Int $n) { # note the `multi` keyword here
  say "Number: $n";
}
multi sayit(Str $s) } # the `sub` is the default
  say "String: $s";
}
sayit("foo"); # prints "String: foo"
sayit(True); # fails at *compile time* with "calling 'sayit' will never work with arguments of types ..."

# with arbitrary precondition:
multi is-big(Int $n where * > 50) { "Yes !" } # using a closure
multi is-big(Int $ where 10..50) { "Quite." } # this uses smart-matching (could use a regexp, etc)
multi is-big(Int $) { "No" }

# you can also name these checks, by creating "subsets":
subset Even of Int where * %% 2;

multi odd-or-even(Even) { "Even" } # the main case using the type. We don't name the argument
multi odd-or-even($) { "Odd" } # "else"

# You can even dispatch based on a positional's argument presence !
multi with-or-without-you(:$with!) { # make it mandatory to be able to dispatch against it
  say "I can live ! Actually, I can't.";
}
multi with-or-without-you {
  say "Definitely can't live.";
}
# This is very, very useful for many purposes, like `MAIN` subs (covered later),
#  and even the language itself is using it in several places.
# `is`, for example, is actually a `multi sub` named `trait_mod:<is>`, and it works off that.
# `is rw`, for example, is a dispatch to a function with this signature:
# sub trait_mod:<is>(Routine $r, :$rw!) {}
# (commented because running this would probably lead to some very surprising side-effects !)


### Scoping
# In Perl 6, contrarily to many scripting languages (Python, Ruby, PHP, for example),
#  you are to declare your variables before using them. You already saw it, with `my`.
# (there are other declarator keywords, like `our`, `has` and `state`, but we'll talk about them later)
# This is called "lexical scoping", where in inner blocks, you can access variables from outer blocks.
my $foo = 'Foo';
sub foo {
  my $bar = 'Bar';
  sub bar {
    say "$foo $bar";
  }
  &bar; # return the function
}
foo()(); #=> 'Foo Bar'

# As you can see, `$foo` and `$bar` were captured.
# But if we were to try and use `$bar` outside of `foo`, the variable would be undefined.
#  (and you'd get a compile time error)

# Perl 6 has another kind of scope : dynamic scope.
# They use the twigil (composed sigil) `*` to mark dynamically-scoped variables:
my $*a = 1;
# Dyamically-scoped variables depend on the current call stack, instead of the current block stack.
sub foo {
  my $*foo = 1;
  bar(); # call `bar` in-place
}
sub bar {
  say $*foo; # Perl 6 will look into the call stack instead, and find `foo`'s `$*a`,
             # even though the blocks aren't nested (they're call-nested).
             #=> 1
}

### Object Model

## Perl 6 has a quite comprehensive object model
## You declare a class with the keyword `class`, fields with `has`, methods with `method`.
## In Perl 6, every field is private, and named `$!attr`, but if you declare it with `$.`,
##  you get a public (immutable) accessor along with it.

# (Perl 6's object model ("SixModel") is very flexible, and allows you to dynamically add methods,
#  change semantics, etc -- This will not be covered here, and you should refer to the Synopsis)

class A {
  has $.field; # `$.field` is immutable. Use `$!field` from inside the class to modify it.
  has $.other-field is rw; # You can, however, mark a public field as being read/write.
  has Int $!private-field = 10;

  method get-value {
    $.field + $!private-field + $n;
  }
  
  method set-value($n) {
    # $.field = $n; # As stated before, you can't use the `$.` immutable version.
    $!field = $n;   # This works, because `$!` is always mutable.
    
    $.other-field = 5; # This works, because `$.other-field` was declared `rw` (mutable).
  }
  
  method !private-method {
    say "This method is private to the class !";
  }
};

# Create a new instance of A with $.field set to 5 :
# note : you can't set private-field from here (more later on)
my $a = A.new(field => 5);
$a.get-value; #=> 18
#$a.field = 5; # This fails, because the `has $.field` is immutable
$a.other-field = 10; # This, however, works, because the public field is mutable (`rw`).

## Perl 6 also has inheritance (along with multiple inheritance ... Considered a misfeature by many)

class A {
  has $.val;
  
  submethod not-inherited {
    say "This method won't be available on B.";
    say "This is most useful for BUILD, which we'll see later";
  }
  
  method bar { $.val * 5 }
}
class B is A { # inheritance uses `is`
  method foo {
    say $.val;
  }
  
  method bar { $.val * 10 } # this shadows A's `bar`
}

my B $b .= new(val => 5); # When you use `my T $var`, `$var` starts off with `T` itself in it,
                # so you can call `new` on it.
                # (`.=` is just the compound operator composed of the dot-call and of the assignment operator
                #  `$a .= b` is the same as `$a = $a.b`)
                # Also note that `BUILD` (the method called inside `new`)  will set parent properties too,
                # so you can pass `val => 5` 
# $b.not-inherited; # This won't work, for reasons explained above
$b.foo; # prints 5
$b.bar; #=> 50, since it calls B's `bar`

## Roles are supported too (also called Mixins in other languages)
role PrintableVal {
  has $!counter = 0;
  method print {
    say $.val;
  }
}

# you "use" a mixin with "does" :
class Item does PrintableVal {
  has $.val;
  
  # When `does`-ed, a `role` literally "mixes in" the class :
  # the methods and fields are put together, which means a class can access
  # the private fields/methods of its roles (but not the inverse !) :
  method access {
    say $!counter++;
  }
  
  # However, this :
  # method print {}
  # is an error, since the compiler wouldn't know which `print` to use :
  # contrarily to inheritance, methods mixed in can't be shadowed - they're put at the same "level"
  
  # NOTE : You can use a role as a class (with `is ROLE`). In this case, methods will be shadowed,
  # since the compiler will consider `ROLE` to be a class
}

### Exceptions
# Exceptions are built on top of classes, usually in the package `X` (like `X::IO`).
# Unlike many other languages, in Perl 6, you put the `CATCH` block *within* the block to `try`.
# By default, a `try` has a `CATCH` block that catches any exception (`CATCH { default {} }`).
# You can redefine it using `when`s (and `default`) to handle the exceptions you want:
try {
  open 'foo';
  CATCH {
    when X::AdHoc { say "unable to open file !" }
    # any other exception will be re-raised, since we don't have a `default`
  }
}

# You can throw an exception using `die`:
die X::AdHoc.new(payload => 'Error !');
# TODO warn
# TODO fail
# TODO CONTROL

### Packages
# Packages are a way to reuse code. Packages are like "namespaces", and any element of the six model
#  (`module`, `role`, `class`, `grammar`, `subset` and `enum`) are actually packages.
#  (you can say that packages are the lowest common denomitor between them)
# Packages play a big part in a language, as Perl is well-known for CPAN,
#  the Comprehensive Perl Archive Network.
# You usually don't use packages directly : you use `class Package::Name::Here;`, or if you
#  only want to export variables/subs, you can use `module`:
module Hello::World { # bracketed form
                      # if `Hello` doesn't exist yet, it'll just be created as an "empty package stub"
                      # that can be redeclared as something else later.
  # declarations here
}
module Parse::Text; # file-scoped form
grammar Parse::Text::Grammar { # A grammar is a fine package, which you could `use`
}

# NOTE for Perl 5 users: even though the `package` keyword exists,
#  the braceless form is invalid (to catch a "perl5ism"). This will error out:
# package Foo; # because Perl 6 will think the entire file is Perl 5
# Just use `module` or the brace version of `package`.

# You can use a module (bring its declarations into scope) with `use`
use JSON::Tiny; # if you installed Rakudo* or Panda, you'll have this module
say from-json('[1]').perl; #=> [1]

# As said before, any part of the six model is also a package.
# Since `JSON::Tiny` uses (its own) `JSON::Tiny::Actions` class, you can use it:
my $actions = JSON::Tiny::Actions.new;

# We'll see how to export variables and subs in the next part:

### Declarators
# In Perl 6, you get different behaviors based on how you declare a variable.
# You've already seen `my` and `has`, we'll now explore the others.

## * `our` (happens at `INIT` time -- see "Phasers" below)
# Along with `my`, there are several others declarators you can use.
# The first one you'll want for the previous part is `our`.
# (All packagish things (`class`, `role`, etc) are `our` by default)
# it's like `my`, but it also creates a package variable:
module Foo::Bar {
  our $n = 1; # note: you can't put a type constraint on an `our` variable
  our sub inc {
    our sub available { # if you try to make scoped `sub`s `our` ... Better know what you're doing (Don't !).
      say "Don't do that. Seriously. You'd get burned.";
    }
    my sub unavailable { # `my sub` is the default
      say "Can't access me from outside, I'm my !";
    }
  }
  
  say ++$n; # lexically-scoped variables are still available
}
say $Foo::Bar::n; #=> 1
Foo::Bar::inc; #=> 2
Foo::Bar::inc; #=> 3

## * `constant` (happens at `BEGIN` time)
# You can use the `constant` keyword to declare a compile-time variable/symbol:
constant Pi = 3.14;
constant $var = 1;

## * `state` (happens at run time, but only once)
# State variables are only executed one time
# (they exist in other langages such as C as `static`)
sub fixed-rand {
  state $val = rand;
  say $rand;
}
fixed-rand for ^10; # will print the same number 10 times

# Note, however, that they exist separately in different enclosing contexts.
# If you declare a function with a `state` within a loop, it'll re-create the variable
# for each iteration of loop. See:
for ^5 -> $a {
  sub foo {
    state $val = rand; # This will be a different value for every value of `$a`
  }
  for ^5 -> $b {
    say foo; # This will print the same value 5 times, but only 5. Next iteration will re-run `rand`
  }
}



### Phasers
# Phasers in Perl 6 are blocks that happen at determined points of time in your program
# When the program is compiled, when a for loop runs, when you leave a block, when
#  an exception gets thrown ... (`CATCH` is actually a phaser !)
# Some of them can be used for their return values, some of them can't
#  (those that can have a "[*]" in the beginning of their explanation text).
# Let's have a look !

## * Compile-time phasers
BEGIN { say "[*] Runs at compile time, as soon as possible, only once" }
CHECK { say "[*] Runs at compile time, instead as late as possible, only once" }

## * Run-time phasers
INIT { say "[*] Runs at run time, as soon as possible, only once" }
END { say "Runs at run time, as late as possible, only once" }

## * Block phasers
ENTER { say "[*] Runs everytime you enter a block, repeats on loop blocks" }
LEAVE { say "Runs everytime you leave a block, even when an exception happened. Repeats on loop blocks." }

PRE { say "Asserts a precondition at every block entry, before ENTER (especially useful for loops)" }
POST { say "Asserts a postcondition at every block exit, after LEAVE (especially useful for loops)" }

## * Block/exceptions phasers
sub {
    KEEP { say "Runs when you exit a block successfully (without throwing an exception)" }
    UNDO { say "Runs when you exit a block unsuccessfully (by throwing an exception)" }
}

## * Loop phasers
for ^5 {
  FIRST { say "[*] The first time the loop is run, before ENTER" }
  NEXT { say "At loop continuation time, before LEAVE" }
  LAST { say "At loop termination time, after LEAVE" }
}

## * Role/class phasers
COMPOSE { "When a role is composed into a class. /!\ NOT YET IMPLEMENTED" }

# They allow for cute trick or clever code ...:
say "This code took " ~ (time - CHECK time) ~ "s to run";

# ... or clever organization:
sub do-db-stuff {
  ENTER $db.start-transaction; # create a new transaction everytime we enter the sub
  KEEP $db.commit; # commit the transaction if all went well
  UNDO $db.rollback; # or rollback if all hell broke loose
}


### More operators thingies !

## Everybody loves operators ! Let's get more of them

## The precedence list can be found here : http://perlcabal.org/syn/S03.html#Operator_precedence
## But first, we need a little explanation about associativity :

# - Binary operators:
$a ! $b ! $c; # with a left-associative `!`, this is `($a ! $b) ! $c`
$a ! $b ! $c; # with a right-associative `!`, this is `$a ! ($b ! $c)`
$a ! $b ! $c; # with a non-associative `!`, this is illegal
$a ! $b ! $c; # with a chain-associative `!`, this is `($a ! $b) and ($b ! $c)`
$a ! $b ! $c; # with a list-associative `!`, this is `infix:<>`

# - Unary operators:
!$a! # with left-associative `!`, this is `(!$a)!`
!$a! # with right-associative `!`, this is `!($a!)`
!$a! # with non-associative `!`, this is illegal

## Create your own operators !
# Okay, you've been reading all of that, so I guess I should try to show you something exciting.
# I'll tell you a little secret (actually not):
# In Perl 6, all operators are actually just funny-looking subroutines.

# You can declare an operator just like you declare a sub:
sub prefix:<win>($winner) { # refer to the operator categories
                            # (yes, it's the "words operator" `<>`)
  say "$winner Won !";
}
win "The King"; #=> The King Won !
                # (prefix is before)

# you can still call the sub with its "full name"
say prefix:<!>(True); #=> False

sub postfix:<!>(Int $n) {
  [*] 2..$n; # using the reduce meta-operator ... See below ;-) !
}
say 5!; #=> 120
        # (postfix is after)


sub infix:<times>(Int $n, Block $r) { # infix in the middle
  for ^$n {
    $r(); # needs the parentheses because it's a scalar
  }
}
3 times -> { say "hello" }; #=> hello
                            #=> hello
                            #=> hello

# For circumfix and post-circumfix ones
sub circumfix:<[ ]>(Int $n) {
  $n ** $n
}
say [5]; #=> 3125
         # circumfix is around

sub postcircumfix:<{ }>(Str $s, Int $idx) { # post-circumfix is "after a term, around something"
  $s.substr($idx, 1);
}
say "abc"{1}; #=> b
              # after the term `"abc"`, and around the index (1)

# This really means a lot -- because everything in Perl 6 uses this.
# For example, to delete a key from a hash, you use the `:delete` adverb (named argument)
%h{$key}:delete;
# equivalent to:
postcircumfix:<{ }>(%h, $key, :delete);
# It's *all* using the same building blocks! Syntactic categories (prefix infix ...),
#  named arguments (adverbs), ..., used to build the language are available to you.

# (you are, obviously, recommended against making an operator out of *everything* --
#  with great power comes great responsibility)

## Meta operators !
# Oh boy, get ready. Get ready, because we're dwelving deep into the rabbit's hole,
#  and you probably won't want to go back to other languages after reading that.
#  (I'm guessing you don't want to already at that point).

# - Reduce meta-operator

## End of the operator list:


## Sequence operator
# The sequence operator is one of Perl 6's most powerful features:
# it's composed of first, on the left, the list you want Perl 6 to deduce from (and might include a closure),
# and on the right, a value or the predicate for when to stop, or even Whatever for a lazy infinite list.
my @list = 1, 2, 3 ... 10; # basic deducing
#my @list = 1, 3, 6 ... 10; # this throws you into an infinite loop, because Perl 6 can't figure out the end
my @list = 1, 2, 3 ...^ 10; # as with ranges, you can exclude the last element (when the predicate matches)
my @list = 1, 3, 9 ... * > 30; # you can use a predicate (with the Whatever Star, here)
my @list = 1, 3, 9 ... { $_ > 30 }; # (equivalent to the above)
my @fib = 1, 1, *+* ... *; # lazy infinite list of prime numbers, computed using a closure !
my @fib = 1, 1, -> $a, $b { $a + $b } ... *; # (equivalent to the above)
say @fib[^10]; #=> 1 1 2 3 5 8 13 21 34 55
               # (using a range as the index)
# Note : as for ranges, once reified, elements aren't re-calculated.
# That's why `@primes[^100]` will take a long time the first time you print it, then be instant


## * Sort comparison
# They return one value of the `Order` enum : `Less`, `Same` and `More` (which numerify to -1, 0 or +1).
1 <=> 4; # sort comparison for numerics
'a' leg 'b'; # sort comparison for string
$obj eqv $obj2; # sort comparison using eqv semantics

## * Generic ordering
3 before 4; # True
'b' after 'a'; # True

## * Short-circuit default operator
# Like `or` and `||`, but instead returns the first *defined* value :
say Any // Nil // 0 // 5; #=> 5

## * Short-circuit exclusive or (XOR)
# Returns `True` if one (and only one) of its arguments is true
say True ^^ False; #=> True

## * Flip Flop
# The flip flop operators (`ff` and `fff`, equivalent to Perl 5/Ruby's `..` and `...`).
#  are operators that take two predicates to test:
# They are `False` until their left side returns `True`, then are `True` until their right side returns `True`.
# Like for ranges, you can exclude the iteration when it became `True`/`False` by using `^` on either side.
# Let's start with an example :
for <well met young hero we shall meet later> {
  # by default, `ff`/`fff` smart-match (`~~`) against `$_`:
  if 'met' ^ff 'meet' { # won't enter the if for "met" (explained in details below).
    .say
  }
  
  if rand == 0 ff rand == 1 { # compare variables other than `$_`
    say "This ... probably will never run ...";
  }
}
# This will print "young hero we shall meet" (excluding "met"):
#  the flip-flop will start returning `True` when it first encounters "met"
#  (but will still return `False` for "met" itself, due to the leading `^` on `ff`),
#  until it sees "meet", which is when it'll start returning `False`.

# The difference between `ff` (flip-flop) and `fff` (flip-flop) is that
#  `ff` will test its right side just as its left side changes to `True`,
#  and can get back to `False` right away (*except* it'll be `True` for the iteration that matched)
#  while `fff` will wait for the next iteration to try its right side, once its left side changed:
.say if 'B' ff 'B' for <A B C B A>; #=> B B
                                    # because the right-hand-side was tested directly (and returned `True`).
                                    # "B"s are still printed since it matched that time
                                    #  (it just went back to `False` right away)
.say if 'B' fff 'B' for <A B C B A>; #=> B C B
                                    # because the right-hand-side wasn't tested until `$_` became "C"
                                    # (and thus did not match directly).

# A flip-flop can change state as many times as needed:
for <test start print this stop you stopped printing start printing again stop not anymore> {
  .say if $_ eq 'start' ^ff^ $_ eq 'stop'; # exclude both "start" and "stop",
                                           #=> "print this printing again"
}

# you might also use a Whatever Star,
# which is equivalent to `True` for the left side or `False` for the right:
for (1, 3, 60, 3, 40, 60) {
 .say if $_ > 50 ff *; # Once the flip-flop reached a number greater than 50, it'll never go back to `False`
                       #=> 60 3 40 60
}

# You can also use this property to create an `If` that'll not execute the first time :
for <a b c> {
  .say if * ^ff *; # the flip-flop is `True` and never goes back to `False`,
                   #  but the `^` makes it *not run* on the first iteration
                   #=> b c
}
```
