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

# Perl 6 has 4 variable types :

## - Scalars. They represent a single value. They start with a `$`

my $str = 'String';
my $str2 = "String"; # double quotes allow for interpolation

# variable names can contain but not end with simple quotes and dashes, and can contain (and end with) underscores :
# my $weird'variable-name_ = 5; # works !

## - Arrays. They represent multiple values. They start with `@`

my @array = 1, 2, 3;
my @array = 'a', 'b', 'c';
# equivalent to :
my @array = <a b c>; # array of words, delimited by space. similar to perl5's qw, or Ruby's %w

say @array[2]; # Array indices start at 0 -- This is the third element

## - Hashes

my %hash = 1 => 2,
           3 => 4;
my %hash = autoquoted => "key",
            "some other" => "value", # trailing commas are okay
            ;
my %hash = <key1 value1 key2 value2> # you can also create a hash from an even-numbered array

say %hash{'key1'}; # You can use {} to get the value from a key
say %hash<key2>; # if it's a string, you can actually use <>

## - Subs (subroutines, or functions in most other languages). Stored in variable, they use `&`
sub say-hello { say "Hello, world" }

sub say-hello-to(Str $name) { # you can provide the type of an argument
                              # and it'll be checked at compile-time

    say "Hello, $name !";
}

# since you can omit parenthesis to call a function with no arguments, you need to use `&` also to capture `say-hello`
my &s = &say-hello;
my &other-s = sub { say "anonymous function !" }

# `->`, lambda with arguments, and string interpolation
my &lambda = -> $argument { "The argument passed to this lambda is $argument" }
# We're going to see how powerful Perl 6 subs are just a little down below, after seeing the basics of operators
# and control flow structures

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
mod() = 52; # in this case, the parentheses are mandatory
say $x; #=> 52

### Control Flow Structures

# You don't need to put parenthesis around the condition, but that also means you always have to use brackets (`{ }`) for their body :

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

# - Ternary conditional
my $a = $condition ?? $value-if-true !! $value-if-false; # `??` and `!!` are like `?` and `:` in other languages'

# - `given`-`when` looks like other languages `switch`, but it's much more powerful thanks to smart matching.
# given just puts its argument into `$_`, and `when` uses it using the "smart matching" operator.
given "foo bar" {
  when /foo/ { # you'll read about the smart-matching operator below -- just know `when` uses it
    say "Yay !";
  }
  when $_.chars > 50 { # smart matching anything with True gives True, so you can also put "normal" conditionals
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

# - `for` - Foreaches an array
for @array -> $variable {
  say "I've found $variable !";
}

# default variable is $_
for @array {
  say "I've got $_";
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

# - `eqv` is canonical equivalence
(1, 2) eqv (1, 3);

# - `~~` is smart matching
# for a complete combinations list, use this table : http://perlcabal.org/syn/S03.html#Smart_matching
'a' ~~ /a/; # true if matches regexp
'key' ~~ %hash; # true if key exists in hash
$arg ~~ &bool-returning-function; # true if the function, passed `$arg` as an argument, returns True
1 ~~ Int; # "is of type"

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
^10; # 0..^10

# This also allows us to demonstrate that Perl 6 has lazy arrays, using the Whatever Star :
my @array = 1..*; # 1 to Infinite !
say @array[^10]; # you can pass arrays as subscripts and it'll return an array of results
                 # this will print "1 2 3 4 5 6 7 8 9 10" (and not run out of memory !)
# Note : when reading an infinite list, Perl 6 will "reify" the elements it needs, then keep them in memory
# They won't be calculated more than once.
                 
# Warning, though : if you try this example in the REPL and juste put `1..*`,
# Perl 6 will be forced to try and evaluate the whole array (to print it),
# so you'll end with an infinite loop.

## * And, Or
3 && 4; # True. Calls `.Bool` on `3`
0 || False; # False. Calls `.Bool` on `0`

## Short-circuit (and tight) versions of the above
$a && $b && $c; # returns the first argument that evaluates to False, or the last argument
$a || $b;

## Sequence operator
# The sequence operator is one of Perl 6's most powerful features :
# it's composed of first, on the left, the list you want Perl 6 to deduce from (and might include a closure),
# and on the right, a value or the predicate for when to stop, or even Whatever for a lazy infinite list
my @list = 1, 2, 3 ... 10; # basic deducing
#my @list = 1, 3, 6 ... 10; # this throws you into an infinite loop, because Perl 6 can't figure out the end
my @list = 1, 2, 3 ...^ 10; # as with ranges, you can exclude the last element (when the predicate matches)
my @list = 1, 3, 9 ... * > 30; # you can use a predicate (with the Whatever Star, here)
my @list = 1, 3, 9 ... { $_ > 30 }; # (equivalent to the above)
my @primes = 1, 1, *+* ... *; # lazy infinite list of prime numbers, computed using a closure !
my @primes = 1, 1, -> $a, $b { $a + $b } ... *; # (equivalent to the above)
say @primes[^10]; #=> 1 1 2 3 5 8 13 21 34 55
# Note : as for ranges, once reified, elements aren't re-calculated.
# That's why `@primes[^100]` will take a long time the first time you print it, then be instant

## More on Subs !
# Perl 6 likes functions. So, in Perl 6, functions are very powerful:

## Multiple Dispatch
# Perl 6 can decide which variant of a `sub` to call based on the type of the arguments,
# or on arbitrary preconditions, using `where` :

# with types
multi sub sayit(Int $n) { # note the `multi` keyword here
  say "Number: $n";
}
multi sayit(Str $s) } # the `sub` is implicit
  say "String: $s";
}
sayit("foo"); # prints "String: foo"
sayit(True); # fails at *compile time* with "calling 'sayit' will never work with arguments of types ..."

# with arbitrary precondition:
multi is-big(Int $n where * > 10) { True }
multi is-big(Int $) { False }

# you can also name these checks, by creating "subsets":
subset Even of Int where * %% 2;


# The last expression of a sub is returned automatically (though you may use the `return` keyword, of course):
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

# We can, for example, add 3 to each value of an array using map :
my @arrayplus3 = map({ $_ + 3 }, @array); # $_ is the implicit argument (the same as for `given` and `for`)

# a sub (`sub {}`) has different semantics than a block (`{}` or `-> {}`) :
# a block doesn't have a function context (though it can have arguments), which means that if you
#  return from it, you're going to return from the parent function, compare:
sub is-in(@array, $elem) {
  # this will `return` out of `is-in` sub
  # once the condition evaluated to True, the loop won't be run anymore
  map({ return True if $_ == $elem }, @array);
}
sub truthy-array(@array) {
  # this will produce an array of `True` and `False` :
  # (you can also say `anon sub` for "anonymous subroutine")
  map(sub { if $_ { return True } else { return False } }, @array); # returns the correct value, even in a `if`
}

# `-> {}` and `{}` are pretty much the same thing, except that the former can take arguments,
#  and that the latter can be mistaken as a hash by the compiler

# You can also use the "whatever star" to create an anonymous function
# (it'll stop at the furthest operator in the current expression)
my @arrayplus3 = map(*+3, @array); # `*+3` is the same as `{ $_ + 3 }`
my @arrayplus3 = map(*+*+3, @array); # also works. Same as `-> $a, $b { $a + $b + 3 }`
say ((*+3)/5)(5); # immediatly execute the function Whatever created -- works even in parens !

# but if you need to have more than one argument (`$_`) in a block (without wanting to resort to `-> {}`),
#  you can also use the implicit argument syntax, `$^` :
map({ $^a + $^b + 3 }, @array); # same as the above

# Note : those are sorted lexicographically. `{ $^b / $^a }` is like `-> $a, b { $ b / $a }`



### Object Model

## Perl 6 has a quite comprehensive object model
## You declare a class with the keyword `class`, fields with `has`, methods with `method`
## `$.` declares a public field, `$!` declares a private field
## (a public field also has `$!`, which is its private interface)

# (Perl 6's object model ("P6Model") is very flexible, and allows you to dynamically add methods,
#  change semantics, etc -- This will not be covered here, and you should refer to the Synopsis)

class A {
  has $.field;
  has Int $!private-field = 10;
  
  method get-value {
    $.field + $!private-field + $n;
  }
  
  method set-value($n) {
    # $.field = $n; # This fails, because a public field is actually an immutable container
                    # (even from inside the class)
                    # You either need to use `is rw` on the `has`
                    # (which will make it mutable, even from outside the class)
                    # or you need to use the `$!` version :
                    
    $!field = $n;   # This works, because `$!` is always mutable
  }
  
  method !private-method {
    say "This method is private to the class !";
  }
};

# Create a new instance of A with $.field set to 5 :
# note : you can't set private-field from here (more later on)
my $a = A.new(field => 5);
$a.get-value; #=> 18
#$a.field = 5; # This fails, because the `has $.field` is lacking the `is rw`

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

my B $b .= new(val => 5); # When you use `my T $var`, `$var` starts off with `T` itself in it, so you can call `new` on it
                # (`.=` is just the compound operator composed of the dot-call and of the assignment operator)
                #
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
COMPOSE { "When a role is composed into a class. /!\ NOT YET IMPLEMENTED /!\" }

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

## Last part of the operator list :

## * Sort comparison
# They return one value of the `Order` enum : `Less`, `Same` and `More` (which numerify to -1, 0 or +1).
1 <=> 4; # sort comparison for numerics
'a' leg 'b'; # sort comparison for string
$obj eqv $obj2; # sort comparison using eqv semantics

## * Generic ordering
3 before 4; # True
'b' after 'a'; # True

## * Flip Flop
# The flip flop operator (spelled `ff` in Perl 6 and sometimes `..` in other languages such as Perl 5 and Ruby),
#  is an operator that takes two boolean values (like a predicate) and keep track of their change as internal state.
# The flip-flop will return `false` until its left side return true, then return true until its right side return true.
# You can also exclude either side (iteration when the left side became true, or the right side became true),
#  using the `^` like with ranges.
# Let's start with an example :
for <well met young hero we shall meet later> {
  if $_ eq 'met' ^ff $_ eq 'meet' { # excludes "met"
    .say
  }
}
# This will print "young hero we shall meet" (excluding "met"):
#  the flip-flop will start returning `True` when it first encounters "met"
#  (but will still return `False` for "met" itself, due to the leading `^` on `ff`),
#  until it sees "meet", which is when it'll start returning `False`.
# A flip-flop can change state as many times as needed:
for <test start print this stop you stopped printing start printing again stop not anymore> {
  .say if $_ eq 'start' ^ff^ $_ eq 'stop'; # exclude both "start" and "stop",
                                           #=> "print this printing again"
}

# you might also use a Whatever Star, which is equivalent to `True` for the left side or `False` for the right :
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
