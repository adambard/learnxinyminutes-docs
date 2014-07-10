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

```perl6
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

# variable names can contain but not end with simple quotes and dashes, and can contain (and end with) underscores

my $weird'variable-name_ = 5;

## - Arrays. They represent multiple values. They start with `@`

my @array = 1, 2, 3;
my @array = 'a', 'b', 'c';
# equivalent to :
my @array = <a b c>; # similar to perl5's qw, or Ruby's %w

say @array[2]; # Arrays are 0-indexed

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

# since you can omit parenthesis to call a function with no arguments, you need to use `&` also to capture `say-hello`
my &s = &say-hello;
my &other-s = sub { say "anonymous function !" }

# `->`, lambda with arguments, and string interpolation
my &lambda = -> $argument { "The argument passed to this lambda is $argument" }

### Control Flow Structures

# You don't need to put parenthesis around the condition, but that also means you always have to use brackets (`{ }`) for their body :

## Conditionals

if True {
  say "It's true !";
}

unless False {
  say "It's not false !";
}


# if (true) say; # Won't work

# `given`-`when` looks like other languages `switch`, but it's much more powerful thanks to smart matching :
given "foo bar" { # given just puts its argument into `$_`, and `when` uses it using the "smart matching" operator.
  when /foo/ { # you'll read about the smart-matching operator below
    say "Yay !";
  }
  when $_.chars > 50 { # smart matching anything with True gives True, so you can also put "normal" conditionals
    say "Quite a long string !";
  }
}

## Looping constructs

### - `loop` is an infinite loop if you don't pass it arguments, but can also be a c-style `for` :
loop {
  say "This is an infinite loop !";
  last; # last breaks out of the loop, like the `break` keyword in other languages
}

loop (my $i = 0; $i < 5; $i++) {
  next if $i == 3; # `next` skips to the next iteration, like `continue` in other languages.
                   # Notice that you can also use postfix conditionals, loops, etc.
  say "This is a C-style for loop !";
}

### - `for` - Foreaches an array
for @array -> $variable {
  say "I've found $variable !";
}

# default variable is $_
for array {
  say "I've got $_";
}

# Note - the "lambda" `->` syntax isn't reserved to for :
if long-computation() -> $result {
  say "The result is $result";
}



# Operators

## Since Perl languages are very much operator-based languages
## Perl 6 operators are actually just funny-looking subroutines, in syntactic categories,
##  like infix:<+> (addition) or prefix:<!> (bool not)

## The categories are :
### - "prefix" : before (like `!` in `!True`).
### - "postfix" : after (like `++` in `$a++`).
### - "infix" : in between (like `*` in `4 * 3`).
### - "circumfix" : around (like `[`-`]` in `[1, 2]`).
### - "post-circumfix" : around, after another term (like `{`-`}` in `%hash{'key'}`)

## The associativity and precedence list are explained below.

## Alright, you're set to go !

## * Equality Checking

### - `==` is numeric comparison
3 == 4; # False
3 != 4; # True

### - `eq` is string comparison
'a' eq 'b';
'a' ne 'b'; # not equal
'a' !eq 'b'; # same as above

### - `eqv` is canonical equivalence
(1, 2) eqv (1, 3);

### - `~~` is smart matching
### for a complete combinations list, use this table : http://perlcabal.org/syn/S03.html#Smart_matching
'a' ~~ /a/; # true if matches regexp
'key' ~~ %hash; # true if key exists in hash
$arg ~~ &bool-returning-function; # true if the function, passed `$arg` as an argument, returns True
1 ~~ Int; # "is of type"

### - `===` is value identity and uses `.WHICH` on the objects to compare them
### - `=:=` is container identity and uses `VAR()` on the objects to compare them

### You also, of course, have `<`, `<=`, `>`, `>=`.
### Their string equivalent are also avaiable : `lt`, `le`, `gt`, `ge`.
3 > 4;

## * Range constructors
3 .. 7; # 3 to 7, both included
### `^` on either side them exclusive on that side :
3 ^..^ 7; # 3 to 7, not included (basically `4 .. 6`)

# * And, Or
3 && 4; # True. Calls `.Bool` on `3`
0 || False; # False. Calls `.Bool` on `0`

## Short-circuit (and tight)
$a && $b && $c; # returns the first argument that evaluates to False, or the last argument
$a || $b;

# More operators thingies !

## Everybody loves operators ! Let's get more of them

## The precedence list can be found here : http://perlcabal.org/syn/S03.html#Operator_precedence
## But first, we need a little explanation about associativity :

### Binary operators:
$a ! $b ! $c; # with a left-associative `!`, this is `($a ! $b) ! $c`
$a ! $b ! $c; # with a right-associative `!`, this is `$a ! ($b ! $c)`
$a ! $b ! $c; # with a non-associative `!`, this is illegal
$a ! $b ! $c; # with a chain-associative `!`, this is `($a ! $b) and ($b ! $c)`
$a ! $b ! $c; # with a list-associative `!`, this is `infix:<>`

### Unary operators:
!$a! # with left-associative `!`, this is `(!$a)!`
!$a! # with right-associative `!`, this is `!($a!)`
!$a! # with non-associative `!`, this is illegal

## And to end the list of operators ...

## * Sort comparison
### They return one value of the `Order` enum : `Less`, `Same` and `More` (which numerify to -1, 0 or +1).
1 <=> 4; # sort comparison for numerics
'a' leg 'b'; # sort comparison for string
$obj eqv $obj2; # sort comparison using eqv semantics

## * Generic ordering
3 before 4; # True
'b' after 'a'; # True

```
