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
given "foo bar" { # given just puts its argument into `$_`, and `when` uses it.
  when /foo/ { # smart matching a string with a regex returns true if it matches
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
```
