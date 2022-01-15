---
language: ruby
filename: learnruby.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
  - ["Dzianis Dashkevich", "https://github.com/dskecse"]
  - ["Levi Bostian", "https://github.com/levibostian"]
  - ["Rahil Momin", "https://github.com/iamrahil"]
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
  - ["Jake Faris", "https://github.com/farisj"]
  - ["Corey Ward", "https://github.com/coreyward"]
  - ["Jannik Siebert", "https://github.com/janniks"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
---

```ruby
# This is a comment

=begin
This is a multi-line comment.
The beginning line must start with "=begin"
and the ending line must start with "=end".

You can do this, or start each line in
a multi-line comment with the # character.
=end

# In Ruby, (almost) everything is an object.
# This includes numbers...
3.class #=> Integer

# ...and strings...
"Hello".class #=> String

# ...and even methods!
"Hello".method(:class).class #=> Method

# Some basic arithmetic
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2 ** 5 #=> 32
5 % 3 #=> 2

# Bitwise operators
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# Arithmetic is just syntactic sugar
# for calling a method on an object
1.+(3) #=> 4
10.* 5 #=> 50
100.methods.include?(:/) #=> true

# Special values are objects
nil # equivalent to null in other languages
true # truth
false # falsehood

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Equality
1 == 1 #=> true
2 == 1 #=> false

# Inequality
1 != 1 #=> false
2 != 1 #=> true

# Apart from false itself, nil is the only other 'falsey' value

!!nil   #=> false
!!false #=> false
!!0     #=> true
!!""    #=> true

# More comparisons
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Combined comparison operator (returns `1` when the first argument is greater, 
# `-1` when the second argument is greater, and `0` otherwise)
1 <=> 10 #=> -1 (1 < 10)
10 <=> 1 #=> 1 (10 > 1)
1 <=> 1 #=> 0 (1 == 1)

# Logical operators
true && false #=> false
true || false #=> true

# There are alternate versions of the logical operators with much lower
# precedence. These are meant to be used as flow-control constructs to chain
# statements together until one of them returns true or false.

# `do_something_else` only called if `do_something` succeeds.
do_something() and do_something_else()
# `log_error` only called if `do_something` fails.
do_something() or log_error()

# String interpolation

placeholder = 'use string interpolation'
"I can #{placeholder} when using double quoted strings"
#=> "I can use string interpolation when using double quoted strings"

# You can combine strings using `+`, but not with other types
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"
"hello #{3}" #=> "hello 3"

# ...or combine strings and operators
'hello ' * 3 #=> "hello hello hello "

# ...or append to string
'hello' << ' world' #=> "hello world"

# You can print to the output with a newline at the end
puts "I'm printing!"
#=> I'm printing!
#=> nil

# ...or print to the output without a newline
print "I'm printing!"
#=> "I'm printing!" => nil

# Variables
x = 25 #=> 25
x #=> 25

# Note that assignment returns the value assigned.
# This means you can do multiple assignment.

x = y = 10 #=> 10
x #=> 10
y #=> 10

# By convention, use snake_case for variable names.
snake_case = true

# Use descriptive variable names
path_to_project_root = '/good/name/'
m = '/bad/name/'

# Symbols are immutable, reusable constants represented internally by an
# integer value. They're often used instead of strings to efficiently convey
# specific, meaningful values.

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Strings can be converted into symbols and vice versa.
status.to_s #=> "pending"
"argon".to_sym #=> :argon

# Arrays

# This is an array.
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arrays can contain different types of items.
[1, 'hello', false] #=> [1, "hello", false]

# You might prefer %w instead of quotes
%w[foo bar baz] #=> ["foo", "bar", "baz"]

# Arrays can be indexed.
# From the front...
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# ...or from the back...
array[-1] #=> 5
array.last #=> 5

# ...or with a start index and length...
array[2, 3] #=> [3, 4, 5]

# ...or with a range...
array[1..3] #=> [2, 3, 4]

# You can reverse an Array.
# Return a new array with reversed values
[1,2,3].reverse #=> [3,2,1]
# Reverse an array in place to update variable with reversed values
a = [1,2,3]
a.reverse! #=> a==[3,2,1] because of the bang ('!') call to reverse

# Like arithmetic, [var] access is just syntactic sugar
# for calling a method '[]' on an object.
array.[] 0 #=> 1
array.[] 12 #=> nil

# You can add to an array...
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Or like this
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# ...and check if an item exists in an array
array.include?(1) #=> true

# Hashes are Ruby's primary dictionary with key/value pairs.
# Hashes are denoted with curly braces.
hash = { 'color' => 'green', 'number' => 5 }

hash.keys #=> ['color', 'number']

# Hashes can be quickly looked up by key.
hash['color'] #=> "green"
hash['number'] #=> 5

# Asking a hash for a key that doesn't exist returns nil.
hash['nothing here'] #=> nil

# When using symbols for keys in a hash, you can use an alternate syntax.

hash = { :defcon => 3, :action => true }
hash.keys #=> [:defcon, :action]

hash = { defcon: 3, action: true }
hash.keys #=> [:defcon, :action]

# Check existence of keys and values in hash
hash.key?(:defcon) #=> true
hash.value?(3) #=> true

# Tip: Both Arrays and Hashes are Enumerable!
# They share a lot of useful methods such as each, map, count, and more.

# Control structures

# Conditionals
if true
  'if statement'
elsif false
  'else if, optional'
else
  'else, also optional'
end

# If a condition controls invocation of a single statement rather than a block of code
# you can use postfix-if notation
warnings = ['Patronimic is missing', 'Address too short']
puts("Some warnings occurred:\n" + warnings.join("\n"))  if !warnings.empty?

# Rephrase condition if `unless` sounds better than `if`
puts("Some warnings occurred:\n" + warnings.join("\n"))  unless warnings.empty?

# Loops
# In Ruby, traditional `for` loops aren't very common. Instead, these 
# basic loops are implemented using enumerable, which hinges on `each`.
(1..5).each do |counter|
  puts "iteration #{counter}"
end

# Which is roughly equivalent to the following, which is unusual to see in Ruby.
for counter in 1..5
  puts "iteration #{counter}"
end

# The `do |variable| ... end` construct above is called a 'block'. Blocks are similar
# to lambdas, anonymous functions or closures in other programming languages. They can
# be passed around as objects, called, or attached as methods.
#
# The 'each' method of a range runs the block once for each element of the range.
# The block is passed a counter as a parameter.

# You can also surround blocks in curly brackets.
(1..5).each { |counter| puts "iteration #{counter}" }

# The contents of data structures can also be iterated using each.
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
end

# If you still need an index you can use 'each_with_index' and define an index
# variable.
array.each_with_index do |element, index|
  puts "#{element} is number #{index} in the array"
end

counter = 1
while counter <= 5 do
  puts "iteration #{counter}"
  counter += 1
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# There are a bunch of other helpful looping functions in Ruby.
# For example: 'map', 'reduce', 'inject', the list goes on.
# Map, for instance, takes the array it's looping over, does something
# to it as defined in your block, and returns an entirely new array.
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# another useful syntax is .map(&:method)
a = ["FOO", "BAR", "BAZ"]
a.map { |s| s.downcase } #=> ["foo", "bar", "baz"]
a.map(&:downcase) #=> ["foo", "bar", "baz"]

# Case construct
grade = 'B'

case grade
when 'A'
  puts 'Way to go kiddo'
when 'B'
  puts 'Better luck next time'
when 'C'
  puts 'You can do better'
when 'D'
  puts 'Scraping through'
when 'F'
  puts 'You failed!'
else
  puts 'Alternative grading system, eh?'
end
#=> "Better luck next time"

# Cases can also use ranges
grade = 82
case grade
when 90..100
  puts 'Hooray!'
when 80...90
  puts 'OK job'
else
  puts 'You failed!'
end
#=> "OK job"

# Exception handling
begin
  # Code here that might raise an exception
  raise NoMemoryError, 'You ran out of memory.'
rescue NoMemoryError => exception_variable
  puts 'NoMemoryError was raised', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'RuntimeError was raised now'
else
  puts 'This runs if no exceptions were thrown at all'
ensure
  puts 'This code always runs no matter what'
end

# Methods

def double(x)
  x * 2
end

# Methods (and blocks) implicitly return the value of the last statement.
double(2) #=> 4

# Parentheses are optional where the interpretation is unambiguous.
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Method arguments are separated by a comma.
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# All methods have an implicit, optional block parameter.
# It can be called with the 'yield' keyword.
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hello world' }

#=> {
#=> hello world
#=> }

# Blocks can be converted into a 'proc' object, which wraps the block 
# and allows it to be passed to another method, bound to a different scope,
# or manipulated otherwise. This is most common in method parameter lists,
# where you frequently see a trailing '&block' parameter that will accept 
# the block, if one is given, and convert it to a 'Proc'. The naming here is
# convention; it would work just as well with '&pineapple'.
def guests(&block)
  block.class #=> Proc
  block.call(4)
end

# The 'call' method on the Proc is similar to calling 'yield' when a block is 
# present. The arguments passed to 'call' will be forwarded to the block as arguments.

guests { |n| "You have #{n} guests." }
# => "You have 4 guests."

# You can pass a list of arguments, which will be converted into an array.
# That's what splat operator ("*") is for.
def guests(*array)
  array.each { |guest| puts guest }
end

# There is also the shorthand block syntax. It's most useful when you need
# to call a simple method on all array items.
upcased = ['Watch', 'these', 'words', 'get', 'upcased'].map(&:upcase)
puts upcased
#=> ["WATCH", "THESE", "WORDS", "GET", "UPCASED"]
 
sum = [1, 2, 3, 4, 5].reduce(&:+)
puts sum
#=> 15

# Destructuring

# Ruby will automatically destructure arrays on assignment to multiple variables.
a, b, c = [1, 2, 3]
a #=> 1
b #=> 2
c #=> 3

# In some cases, you will want to use the splat operator: `*` to prompt destructuring
# of an array into a list.
ranked_competitors = ["John", "Sally", "Dingus", "Moe", "Marcy"]

def best(first, second, third)
  puts "Winners are #{first}, #{second}, and #{third}."
end

best *ranked_competitors.first(3) #=> Winners are John, Sally, and Dingus.

# The splat operator can also be used in parameters.
def best(first, second, third, *others)
  puts "Winners are #{first}, #{second}, and #{third}."
  puts "There were #{others.count} other participants."
end

best *ranked_competitors 
#=> Winners are John, Sally, and Dingus.
#=> There were 2 other participants.

# By convention, all methods that return booleans end with a question mark.
5.even? #=> false
5.odd? #=> true

# By convention, if a method name ends with an exclamation mark, it does something destructive
# like mutate the receiver. Many methods have a ! version to make a change, and
# a non-! version to just return a new changed version.
company_name = "Dunder Mifflin"
company_name.upcase #=> "DUNDER MIFFLIN"
company_name #=> "Dunder Mifflin"
# We're mutating company_name this time.
company_name.upcase! #=> "DUNDER MIFFLIN"
company_name #=> "DUNDER MIFFLIN"

# Classes

# You can define a class with the 'class' keyword.
class Human

  # A class variable. It is shared by all instances of this class.
  @@species = 'H. sapiens'

  # Basic initializer
  def initialize(name, age = 0)
    # Assign the argument to the 'name' instance variable for the instance.
    @name = name
    # If no age given, we will fall back to the default in the arguments list.
    @age = age
  end

  # Basic setter method
  def name=(name)
    @name = name
  end

  # Basic getter method
  def name
    @name
  end

  # The above functionality can be encapsulated using the attr_accessor method as follows.
  attr_accessor :name

  # Getter/setter methods can also be created individually like this.
  attr_reader :name
  attr_writer :name

  # A class method uses self to distinguish from instance methods.
  # It can only be called on the class, not an instance.
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end

# Instantiating of a class
jim = Human.new('Jim Halpert')
dwight = Human.new('Dwight K. Schrute')

# You can call the methods of the generated object.
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Calling of a class method
Human.say('Hi') #=> "Hi"

# Variable's scopes are defined by the way we name them.
# Variables that start with $ have global scope.
$var = "I'm a global var"
defined? $var #=> "global-variable"

# Variables that start with @ have instance scope.
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# Variables that start with @@ have class scope.
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# Variables that start with a capital letter are constants.
Var = "I'm a constant"
defined? Var #=> "constant"

# Class is also an object in ruby. So a class can have instance variables.
# A class variable is shared among the class and all of its descendants.

# Base class
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Derived class
class Worker < Human
end

Human.foo #=> 0
Worker.foo #=> 0

Human.foo = 2
Worker.foo #=> 2

# A class instance variable is not shared by the class's descendants.
class Human
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Doctor < Human
end

Human.bar #=> 0
Doctor.bar #=> nil

module ModuleExample
  def foo
    'foo'
  end
end

# Including modules binds their methods to the class instances.
# Extending modules binds their methods to the class itself.
class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     #=> NoMethodError: undefined method `foo' for Person:Class
Person.new.foo #=> "foo"
Book.foo       #=> "foo"
Book.new.foo   #=> NoMethodError: undefined method `foo'

# Callbacks are executed when including and extending a module
module ConcernExample
  def self.included(base)
    base.extend(ClassMethods)
    base.send(:include, InstanceMethods)
  end

  module ClassMethods
    def bar
      'bar'
    end
  end

  module InstanceMethods
    def qux
      'qux'
    end
  end
end

class Something
  include ConcernExample
end

Something.bar     #=> "bar"
Something.qux     #=> NoMethodError: undefined method `qux'
Something.new.bar #=> NoMethodError: undefined method `bar'
Something.new.qux #=> "qux"
```

## Additional resources

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [An Interactive Tutorial for Ruby](https://rubymonk.com/) - Learn Ruby through a series of interactive tutorials.
- [Official Documentation](http://ruby-doc.org/core)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
- [Try Ruby](https://try.ruby-lang.org/) - Learn the basic of Ruby programming language, interactive in the browser.
