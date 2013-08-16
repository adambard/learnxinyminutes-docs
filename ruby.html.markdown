---
language: ruby
filename: learnruby.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
---

```ruby
# This is a comment

=begin
This is a multiline comment
No-one uses them
You shouldn't either
=end

# First and foremost: Everything is an object.

# Numbers are objects

3.class #=> Fixnum

3.to_s #=> "3"


# Some basic arithmetic
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Arithmetic is just syntactic sugar
# for calling a method on an object
1.+(3) #=> 4
10.* 5 #=> 50

# Special values are objects
nil # Nothing to see here
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
!true  #=> false
!false #=> true

# apart from false itself, nil is the only other 'falsey' value

!nil   #=> true
!false #=> true
!0     #=> false

# More comparisons
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Strings are objects

'I am a string'.class #=> String
"I am a string too".class #=> String

placeholder = "use string interpolation"
"I can #{placeholder} when using double quoted strings"
#=> "I can use string interpolation when using double quoted strings"


# print to the output
puts "I'm printing!"

# Variables
x = 25 #=> 25
x #=> 25

# Note that assignment returns the value assigned
# This means you can do multiple assignment:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# By convention, use snake_case for variable names
snake_case = true

# Use descriptive variable names
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Symbols (are objects)
# Symbols are immutable, reusable constants represented internally by an
# integer value. They're often used instead of strings to efficiently convey
# specific, meaningful values

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Arrays

# This is an array
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arrays can contain different types of items

[1, "hello", false] #=> [1, "hello", false]

# Arrays can be indexed
# From the front
array[0] #=> 1
array[12] #=> nil

# Like arithmetic, [var] access
# is just syntactic sugar
# for calling a method [] on an object
array.[] 0 #=> 1
array.[] 12 #=> nil

# From the end
array[-1] #=> 5

# With a start and end index
array[2, 4] #=> [3, 4, 5]

# Or with a range
array[1..3] #=> [2, 3, 4]

# Add to an array like this
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Hashes are Ruby's primary dictionary with keys/value pairs.
# Hashes are denoted with curly braces:
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']

# Hashes can be quickly looked up by key:
hash['color'] #=> 'green'
hash['number'] #=> 5

# Asking a hash for a key that doesn't exist returns nil:
hash['nothing here'] #=> nil

# Since Ruby 1.9, there's a special syntax when using symbols as keys:

new_hash = { defcon: 3, action: true}

new_hash.keys #=> [:defcon, :action]

# Tip: Both Arrays and Hashes are Enumerable
# They share a lot of useful methods such as each, map, count, and more

# Control structures

if true
  "if statement"
elsif false
  "else if, optional"
else
  "else, also optional"
end

for counter in 1..5
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# HOWEVER, No-one uses for loops.
# Instead you should use the "each" method and pass it a block.
# A block is a bunch of code that you can pass to a method like "each".
# It is analogous to lambdas, anonymous functions or closures in other
# programming languages.
#
# The "each" method of a range runs the block once for each element of the range.
# The block is passed a counter as a parameter.
# Calling the "each" method with a block looks like this:

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# You can also surround blocks in curly brackets:
(1..5).each {|counter| puts "iteration #{counter}"}

# The contents of data structures can also be iterated using each.
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
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

grade = 'B'

case grade
when 'A'
  puts "Way to go kiddo"
when 'B'
  puts "Better luck next time"
when 'C'
  puts "You can do better"
when 'D'
  puts "Scraping through"
when 'F'
  puts "You failed!"
else
  puts "Alternative grading system, eh?"
end

# Functions

def double(x)
  x * 2
end

# Functions (and all blocks) implicitly return the value of the last statement
double(2) #=> 4

# Parentheses are optional where the result is unambiguous
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# Method arguments are separated by a comma
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# All methods have an implicit, optional block parameter
# it can be called with the 'yield' keyword

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'hello world' }

# {
# hello world
# }


# Define a class with the class keyword
class Human

  # A class variable. It is shared by all instances of this class.
  @@species = "H. sapiens"

  # Basic initializer
  def initialize(name, age=0)
    # Assign the argument to the "name" instance variable for the instance
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

  # A class method uses self to distinguish from instance methods.
  # It can only be called on the class, not an instance.
  def self.say(msg)
    puts "#{msg}"
  end

  def species
    @@species
  end

end


# Instantiate a class
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Let's call a couple of methods
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Call the class method
Human.say("Hi") #=> "Hi"

# Class also is object in ruby. So class can have instance variables.
# Class variable is shared among the class and all of its descendants.

# base class
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

#  derived class
class Worker < Human
end

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# Class instance variable is not shared by the class's descendants.

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

Human.bar # 0
Doctor.bar # nil

```
