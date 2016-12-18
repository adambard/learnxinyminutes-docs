---
language: crystal
filename: learncrystal.cr
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]

---

```crystal

# This is a comment

# Everything is an object
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# Falsey values are: nil, false and null pointers
!nil   #=> true  : Bool
!false #=> true  : Bool
!0     #=> false : Bool

# Integers

1.class #=> Int32

# Four signed integer types
1_i8.class  #=> Int8
1_i16.class #=> Int16
1_i32.class #=> Int32
1_i64.class #=> Int64

# Four unsigned integer types
1_u8.class  #=> UInt8
1_u16.class #=> UInt16
1_u32.class #=> UInt32
1_u64.class #=> UInt64

2147483648.class          #=> Int64
9223372036854775808.class #=> UInt64

# Binary numbers
0b1101 #=> 13 : Int32

# Octal numbers
0o123 #=> 83 : Int32

# Hexadecimal numbers
0xFE012D #=> 16646445 : Int32
0xfe012d #=> 16646445 : Int32

# Floats

1.0.class #=> Float64

# There are two floating point types
1.0_f32.class #=> Float32
1_f32.class   #=> Float32

1e10.class    #=> Float64
1.5e10.class  #=> Float64
1.5e-7.class  #=> Float64

# Chars

'a'.class #=> Char

# Octal codepoint
'\101' #=> 'A' : Char

# Unicode codepoint
'\u0041' #=> 'A' : Char

# Strings

"s".class #=> String

# Strings are immutable
s = "hello, "  #=> "hello, "        : String
s.object_id    #=> 134667712        : UInt64
s += "Crystal" #=> "hello, Crystal" : String
s.object_id    #=> 142528472        : UInt64

# Supports interpolation
"sum = #{1 + 2}" #=> "sum = 3" : String

# Multiline string
"This is
   multiline string"

# String with double quotes
%(hello "world") #=> "hello \"world\""

# Symbols
# Immutable, reusable constants represented internally as Int32 integer value.
# They're often used instead of strings to efficiently convey specific,
# meaningful values

:symbol.class #=> Symbol

sentence = :question?     # :"question?" : Symbol

sentence == :question?    #=> true  : Bool
sentence == :exclamation! #=> false : Bool
sentence == "question?"   #=> false : Bool

# Arrays

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Int32 | String | Char)

# Empty arrays should define a type
[]               # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32      #=> [] : Array(Int32)
Array(Int32).new #=> [] : Array(Int32)

# Arrays can be indexed
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5] : Array(Int32)
array[0]                #=> 1               : Int32
array[10]               # raises IndexError
array[-6]               # raises IndexError
array[10]?              #=> nil             : (Int32 | Nil)
array[-6]?              #=> nil             : (Int32 | Nil)

# From the end
array[-1] #=> 5

# With a start index and size
array[2, 3] #=> [3, 4, 5]

# Or with range
array[1..3] #=> [2, 3, 4]

# Add to an array
array << 6  #=> [1, 2, 3, 4, 5, 6]

# Remove from the end of the array
array.pop #=> 6
array     #=> [1, 2, 3, 4, 5]

# Remove from the beginning of the array
array.shift #=> 1
array       #=> [2, 3, 4, 5]

# Check if an item exists in an array
array.includes? 3 #=> true

# Special syntax for an array of string and an array of symbols
%w(one two three) #=> ["one", "two", "three"] : Array(String)
%i(one two three) #=> [:one, :two, :three]    : Array(Symbol)

# There is a special array syntax with other types too, as long as
# they define a .new and a #<< method
set = Set{1, 2, 3} #=> [1, 2, 3]
set.class          #=> Set(Int32)

# The above is equivalent to
set = Set(typeof(1, 2, 3)).new
set << 1
set << 2
set << 3

# Hashes

{1 => 2, 3 => 4}.class   #=> Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class #=> Hash(Int32 | Char, Int32)

# Empty hashes should specify a type
{}                     # Syntax error
{} of Int32 => Int32   # {}
Hash(Int32, Int32).new # {}

# Hashes can be quickly looked up by key
hash = {"color" => "green", "number" => 5}
hash["color"]        #=> "green"
hash["no_such_key"]  #=> Missing hash key: "no_such_key" (KeyError)
hash["no_such_key"]? #=> nil

# Check existence of keys hash
hash.has_key? "color" #=> true

# Special notation for symbol and string keys
{key1: 'a', key2: 'b'}     # {:key1 => 'a', :key2 => 'b'}
{"key1": 'a', "key2": 'b'} # {"key1" => 'a', "key2" => 'b'}

# Special hash literal syntax with other types too, as long as
# they define a .new and a #[]= methods
class MyType
  def []=(key, value)
    puts "do stuff"
  end
end

MyType{"foo" => "bar"}

# The above is equivalent to
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# Ranges

1..10                  #=> Range(Int32, Int32)
Range.new(1, 10).class #=> Range(Int32, Int32)

# Can be inclusive or exclusive
(3..5).to_a  #=> [3, 4, 5]
(3...5).to_a #=> [3, 4]

# Check whether range includes the given value or not
(1..8).includes? 2 #=> true

# Tuples are a fixed-size, immutable, stack-allocated sequence of values of
# possibly different types.
{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# Acces tuple's value by its index
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> syntax error : Index out of bound

# Can be expanded into multiple variables
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"

# Procs represent a function pointer with an optional context (the closure data)
# It is typically created with a proc litteral
proc = ->(x : Int32) { x.to_s }
proc.class # Proc(Int32, String)
# Or using the new method
Proc(Int32, String).new { |x| x.to_s }

# Invoke proc with call method
proc.call 10 #=> "10"

# Control statements

if true
  "if statement"
elsif false
  "else-if, optional"
else
  "else, also optional"
end

puts "if as a suffix" if true

# If as an expression
a = if 2 > 1
      3
    else
      4
    end

a #=> 3

# Ternary if
a = 1 > 2 ? 3 : 4 #=> 4

# Case statement
cmd = "move"

action = case cmd
  when "create"
    "Creating..."
  when "copy"
    "Copying..."
  when "move"
    "Moving..."
  when "delete"
    "Deleting..."
end

action #=> "Moving..."

# Loops
index = 0
while index <= 3
  puts "Index: #{index}"
  index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

index = 0
until index > 3
  puts "Index: #{index}"
  index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# But the preferable way is to use each
(1..3).each do |index|
  puts "Index: #{index}"
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# Variable's type depends on the type of the expression
# in control statements
if a < 3
  a = "hello"
else
  a = true
end
typeof a #=> (Bool | String)

if a && b
  # here both a and b are guaranteed not to be Nil
end

if a.is_a? String
  a.class #=> String
end

# Functions

def double(x)
  x * 2
end

# Functions (and all blocks) implicitly return the value of the last statement
double(2) #=> 4

# Parentheses are optional where the call is unambiguous
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Method arguments are separated by a comma
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# All methods have an implicit, optional block parameter
# it can be called with the 'yield' keyword

def surround
  puts '{'
  yield
  puts '}'
end

surround { puts "hello world" }

# {
# hello world
# }


# You can pass a block to a function
# "&" marks a reference to a passed block
def guests(&block)
  block.call "some_argument"
end

# You can pass a list of arguments, which will be converted into an array
# That's what splat operator ("*") is for
def guests(*array)
  array.each { |guest| puts guest }
end

# If a method returns an array, you can use destructuring assignment
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast #=> "pancake"
dinner    #=> "quesadilla"

# By convention, all methods that return booleans end with a question mark
5.even? # false
5.odd?  # true

# And if a method ends with an exclamation mark, it does something destructive
# like mutate the receiver. Some methods have a ! version to make a change, and
# a non-! version to just return a new changed version
company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald"  #=> "Donald Mifflin"
company_name  #=> "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
company_name  #=> "Donald Mifflin"


# Define a class with the class keyword
class Human

  # A class variable. It is shared by all instances of this class.
  @@species = "H. sapiens"

  # type of name is String
  @name : String

  # Basic initializer
  # Assign the argument to the "name" instance variable for the instance
  # If no age given, we will fall back to the default in the arguments list.
  def initialize(@name, @age = 0)
  end

  # Basic setter method
  def name=(name)
    @name = name
  end

  # Basic getter method
  def name
    @name
  end

  # The above functionality can be encapsulated using the attr_accessor method as follows
  property :name

  # Getter/setter methods can also be created individually like this
  getter :name
  setter :name

  # A class method uses self to distinguish from instance methods.
  # It can only be called on the class, not an instance.
  def self.say(msg)
    puts msg
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
Human.say("Hi") #=> print Hi and returns nil

# Variables that start with @ have instance scope
class TestClass
  @var = "I'm an instance var"
end

# Variables that start with @@ have class scope
class TestClass
  @@var = "I'm a class var"
end
# Variables that start with a capital letter are constants
Var = "I'm a constant"
Var = "can't be updated" # Already initialized constant Var

# Class is also an object in crystal. So class can have instance variables.
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

# derived class
class Worker < Human
end

Human.foo   #=> 0
Worker.foo  #=> 0

Human.foo = 2 #=> 2
Worker.foo    #=> 0

Worker.foo = 3 #=> 3
Human.foo   #=> 2
Worker.foo  #=> 3

module ModuleExample
  def foo
    "foo"
  end
end

# Including modules binds their methods to the class instances
# Extending modules binds their methods to the class itself

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => undefined method 'foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => undefined method 'foo' for Book


# Exception handling

# Define new exception
class MyException < Exception
end

# Define another exception
class MyAnotherException < Exception; end

ex = begin
   raise MyException.new
rescue ex1 : IndexError
  "ex1"
rescue ex2 : MyException | MyAnotherException
  "ex2"
rescue ex3 : Exception
  "ex3"
rescue ex4 # catch any kind of exception
  "ex4"
end

ex #=> "ex2"

```

## Additional resources

- [Official Documentation](http://crystal-lang.org/)
