---
language: crystal
filename: learncrystal.cr
contributors:
  - ["Vitalii Elenhaupt", "http://veelenga.com"]

---

```crystal

# This is a comment

# Everything is an object
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# Falsey values are: nil, false and null pointers
!nil   #=> true
!false #=> true
!0     #=> false

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
0b1101 #=> 13

# Octal numbers
0o123 #=> 83

# Hexadecimal numbers
0xFE012D #=> 16646445
0xfe012d #=> 16646445

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
'\101' #=> 'A'

# Unicode codepoint
'\u0041' #=> 'A'

# Strings

"s".class #=> String

# Strings are immutable
s = "hello, "  #=> "hello, "
s.object_id    #=> 134667712
s += "Crystal" #=> "hello, Crystal"
s.object_id    #=> 142528472

# Supports interpolation
"sum = #{1 + 2}" #=> "sum = 3"

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

sentence = :question?

sentence == :question?    #=> true
sentence == :exclamation! #=> false
sentence == "question?"   #=> false

# Arrays

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Int32 | String | Char)

# Empty arrays should define a type
[]               # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32      #=> []
Array(Int32).new #=> []

# Arrays can be indexed
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]
array[0]                #=> 1
array[10]               # raises IndexError
array[-6]               # raises IndexError
array[10]?              #=> nil
array[-6]?              #=> nil

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
%w(one two three) #=> ["one", "two", "three"]
%i(one two three) #=> [:one, :two, :three]

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

# Empty hashes should define a type
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
MyType{"foo": "bar"}

# The above is equivalent to
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# Ranges

1..10                  #=> Range(Int32, Int32)
Range.new(1..10).class #=> Range(Int32, Int32)

# Can be inclusive or exclusive
3..5.to_a  #=> [3, 4, 5]
3...5.to_a #=> [3, 4]

# Check whether range includes the given value or not
1..8.includes? 2 #=> true

# Tuples are a fixed-size, immutable, stack-allocated sequence of values of
# possibly different types.
{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# Acces tuple's value with index
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> syntax error

# Can be expanded into multiple variables
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"

# Procs represent a functional pointer with an optional context
proc = ->(x : Int32) { x.to_s }
proc.class # (Int32 -> Nil)

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
typeof a #=> (String | Bool)

if a && b
  # here both a and b are guaranteed not to be Nil
end

if a.is_a? String
  a.class #=> String
end

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

# TODO: Types and methods

```

## Additional resources

- [Official Documentation](http://crystal-lang.org/)
