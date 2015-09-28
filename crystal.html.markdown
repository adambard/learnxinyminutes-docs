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

# Same as Array(Int32).new
[]          # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32 #=> []

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
# TODO: to be continued

```

## Additional resources

- [Official Documentation](http://crystal-lang.org/)
