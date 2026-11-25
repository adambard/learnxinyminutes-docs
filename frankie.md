---
name: Frankie
filename: learnfrankie.fk
contributors:
    - ["Blag aka. Alvaro Tejada Galindo", "https://github.com/atejada"]
---

Frankie is a small language that compiles to Python, with Ruby-like syntax,
R-style vectors and statistics, and FORTRAN-style `//` and `**`. It is pure
Python 3.10+ with no dependencies. This article targets Frankie v1.22.x.

[Frankie's Website](https://atejada.github.io/frankielanguage/)

Install with `brew install atejada/frankie/frankie`, then run any file with
`frankiec run file.fk`. The source is at <https://github.com/atejada/Frankie>.

```ruby
# ---------------------------------------------------------------
# 1. Basics: comments, output, variables
# ---------------------------------------------------------------

# Single-line comments start with #.

puts "Hello, Frankie!"        # puts prints with a newline
print "no newline, "          # print doesn't add one
puts ""

name = "Frankie"              # variables need no declaration
age = 1
pi_ish = 3.14
alive = true
nothing = nil

puts "#{name} is #{age} year(s) old"   # string interpolation with #{...}

const MAX_USERS = 100         # constants can't be reassigned

# Truthiness: everything is true except false, nil, 0, 0.0, "", [] and {}.
puts "empty string is falsy" unless ""
puts "non-empty string is truthy" if "hi"

# ---------------------------------------------------------------
# 2. Numbers and strings
# ---------------------------------------------------------------

puts 10 / 3                   # 3.3333333333333335  (/ is always a float)
puts 10 // 3                  # 3                   (integer division)
puts 10 % 3                   # 1
puts 2 ** 8                   # 256                 (exponent)
puts 1_000_000                # underscores are allowed in numbers
puts 2.5e-3                   # scientific notation

# Strings are immutable; every method returns a new string.
s = "Frankie"
puts s.upcase
puts s[0]                     # "F"
puts s[-1]                    # "e"
puts s[1..3]                  # "ran"  (inclusive range)
puts s[1...3]                 # "ra"   (exclusive range)
puts s[99]                    # nil -- out of range doesn't crash

# Safe navigation: &. returns nil instead of crashing on nil.
maybe = nil
puts maybe&.upcase

# Heredocs
text = <<~TXT
  Indented text, with
  the common indent removed.
TXT
puts text

# ---------------------------------------------------------------
# 3. Control flow
# ---------------------------------------------------------------

x = 7
if x > 10
  puts "big"
elsif x > 5
  puts "medium"
else
  puts "small"
end

puts "x is odd" if x % 2 == 1         # postfix if
puts x > 5 ? "above five" : "five or less"   # ternary

# if is an expression, so it returns a value
size = if x > 5 then "large" else "small" end
puts size

# case/when works with values and ranges
score = 82
case score
when 90..100
  puts "A"
when 80..89
  puts "B"
else
  puts "C or below"
end

# ---------------------------------------------------------------
# 4. Loops
# ---------------------------------------------------------------

for i in 1..3                 # 1..3 includes 3, 1...3 doesn't
  puts "for: #{i}"
end

n = 0
while n < 3
  n += 1                      # compound assignment
end
puts "while ended at #{n}"

3.times do |i|
  puts "times: #{i}"
end

# loop runs forever until you break; break/next also work in other loops
count = 0
loop do
  count += 1
  next if count == 2
  break if count > 3
  puts "loop: #{count}"
end

# Labeled loops (v1.22): break out of nested loops in one go
loop :outer do
  loop :inner do
    puts "breaking out of both"
    break :outer
  end
end

# ---------------------------------------------------------------
# 5. Functions
# ---------------------------------------------------------------

# The last expression is returned implicitly.
def add(a, b)
  a + b
end
puts add(2, 3)

# Default parameters
def greet(who = "world")
  "Hello, #{who}!"
end
puts greet()
puts greet("Frankie")

# Optional type annotations (v1.19), checked statically only
def area(r: Float) -> Float
  3.14159 * r ** 2
end
puts area(2.0)

# Names may end in ? or !
def even?(n)
  n % 2 == 0
end
puts even?(4)

# Return several values and destructure them
def min_max(v)
  [min(v), max(v)]
end
lo, hi = min_max([4, 8, 1, 9])
puts "lo=#{lo} hi=#{hi}"

# Recursion
def fact(n)
  n <= 1 ? 1 : n * fact(n - 1)
end
puts fact(10)

# ---------------------------------------------------------------
# 6. Vectors (lists) and hashes (dictionaries)
# ---------------------------------------------------------------

nums = [5, 3, 8, 1]
puts nums.length
puts nums[0]
puts nums.sort
puts nums.reverse
nums.push(10)
puts nums

# Iterator methods take blocks: do |x| ... end
doubled = nums.map do |n|
  n * 2
end
puts doubled

big = nums.select do |n|
  n > 4
end
puts big

total = nums.reduce(0) do |acc, n|
  acc + n
end
puts total

# Vector arithmetic is element-wise (R style)
puts [1, 2, 3] + [10, 20, 30]

# Statistics are built in
puts mean([1, 2, 3, 4])
puts median([1, 2, 3, 4])

# Hashes
person = {name: "Ana", age: 30}
puts person["name"]
person["city"] = "Lima"
puts person.keys
puts person.has_key?("age")

# merge hashes with |
more = person | {country: "Peru"}
puts more

# Destructuring
first, second = [1, 2]
puts "#{first} #{second}"

# Count things in one line
puts "mississippi".chars.tally

# ---------------------------------------------------------------
# 7. Records and enums
# ---------------------------------------------------------------

record Point(x, y)
pt = Point(3, 4)
puts pt.x
puts pt.y

enum Status(pending, active, done)
puts Status.active
puts Status.values

# ---------------------------------------------------------------
# 8. Lambdas
# ---------------------------------------------------------------

double = ->(n) { n * 2 }
puts double.call(21)
puts double.(21)              # shorthand

# Pipe: feed the left value into the function on the right
total_piped = [3, 1, 2] |> sum
puts total_piped

# ---------------------------------------------------------------
# 9. Error handling
# ---------------------------------------------------------------

begin
  1 / 0
rescue ZeroDivisionError e
  puts "caught: #{e}"
end

# Define and raise your own errors
error NotFound
begin
  raise NotFound, "no such thing"
rescue NotFound e
  puts "custom: #{e}"
end

# ---------------------------------------------------------------
# 10. Ranges, sequences and math
# ---------------------------------------------------------------

puts (1..5).to_a
puts seq(0, 10, 5)
puts sqrt(16)
puts factorial(5)
puts sin(0)
puts PI

# Complex numbers and matrices (v1.22)
puts complex_new(3, 4)
puts matrix_identity(2)

# ---------------------------------------------------------------
# 11. Files, JSON, time
# ---------------------------------------------------------------

puts json_encode({a: 1, b: [1, 2]})
puts json_parse("{\"x\": 5}")["x"]

# ---------------------------------------------------------------
# 12. Organizing code and going further
# ---------------------------------------------------------------

# require "file.fk"          merges another file into this scope
# import "file.fk" as lib    loads it under its own namespace (lib.func)
# stitch "frankiecolor"      loads an official library (a "stitch")
#
# Other things worth exploring:
#   - web_app() for a built-in web server and WebSockets
#   - db_open() for built-in SQLite
#   - test / assert_eq with `frankiec test`
#   - frankiegame / frankiecanvas stitches for terminal and browser games
#   - frankiec bundle to ship a program as one .py file
#
# Source and docs: https://github.com/atejada/Frankie
```
