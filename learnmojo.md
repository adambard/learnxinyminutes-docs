---
name: Mojo
contributors:
    - ["darthlasersword", "https://github.com/darthlasersword"]
filename: learnmojo.mojo
---

Mojo is a systems programming language that keeps Python's syntax while
adding static typing, memory safety, and the performance of C and Rust.
It's developed by Modular and reached version 1.0 in August 2026. If you
know Python, you likely already know most of Mojo's look.

*Note: Mojo is still evolving. This tutorial reflects the language
as of Mojo 1.0 (August 2026), but syntax, keywords, and standard library
APIs have changed substantially from release to release. Treat this as a
starting point, not a reference — check the official docs and release
notes for current behavior before relying on anything here.*

Docs: https://docs.modular.com/mojo/

Release notes: https://docs.modular.com/mojo/releases/

---

Without further ado, let's begin!

```mojo
# Comments begin with a hash sign.

"""
Docstrings use triple quotes,
just like in Python.
"""

####################################################
## 0. Getting Started
####################################################

# Every Mojo program needs a `main()` function as its entry point

def main():
    print("Hello, Mojo!")  # => Hello, Mojo!

# All functions are declared with `def`

def add(a: Int, b: Int) -> Int:
    return a + b

add(2, 3)   # => 5

# Functions are non-raising by default. If a function (or something it
# calls) can raise an error, mark it with `raises`:

def risky() raises:
    raise Error("something went wrong")

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# Mojo is statically typed. Common scalar types:

var an_int: Int = 42          # machine integer (64-bit by default)
var tiny: Int8 = 7            # fixed-width integers: Int8/16/32/64
var big: UInt64 = 42          # unsigned variants too
var pi: Float64 = 3.14        # Float32 and Float64
var flag: Bool = True         # True / False

# Types never change after declaration:
an_int = "nope"   # Error: cannot convert StringLiteral to Int

# Arithmetic works as expected
1 + 1    # => 2
10 * 3   # => 30
7 // 2   # => 3   (integer floor division)
7 % 2    # => 1   (modulo)
2 ** 10  # => 1024

# Int division truncates toward zero:
-7 // 2  # => -3

# Cast explicitly when you need floating-point math:
Float64(7) / 2.0  # => 3.5

# Comparisons yield Bool
1 < 2           # => True
2 == 2          # => True
"abc" != "abd"  # => True

# Boolean operators
True and False   # => False
False or True    # => True
not True         # => False

# Strings are UTF-8 encoded
var s = String("hello")
s += " world"
len(s)      # => 11
s[0]        # => 'h'

str(123)    # => "123"  (convert numbers to strings)

# Ordinary string literals don't interpolate braces. For interpolation,
# prefix the literal with `t` to create a *template string*:
var name = "Mojo"
print(t"Hello, {name}!")      # => Hello, Mojo!
print(t"2+2 is {2 + 2}")      # => 2+2 is 4

# The older alternative is String.format(), using positional/auto indices:
print("{} plus {} is {}".format(2, 2, 4))   # => 2 plus 2 is 4

# SIMD vectors are built into the language. This is where Mojo shines:
var vec = SIMD[DType.float32, 4](1, 2, 3, 4)
vec * 2     # => [2.0, 4.0, 6.0, 8.0] in a single instruction
vec.reduce_max()   # => 4.0

####################################################
## 2. Variables, Ownership, and Argument Conventions
####################################################

# Declare variables with `var` (the only variable keyword):
var counter = 0
counter += 1   # counter is now 1

# Every value has exactly one owner at a time. When the owner dies,
# the value is destroyed.

# Function arguments declare their convention with a keyword:

# default (no keyword): an immutable reference, read but don't modify
def inspect(value: Int) -> Int:
    return value * 2

# `mut`: a mutable reference, changes inside are visible outside
def bump(mut n: Int):
    n += 1

# `var`: the function takes ownership of the value
def consume(var msg: String) -> String:
    return msg + "!"

# At the call site, use the postfix `^` sigil to transfer ownership.
# After transferring, the original variable is uninitialized:
var text = String("hi")
text = consume(text^)

####################################################
## 3. Collections
####################################################

# Dynamic arrays (like Python lists):
var nums = List[Int](1, 2, 3)
nums.append(4)
nums[0]       # => 1
nums[-1]      # => 4
len(nums)     # => 4

for x in nums:
    print(x)  # prints 1 2 3 4

# List literals work too:
var values: List[Int] = [1, 2, 3, 4]

# Tuples are fixed-size heterogeneous groups:
var pair = (1, "one")
pair[0]        # => 1
pair[1]        # => "one"

# Dictionaries:
var ages = Dict[String, Int]()
ages["Ana"] = 30
ages["Bo"] = 25
ages["Ana"]            # => 30
ages.get("Zed", -1)    # => -1  (default if missing)

for key, value in ages.items():
    print(key, value)

####################################################
## 4. Control Flow
####################################################

var score = 87

if score >= 90:
    print("A")
elif score >= 80:
    print("B")   # this branch runs
else:
    print("C")

# While loops:
var i = 0
while i < 3:
    print(i)     # prints 0 1 2
    i += 1

# For loops iterate over anything iterable:
for c in String("abc"):
    print(c)     # prints a b c

for j in range(3):
    print(j)     # prints 0 1 2

for k in range(10, 20, 5):
    print(k)     # prints 10 15

# Errors are handled with try/except:
try:
    risky()
except e:
    print("Handled:", e)   # must handle errors in non-raising functions

####################################################
## 5. Structs (Mojo Doesn't Have Classes... *Yet*)
####################################################

# Mojo doesn't have a `class` keyword in 1.0. Classes defined by the user are still on
# the roadmap. Instead, Mojo uses `struct`. Structs are bound at compile
# time. They don't have dynamic attributes or runtime modification, making
# them fast and safe.

struct Point:
    var x: Float64
    var y: Float64

    # Constructors receive `out self`: an uninitialized value that the
    # constructor must initialize before returning.
    def __init__(out self, x: Float64, y: Float64):
        self.x = x
        self.y = y

    def distance_from_origin(self) -> Float64:
        return math.sqrt(self.x**2 + self.y**2)

@fieldwise_init   # generates the field-by-field constructor for you
struct Label:
    var text: String
    var priority: Int = 0

var p = Point(3.0, 4.0)
p.distance_from_origin()   # => 5.0

# Traits define shared interfaces. A struct conforms by implementing
# every requirement:

trait Describable:
    fn describe(self) -> String: ...

struct Circle(Describable):
    var radius: Float64

    def __init__(out self, r: Float64):
        self.radius = r

    def describe(self) -> String:
        return t"circle of radius {self.radius}"

# To accept any conforming type, use the trait as a *parameter* bound
# (square brackets), not as a plain argument type:
def show[T: Describable](thing: T):
    print(thing.describe())

show(Circle(2.0))   # => circle of radius 2.0

####################################################
## 6. Generics and Compile-Time Programming
####################################################

# `comptime` declares compile-time constants and type aliases:
comptime WIDTH = 640
comptime GREETING = "hello"
comptime MyInt = Int   # comptime can also alias a type

# Parameters live in square brackets and are resolved at compile time.
# The compiler generates a specialized version of the function for each
# distinct parameter value:

def repeat[count: Int](msg: String):
    comptime for i in range(count):   # evaluated during compilation
        print(msg)

repeat[3]("Hi")   # prints Hi three times; one specialized copy of the loop

# You can parameterize over types AND integers:
def matrix[T: DType, rows: Int, cols: Int]():
    pass

# Standard library types like List and Dict are themselves generic:
var words = List[String]("alpha", "beta")

####################################################
## 7. Interoperability with Python
####################################################

# Mojo can use any Python library directly:

from std.python import Python

def use_numpy() raises:
    var np = Python.import_module("numpy")
    var arr = np.array([1, 2, 3, 4])
    print(arr.mean())   # => 2.5

# Your existing Python ecosystem keeps working while you migrate hot
# paths to Mojo incrementally.

####################################################
## 8. Parallelism and Performance
####################################################

# Mojo was designed around hardware acceleration. A taste:

from algorithm import parallelize

def square_all(mut data: List[Int]):
    def worker(i: Int):
        data[i] = data[i] * data[i]

    parallelize[worker](data, len(data))

# Other tools in the box:
#   - SIMD vectors (shown above) for data-level parallelism
#   - a limited async/await for asynchronous I/O (a full async runtime
#     is still on the roadmap post-1.0)
#   - explicit references (`ref`) and pointers when you need them
#   - no global interpreter lock. You get true multithreading out of the box

```

## Quick Differences From Python

- The `fn` keyword has been removed from Mojo; use `def` instead
- Uses `var` for mutable types; everything else is statically typed
- Ownership model: default/mut/var/ref/out argument conventions
- `^` transfers ownership; values die with their owner
- `struct` instead of `class`. Mojo doesn't have classes yet (as of 1.0)
- Plain string literals don't interpolate; use `t"..."` template strings or String.format() for that
- Traits via parameter bounds instead of duck typing
- Parameters in `[]` enable compile-time metaprogramming; `comptime` declares compile-time constants and type aliases
- Functions don't raise unless marked `raises`
- Most valid Python still runs inside Mojo via the interop layer

### Free Online Resources

* [The Official Mojo Docs](https://docs.modular.com/mojo/)
* [Mojo Manual](https://docs.modular.com/mojo/manual/)
* [Modular's GitHub](https://github.com/modular/modular)
* [Mojo Release Notes](https://docs.modular.com/mojo/releases/)
* [Mojo Community Discord](https://discord.gg/modular)
