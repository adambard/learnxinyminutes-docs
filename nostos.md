---
name: Nostos
filename: learnnostos.nos
contributors:
    - ["Hallvard Ystad", "https://github.com/hallyhaa"]
---

Nostos is a functional-first programming language with lightweight
concurrency, pattern matching, and non-blocking I/O. Built in Rust, it
features a register-based VM with optional JIT compilation, Erlang-style
processes, and Hindley-Milner type inference. The name comes from Greek
(nostos) meaning "homecoming."

```nostos
# Single-line comments start with a hash.

#* Multi-line comments
   are wrapped like this. *#

## Documentation comments use double hash.
## They are available via introspection at runtime.

## 1. Primitive Types

# Integers (64-bit signed)
42
-17
0xFF          # hexadecimal
0b1010        # binary
1_000_000     # underscores for readability

# Floats (64-bit IEEE 754)
3.14159
1.2e-10

# Decimals (arbitrary precision)
0.1d          # exact decimal, no floating-point surprises

# Booleans
true
false

# Characters
'a'
'\n'

# Strings (UTF-8, double-quoted)
"Hello, world!"
"Interpolation: ${40 + 2}"    # => "Interpolation: 42"

# Unit (like void, but is a value)
()

## 2. Variables

# Immutable by default
x = 5
name = "Alice"

# Mutable variables use `var`
var count = 0
count = count + 1     # OK
count += 1            # shorthand works too

# Variables are block-scoped. Rebinding an immutable variable to
# a different value is a runtime error (like Erlang's unification).
x = 5
x = 5             # OK — same value
# x = 10          # ERROR — x is already bound to 5

# Shadowing in inner scopes is allowed
result = {
    x = 10        # shadows outer x within this block
    x + 1         # => 11
}
# x is still 5 here

# Type annotations are optional (inference handles most cases)
Int age = 30
String greeting = "hi"
List[Int] nums = [1, 2, 3]

## 3. Operators

# Arithmetic
1 + 2         # => 3
10 - 3        # => 7
4 * 5         # => 20
10 / 3        # => 3 (integer division)
10 % 3        # => 1 (modulo)
2 ** 10       # => 1024 (exponentiation)

# Comparison
1 == 1        # => true
1 != 2        # => true
3 < 5         # => true
5 >= 5        # => true

# Logical (short-circuit)
true && false  # => false
false || true  # => true
!true          # => false

# String concatenation
"Hello" ++ " " ++ "world"  # => "Hello world"

# List cons (prepend)
1 :: [2, 3]   # => [1, 2, 3]

## 4. Collections

# Lists (immutable, linked)
numbers = [1, 2, 3, 4, 5]
empty = []
nested = [[1, 2], [3, 4]]

# Head and tail via pattern matching
[head | tail] = [1, 2, 3]
# head => 1, tail => [2, 3]

# Maps (immutable, key-value)
person = %{"name": "Alice", "city": "Paris"}
squares = %{1: 1, 2: 4, 3: 9}
empty_map = %{}

# Sets (immutable, unique elements)
tags = #{"urgent", "review", "bug"}
unique = #{1, 2, 3, 2, 1}    # => #{1, 2, 3}
empty_set = #{}

# Tuples (fixed-size, mixed types)
pair = (1, "hello")
triple = (true, 3.14, "yes")

# Typed arrays (mutable, contiguous memory)
ints = newInt64Array(10)
floats = newFloat64Array(5)
ints[0] = 42                  # index assignment
x = ints[0]                   # index access

## 5. Functions

# Simple function definition
double(x) = x * 2
add(a, b) = a + b

# Multi-line functions use braces
process(data) = {
    validated = validate(data)
    transformed = transform(validated)
    save(transformed)       # last expression is the return value
}

# Type annotations on functions
length(s: String) -> Int = s.length()

# Lambda expressions
square = x => x * 2
sum = (a, b) => a + b

# Multi-line lambdas
transform = x => {
    y = x * 2
    y + 1
}

# Functions are first-class values
apply(f, x) = f(x)
apply(double, 5)    # => 10

# Function composition
compose(f, g) = x => f(g(x))

# UFCS: any function can be called as a method
# length(list) and list.length() are equivalent
[1, 2, 3].length()       # => 3
"hello".length()          # => 5

# Method chaining via UFCS
result = [1, 2, 3, 4, 5]
    .filter(x => x % 2 == 0)     # [2, 4]
    .map(x => x * x)             # [4, 16]
    .fold(0, (a, b) => a + b)    # 20

## 6. Pattern Matching

# Multi-clause functions match on arguments.
# All clauses must be consecutive (no other code in between)
# and in the same file. They are tried top-to-bottom.
fib(0) = 0
fib(1) = 1
fib(n) = fib(n - 1) + fib(n - 2)

# Pattern matching on lists
sum([]) = 0
sum([head | tail]) = head + sum(tail)

# Quicksort in three lines
quicksort([]) = []
quicksort([pivot | rest]) =
    quicksort(rest.filter(x => x < pivot))
    ++ [pivot]
    ++ quicksort(rest.filter(x => x >= pivot))

# Guards with `when`
abs(n) when n >= 0 = n
abs(n) = -n

classify(n) when n > 0 = "positive"
classify(n) when n < 0 = "negative"
classify(0) = "zero"

# Match expressions
describe(n) = match n {
    0 -> "zero"
    1 -> "one"
    _ -> "many"
}

# Pattern matching on algebraic types (see section 8)
area(Circle(r)) = 3.14159 * r * r
area(Rectangle(w, h)) = w * h

# Tuple pattern matching
compare(a, b) = match (a > b, a < b) {
    (true, _) -> "greater"
    (_, true) -> "less"
    _ -> "equal"
}

# Destructuring assignment
(a, b) = (1, 2)
{name: n, age: a} = %{"name": "Alice", "age": 30}
[first | rest] = [1, 2, 3]

# Wildcard _ ignores a value
(_, second) = (1, 2)    # second => 2

## 7. Control Flow

# If expression (always returns a value)
max(a, b) = if a > b then a else b

sign(n) =
    if n > 0 then 1
    else if n < 0 then -1
    else 0

# For loops (end is exclusive)
main() = {
    var total = 0
    for i = 1 to 11 {
        total = total + i
    }
    # total => 55

    # While loops
    var n = 10
    while n > 0 {
        println(show(n))
        n = n - 1
    }

    # Break and continue
    var found = -1
    for i = 0 to 100 {
        if i * i > 50 then {
            found = i
            break
        } else ()
    }

    # Early return
    found
}

## 8. Types

# Record types (product types)
type Point = { x: Float, y: Float }
type User = { name: String, age: Int }

# Construction
p = Point(3.0, 4.0)             # positional
p = Point(x: 3.0, y: 4.0)      # named fields

# Field access
p.x     # => 3.0

# Functional update (creates a new record)
q = Point(p, x: 10.0)    # copy p with x overridden

# Sum types (algebraic data types / variants)
type Shape =
    | Circle(Float)
    | Rectangle(Float, Float)
    | Triangle(Float, Float, Float)

type Direction = North | South | East | West

# Generic types
type Tree[T] = Leaf | Node(T, Tree[T], Tree[T])

type Option[T] = None | Some(T)
type Result[T, E] = Ok(T) | Err(E)

# Pattern matching on types
depth(Leaf) = 0
depth(Node(_, left, right)) =
    1 + max(depth(left), depth(right))

# Mutable record types
var type MutPoint = { x: Float, y: Float }
mp = MutPoint(1.0, 2.0)
mp.x = 5.0    # direct mutation OK

## 9. Traits

# Define a trait (interface)
trait Animal
    speak(self) -> String
    describe(self) -> String
end

# Implement a trait for a type
type Dog = { name: String, age: Int }
type Cat = { name: String, lives: Int }

Dog: Animal
    speak(self) = "Woof! I'm " ++ self.name
    describe(self) = self.name ++ ", age " ++ show(self.age)
end

Cat: Animal
    speak(self) = "Meow! I'm " ++ self.name
    describe(self) = self.name ++ ", " ++ show(self.lives) ++ " lives"
end

# Supertraits (trait inheritance)
# trait Serializable: Show
#     toJson(self) -> String
# end

# Using traits
dog = Dog("Buddy", 5)
dog.speak()       # => "Woof! I'm Buddy"
dog.describe()    # => "Buddy, age 5"

## 10. Modules and Imports

# Import everything from a module
# use math.*

# Import specific items
# use stdlib.server.{serve, respondText}

# Import with alias
# use graphics.{draw as graphicsDraw}

# Public visibility with `pub` (private by default)
# module Geometry
#     pub type Point = { x: Float, y: Float }
#     pub distance(a: Point, b: Point) = ...
#     square(x) = x * x    # private helper
# end

## 11. Error Handling

# Throw any value as an exception
safe_divide(a, b) =
    if b == 0 then throw("division by zero")
    else a / b

# Try/catch with pattern matching
result = try { safe_divide(10, 0) }
    catch {
        "division by zero" -> -1
        other -> -2
    }

# Nested try/catch
outer =
    try {
        try { throw("deep") }
        catch { "deep" -> "handled" }
    }
    catch { e -> "outer: " ++ e }
# => "handled"

## 12. Concurrency

# Nostos uses lightweight processes (like Erlang).
# Each process is ~2KB. You can spawn millions of them.
# All I/O is non-blocking — no async/await needed.

# Spawn a process
pid = spawn { some_function() }

# Get current process ID
me = self()

# Send a message with <-
pid <- "hello"
pid <- 42
pid <- (x, y, z)

# Receive messages with pattern matching
receive {
    "hello" -> println("got hello")
    n -> println("got: " ++ show(n))
}

# A stateful counter server using message passing
type Msg = Inc(Pid) | Get(Pid) | Reply(Int)

counter_loop(state) = receive {
    Inc(sender) -> {
        sender <- Reply(state + 1)
        counter_loop(state + 1)
    }
    Get(sender) -> {
        sender <- Reply(state)
        counter_loop(state)
    }
}

increment(counter) = {
    counter <- Inc(self())
    receive { Reply(val) -> val }
}

# Ping-pong between two processes
type PingMsg = Ping(Pid, Int) | Pong(Int) | Done(Int) | Stop

ping(pong_pid, 0, parent) = parent <- Done(0)
ping(pong_pid, n, parent) = {
    pong_pid <- Ping(self(), n)
    receive {
        Pong(m) -> ping(pong_pid, m - 1, parent)
    }
}

pong() = receive {
    Ping(sender, n) -> {
        sender <- Pong(n)
        pong()
    }
    Stop -> ()
}

# Thread-safe global variables with mvar (type annotation required)
mvar requestCount: Int = 0

# Reads and writes are atomic within a function — no other
# process can modify the mvar between read and write here.
increment() = { requestCount = requestCount + 1; requestCount }

## 13. Standard Library

# List operations (all chainable via UFCS)
[1, 2, 3].map(x => x * 2)               # => [2, 4, 6]
[1, 2, 3, 4].filter(x => x > 2)         # => [3, 4]
[1, 2, 3].fold(0, (acc, x) => acc + x)  # => 6
[3, 1, 2].sort()                         # => [1, 2, 3]
[1, 2, 3].reverse()                      # => [3, 2, 1]
[1, 2, 3].length()                       # => 3
[1, 2, 3].take(2)                        # => [1, 2]
[1, 2, 3].drop(1)                        # => [2, 3]
[[1, 2], [3, 4]].flatten()               # => [1, 2, 3, 4]
[1, 2].zip(["a", "b"])                   # => [(1, "a"), (2, "b")]
[1, 2, 3].any(x => x > 2)               # => true
[1, 2, 3].all(x => x > 0)               # => true

# String operations
"hello".length()                    # => 5
"hello".toUpper()                   # => "HELLO"
"  hello  ".trim()                  # => "hello"
"hello".contains("ell")            # => true
"hello".startsWith("he")           # => true
"hello".replace("l", "L")         # => "heLlo"
"hello".reverse()                   # => "olleh"
show(42)                            # => "42" (any value to string)

# JSON
data = jsonParse("{\"name\": \"Alice\"}")
str = jsonStringify(data)

# File I/O
(status, content) = File.readAll("config.txt")
(status, _) = File.writeAll("output.txt", "Hello!")
(status, exists) = File.exists("config.txt")

# Encoding
encoded = Base64.encode("Hello!")
encoded = Url.encode("a=1&b=2")

# Time
now = Time.now()
formatted = Time.format(now, "%Y-%m-%d %H:%M:%S")

# Crypto
hash = Crypto.sha256("data")
bcrypt = Crypto.bcryptHash("password", 12)
valid = Crypto.bcryptVerify("password", bcrypt)

## 14. HTTP and Networking

# HTTP client
(status, resp) = Http.get("https://api.example.com/data")
(status, resp) = Http.post("https://api.example.com", body)

# HTTP server (handler receives a single request argument)
use stdlib.server.*

handler(req) =
    Server.respond(req.id, 200,
        [("Content-Type", "text/plain")],
        "Hello, World!")

main() = serve(8080, handler)    # each request is spawned as a process

# TCP sockets
server = Tcp.listen(9000)
client = Tcp.accept(server)
Tcp.send(client, "Hello!")
message = Tcp.receive(client)

# WebSockets
ws = WebSocket.connect("wss://echo.websocket.org")
WebSocket.send(ws, "Hello!")
response = WebSocket.recv(ws)

## 15. PostgreSQL

# Connect and query
conn = Pg.connect(
    "host=localhost dbname=mydb user=postgres password=secret")

# Parameterized queries (prevents SQL injection)
rows = Pg.query(conn,
    "SELECT name, email FROM users WHERE age > $1",
    (18))

rows.map(row => println(row.0 ++ ": " ++ row.1))

# Typed results
use stdlib.db.{query}
type User = { name: String, email: String }
users: List[User] = query[User](conn,
    "SELECT name, email FROM users", ())

# Transactions
Pg.begin(conn)
Pg.query(conn, "INSERT INTO users (name) VALUES ($1)", ("Alice"))
Pg.commit(conn)

Pg.close(conn)

## 16. Introspection

# Nostos has full runtime introspection.

# Value introspection
typeOf(42)            # get the type of any value
point.fields()        # list field names of a record
point.toMap()         # convert record to map

# Function introspection
fib.name              # => "fib"
fib.arity             # => 1
fib.source            # => source code as string

# REPL tools
# :profile fib(35)    — measure execution time
# :debug factorial    — set a breakpoint
# :type expr          — show inferred type

## 17. FFI (Rust Extensions)

# Nostos can call into Rust crates via extensions.

# In nostos.toml:
# [extensions]
# glam = { git = "https://github.com/pegesund/nostos-glam" }

# In code:
# import glam
# v = glam.vec3(1.0, 2.0, 3.0)
# normalized = v.normalize()

## 18. Putting It All Together

# A complete binary search tree example

type Tree[T] = Leaf | Node(T, Tree[T], Tree[T])

size(Leaf) = 0
size(Node(_, left, right)) = 1 + size(left) + size(right)

sum_tree(Leaf) = 0
sum_tree(Node(v, left, right)) =
    v + sum_tree(left) + sum_tree(right)

inorder(Leaf) = []
inorder(Node(v, left, right)) =
    inorder(left) ++ [v] ++ inorder(right)

map_tree(_, Leaf) = Leaf
map_tree(f, Node(v, left, right)) =
    Node(f(v), map_tree(f, left), map_tree(f, right))

main() = {
    tree = Node(4,
        Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)),
        Node(6, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf))
    )

    println("Size: " ++ show(size(tree)))       # 7
    println("Sum: " ++ show(sum_tree(tree)))     # 28
    println("Sorted: " ++ show(inorder(tree)))   # [1,2,3,4,5,6,7]

    doubled = map_tree(x => x * 2, tree)
    println("Doubled sum: " ++ show(sum_tree(doubled)))  # 56
}
```

## Further Reading

* [Nostos Tutorial](https://heynostos.tech) - Full tutorial and documentation
* [Nostos on GitHub](https://github.com/pegesund/nostos)
* [Nostos on GitHub](https://github.com/pegesund/nostos/blob/master/README.md) - Source code and examples
* [Nostos Releases](https://github.com/pegesund/nostos/releases/latest)
* [VS Code Extension](https://github.com/pegesund/nostos/tree/master/editors/vscode) - Editor support with LSP
