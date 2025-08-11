---
name: Odin
contributors:
    - ["Collin MacDonald", "https://github.com/CollinEMac"]
filename: learnodin.odin
---

Odin was created by Bill "gingerBill" Hall. It is a general-purpose systems
programming language that emphasizes simplicity, readability, and performance
without garbage collection. Odin bills itself as "the C alternative for the
joy of programming."

```
// Single line comments start with two slashes.

/*
   Multiline comments start with slash-star,
   and end with star-slash. They can be nested!
   /*
       Like this!
   */
*/

// This is the classic "hello world" program in Odin.
package main

import "core:fmt"

main :: proc() {
    fmt.println("Hellope!")
}
```

## 1. Basic Data Types and Operators

```
// Integers - Odin has explicit sized integer types
x: i32 = 42          // 32-bit signed integer
y: u64 = 100         // 64-bit unsigned integer
z: int = 123         // Platform-dependent integer (usually i64)

// You can use underscores for readability in numbers
big_number := 1_000_000

// Floating point numbers
pi: f32 = 3.14159    // 32-bit float
e: f64 = 2.71828     // 64-bit float (default for float literals)

// Boolean
is_true: bool = true
is_false: bool = false

// Rune (Unicode character)
letter: rune = 'A'
emoji: rune = '🚀'

// Strings
name: string = "Odin Programming"
raw_string := `C:\Windows\System32`  // Raw string with backticks

// String length (in bytes, not characters!)
length := len(name)

// Arithmetic operators work as you'd expect
result := 10 + 5    // 15
diff := 10 - 5      // 5
product := 10 * 5   // 50
quotient := 10 / 5  // 2
remainder := 10 % 3 // 1

// Comparison operators
is_equal := 10 == 10        // true
not_equal := 10 != 5        // true
greater := 10 > 5           // true
less_equal := 5 <= 10       // true

// Logical operators
and_result := true && false  // false
or_result := true || false   // true
not_result := !true          // false

// Bitwise operators
bit_and := 0b1010 & 0b1100   // 0b1000
bit_or := 0b1010 | 0b1100    // 0b1110
bit_xor := 0b1010 ~ 0b1100   // 0b0110 (note: ~ is XOR in Odin)
bit_not := ~u8(0b1010)           // bitwise NOT

```

## 2. Variables and Constants

```
// Variable declaration with type inference
some_number := 42            // Type inferred as int
some_text := "Hello"         // Type inferred as string

// Explicit type declaration
explicit_int: int = 42
explicit_float: f64 = 3.14

// Uninitialized variables are zero-initialized
uninitialized_int: int       // Defaults to 0
uninitialized_bool: bool     // Defaults to false
uninitialized_string: string // Defaults to ""

// Constants are defined with ::
PI :: 3.14159
MESSAGE :: "This is a constant"

// Typed constants
TYPED_CONSTANT : f32 : 2.71828

// Multiple assignment
a, b := 10, 20
a, b = b, a  // Swap values
```

## 3. Arrays and Slices

```
// Fixed-size arrays
numbers: [5]int = {1, 2, 3, 4, 5}
chars: [3]rune = {'A', 'B', 'C'}

// Array with inferred size
inferred := [?]int{10, 20, 30, 40}

// Zero-initialized array
zeros: [10]int  // All elements are 0

// Accessing array elements
first := numbers[0]     // 1
last := numbers[4]      // 5
array_length := len(numbers)  // 5

// Slices - dynamic views into arrays
slice: []int = {1, 2, 3, 4, 5}  // Slice literal
array_slice := numbers[1:4]     // Slice of array from index 1 to 3
full_slice := numbers[:]        // Slice of entire array

// Dynamic arrays - can grow and shrink
dynamic_array: [dynamic]int
append(&dynamic_array, 1)
append(&dynamic_array, 2, 3, 4)  // Append multiple elements

// Remember to clean up dynamic arrays
defer delete(dynamic_array)
```

## 4. Control Flow

```
// If statements
age := 25
if age >= 18 {
    fmt.println("Adult")
} else if age >= 13 {
    fmt.println("Teenager")
} else {
    fmt.println("Child")
}

// For loops - Odin's only loop construct
// C-style for loop
for i := 0; i < 10; i += 1 {
    fmt.println(i)
}

// While-style loop
counter := 0
for counter < 5 {
    fmt.println(counter)
    counter += 1
}

// Infinite loop
for {
    // This runs forever (until break)
    break  // Exit the loop
}

// Iterating over arrays/slices with index
numbers_array := [3]int{10, 20, 30}
for value, index in numbers_array {
    fmt.printf("Index %d: Value %d\n", index, value)
}

// Iterating over just values
for value in numbers_array {
    fmt.println(value)
}

// Switch statements
day := "Monday"
switch day {
case "Monday", "Tuesday", "Wednesday", "Thursday", "Friday":
    fmt.println("Weekday")
case "Saturday", "Sunday":
    fmt.println("Weekend")
case:  // Default case
    fmt.println("Unknown day")
}

// Switch with no condition (like if-else chain)
switch {
case age < 13:
    fmt.println("Child")
case age < 20:
    fmt.println("Teenager")
case:
    fmt.println("Adult")
}
```

## 5. Procedures (Functions)

```
// Basic procedure definition
add :: proc(a: int, b: int) -> int {
    return a + b
}

// Procedure with multiple return values
divide :: proc(a: int, b: int) -> (int, bool) {
    if b == 0 {
        return 0, false  // Division by zero
    }
    return a / b, true
}

// Using the procedure
sum := add(5, 3)                    // 8
quotient, ok := divide(10, 2)       // 5, true
quotient_bad, ok_bad := divide(10, 0) // 0, false

// Something akin to overloading can be mimicked using producure groups 
greet_string :: proc(name: string) {
    fmt.printf("Hello, %s!\n", name)
}

greet_nothing :: proc() {
    greet_string("World") // or greet("World") will work with the following procedure group
}

greet :: proc{
    greet_string,
    greet_nothing,
}

// Variadic procedures (variable number of arguments)
sum_all :: proc(numbers: ..int) -> int {
    total := 0
    for number in numbers {
        total += number
    }
    return total
}

result_sum := sum_all(1, 2, 3, 4, 5)  // 15
```

## 6. Structs

```
// Struct definition
Person :: struct {
    name: string,
    age:  int,
    height: f32,
}

// Creating struct instances
person1 := Person{
    name = "Alice",
    age = 30,
    height = 5.6,
}

// Partial initialization (remaining fields are zero-initialized)
person2 := Person{
    name = "Bob",
    // age and height default to 0
}

// Accessing struct fields
fmt.printf("%s is %d years old\n", person1.name, person1.age)

// Modifying struct fields
person1.age = 31

// Procedure that works with structs
celebrate_birthday :: proc(person: ^Person) {  // ^ means pointer
    person.age += 1
    fmt.printf("Happy birthday! %s is now %d\n", person.name, person.age)
}

celebrate_birthday(&person1)  // Pass address with &
```

## 7. Enums and Unions

```
// Enums
Color :: enum {
    RED,
    GREEN,
    BLUE,
    YELLOW,
}

my_color := Color.RED

// Enums with explicit values
Status :: enum u8 {
    OK = 0,
    ERROR = 1,
    WARNING = 2,
}

// Unions
IntOrBool :: union {int, bool}

f: IntOrBool = 123

// Pattern matching with unions
switch _ in f {
case int:  fmt.println("int")
case bool: fmt.println("bool")
case:
}
```

## 8. Maps

```
// Map declaration
scores: map[string]int

// Initialize map
scores = make(map[string]int)
defer delete(scores)  // Clean up when done

// Add key-value pairs
scores["Alice"] = 95
scores["Bob"] = 87
scores["Charlie"] = 92

// Access values
alice_score := scores["Alice"]  // 95

// Check if key exists
bob_score, exists := scores["Bob"]
if exists {
    fmt.printf("Bob's score: %d\n", bob_score)
}

// Iterate over map
for name, score in scores {
    fmt.printf("%s: %d\n", name, score)
}
```

## 9. Pointers and Memory Management

```
// Pointers
number := 42
number_ptr := &number        // Get address of number
value := number_ptr^         // Dereference pointer (get value)

fmt.printf("Value: %d, Address: %p\n", value, number_ptr)

// Dynamic memory allocation
// new() allocates and returns a pointer
int_ptr := new(int)
int_ptr^ = 100
defer free(int_ptr)  // Clean up memory

// make() for complex types
my_slice := make([]int, 5)    // Slice with length 5
defer delete(my_slice)
```

## 10. Error Handling

```
// Odin uses multiple return values for error handling
read_file :: proc(filename: string) -> (string, bool) {
    // Simulate file reading
    if filename == "" {
        return "", false  // Error case
    }
    return "file contents", true  // Success case
}

// Using the error-returning procedure
content, success := read_file("myfile.txt")
if !success {
    fmt.println("Failed to read file")
} else {
    fmt.printf("File content: %s\n", content)
}

// Common pattern with or_return
parse_number :: proc(s: string) -> (int, bool) {
    // This is a simplified example
    if s == "42" {
        return 42, true
    }
    return 0, false
}

example_with_error_handling :: proc() -> bool {
    // or_return automatically returns false if the second value is false
    num := parse_number("42") or_return
    fmt.printf("Parsed number: %d\n", num)
    return true
}
```

## 11. Packages and Imports

```
package main
// Every .odin file starts with a package declaration

// Import from core library
import "core:fmt"
import "core:strings"
import "core:os"

// Import with alias
import str "core:strings"

main :: proc() {
    // Using imported procedures
    text := "Hello, World!"
    upper_text := strings.to_upper(text)
    fmt.println(upper_text)
}

// Import from vendor packages (external libraries)
// import "vendor:raylib"
```

## 12. Compile-time Features

```
// Compile-time conditionals
when ODIN_OS == .Windows {
    // Windows-specific code
    fmt.println("Running on Windows")
} else when ODIN_OS == .Linux {
    // Linux-specific code
    fmt.println("Running on Linux")
} else {
    // Other platforms
    fmt.println("Running on other platform")
}

// Compile-time constants
ODIN_DEBUG :: #config(DEBUG, false)

when ODIN_DEBUG {
    fmt.println("Debug mode enabled")
}

// Generics (Parametric polymorphism)
Generic_Array :: struct($T: typeid) {
    data: []T,
}

max :: proc(a: $T, b: T) -> T {
    return a if a > b else b
}

max_int := max(10, 20)      // T becomes int
max_float := max(3.14, 2.71) // T becomes f64
```

## 13. Built-in Data Structures

```
// Bit sets for flags
File_Mode :: enum {
    READ,
    WRITE,
    EXECUTE,
}

permissions: bit_set[File_Mode]
permissions |= {.READ, .WRITE}        // Set multiple flags
permissions &~= {.WRITE}              // Remove flag
has_read := .READ in permissions      // Check flag
is_readonly := permissions == {.READ} // Compare sets

// Complex numbers
z1 := complex64(3 + 4i)
z2 := complex64(1 - 2i)
sum := z1 + z2                        // (4 + 2i)
magnitude := abs(z1)                  // 5.0

// Matrices for linear algebra
transform := matrix[3, 3]f32{
    1, 0, 5,  // Translation X = 5
    0, 1, 3,  // Translation Y = 3  
    0, 0, 1,  // Homogeneous coordinate
}

point := [3]f32{10, 20, 1}
transformed := transform * point      // Matrix multiplication

// Quaternions for 3D rotations
identity_rot := quaternion(w = 1, x = 0, y = 0, z = 0)  // No rotation
rotation_90_z := quaternion(w = 0.707, x = 0, y = 0, z = 0.707)  // 90° around Z
```

## 14. Context System and Defer

```
// Odin has an implicit context system for threading allocators,
// loggers, and other utilities through your program

example_with_context :: proc() {
    // Save current context
    old_allocator := context.allocator
    
    // Use a different allocator temporarily
    temp_allocator := context.temp_allocator
    context.allocator = temp_allocator
    
    // All allocations in this scope use temp_allocator
    temp_data := make([]int, 100)
    // No need to delete temp_data - it's automatically cleaned up
    
    // Restore original allocator
    context.allocator = old_allocator
}

// defer ensures cleanup happens when scope exits
resource_management_example :: proc() {
    // Allocate some memory
    buffer := make([]u8, 1024)
    defer delete(buffer)  // Always freed when function exits
    
    // Allocate a map
    data := make(map[string]int)
    defer delete(data)    // Always cleaned up when function exits
    
    // Use buffer and data...
    // They're automatically cleaned up even if we return early
}
```

## Further Reading

The [Odin Programming Language website](https://odin-lang.org/) provides 
excellent documentation and examples.

The [overview](https://odin-lang.org/docs/overview/) covers much of the 
language in detail.

The [GitHub examples repository](https://github.com/odin-lang/examples) 
contains idiomatic Odin code examples.

For learning resources:
- ["Understanding the Odin Programming Language" by Karl Zylinski](https://odinbook.com)
- [Odin Discord](https://discord.gg/sVBPHEv) for community support
- [FAQ](https://odin-lang.org/docs/faq/) for common questions
