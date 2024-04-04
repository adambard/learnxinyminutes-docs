---
language: zig
filename: learnzig.zig
contributors:
    - ["Philippe Pittoli", "https://karchnu.fr/"]
---

[Zig][ziglang] aims to be a replacement for the C programming language.

**WARNING**: this document expects you to understand a few basic concepts in computer science, such as pointers, stack and heap memory, etc.

**WARNING**: Zig isn't considered as ready for production. Bugs are expected.

Prior knowledge of C is recommended.

## Quick overview: Zig compared to C

- Syntax is mostly the same, with some improvements (less ambiguity).
- Zig introduces namespaces.
- `try` and `catch` mechanism, which is both convenient, efficient and optional.
- Most of the C undefined behaviors (UBs) are fixed.
- Compared to C, raw pointers are safer to use and less likely to be needed.
  * The type system distinguishes between a pointer to a single value, or multiple values, etc.
  * Slices are preferred, which is a structure with a pointer and a runtime known size, which characterizes most uses of pointers in the first place.
- Some arbitrary language limitations are removed. For example, enumerations, structures and unions can have functions.
- Simple access to SIMD operations (basic maths on vectors).
- Zig provides both low-level features of C and the one provided through compiler extensions.
  For example: packed structures.
- An extensive standard library, including data structures and algorithms.
- Cross-compilation capability is provided by default, without any dependency.
  Different libc are provided to ease the process.
  Cross-compilation works from, and to, any operating system and architecture.

## Zig language

```zig
//! Top-level documentation.

/// Documentation comment.

// Simple comment.
```

### Hello world.

```zig
// Import standard library, reachable through the "std" constant.
const std = @import("std");

// "info" now refers to the "std.log.info" function.
const info = std.log.info;

// Usual hello world.
// syntax: [pub] fn <function-name>(<arguments>) <return-type> { <body> }
pub fn main() void {
    // Contrary to C functions, Zig functions have a fixed number of arguments.
    // In C: "printf" takes any number of arguments.
    // In Zig: std.log.info takes a format and a list of elements to print.
    info("hello world", .{});  // .{} is an empty anonymous tuple.
}
```

### Booleans, integers and float.

```zig
// Booleans.
// Keywords are preferred to operators for boolean operations.
print("{}\n{}\n{}\n", .{
    true and false,
    true or false,
    !true,
});

// Integers.
const one_plus_one: i32 = 1 + 1;
print("1 + 1 = {}\n", .{one_plus_one}); // 2

// Floats.
const seven_div_three: f32 = 7.0 / 3.0;
print("7.0 / 3.0 = {}\n", .{seven_div_three}); // 2.33333325e+00

// Integers have arbitrary value lengths.
var myvar: u10 = 5; // 10-bit unsigned integer
// Useful for example to read network packets, or complex binary formats.

// Number representation is greatly improved compared to C.
const one_billion = 1_000_000_000;         // Decimal.
const binary_mask = 0b1_1111_1111;         // Binary. Ex: network mask.
const permissions = 0o7_5_5;               // Octal.  Ex: Unix permissions.
const big_address = 0xFF80_0000_0000_0000; // Hexa.   Ex: IPv6 address.


// Overflow operators: tell the compiler when it's okay to overflow.
var i: u8 = 0;  // "i" is an unsigned 8-bit integer
i  -= 1;        // runtime overflow error (unsigned value always are positive)
i -%= 1;        // okay (wrapping operator), i == 255

// Saturation operators: values will stick to their lower and upper bounds.
var i: u8 = 200;   // "i" is an unsigned 8-bit integer (values: from 0 to 255)
i  +| 100 == 255   // u8: won't go higher than 255
i  -| 300 == 0     // unsigned, won't go lower than 0
i  *| 2   == 255   // u8: won't go higher than 255
i <<| 8   == 255   // u8: won't go higher than 255
```

### Arrays.

```zig
// An array is a well-defined structure with a length attribute (len).

// 5-byte array with undefined content (stack garbage).
var array1: [5]u8 = undefined;

// 5-byte array with defined content.
var array2 = [_]u8{ 1, 2, 3, 4, 5 };
// [_] means the compiler knows the length at compile-time.

// 1000-byte array with defined content (0).
var array3 = [_]u8{0} ** 1000;

// Another 1000-byte array with defined content.
// The content is provided by the "foo" function, called at compile-time and
// allows complex initializations.
var array4 = [_]u8{foo()} ** 1000;

// In any case, array.len gives the length of the array,
// array1.len and array2.len produce 5, array3.len and array4.len produce 1000.


// Modifying and accessing arrays content.

// Array of 10 32-bit undefined integers.
var some_integers: [10]i32 = undefined;

some_integers[0] = 30; // first element of the array is now 30

var x = some_integers[0]; // "x" now equals to 30, its type is inferred.
var y = some_integers[1]; // Second element of the array isn't defined.
                          // "y" got a stack garbage value (no runtime error).

// Array of 10 32-bit undefined integers.
var some_integers: [10]i32 = undefined;

var z = some_integers[20]; // index > array size, compilation error.

// At runtime, we loop over the elements of "some_integers" with an index.
// Index i = 20, then we try:
try some_integers[i]; // Runtime error 'index out of bounds'.
                      // "try" keyword is necessary when accessing an array with
                      // an index, since there is a potential runtime error.
                      // More on that later.
```

### Multidimensional arrays.

```zig
const mat4x4 = [4][4]f32{
    [_]f32{ 1.0, 0.0, 0.0, 0.0 },
    [_]f32{ 0.0, 1.0, 0.0, 1.0 },
    [_]f32{ 0.0, 0.0, 1.0, 0.0 },
    [_]f32{ 0.0, 0.0, 0.0, 1.0 },
};

// Access the 2D array then the inner array through indexes.
try expect(mat4x4[1][1] == 1.0);

// Here we iterate with for loops.
for (mat4x4) |row, row_index| {
    for (row) |cell, column_index| {
        // ...
    }
}
```

### Strings.

```zig
// Simple string constant.
const greetings = "hello";
// ... which is equivalent to:
const greetings: *const [5:0]u8 = "hello";
// In words: "greetings" is a constant value, a pointer on a constant array of 5
// elements (8-bit unsigned integers), with an extra '0' at the end.
// The extra "0" is called a "sentinel value".

print("string: {s}\n", .{greetings});

// This represents rather faithfully C strings. Although, Zig strings are
// structures, no need for "strlen" to compute their size.
// greetings.len == 5
```

### Slices.

```zig
// A slice is a pointer and a size, an array without compile-time known size.
// Slices have runtime out-of-band verifications.

const array = [_]u8{1,2,3,4,5};     // [_] = array with compile-time known size.
const slice = array[0..array.len];  // "slice" represents the whole array.
                                    // slice[10] gives a runtime error.
```

### Pointers.

```zig
// Pointer on a value can be created with "&".
const x: i32 = 1;
const pointer: *i32 = &x;  // "pointer" is a pointer on the i32 var "x".
print("1 = {}, {}\n", .{x, pointer});

// Pointer values are accessed and modified with ".*".
if (pointer.* == 1) {
    print("x value == {}\n", .{pointer.*});
}

// ".?" is a shortcut for "orelse unreachable".
const foo = pointer.?; // Get the pointed value, otherwise crash.
```

### Optional values (?<type>).

```zig
// An optional is a value than can be of any type or null.

// Example: "optional_value" can either be "null" or an unsigned 32-bit integer.
var optional_value: ?u32 = null; // optional_value == null
optional_value = 42;             // optional_value != null

// "some_function" returns ?u32
var x = some_function();
if (x) |value| {
    // In case "some_function" returned a value.
    // Do something with 'value'.
}
```

### Errors.

```zig
// Zig provides an unified way to express errors.

// Errors are defined in error enumerations, example:
const Error = error {
    WatchingAnyNetflixTVShow,
    BeOnTwitter,
};

// Normal enumerations are expressed the same way, but with "enum" keyword.
const SuccessStory = enum {
    DoingSport,
    ReadABook,
};


// Error union (!).
// Either the value "mylife" is an an error or a normal value.
var mylife: Error!SuccessStory = Error.BeOnTwitter;
// mylife is an error. Sad.

mylife = SuccessStory.ReadABook;
// Now mylife is an enum.


// Zig ships with many pre-defined errors. Example:
const value: anyerror!u32 = error.Broken;


// Handling errors.

// Some error examples.
const Error = error {
    UnExpected,
    Authentication,
};

// "some_function" can either return an "Error" or an integer.
fn some_function() Error!u8 {
    return Error.UnExpected; // It returns an error.
}

// Errors can be "catch" without intermediate variable.
var value = some_function() catch |err| switch(err) {
    Error.UnExpected     => return err,   // Returns the error.
    Error.Authentication => unreachable,  // Not expected. Crashes the program.
    else                 => unreachable,
};

// An error can be "catch" without giving it a name.
const unwrapped = some_function() catch 1234; // "unwrapped" = 1234

// "try" is a very handy shortcut for "catch |err| return err".
var value = try some_function();
// If "some_function" fails, the current function stops and returns the error.
// "value" can only have a valid value, the error already is handled with "try".
```

### Control flow.

```zig
// Conditional branching.

if (condition) {
    ...
}
else {
    ...
}

// Ternary.
var value = if (condition) x else y;

// Shortcut for "if (x) x else 0"
var value = x orelse 0;

// If "a" is an optional, which may contain a value.
if (a) |value| {
    print("value: {}\n", .{value});
}
else {
    print("'a' is null\n", .{});
}

// Get a pointer on the value (if it exists).
if (a) |*value| { value.* += 1; }


// Loops.

// Syntax examples:
//   while (condition) statement
//   while (condition) : (end-of-iteration-statement) statement
//
//   for (iterable) statement
//   for (iterable) |capture| statement
//   for (iterable) statement else statement

// Note: loops work the same way over arrays or slices.

// Simple "while" loop.
while (i < 10) { i += 1; }

// While loop with a "continue expression"
// (expression executed as the last expression of the loop).
while (i < 10) : (i += 1) { ... }
// Same, with a more complex continue expression (block of code).
while (i * j < 2000) : ({ i *= 2; j *= 3; }) { ... }

// To iterate over a portion of a slice, reslice.
for (items[0..1]) |value| { sum += value; }

// Loop over every item of an array (or slice).
for (items) |value| { sum += value; }

// Iterate and get pointers on values instead of copies.
for (items) |*value| { value.* += 1; }

// Iterate with an index.
for (items) |value, i| { print("val[{}] = {}\n", .{i, value}); }

// Iterate with pointer and index.
for (items) |*value, i| { print("val[{}] = {}\n", .{i, value}); value.* += 1; }


// Break and continue are supported.
for (items) |value| {
    if (value == 0)  { continue; }
    if (value >= 10) { break;    }
    // ...
}

// For loops can also be used as expressions.
// Similar to while loops, when you break from a for loop,
// the else branch is not evaluated.
var sum: i32 = 0;
// The "for" loop has to provide a value, which will be the "else" value.
const result = for (items) |value| {
    if (value != null) {
        sum += value.?; // "result" will be the last "sum" value.
    }
} else 0;                  // Last value.
```

### Labels.

```zig
// Labels are a way to name an instruction, a location in the code.
// Labels can be used to "continue" or "break" in a nested loop.
outer: for ([_]i32{ 1, 2, 3, 4, 5, 6, 7, 8 }) |_| {
    for ([_]i32{ 1, 2, 3, 4, 5 }) |_| {
        count += 1;
        continue :outer; // "continue" for the first loop.
    }
} // count = 8
outer: for ([_]i32{ 1, 2, 3, 4, 5, 6, 7, 8 }) |_| {
    for ([_]i32{ 1, 2, 3, 4, 5 }) |_| {
        count += 1;
        break :outer; // "break" for the first loop.
    }
} // count = 1


// Labels can also be used to return a value from a block.
var y: i32 = 5;
const x = blk: {
    y += 1;
    break :blk y; // Now "x" equals 6.
};
// Relevant in cases like "for else" expression (explained in the following).

// For loops can be used as expressions.
// When you break from a for loop, the else branch is not evaluated.
// WARNING: counter-intuitive.
//      The "for" loop will run, then the "else" block will run.
//      The "else" keyword has to be followed by the value to give to "result".
//      See later for another form.
var sum: u8 = 0;
const result = for (items) |value| {
    sum += value;
} else 8; // result = 8

// In this case, the "else" keyword is followed by a value, too.
// However, the syntax is different: it is labeled.
// Instead of a value, there is a label followed by a block of code, which
// allows to do stuff before returning the value (see the "break" invocation).
const result = for (items) |value| { // First: loop.
    sum += value;
} else blk: {                        // Second: "else" block.
    std.log.info("executed AFTER the loop!", .{});
    break :blk sum; // The "sum" value will replace the label "blk".
};
```

### Switch.

```zig
// As a switch in C, but slightly more advanced.
// Syntax:
//   switch (value) {
//       pattern => expression,
//       pattern => expression,
//       else    => expression
//   };

// A switch only checking for simple values.
var x = switch(value) {
    Error.UnExpected     => return err,
    Error.Authentication => unreachable,
    else                 => unreachable,
};

// A slightly more advanced switch, accepting a range of values:
const foo: i32 = 0;
const bar = switch (foo) {
    0                        => "zero",
    1...std.math.maxInt(i32) => "positive",
    else                     => "negative",
};
```

### Structures.

```zig
// Structure containing a single value.
const Full = struct {
    number: u16,
};

// Packed structure, with guaranteed in-memory layout.
const Divided = packed struct {
    half1: u8,
    quarter3: u4,
    quarter4: u4,
};

// Point is a constant representing a structure containing two u32, "x" and "y".
// "x" has a default value, which wasn't possible in C.
const Point = struct {
    x: u32 = 1, // default value
    y: u32,
};

// Variable "p" is a new Point, with x = 1 (default value) and y = 2.
var p = Point{ .y = 2 };

// Fields are accessed as usual with the dot notation: variable.field.
print("p.x: {}\n", .{p.x}); // 1
print("p.y: {}\n", .{p.y}); // 2


// A structure can also contain public constants and functions.
const Point = struct {
    pub const some_constant = 30;

    x: u32,
    y: u32,

    // This function "init" creates a Point and returns it.
    pub fn init() Point {
        return Point{ .x = 0, .y = 0 };
    }
};


// How to access a structure public constant.
// The value isn't accessed from an "instance" of the structure, but from the
// constant representing the structure definition (Point).
print("constant: {}\n", .{Point.some_constant});

// Having an "init" function is rather idiomatic in the standard library.
// More on that later.
var p = Point.init();
print("p.x: {}\n", .{p.x}); // p.x = 0
print("p.y: {}\n", .{p.y}); // p.y = 0


// Structures often have functions to modify their state, similar to
// object-oriented programming.
const Point = struct {
    const Self = @This(); // Refers to its own type (later called "Point").

    x: u32,
    y: u32,

    // Take a look at the signature. First argument is of type *Self: "self" is
    // a pointer on the instance of the structure.
    // This allows the same "dot" notation as in OOP, like "instance.set(x,y)".
    // See the following example.
    pub fn set(self: *Self, x: u32, y: u32) void {
        self.x = x;
        self.y = y;
    }

    // Again, look at the signature. First argument is of type Self (not *Self),
    // this isn't a pointer. In this case, "self" refers to the instance of the
    // structure, but can't be modified.
    pub fn getx(self: Self) u32 {
        return self.x;
    }

    // PS: two previous functions may be somewhat useless.
    //     Attributes can be changed directly, no need for accessor functions.
    //     It was just an example.
};

// Let's use the previous structure.
var p = Point{ .x = 0, .y = 0 }; // "p" variable is a Point.

p.set(10, 30); // x and y attributes of "p" are modified via the "set" function.
print("p.x: {}\n", .{p.x}); // 10
print("p.y: {}\n", .{p.y}); // 30

// In C:
//   1. We would have written something like: point_set(p, 10, 30).
//   2. Since all functions are in the same namespace, it would have been
//      very cumbersome to create functions with different names for different
//      structures. Many long names, painful to read.
//
// In Zig, structures provide namespaces for their own functions.
// Different structures can have the same names for their functions,
// which brings clarity.
```

### Tuples.

```zig
// A tuple is a list of elements, possibly of different types.

const foo = .{ "hello", true, 42 };
// foo.len == 3
```

### Enumerations.

```zig
const Type = enum { ok, not_ok };

const CardinalDirections = enum { North, South, East, West };
const direction: CardinalDirections = .North;
const x = switch (direction) {
    // shorthand for CardinalDirections.North
    .North => true,
    else => false
};

// Switch statements need exhaustiveness.
// WARNING: won't compile. East and West are missing.
const x = switch (direction) {
    .North => true,
    .South => true,
};


// Switch statements need exhaustiveness.
// Won't compile: East and West are missing.
const x = switch (direction) {
    .North => true,
    .South => true,
    .East,          // Its value is the same as the following pattern: false.
    .West => false,
};


// Enumerations are like structures: they can have functions.
```

### Unions.

```zig
const Bar = union {
    boolean: bool,
    int: i16,
    float: f32,
};

// Both syntaxes are equivalent.
const foo = Bar{ .int = 42 };
const foo: Bar = .{ .int = 42 };

// Unions, like enumerations and structures, can have functions.
```

### Tagged unions.

```zig
// Unions can be declared with an enum tag type, allowing them to be used in
// switch expressions.

const MaybeEnum = enum {
    success,
    failure,
};

const Maybe = union(MaybeEnum) {
    success: u8,
    failure: []const u8,
};

// First value: success!
const yay = Maybe{ .success = 42 };
switch (yay) {
    .success => |value|     std.log.info("success: {}", .{value}),
    .failure => |err_msg|   std.log.info("failure: {}", .{err_msg}),
}

// Second value: failure! :(
const nay = Maybe{ .failure = "I was too lazy" };
switch (nay) {
    .success => |value|     std.log.info("success: {}", .{value}),
    .failure => |err_msg|   std.log.info("failure: {}", .{err_msg}),
}
```

### Defer and errdefer.

```zig
// Make sure that an action (single instruction or block of code) is executed
// before the end of the scope (function, block of code).
// Even on error, that action will be executed.
// Useful for memory allocations, and resource management in general.

pub fn main() void {
    // Should be executed at the end of the function.
    defer print("third!\n", .{});

    {
        // Last element of its scope: will be executed right away.
        defer print("first!\n", .{});
    }

    print("second!\n", .{});
}

fn hello_world() void {
    defer print("end of function\n", .{}); // after "hello world!"

    print("hello world!\n", .{});
}

// errdefer executes the instruction (or block of code) only on error.
fn second_hello_world() !void {
    errdefer print("2. something went wrong!\n", .{}); // if "foo" fails.
    defer    print("1. second hello world\n", .{});    // executed after "foo"

    try foo();
}
// Defer statements can be seen as stacked: first one is executed last.
```

### Memory allocators.
Memory isn't managed directly in the standard library, instead an "allocator" is asked every time an operation on memory is required.
Thus, the standard library lets developers handle memory as they need, through structures called "allocators", handling all memory operations.

**NOTE**: the choice of the allocator isn't in the scope of this document.
A whole book could be written about it.
However, here are some examples, to get an idea of what you can expect:
- `page_allocator`.
  Allocate a whole page of memory each time we ask for some memory.
  Very simple, very dumb, very wasteful.
- `GeneralPurposeAllocator`.
  Get some memory first and manage some buckets of memory in order to
  reduce the number of allocations.
  A bit complex. Can be combined with other allocators.
  Can detect leaks and provide useful information to find them.
- `FixedBufferAllocator`.
  Use a fixed buffer to get its memory, don't ask memory to the kernel.
  Very simple, limited and wasteful (can't deallocate), but very fast.
- `ArenaAllocator`.
  Allow to free all allocated memory at once.
  To use in combinations with another allocator.
  Very simple way of avoiding leaks.

A first example.

```zig
// "!void" means the function doesn't return any value except for errors.
// In this case we try to allocate memory, and this may fail.
fn foo() !void {
    // In this example we use a page allocator.
    var allocator = std.heap.page_allocator;

    // "list" is an ArrayList of 8-bit unsigned integers.
    // An ArrayList is a contiguous, growable list of elements in memory.
    var list = try ArrayList(u8).initAllocated(allocator);
    defer list.deinit(); // Free the memory at the end of the scope. Can't leak.
    // "defer" allows to express memory release right after its allocation,
    // regardless of the complexity of the function (loops, conditions, etc.).

    list.add(5); // Some memory is allocated here, with the provided allocator.

    for (list.items) |item| {
        std.debug.print("item: {}\n", .{item});
    }
}
```

### Memory allocation combined with error management and defer.

```zig
fn some_memory_allocation_example() !void {
    // Memory allocation may fail, so we "try" to allocate the memory and
    // in case there is an error, the current function returns it.
    var buf = try page_allocator.alloc(u8, 10);
    // Defer memory release right after the allocation.
    // Will happen even if an error occurs.
    defer page_allocator.free(buf);

    // Second allocation.
    // In case of a failure, the first allocation is correctly released.
    var buf2 = try page_allocator.alloc(u8, 10);
    defer page_allocator.free(buf2);

    // In case of failure, both previous allocations are correctly deallocated.
    try foo();
    try bar();

    // ...
}
```

### Memory allocators: a taste of the standard library.

```zig
// Allocators: 4 main functions to know
//   single_value = create (type)
//   destroy (single_value)
//   slice = alloc (type, size)
//   free (slice)

// Page Allocator
fn page_allocator_fn() !void {
    var slice = try std.heap.page_allocator.alloc(u8, 3);
    defer std.heap.page_allocator.free(slice);

    // playing_with_a_slice(slice);
}

// GeneralPurposeAllocator
fn general_purpose_allocator_fn() !void {
    // GeneralPurposeAllocator has to be configured.
    // In this case, we want to track down memory leaks.
    const config = .{.safety = true};
    var gpa = std.heap.GeneralPurposeAllocator(config){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var slice = try allocator.alloc(u8, 3);
    defer allocator.free(slice);

    // playing_with_a_slice(slice);
}

// FixedBufferAllocator
fn fixed_buffer_allocator_fn() !void {
    var buffer = [_]u8{0} ** 1000; // array of 1000 u8, all initialized at zero.
    var fba  = std.heap.FixedBufferAllocator.init(buffer[0..]);
    // Side note: buffer[0..] is a way to create a slice from an array.
    //            Since the function takes a slice and not an array, this makes
    //            the type system happy.

    var allocator = fba.allocator();

    var slice = try allocator.alloc(u8, 3);
    // No need for "free", memory cannot be freed with a fixed buffer allocator.
    // defer allocator.free(slice);

    // playing_with_a_slice(slice);
}

// ArenaAllocator
fn arena_allocator_fn() !void {
    // Reminder: arena doesn't allocate memory, it uses an inner allocator.
    // In this case, we combine the arena allocator with the page allocator.
    var arena = std.heap.arena_allocator.init(std.heap.page_allocator);
    defer arena.deinit(); // end of function = all allocations are freed.

    var allocator = arena.allocator();

    const slice = try allocator.alloc(u8, 3);
    // No need for "free", memory will be freed anyway.

    // playing_with_a_slice(slice);
}


// Combining the general purpose and arena allocators. Both are very useful,
// and their combinations should be in everyone's favorite cookbook.
fn gpa_arena_allocator_fn() !void {
    const config = .{.safety = true};
    var gpa = std.heap.GeneralPurposeAllocator(config){};
    defer _ = gpa.deinit();

    const gpa_allocator = gpa.allocator();

    var arena = arena_allocator.init(gpa_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var slice = try allocator.alloc(u8, 3);
    defer allocator.free(slice);

    // playing_with_a_slice(slice);
}
```

### Comptime.

```zig
// Comptime is a way to avoid the pre-processor.
// The idea is simple: run code at compilation.

inline fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

var res = max(u64, 1, 2);
var res = max(f32, 10.50, 32.19);


// Comptime: creating generic structures.

fn List(comptime T: type) type {
    return struct {
        items: []T,

        fn init()   ... { ... }
        fn deinit() ... { ... }
        fn do()     ... { ... }
    };
}

const MyList = List(u8);


// use
var list = MyList{
    .items = ... // memory allocation
};

list.items[0] = 10;
```

### Conditional compilation.

```zig
const available_os = enum { OpenBSD, Linux };
const myos = available_os.OpenBSD;


// The following switch is based on a constant value.
// This means that the only possible outcome is known at compile-time.
// Thus, there is no need to build the rest of the possibilities.
// Similar to the "#ifdef" in C, but without requiring a pre-processor.
const string = switch (myos) {
   .OpenBSD => "OpenBSD is awesome!",
   .Linux => "Linux rocks!",
};

// Also works in this case.
const myprint = switch(myos) {
    .OpenBSD => std.debug.print,
    .Linux => std.log.info,
}
```

### Testing our functions.

```zig
const std = @import("std");
const expect = std.testing.expect;

// Function to test.
pub fn some_function() bool {
    return true;
}

// This "test" block can be run with "zig test".
// It will test the function at compile-time.
test "returns true" {
    expect(false == some_function());
}
```

### Compiler built-ins.
The compiler has special functions called "built-ins", starting with an "@".
There are more than a hundred built-ins, allowing very low-level stuff:
- compile-time errors, logging, verifications
- type coercion and conversion, even in an unsafe way
- alignment management
- memory tricks (such as getting the byte offset of a field in a struct)
- calling functions at compile-time
- including C headers to transparently call C functions
- atomic operations
- embed files into the executable (@embedFile)
- frame manipulations (for async functions, for example)
- etc.

Example: enums aren't integers, they have to be converted with a built-in.

```zig
const Value = enum { zero, stuff, blah };
if (@enumToInt(Value.zero)  == 0) { ... }
if (@enumToInt(Value.stuff) == 1) { ... }
if (@enumToInt(Value.blah)  == 2) { ... }
```

### A few "not yourself in the foot" measures in the Zig language.

- Namespaces: name conflicts are easily avoided.
  In practice, that means a unified API between different structures (data types).
- Enumerations aren't integers. Comparing an enumeration to an integer requires a conversion.
- Explicit casts, coercion exists but is limited.
  Types are slightly more enforced than in C, just a taste:
    Pointers aren't integers, explicit conversion is necessary.
    You won't lose precision by accident, implicit coercions are only authorized in cases where no precision can be lost.
    Unions cannot be reinterpreted (in a union with an integer and a float, one cannot take a value for another by accident).
    Etc.
- Removing most of the C undefined behaviors (UBs), and when the compiler encounters one, it stops.
- Slice and Array structures are preferred to pointers.
  Types enforced by the compiler are less prone to errors than pointer manipulations.
- Numerical overflows produce an error, unless explicitly accepted using wrapping operators.
- `try` and `catch` mechanism.
  It's both handy, trivially implemented (simple error enumeration), and it takes almost no space nor computation time.
- Unused variables are considered to be errors by the compiler.
- Many pointer types exist in order to represent what is pointed to.
  Example: is this a single value or an array, is the length known, etc.
- Structures need a value for their attributes, and it is still possible to give an undefined value (stack garbage), but at least it is explicitly undefined.


## Further Reading

For a start, some concepts are presented on [zig.guide][zigguide].

The [official website][zigdoc] provides the reference documentation of the language. The standard library [has its own documentation][zigstd].

[ziglang]: https://ziglang.org
[zigguide]: https://zig.guide/
[zigdoc]: https://ziglang.org/documentation/
[zigstd]: https://ziglang.org/documentation/master/std/
