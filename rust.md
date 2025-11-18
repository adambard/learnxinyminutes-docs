---
name: Rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
filename: learnrust.rs
---

Rust is a programming language developed by Mozilla Research.
Rust combines low-level control over performance with high-level convenience and
safety guarantees.

It achieves these goals without requiring a garbage collector or runtime, making
it possible to use Rust libraries as a "drop-in replacement" for C.

Rust’s first release, 0.1, occurred in January 2012, and for 3 years development
moved so quickly that until recently the use of stable releases was discouraged
and instead the general advice was to use nightly builds.

On May 15th 2015, Rust 1.0 was released with a complete guarantee of backward
compatibility. Improvements to compile times and other aspects of the compiler are
currently available in the nightly builds. Rust has adopted a train-based release
model with regular releases every six weeks. Rust 1.1 beta was made available at
the same time of the release of Rust 1.0.

Although Rust is a relatively low-level language, it has some functional
concepts that are generally found in higher-level languages. This makes
Rust not only fast, but also easy and efficient to code in.

```rust
// This is a comment. Line comments look like this...
// and extend multiple lines like this.

/* Block comments
  /* can be nested. */ */

/// Documentation comments look like this and support markdown notation.
/// # Examples
///
/// ```
/// let five = 5
/// ```

///////////////
// 1. Basics //
///////////////

#[allow(dead_code)]
// Functions
// `i32` is the type for 32-bit signed integers
fn add2(x: i32, y: i32) -> i32 {
    // Implicit return (no semicolon)
    x + y
}

#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
// Main function
fn main() {
    // Numbers //

    // Immutable bindings
    let x: i32 = 1;

    // Integer/float suffixes
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // Type inference
    // Most of the time, the Rust compiler can infer what type a variable is, so
    // you don’t have to write an explicit type annotation.
    // Throughout this tutorial, types are explicitly annotated in many places,
    // but only for demonstrative purposes. Type inference can handle this for
    // you most of the time.
    let implicit_x = 1;
    let implicit_f = 1.3;

    // Arithmetic
    let sum = x + y + 13;

    // Mutable variable
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // Strings //

    // String literals
    let x: &str = "hello world!";

    // Printing
    println!("{} {}", f, x); // 1.3 hello world!

    // A `String` – a heap-allocated string
    // Stored as a `Vec<u8>` and always holds a valid UTF-8 sequence, 
    // which is not null terminated.
    let s: String = "hello world".to_string();

    // A string slice – an immutable view into another string
    // This is basically an immutable pointer and length of a string – it
    // doesn’t actually contain the contents of a string, just a pointer to
    // the beginning and a length of a string buffer,
    // statically allocated or contained in another object (in this case, `s`).
    // The string slice is like a view `&[u8]` into `Vec<T>`.
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // Vectors/arrays //

    // A fixed-size array
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // A dynamic array (vector)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // A slice – an immutable view into a vector or array
    // This is much like a string slice, but for vectors
    let slice: &[i32] = &vector;

    // Use `{:?}` to print something debug-style
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Tuples //

    // A tuple is a fixed-size set of values of possibly different types
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // Destructuring `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // Indexing
    println!("{}", x.1); // hello

    //////////////
    // 2. Types //
    //////////////

    // Struct
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // A struct with unnamed fields, called a ‘tuple struct’
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // Basic C-like enum
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // Enum with fields
    // If you want to make something optional, the standard
    // library has `Option`
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // Generics //

    struct Foo<T> { bar: T }

    // This is defined in the standard library as `Option`
    // `Option` is used in place of where a null pointer
    // would normally be used.
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // Methods //

    impl<T> Foo<T> {
        // Methods take an explicit `self` parameter
        fn bar(&self) -> &T { // self is borrowed
            &self.bar
        }
        fn bar_mut(&mut self) -> &mut T { // self is mutably borrowed
            &mut self.bar
        }
        fn into_bar(self) -> T { // here self is consumed
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.bar()); // 1

    // Traits (known as interfaces or typeclasses in other languages) //

    trait Frobnicate<T> {
        fn frobnicate(self) -> Option<T>;
    }

    impl<T> Frobnicate<T> for Foo<T> {
        fn frobnicate(self) -> Option<T> {
            Some(self.bar)
        }
    }

    let another_foo = Foo { bar: 1 };
    println!("{:?}", another_foo.frobnicate()); // Some(1)

    // Function pointer types // 

    fn fibonacci(n: u32) -> u32 {
        match n {
            0 => 1,
            1 => 1,
            _ => fibonacci(n - 1) + fibonacci(n - 2),
        }
    }

    type FunctionPointer = fn(u32) -> u32;

    let fib : FunctionPointer = fibonacci;
    println!("Fib: {}", fib(4)); // 5

    /////////////////////////
    // 3. Pattern matching //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // Advanced pattern matching
    struct FooBar { x: i32, y: OptionalI32 }
    let bar = FooBar { x: 15, y: OptionalI32::AnI32(32) };

    match bar {
        FooBar { x: 0, y: OptionalI32::AnI32(0) } =>
            println!("The numbers are zero!"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } if n == m =>
            println!("The numbers are the same"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } =>
            println!("Different numbers: {} {}", n, m),
        FooBar { x: _, y: OptionalI32::Nothing } =>
            println!("The second number is Nothing!"),
    }

    /////////////////////
    // 4. Control flow //
    /////////////////////

    // `for` loops/iteration
    let array = [1, 2, 3];
    for i in array {
        println!("{}", i);
    }

    // Ranges
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // prints `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // `if` as expression
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` loop
    while 1 == 1 {
        println!("The universe is operating normally.");
        // break statement gets out of the while loop.
        //  It avoids useless iterations.
        break
    }

    // Infinite loop
    loop {
        println!("Hello!");
        // break statement gets out of the loop
        break
    }

    /////////////////////////////////
    // 5. Memory safety & pointers //
    /////////////////////////////////

    // Owned pointer – only one thing can ‘own’ this pointer at a time
    // This means that when the `Box` leaves its scope, it will be automatically deallocated safely.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // dereference
    // Here, `now_its_mine` takes ownership of `mine`. In other words, `mine` is moved.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // this would not compile because `now_its_mine` now owns the pointer

    // Reference – an immutable pointer that refers to other data
    // When a reference is taken to a value, we say that the value has been ‘borrowed’.
    // While a value is borrowed immutably, it cannot be mutated or moved.
    // A borrow is active until the last use of the borrowing variable.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // Unlike `mine`, `var` can still be used
    println!("{}", *ref_var);
    // var = 5; // this would not compile because `var` is borrowed
    // *ref_var = 6; // this would not either, because `ref_var` is an immutable reference
    ref_var; // no-op, but counts as a use and keeps the borrow active
    var = 2; // ref_var is no longer used after the line above, so the borrow has ended

    // Mutable reference
    // While a value is mutably borrowed, it cannot be accessed at all.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*' is used to point to the mutably borrowed var2

    println!("{}", *ref_var2); // 6 , // var2 would not compile.
    // ref_var2 is of type &mut i32, so stores a reference to an i32, not the value.
    // var2 = 2; // this would not compile because `var2` is borrowed.
    ref_var2; // no-op, but counts as a use and keeps the borrow active until here

    ///////////////////////////////////////////////////////
    // 6. FFI (Foreign Function Interface) - Calling C //
    ///////////////////////////////////////////////////////

    // Rust has excellent support for calling C code through FFI
    // This allows Rust to use existing C libraries and be used as a C library itself
}

// Declaring external C functions
// The `extern` block tells Rust about C functions we want to call
extern "C" {
    // Standard C library functions
    fn abs(input: i32) -> i32;
    fn sqrt(input: f64) -> f64;
    fn puts(s: *const i8) -> i32;

    // Custom C library functions
    // fn my_c_function(x: i32) -> i32;
}

// Calling C functions requires `unsafe` because Rust can't verify C code safety
fn call_c_functions() {
    unsafe {
        println!("Absolute value of -3 = {}", abs(-3));
        println!("Square root of 9.0 = {}", sqrt(9.0));
    }
}

// C-compatible struct with #[repr(C)]
// This ensures the memory layout matches C expectations
#[repr(C)]
struct Point {
    x: f64,
    y: f64,
}

#[repr(C)]
struct Person {
    name: *const i8,  // C string (char*)
    age: u32,
}

// Enums with explicit discriminants for C compatibility
#[repr(C)]
enum Status {
    Ok = 0,
    Error = 1,
    Pending = 2,
}

// Exposing Rust functions to C with #[no_mangle] and extern "C"
// #[no_mangle] prevents Rust from changing the function name
#[no_mangle]
pub extern "C" fn rust_add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn rust_process_point(point: *const Point) -> f64 {
    unsafe {
        if point.is_null() {
            return 0.0;
        }
        let p = &*point;
        (p.x * p.x + p.y * p.y).sqrt()
    }
}

// Type aliases for C types (using libc crate)
// In real code: use libc::{c_int, c_char, c_void, size_t};
type c_int = i32;
type c_char = i8;

// Working with C strings
use std::ffi::{CStr, CString};

fn string_conversion_example() {
    // Rust String to C string (CString)
    let rust_string = String::from("Hello from Rust!");
    let c_string = CString::new(rust_string).expect("CString creation failed");
    let c_ptr = c_string.as_ptr();

    // Now c_ptr can be passed to C functions expecting char*
    unsafe {
        // puts(c_ptr);  // Uncomment to call C function
    }

    // C string to Rust String
    // Assuming we got a *const c_char from C code
    unsafe {
        let c_str = CStr::from_ptr(c_ptr);
        let rust_str = c_str.to_str().expect("Invalid UTF-8");
        let owned_string = rust_str.to_owned();
        println!("Converted from C: {}", owned_string);
    }
}

// Function pointers for C callbacks
type CallbackFn = extern "C" fn(i32) -> i32;

#[no_mangle]
pub extern "C" fn apply_callback(value: i32, callback: CallbackFn) -> i32 {
    callback(value)
}

// Example callback function
extern "C" fn double_value(x: i32) -> i32 {
    x * 2
}

// Opaque types - when you don't need to know the C struct's internals
#[repr(C)]
pub struct OpaqueHandle {
    _private: [u8; 0],  // Zero-sized type, prevents direct instantiation
}

// Common FFI patterns
extern "C" {
    // Creating/destroying opaque handles
    fn create_handle() -> *mut OpaqueHandle;
    fn destroy_handle(handle: *mut OpaqueHandle);

    // Passing arrays
    fn process_array(data: *const i32, len: usize) -> i32;

    // Output parameters (pointer to write results)
    fn get_values(out_x: *mut i32, out_y: *mut i32) -> c_int;
}

fn ffi_patterns_example() {
    unsafe {
        // Working with opaque handles
        let handle = create_handle();
        // ... use handle ...
        destroy_handle(handle);

        // Passing Rust slice to C
        let data = vec![1, 2, 3, 4, 5];
        let result = process_array(data.as_ptr(), data.len());

        // Output parameters
        let mut x: i32 = 0;
        let mut y: i32 = 0;
        let status = get_values(&mut x as *mut i32, &mut y as *mut i32);
        if status == 0 {
            println!("Got values: x={}, y={}", x, y);
        }
    }
}

// Linking to C libraries
// In Cargo.toml, you can specify:
// [build-dependencies]
// cc = "1.0"
//
// Then in build.rs:
// fn main() {
//     cc::Build::new()
//         .file("src/mylib.c")
//         .compile("mylib");
// }

// Or link to system libraries:
// #[link(name = "m")]  // Links to libm (math library)
// extern "C" {
//     fn cos(x: f64) -> f64;
// }

// Using bindgen to auto-generate Rust bindings from C headers
// In build.rs:
// fn main() {
//     println!("cargo:rerun-if-changed=wrapper.h");
//
//     let bindings = bindgen::Builder::default()
//         .header("wrapper.h")
//         .generate()
//         .expect("Unable to generate bindings");
//
//     bindings
//         .write_to_file("src/bindings.rs")
//         .expect("Couldn't write bindings!");
// }

// Safety guidelines for FFI:
// 1. Always use `unsafe` blocks for FFI calls
// 2. Validate pointers before dereferencing (check for null)
// 3. Be careful with lifetimes - C doesn't track ownership
// 4. Use CString/CStr for string conversion
// 5. Match C struct layout with #[repr(C)]
// 6. Document safety invariants
// 7. Create safe Rust wrappers around unsafe FFI code

// Example: Safe wrapper around unsafe FFI
pub struct SafeHandle {
    inner: *mut OpaqueHandle,
}

impl SafeHandle {
    pub fn new() -> Option<Self> {
        unsafe {
            let handle = create_handle();
            if handle.is_null() {
                None
            } else {
                Some(SafeHandle { inner: handle })
            }
        }
    }
}

impl Drop for SafeHandle {
    fn drop(&mut self) {
        unsafe {
            if !self.inner.is_null() {
                destroy_handle(self.inner);
            }
        }
    }
}

// Common FFI crates in the Rust ecosystem:
// - libc: Raw bindings to platform C libraries
// - bindgen: Automatically generate Rust FFI bindings
// - cc: Compile C/C++ code and link it with Cargo
// - cbindgen: Generate C headers from Rust code
// - cxx: Safe interop between Rust and C++
```

## Further reading

For a deeper-yet-still-fast explanation into Rust and its symbols/keywords, the
[half-hour to learn Rust](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)
article by Fasterthanlime explains (almost) everything in a clear and concise way!

There’s a lot more to Rust—this is just the basics of Rust so you can understand
the most important things. To learn more about Rust, read [The Rust Programming
Language](http://doc.rust-lang.org/book/index.html) and check out the
[/r/rust](http://reddit.com/r/rust) subreddit. The folks on the #rust channel on
irc.mozilla.org are also always keen to help newcomers.

You can also try out features of Rust with an online compiler at the official
[Rust Playground](https://play.rust-lang.org) or on the main
[Rust website](http://rust-lang.org).

For FFI specifically, check out:
* [The Rustonomicon - FFI Chapter](https://doc.rust-lang.org/nomicon/ffi.html) - Advanced FFI topics
* [The Rust FFI Omnibus](http://jakegoulding.com/rust-ffi-omnibus/) - Examples of using Rust code from other languages
* [bindgen documentation](https://rust-lang.github.io/rust-bindgen/) - Auto-generate Rust FFI bindings
* [cbindgen documentation](https://github.com/mozilla/cbindgen) - Generate C headers from Rust code
* [libc crate](https://docs.rs/libc/) - Raw FFI bindings to platform C libraries
