---
language: rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
filename: learnrust.rs
---

Rust is an in-development programming language developed by Mozilla Research.
It is relatively unique among systems languages in that it can assert memory
safety *at compile time*. Rust’s first alpha release occurred in January
2012, and development moves so quickly that at the moment the use of stable
releases is discouraged, and instead one should use nightly builds.

Although Rust is a relatively low-level language, Rust has some functional
concepts that are generally found in higher-level languages. This makes
Rust not only fast, but also easy and efficient to code in.

```rust
// This is a comment. Single-line look like this...
/* ...and multi-line comment look like this */

///////////////
// 1. Basics //
///////////////

// Functions
fn add2(x: int, y: int) -> int {
    // Implicit return (no semicolon)
    x + y
}

// Main function
fn main() {
    // Numbers //

    // Immutable bindings
    let x: int = 1;

    // Integer/float suffixes
    let y: int = 13i;
    let f: f64 = 1.3f64;

    // Type inference
    let implicit_x = 1i;
    let implicit_f = 1.3f64;

    // Maths
    let sum = x + y + 13i;

    // Mutable variable
    let mut mutable = 1;
    mutable += 2;

    // Strings //
    
    // String literals
    let x: &'static str = "hello world!";

    // Printing
    println!("{} {}", f, x); // 1.3 hello world

    // A `String` - a heap-allocated string
    let s: String = "hello world".to_string();

    // A string slice - an immutable view into another string
    // This is basically an immutable pointer to a string - it doesn’t
    // actually contain the characters of a string, just a pointer to
    // something that does (in this case, `s`)
    let s_slice: &str = s.as_slice();

    println!("{} {}", s, s_slice); // hello world hello world

    // Vectors/arrays //

    // A fixed-size array
    let four_ints: [int, ..4] = [1, 2, 3, 4];

    // A dynamically-sized vector
    let mut vector: Vec<int> = vec![1, 2, 3, 4];
    vector.push(5);

    // A slice - an immutable view into a vector or array
    // This is much like a string slice, but for vectors
    let slice: &[int] = vector.as_slice();

    println!("{} {}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    //////////////
    // 2. Types //
    //////////////
    
    // Struct
    struct Point {
        x: int,
        y: int,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // Tuple struct
    struct Point2(int, int);

    let origin2 = Point2(0, 0);

    // Basic C-like enum
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Up;

    // Enum with fields
    enum OptionalInt {
        AnInt(int),
        Nothing,
    }

    let two:     OptionalInt = AnInt(2);
    let nothing: OptionalInt = Nothing;

    // Generics //

    struct Foo<T> { bar: T }

    // This is defined in the standard library as `Option`
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // Methods //

    impl<T> Foo<T> {
        // Methods take an explicit `self` parameter
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1i };
    println!("{}", a_foo.get_bar()); // 1

    // Traits (interfaces) //

    trait Frobnicate<T> {
        fn frobnicate(self) -> Option<T>;
    }

    impl<T> Frobnicate<T> for Foo<T> {
        fn frobnicate(self) -> Option<T> {
            Some(self.bar)
        }
    }

    println!("{}", a_foo.frobnicate()); // Some(1)

    /////////////////////////
    // 3. Pattern matching //
    /////////////////////////
    
    let foo = AnInt(1);
    match foo {
        AnInt(n) => println!("it’s an int: {}", n),
        Nothing  => println!("it’s nothing!"),
    }

    // Advanced pattern matching
    struct FooBar { x: int, y: OptionalInt }
    let bar = FooBar { x: 15, y: AnInt(32) };

    match bar {
        FooBar { x: 0, y: AnInt(0) } =>
            println!("The numbers are zero!"),
        FooBar { x: n, y: AnInt(m) } if n == m =>
            println!("The numbers are the same"),
        FooBar { x: n, y: AnInt(m) } =>
            println!("Different numbers: {} {}", n, m),
        FooBar { x: _, y: Nothing } =>
            println!("The second number is Nothing!"),
    }

    /////////////////////
    // 4. Control flow //
    /////////////////////

    // `for` loops/iteration
    let array = [1i, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    for i in range(0u, 10) {
        print!("{} ", i);
    }
    println!("");
    // prints `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1i == 1 {
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
    while 1i == 1 {
        println!("The universe is operating normally.");
    }

    // Infinite loop
    loop {
        println!("Hello!");
    }

    /////////////////////////////////
    // 5. Memory safety & pointers //
    /////////////////////////////////
    
    // Owned pointer - only one thing can ‘own’ this pointer at a time
    let mut mine: Box<int> = box 3;
    *mine = 5; // dereference
    let mut now_its_mine = mine;
    *now_its_mine += 2;
    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // this would error

    // Reference - an immutable pointer that refers to other data
    let mut var = 4i;
    var = 3;
    let ref_var: &int = &var;
    println!("{}", var); // Unlike `box`, `var` can still be used
    println!("{}", *ref_var);
    // var = 5; // this would error
    // *ref_var = 6; // this would too

    // Mutable reference
    let mut var2 = 4i;
    let ref_var2: &mut int = &mut var2;
    *ref_var2 += 2;
    println!("{}", *ref_var2); // 6
    // var2 = 2; // this would error
}
```

## Further reading

There’s a lot more to Rust—this is just the basics of Rust so you can
understand the most important things. To learn more about Rust, read the
[Rust tutorial](http://doc.rust-lang.org/tutorial.html) and check out the
[/r/rust](http://reddit.com/r/rust) subreddit. The folks on the #rust channel
on irc.mozilla.org are also always keen to help newcomers.

You can also try out features of Rust with an online compiler at the official
[Rust playpen](http://play.rust-lang.org) or on the main
[Rust website](http://rust-lang.org).
