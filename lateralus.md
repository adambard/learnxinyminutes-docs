---
name: Lateralus
contributors:
    - ["bad-antics", "https://github.com/bad-antics"]
filename: learnlateralus.ltl
---

Lateralus is a self-hosted systems programming language with a bytecode VM, a
native AOT backend, a tracing garbage collector, structural typing, and a
package manager. It is the implementation language of LateralusOS — a hobby
operating system written almost entirely in Lateralus.

Source: <https://github.com/bad-antics/lateralus-lang>
Tree-sitter grammar: <https://github.com/bad-antics/tree-sitter-lateralus>

```ltl
// Single-line comment.
/* Block
   comment. */

// ---------------------------------------------------------------------------
// 1. Declarations
// ---------------------------------------------------------------------------

import std.io
import std.collections

// Constants are immutable, type-inferred unless annotated.
const VERSION: string = "1.0"
const MAX     = 4096            // i32 by default

// `let` declares a mutable binding; `let mut` is the same — `let` is mutable
// by default, `const` is the immutable form.
let counter: i32 = 0

// ---------------------------------------------------------------------------
// 2. Functions
// ---------------------------------------------------------------------------

pub fn add(a: i32, b: i32) -> i32 {
    return a + b
}

// Trailing-expression return (no semicolon, no `return` keyword).
pub fn square(n: i32) -> i32 { n * n }

// Generic function. Type parameters live in `<...>`.
pub fn max<T>(a: T, b: T) -> T where T: Ord {
    if a > b { a } else { b }
}

// ---------------------------------------------------------------------------
// 3. Types
// ---------------------------------------------------------------------------

struct Point {
    x: f64,
    y: f64,
}

enum Shape {
    Circle(f64),               // radius
    Rect { w: f64, h: f64 },   // named fields
    Polygon([Point]),          // dynamic array
}

impl Shape {
    pub fn area(self) -> f64 {
        match self {
            Shape::Circle(r)        => 3.14159 * r * r,
            Shape::Rect { w, h }    => w * h,
            Shape::Polygon(points)  => shoelace(points),
        }
    }
}

// ---------------------------------------------------------------------------
// 4. Pipelines
// ---------------------------------------------------------------------------

// `|>` pipes the left value as the first argument of the right call.
let total =
    [1, 2, 3, 4, 5]
    |> filter(|x| x % 2 == 0)
    |> map(|x| x * x)
    |> sum()

// ---------------------------------------------------------------------------
// 5. Pattern matching
// ---------------------------------------------------------------------------

fn classify(n: i32) -> string {
    match n {
        0           => "zero",
        1..=9       => "small",
        10..=99     => "medium",
        _ if n < 0  => "negative",
        _           => "large",
    }
}

// ---------------------------------------------------------------------------
// 6. Error handling
// ---------------------------------------------------------------------------

fn read_config(path: string) -> Result<Config, IoError> {
    let bytes = io.read_file(path)?      // `?` propagates errors
    let text  = utf8.decode(bytes)?
    Config::parse(text)
}

// ---------------------------------------------------------------------------
// 7. Concurrency
// ---------------------------------------------------------------------------

// Lightweight tasks scheduled by the runtime.
spawn {
    for msg in inbox {
        io.println(msg)
    }
}

// Channels.
let (tx, rx) = chan.<i32>()
spawn { tx.send(42) }
io.println(rx.recv())

// ---------------------------------------------------------------------------
// 8. Building & running
// ---------------------------------------------------------------------------

// $ ltlc build hello.ltl   # AOT compile
// $ ltlc run   hello.ltl   # interpret on the bytecode VM
// $ ltlpkg install <name>  # package manager
```

## Further reading

- The Lateralus language repo: <https://github.com/bad-antics/lateralus-lang>
- LateralusOS (the OS written in it): <https://github.com/bad-antics/lateralus-os>
- The Lateralus standard library: <https://github.com/bad-antics/lateralus-stdlib>
