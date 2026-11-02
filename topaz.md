---
name: Topaz
contributors:
    - ["STUDIO HAZE", "https://topaz.ooo/"]
filename: learntopaz.tpz
---

Topaz is a general-purpose programming language with typesetting built in. Code
that does the same job looks the same no matter who writes it.

Topaz is developed by STUDIO HAZE. This is distinct from other languages named
Topaz. Its compiler is written in Topaz and emits Rust.

Save the following code as `learntopaz.tpz` and run
`topaz run learntopaz.tpz --compiler rust` to select the reference compiler.
This tour uses features available in the official Topaz 5.21.0 executable.
Checking and direct execution do not need Cargo; building native programs does.

```text
// Line comments start with two slashes.
/* Block comments can span
   more than one line. */

import std.io

// Bindings infer types, or you can state a type explicitly.
const DEFAULT_MINUTES = 25
let language: string = "Topaz"
let ready: bool = true
let measurement: float = 2.5

// A let binding is immutable unless it is declared mut.
let mut completed: int = 0
completed = completed + 1
io.writeLine("{language}: {ready}, {measurement}, {completed}")
// Topaz: true, 2.5, 1

// A type mismatch is rejected before execution:
// let invalid: int = "25"

// Blocks and if expressions yield their final expression.
let total = {
    let base = 20
    base + 5
}
let label = if total >= DEFAULT_MINUTES { "ready" } else { "short" }
io.writeLine(label) // ready

// Named functions have typed parameters and return values.
function double(value: int) -> int {
    value * 2
}
let increment: (int) -> int = value => value + 1
io.writeLine("{double(increment(3))}") // 8

// Arrays, maps, and sets have distinct syntax.
let scores: Array<int> = [10, 25, 10]
let names = map { "Ada": 10, "Lin": 25 }
let unique = set { 10, 25, 10 }
let focused = [ for score in scores if score >= 20 => score * 2 ]
io.writeLine("{scores[0]}:{focused}:{unique.toArray()}:{names.keys}")
// 10:[50]:[10, 25]:[Ada, Lin]

// Iteration can update an explicitly mutable binding.
let mut sum = 0
for score in scores {
    sum = sum + score
}
io.writeLine("{sum}") // 45

// Records name their fields and can supply defaults.
record Task {
    title: string,
    done: bool = false,
}
let task = Task { title: "Read a tour" }
io.writeLine("{task.title}: {task.done}") // Read a tour: false

// Safe lookup returns Option<T>; pattern matching handles both cases.
let selection: Option<int> = scores.get(99)
let selected = match selection {
    case Some(value) => "found {value}"
    case None => "missing"
}
io.writeLine(selected) // missing

// Result<T, E> represents an expected failure as a value.
function positive(value: int) -> Result<int, string> {
    if value > 0 { Ok(value) } else { Err("expected a positive integer") }
}

// ? unwraps Ok and propagates Err from a Result-returning function.
function twicePositive(value: int) -> Result<int, string> {
    let accepted = positive(value)?
    Ok(accepted * 2)
}

for value in [3, 0] {
    let answer = match twicePositive(value) {
        case Ok(number) => "accepted {number}"
        case Err(message) => "rejected: {message}"
    }
    io.writeLine(answer)
}
// accepted 6
// rejected: expected a positive integer

// Runtime faults, such as indexing outside an array, are distinct from
// explicit Err values. Prefer checked operations such as get when absence
// is part of normal program logic.
```

## Further reading

- [Official first program](https://topaz.ooo/v5.21/en/docs/getting-started)
- [Language tour](https://topaz.ooo/v5.21/en/docs/language-tour)
- [Compiler release source](https://github.com/studiohaze/topaz)
