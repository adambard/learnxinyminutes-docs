---
name: SEMAPRAX
contributors:
    - ["Wavect GmbH", "https://wavect.io/semaprax/"]
filename: learnsemaprax.spx
---

SEMAPRAX is an experimental systems programming language built around a stable
semantic program graph. Human-readable `.spx` source is the canonical Git
projection, while the versioned graph is the preferred interface for agents.

The current Rust implementation is pre-alpha research software. It has bounded
native C11/Clang and WebAssembly Core compiler lanes, but its language, graph
schemas, diagnostics, and ABIs will change. Do not use it for production or
safety-critical workloads.

Official site: <https://wavect.io/semaprax/>
Source: <https://github.com/wavect/semaprax>

```text
// SEMAPRAX source files use the .spx extension.
module tutorial.semaprax;

// Capabilities are declared at module scope and named on functions that use
// them. This example models an effect; it performs no clock I/O itself.
permit { clock.read }

// Public declarations and fields carry persistent semantic identities.
@id("tutorial.point")
record Point {
    @id("tutorial.point.x")
    x: i64,
    @id("tutorial.point.y")
    y: i64,
    @id("tutorial.point.visible")
    visible: bool,
}

@id("tutorial.shift")
fn shift(point: Point, amount: i64) -> Point
{
    // Values are immutable by default. `with` returns an updated record.
    point with { x: point.x + amount }
}

@id("tutorial.counter")
class Counter {
    @id("tutorial.counter.value")
    value: i64,

    @id("tutorial.counter.get")
    fn get(self: Counter) -> i64
    {
        self.value
    }

    @id("tutorial.counter.bumped")
    fn bumped(self: Counter, amount: i64) -> Counter
    {
        Counter { value: self.value + amount }
    }
}

@id("tutorial.scalar_demo")
fn scalar_demo() -> bool
{
    // Unsuffixed integers are i64. Other numeric widths use suffixes.
    let small = 7i32;
    let byte = 255u8;
    let half = 0.5f32;
    let precise = 3.0;
    let rune = '\u{2603}';
    let text = string_concat("sema", "prax");

    small == 7i32 && byte == 255u8 && half == 0.5f32 &&
        precise == 3.0 && rune == '\u{2603}' &&
        string_len(text) == 8
}

@id("tutorial.sign")
fn sign(value: i64) -> i64
{
    match value {
        0 => 0,
        -1 | -2 => -9,
        n if n < 0 => -1,
        n => 1,
    }
}

@id("tutorial.sum_to")
fn sum_to(limit: i64) -> i64
    requires limit >= 0
    ensures result >= 0
{
    // Local mutation is explicit and limited to `let mut` bindings.
    let mut current = limit;
    let mut total = 0;
    while current > 0 {
        total = total + current;
        current = current - 1;
        // The bounded while profile ends its body with a bool guard.
        current > 0
    }
    total
}

@id("tutorial.logical_tick")
fn logical_tick(value: i64) -> i64
    uses { clock.read }
    ensures result == value + 1
{
    value + 1
}

// Resource boundaries state whether a value is borrowed or transferred.
@id("tutorial.buffer")
resource Buffer {
    @id("tutorial.buffer.drop")
    drop trivial;
}

@id("tutorial.inspect")
fn inspect(buffer: borrow Buffer) -> i64
{
    1
}

@id("tutorial.consume")
fn consume(buffer: own Buffer) -> i64
{
    inspect(buffer)
}

@id("app.main")
fn main() -> i64
    uses { clock.read }
    ensures result == 42
{
    let point = Point { x: 18, y: 23, visible: true };
    let moved = shift(point, 1);
    let counter = Counter { value: moved.x + moved.y };
    let answer = counter.bumped(0);

    if scalar_demo() && sign(-3) == -1 && sum_to(3) == 6 &&
        answer.get() == 42 {
        logical_tick(41)
    } else {
        0
    }
}

// Parse, type-check, and verify:
// $ semaprax check learnsemaprax.spx
// Run through the native C11/Clang lane:
// $ semaprax run learnsemaprax.spx
// Inspect the deterministic semantic graph:
// $ semaprax graph learnsemaprax.spx
```

## Further reading

- [SEMAPRAX project page](https://wavect.io/semaprax/)
- [Source repository](https://github.com/wavect/semaprax)
- [Language and compiler RFC](https://github.com/wavect/semaprax/blob/main/docs/RFC-0001.md)
- [Evidence-gated completion matrix](https://github.com/wavect/semaprax/blob/main/docs/COMPLETION-MATRIX.md)
