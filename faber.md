---
name: Faber
filename: learnfaber.fab
contributors:
    - ["Ian Zepp", "https://github.com/ianzepp"]
---

Faber (Faber Romanus) is a programming language designed to be written by
language models and read by people. Its grammar is small and mechanical:
types come before names, every operator is a single glyph with one meaning,
and conversions, absence and failure are always explicit. The same front end
feeds a reference interpreter (`faber run`) and code generators for C, Rust,
Go, TypeScript, WebAssembly and more.

Faber is experimental through version 1.x, and minor releases can break
code. Everything below checks and runs with Faber 1.19.

```
# Comments start with `#`. Since 1.19 a comment may also end a line that
# completes a statement; a `#` inside an unfinished expression is still a
# lexer error (LEX007).

# Faber is type-first: the type always comes before the name.
# Every operator is one glyph with one meaning:
#   ←  store a value at run time      →  return type
#   ⇥  error channel                  ↦  explicit conversion
#   ≡ ≢  equal / not equal            ≺ ≻ ≤ ≥  ordering
# `=` never assigns: it only sets compile-time facts and defaults.

###########################################
## 1. Bindings and primitive types
###########################################

# `const` is a compile-time fact, written with `=`.
const int LIMIT = 3

# A function: parameter types before names, return type after `→`.
fn greet(string name) → string {
    # `§` marks a slot; calling a string literal fills its slots.
    return "Salve, §!"(name)
}

fn bindings() → void {
    # `let` binds a fixed value; the type slot is never omitted.
    let int answer ← 42
    let float ratio ← 0.5
    let bool ready ← true
    let string who ← "Marcus"

    # `_` in the type slot asks the compiler to infer it.
    let _ inferred ← answer + 1

    # `mut` binds a value you may store into again.
    mut int counter ← 0
    counter ← counter + 1

    print greet(who)
    print "§ § § §"(answer, ratio, ready, inferred)
    print counter
    print LIMIT
}

###########################################
## 2. Numbers follow mathematics
###########################################

fn numbers() → void {
    # `/` and `%` floor, so the results below are -4 and 1.
    print -7 / 2
    print -7 % 2

    # Arithmetic is exact inside an expression; a width limit applies
    # once, where the value is stored. (200 + 100) / 2 fits in a u8.
    let wrapping<u8> a ← 200
    let wrapping<u8> b ← 100
    let wrapping<u8> mean ← (a + b) / 2
    print mean

    # Crossing number families is an explicit `↦` conversion, and a
    # float-to-integer conversion must name its rounding mode.
    # A conversion that can fail (NaN, "4x") takes a fallback with `⊥`.
    let float x ← 2.5
    let int even ← x ↦ int via HalfEven ⊥ 0
    let int down ← x ↦ int via Floor ⊥ 0
    let int parsed ← "4x" ↦ int ⊥ -1
    print "§ § §"(even, down, parsed)
}

###########################################
## 3. Absence is a type
###########################################

fn absence() → void {
    # There is no `int?`: a value that may be missing is a union.
    let list<int> xs ← [10, 20, 30]
    let int ∪ none tenth ← xs.get(9)
    print tenth
    print tenth coalesce 0
}

###########################################
## 4. Control flow
###########################################

fn control() → void {
    let int score ← 85
    if score ≥ 90 {
        print "A"
    }
    elif score ≥ 80 {
        print "B"
    }
    else {
        print "C"
    }

    # A one-statement body can follow `then`.
    if score ≢ 0 then print "nonzero"

    # Walk a list with `for from`.
    let list<string> names ← ["Ana", "Ben"]
    for from names let name {
        print name
    }

    # A range picks its direction from its bounds. `‥` stops before
    # the end bound and `…` includes it: this prints 3, 2, 1.
    for range 3‥0 let i {
        print i
    }

    mut int n ← 0
    while n ≺ LIMIT {
        n ← n + 1
    }
    print n

    # `switch` compares a value against literal cases.
    let int code ← 404
    switch code {
        case 200 {
            print "ok"
        }
        case 404 {
            print "not found"
        }
    }
}

###########################################
## 5. Collections and closures
###########################################

fn collections() → void {
    let list<int> nums ← [1, 2, 3, 4, 5]
    print nums.length()

    # `∴` ("therefore") introduces a closure body.
    print nums.filter(int x ∴ x % 2 ≡ 0)
    print nums.map(int x ∴ x * 10)
    print nums.reduce((int acc, int x) ∴ acc + x, 0)

    # A closure is a value with a function type.
    let (int) → int twice ← int x ∴ x * 2
    print twice(21)

    # Maps. Indexing a missing key stops the program; `.get` is the
    # checked form and returns `V ∪ none`.
    mut map<string, int> ages ← empty
    ages["Marcus"] ← 30
    print ages["Marcus"]
    print ages.get("Julia")
}

###########################################
## 6. Classes and unions
###########################################

class Rect {
    # A field with `=` has a default.
    mut int width = 1
    mut int height = 1

    fn area() → int {
        return self.width * self.height
    }
}

# A union is a closed set of variants that may carry data.
union Shape {
    Circle { int radius },
    Square { int side }
}

fn describe(Shape s) → string {
    match s {
        case Circle let r {
            return "circle of radius §"(r)
        }
        case Square let side {
            return "square of side §"(side)
        }
    }
}

fn types() → void {
    let Rect r ← Rect { width = 3, height = 4 }
    print r.area()
    print describe(variant Circle { radius = 2 })
}

###########################################
## 7. Errors are a typed second channel
###########################################

# `⇥` names what a function can fail with; `throw` sends it.
fn divide(int a, int b) → int ⇥ string {
    if b ≡ 0 then throw "division by zero"
    return a / b
}

fn errors() → void {
    # Supply a recovery value with `⊥`...
    print divide(7, 0) ⊥ -1

    # ...or handle the error with `do` / `catch`.
    do {
        print divide(7, 2)
        print divide(1, 0)
    }
    catch err {
        print "caught: §"(err)
    }
}

###########################################
## 8. Generics
###########################################

fn first<T>(list<T> items, T fallback) → T {
    return items.get(0) coalesce fallback
}

###########################################
## 9. Reader languages
###########################################

# Keywords, type names and diagnostics come from a locale pack, and a
# file uses exactly one. `faber convert --to la` rewrites `greet` in
# the Latin pack (frontmatter `+++ locale = "la" +++`):
#
#   functio greet(textus name) → textus {
#       redde "Salve, §!"(name)
#   }
#
# The program is the same; only the reader's spelling changes.

# `main` is the entry point.
main {
    bindings()
    numbers()
    absence()
    control()
    collections()
    types()
    errors()
    print first<int>([], 7)
}
```

Save the code as `learnfaber.fab`, then:

```bash
faber check learnfaber.fab   # parse and type-check only
faber run learnfaber.fab     # run it on the reference interpreter
faber explain SEM010         # look up any diagnostic code
```

## Further reading

* [faberlang.dev](https://faberlang.dev): documentation, install guide and
  language reference
* [faberlang/faber](https://github.com/faberlang/faber): compiler front end,
  reference runner, standard library and example corpus
* [Release downloads](https://github.com/faberlang/releases/releases/latest)
