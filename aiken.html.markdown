---
language: Aiken
contributors:
    - ["Riley Kilgore", "http://github.com/Riley-Kilgore"]
    - ["Matthias Benkort", "https://github.com/KtorZ"]
filename: learnaiken.ak
---

Aiken is a programming language designed specifically for writing smart contracts on the Cardano blockchain. It compiles to Untyped Plutus Lambda Calculus (UPLC), which evaluates on the Cardano-Node.

The language includes features tailored for blockchain operations and smart contract development. These include built-in functions based on what's available in UPLC, with upcoming additions like BLS12-381 curve primitives.

Aiken comes with a standard library and prelude that are particularly useful for creating Cardano smart contracts. The language supports various programming constructs such as functions, validators, pattern matching, and algebraic data types.

Aiken accomodates to developer experience with innovations like CIP-57 (Blueprints) having been implemented. This allows developers to write less code to build transactions that are compliant with their validators.

As a relatively new language, Aiken is evolving to meet the needs of the Cardano ecosystem. Its development is guided by the requirements of smart contract creation on the Cardano Blockchain.

```aiken
//// Variables & Constants

// Constants are defined using 'const'
// Constants may be used at the top level of a module
const my_constant: Int = 42

// Values assigned to let-bindings are immutable
// New bindings can shadow previous bindings
let x = 1
let y = x
let x = 2

// y + x == 3

//// Functions

// Functions are defined using 'fn'

// Functions can be named
fn add(a: Int, b: Int) -> Int {
  a + b
}

// Annotations are often optional (albeit recommended), as they can be inferred.
fn add_inferred(a, b) {
  a + b
}

// Functions are private by default. They can be exported with the 'pub' keyword.
pub fn public_function(x: Int) -> Int {
  x * 2
}

// Functions can accept functions as arguments
fn apply_function_twice(x, f: fn(t) -> t) {
  f(f(x))
}

// We can achieve the same outcome as above with pipelining using '|>'
fn apply_function_with_pipe_twice(x, f: fn(t) -> t) {
  x
  |> f
  |> f
}

// Function can be partially applied to create new functions. This behavior is also referred to as 'function captures'.
fn capture_demonstration() {
  let add_one = add(_, 1)
  apply_function_twice(2, add_one) // Evaluates to 4
}

// Backpassing

// Functions that take callbacks can leverage backpassing using `<-`
fn cubed_backpassing(n) {
  let total <- apply_function_twice(n)
  total * n
} 
    
// which is equivalent to writing:
fn cubed_callback(n) {
  apply_function_twice(n, fn(total) {
    total * n
  })
}

//// Data Types

// Aiken has several built-in types:
const my_int: Int = 10
const my_string: String = @"Hello, Aiken!"
const my_bool: Bool = True

// Lists
const my_list: List<Int> = [1, 2, 3, 4, 5]

// Tuples
const my_tuple: (Int, String) = (1, @"one")

// Records can be defined using 'type', a constructor and typed fields. 
type RGB {
  red: Int,
  green: Int,
  blue: Int,
}

// Updating some of the fields of a custom type record.
fn set_red_255(rgb: RGB) {
  RGB {..rgb, red: 255}
}

// Enums are also supported
// As well as full algebraic data-types, with (or without) generic arguments. 
pub type Option<a> {
  Some(a)
  None
}

// Types can also be aliased
type CartesianCoordinates = (Int, Int)

// Validators are special functions in Aiken for smart contracts
validator {
  fn spend(datum: Data, redeemer: Data, context: Data) -> Bool {
    // Smart contract logic goes here
    True
  }
}

// 'Data' can be casted to an extracted from using custom types.
fn to_datum(datum: Data) -> Datum {
    expect d: Datum = datum
    // The line above will fail if datum is not a valid representation of Datum.
    d
}

// Custom data types may be used in contexts that require 'Data'
// The custom data type will be implicitly converted to 'Data'

//// Control Flow

// If expressions
fn abs(x: Int) -> Int {
  if x < 0 {
    -x
  } else {
    x
  }
}

// When expressions (pattern matching)
fn describe_list(list: List<Int>) -> String {
  when list is {
    [] -> @"The list is empty"
    [x] -> @"The list has one element"
    [x, y, ..] -> @"The list has multiple elements"
  }
}

// This syntax can be used instead of when to check a single case and fail otherwise.
expect Some(foo) = my_optional_value

// When the left-hand side pattern is `True`, it can be omitted for conciseness. 
fn positive_add(a, b) {
  // Error if the sum is negative, return it otherwise
  let sum = add(abs(a), abs(b))
  expect sum >= 0
  sum
}

// 'and' and 'or' boolean operators come with their own syntactic sugar
fn my_validation_logic() -> Bool {
  (should_satisfy_condition_1 && should_satisfy_condition_2) || should_satisfy_condition_3 || should_satisfy_condition_4 
}

fn my_more_readable_validation_logic() -> Bool {
  or {
    and {
      should_satisfy_condition_1,
      should_satisfy_condition_2,   
    },
    should_satisfy_condition_3,
    should_satisfy_condition_4,
  }
}

// Sometimes it is useful to have an unfinished function which still allows compilation
// The 'todo' key word will always fail, and provide a warning upon compilation.
fn favourite_number() -> Int {
  // The type annotations says this returns an Int, but we don't need
  // to implement it yet.
  todo @"An optional message to the user"
}

// The fail keyword works similarly, but without any warning on compilation. 
fn expect_some_value(opt: Option<a>) -> a {
  when opt is {
    Some(a) -> a
    None -> fail @"An optional message to the user"
    // We want this to fail when we encounter 'None'.
  }
}

// Trace for debugging
fn debug_function(x: Int) -> Int {
  trace @"We can trace using this syntax."
  (x * 2 < 10)? // This will trace if false.
}

//// Modules and Imports

// Importing modules
use aiken/list

// Using imported functions
fn use_imports() {
  let numbers = [1, 2, 3, 4, 5]
  list.at(2) // evaluates to 3
}

//// Testing

// Tests are defined using the 'test' keyword
test test_add() {
  let result = add(2, 3)
  result == 5
}
```

## Further reading

The [Aiken Documentation](https://aiken-lang.org/installation-instructions) serves as a great place to get started with not only a comprehensive view of Aiken, but also other resources for getting started in development on the Cardano Blockchain.

You can also try out features of Aiken with an online compiler at the official
[Aiken Playground](https://play.aiken-lang.org) or on the main [Aiken website](http://aiken-lang.org).