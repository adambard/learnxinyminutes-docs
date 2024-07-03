---
language: Aiken
contributors:
    - ["Riley-Kilgore", "http://github.com/Riley-Kilgore"]
filename: learnaiken.ak
---

Aiken is a programming language designed specifically for writing smart contracts on the Cardano blockchain. It compiles to Untyped Plutus Lambda Calculus (UPLC), which evaluates on the Cardano-Node.

The language includes features tailored for blockchain operations and smart contract development. These include built-in functions based on what's available in UPLC, with upcoming additions like BLS12-381 curve primitives.

Aiken comes with a standard library and prelude that are particularly useful for creating Cardano smart contracts. The language supports various programming constructs such as functions, validators, pattern matching, and algebraic data types.

Aiken accomodates to developer experience with innovations like CIP-57 (Blueprints) having been implemented. This allows developers to write less code to build transactions that are compliant with their validators.

As a relatively new language, Aiken is evolving to meet the needs of the Cardano ecosystem. Its development is guided by the requirements of smart contract creation on the Cardano Blockchain.

```aiken
//// Basic Syntax and Structure

// Comments in Aiken start with '//'
// Module-level comments use '////'
// Documentation comments use '///'
//// This is a module-level comment
/// This is a documentation comment

// Constants are defined using 'const'
const my_constant: Int = 42

// Functions are defined using 'fn'
fn add(a: Int, b: Int) -> Int {
  a + b
}

// Public functions are marked with 'pub'
pub fn public_function(x: Int) -> Int {
  x * 2
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

// Custom types can be defined using 'type'
type RGB {
  red: Int,
  green: Int,
  blue: Int,
}

// Enums are also supported
pub type Option<a> {
  Some(a)
  None
}

// Validators are special functions in Aiken for smart contracts
validator {
  fn spend(datum: Data, redeemer: Data, context: Data) -> Bool {
    // Smart contract logic goes here
    True
  }
}

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
test positive_add() {
  let result = add(2, 3)
  result == 5
}

// Byte arrays
const my_bytes: ByteArray = #[01, 02, 03, 04]
const my_bytes_2: ByteArray = #"1234"

// Todo for unfinished code
fn unfinished_function() {
  todo
}
```

## Further reading

The [Aiken Documentation](https://aiken-lang.org/installation-instructions) serves as a great place to get started with not only a comprehensive view of Aiken, but also other resources for getting started in development on the Cardano Blockchain.

You can also try out features of Aiken with an online compiler at the official
[Aiken Playground](https://play.aiken-lang.org) or on the main [Aiken website](http://aiken-lang.org).