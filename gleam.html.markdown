---
language: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
---

Gleam is a new language for Erlang's BEAM virtual machine that relies on the
power of a robust type system, the expressiveness of functional programming,
and the highly concurrent fault-tolerant Erlang runtime using familiar and
modern syntax inspired by languages like OCaml, Rust and Elixir.

Being a pretty modern development, Gleam comes with a compiler, a build tool,
a code formatter, several editor integrations, and a package manager.

Being part of the larger BEAM ecosystem, the programs created with Gleam can
also make use of thousands of published packages written in Erlang or Elixir.

The design of the language is very concise so it features no null values,
no exceptions, clear error messages, and a practical type system.

JavaScript is additionally supported as a compile target, so you can run Gleam
code in browser or any other JS-enabled runtime. When using this feature,
TypeScript definitions get created, so you can interact with your Gleam code
confidently, even from the outside.

```
//// This comment with four slashes is a module-level.
//// This kind of comments are used to describe the whole module.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/iterator
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/string as text

// A type's name always starts with a capital letter, contrasting to variables
// and functions, which start with a lowercase letter.

// When the pub keyword is used the type alias is public and can be referred to
// by other modules.

pub type UserId =
  Int

pub fn main() {
  io.println("Hello from learnxinmyminutes.com!")
  // io.println("This statement got commented out by a two slashes comment.!")

  // Modules are the units in which all Gleam code gets organized.
  // In a module you will find a bunch of definitions of types, functions, etc.
  // that seem to belong together.
  // For example, the gleam/io module contains a variety of functions for
  // printing, like println.

  // All gleam code is in some module or other, whose name comes from the name
  // of the file it's in.
  // For example, gleam/io is in a file called io.gleam in a directory called
  // gleam.

  // Gleam has a robust static type system that helps you as you write and edit
  // code, catching mistakes and showing you where to make changes.
  // io.println(10)
  // If you uncomment the previous line you'll get a compile time error reported
  // as the io.println function only works with strings, not ints.

  // The compile will output an error that looks like this:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │   io.println(10)
  //  │              ^^
  //
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // Working with numbers

  // When running on the Erlang virtual machine ints have no maximum and minimum
  // size.
  // When running on JavaScript runtimes ints are represented using JavaScript's
  // 64 bit floating point numbers.

  // Int arithmetic
  io.debug(1 + 1)
  io.debug(5 - 1)
  io.debug(5 / 2)
  io.debug(3 * 3)
  io.debug(5 % 2)

  // Int comparisons
  io.debug(2 > 1)
  io.debug(2 < 1)
  io.debug(2 >= 1)
  io.debug(2 <= 1)

  // Equality works for any type and is checked structurally, meaning that two
  // values are equal if they have the same structure rather than if they are at
  // the same memory location.
  io.debug(1 == 1)
  // True
  io.debug(2 != 2)
  // False

  // Standard library int functions
  io.debug(int.min(142, 137))
  // 137
  io.debug(int.clamp(-80, min: 0, max: 100))
  // 0
  io.debug(int.base_parse("10", 2))
  // Ok(2)

  // Binary, octal, and hex Int literals
  io.debug(0b00001111)
  io.debug(0o17)
  io.debug(0xF)

  // Use underscores to enhance integer readability
  io.debug(1_000_000)

  // Gleam's numerical operators are not overloaded, so there are dedicated
  // operators for working with floats.

  // Float arithmetic
  io.debug(1.0 +. 1.5)
  io.debug(5.0 -. 1.5)
  io.debug(5.0 /. 2.5)
  io.debug(3.0 *. 3.5)

  // Float comparisons
  io.debug(2.2 >. 1.3)
  io.debug(2.2 <. 1.3)
  io.debug(2.2 >=. 1.3)
  io.debug(2.2 <=. 1.3)

  // Floats are represented as 64-bit floating point numbers on both the Erlang
  // and JavaScript runtimes.
  // The floating point behaviour is native to their respective runtimes, so
  // their exact behaviour will be slightly different on the two runtimes.

  // Under the JavaScript runtime, exceeding the maximum (or minimum)
  // representable value for a floating point value will result in Infinity
  // (or -Infinity). Should you try to divide two infinities you will get NaN
  // as a result.

  // When running on the BEAM any overflow will raise an error. So there is no
  // NaN or Infinity float value in the Erlang runtime.

  // Division by zero is not an error
  io.debug(3.14 /. 0.0)
  // 0.0

  // Standard library float functions
  io.debug(float.max(2.0, 9.5))
  // 9.5
  io.debug(float.ceiling(5.4))
  // 6.0

  // Underscores for floats are also supported
  io.debug(10_000.01)

  // Division by zero will not overflow but is instead defined to be zero.

  // Working with strings
  io.debug("⭐ Gleam ⭐ - 별")
  io.debug(
    "this
    is
    a
    multi
    line
    string",
  )
  io.debug("\u{1F600}")
  // Outputs a smiley 😀

  // Double quote can be escaped
  io.println("\"X\" marks the spot")

  // String concatenation
  io.debug("One " <> "Two")

  // String functions
  io.debug(text.reverse("1 2 3 4 5"))
  io.debug(text.append("abc", "def"))

  io.println(text.reverse("!desrever tog gnirts sihT"))
  // Outputs "This string got reversed!"

  // Several escape sequences are supported:

  // \" - double quote
  // \\ - backslash
  // \f - form feed
  // \n - newline
  // \r - carriage return
  // \t - tab

  // Bool operators
  // The || and && operators work by short-circuiting

  io.debug(True && False)
  // False

  io.debug(True && True)
  // True

  io.debug(False || False)
  // False

  io.debug(False || True)
  // True

  // Bool functions
  io.debug(bool.to_string(True))
  // "True"

  io.debug(bool.to_int(False))
  // 0

  // Assignments
  let x = "Original value"
  io.debug(x)

  // Assign `y` to the value of `x`
  let y = x
  io.debug(y)

  // Assign `x` to a new value
  let x = "New value"
  io.debug(x)

  // The `y` still refers to the original value
  io.debug(y)

  // In Gleam variable and function names are written in snake_case.
  let answer_to_the_universe = 42
  io.debug(answer_to_the_universe)

  let and_everything = answer_to_the_universe
  // Now using a variable produces a warning

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │   let and_everything = answer_to_the_universe
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_and_everything`.

  // Type annotations

  let _name: String = "Gleam"

  let _is_cool: Bool = True

  let _version: Int = 1
  // Useful for documentation purposes but they do not change how the compiler
  // type checks the code beyond making sure the annotation matches the type,
  // otherwise you get an error.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │   let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // Type aliases
  let one: UserId = 1
  // Refer to the beginning of the file for the definition of the UserId type

  let two: Int = 2

  // Aliases are just for creating more readable code and more precise
  // documentation.
  // Under the hood they are still values of the same type so operations
  // still work
  io.debug(one + two)
  // 3

  // Blocks: scoping and value
  let radius = {
    let value = 100.0
    value
  }
  // io.debug(value) // <- This will not compile because "value" is out of scope

  let area = 3.14159 *. radius *. radius
  io.debug(area)

  // Use blocks to group operations instead of parenthesis
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  io.debug(n1 != n2)
  // True

  // Lists

  // Nephews of Scrooge McDuck
  let nephews = ["Huey", "Dewey", "Louie"]
  io.debug(nephews)
  // ["Huey", "Dewey", "Louie"]

  // Immutably prepend so the original list is not changed
  io.debug(["Donald", ..nephews])
  // ["Donald", "Huey", "Dewey", "Louie"]

  // Some standard library functions for lists

  list.each(nephews, io.println)
  // Huey
  // Dewey
  // Louie

  io.debug(list.drop(nephews, 2))
  // ["Louie"]

  more_examples()
  more_function_examples()
  generic_typing_examples()
  beloved_pipelines_demo()
  labels_in_function_calls()
  showcase_flow_control()
  more_on_recursion()
  more_on_pattern_matching()
  showcase_types()
  more_on_types()
  more_on_callbacks()
  showcase_externals()
  showcase_panic()
}

// The fn keyword is used to define new functions.
fn multiply(a: Int, b: Int) -> Int {
  // No explicit return
  // The last expression gets returned
  a * b
}

// The double and multiply functions are defined without the pub keyword.
// This makes them private functions, they can only be used within this module.
// If another module attempted to use them it would result in a compiler error.
fn double(a: Int) -> Int {
  multiply(a, 2)
}

// Only public functions are exported and can be called from outside the module.

// Type annotations are optional for function arguments and return values
// but are considered good practice for clarity and in order to encourage
// intentional and thoughtful design.

pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 } && { { year % 100 != 0 } || { year % 400 == 0 } }
}

fn more_examples() {
  // Debug also returns a value so its output is the return value of
  // this function
  io.debug(double(10))
  // 20
  io.debug(is_leap_year(2000))
  // True
}

// Gleam supports higher-order functions:
// They can be assigned to variables, passed as arguments to other functions
// or even be returned as values from blocks or other functions
fn call_func_on_int(func: fn(Int) -> Int, value: Int) -> Int {
  func(value)
}

fn more_function_examples() -> Int {
  io.debug(call_func_on_int(double, 2))
  // 4

  let square = fn(x: Int) -> Int { x * x }
  io.debug(square(3))
  // 9

  // Calling an anonymous function immediately after defining it
  io.debug(fn(x: Int) { x + 1 }(1))

  // Closure example
  let make_adder = fn(n: Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { argument + n }
  }

  let adder_of_fives = make_adder(5)
  io.debug(adder_of_fives(10))
  // 15

  // Anonymous functions can be used interchangeably with named functions.
  io.debug(call_func_on_int(fn(x: Int) -> Int { x + 100 }, 900))
  // 1000

  // Let's create a function decorator
  let twice = fn(wrapped_func: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { wrapped_func(wrapped_func(argument)) }
  }
  let quadruple = twice(double)
  io.debug(quadruple(1))

  let quadruple_2 = fn(a: Int) -> Int { multiply(4, a) }
  io.debug(quadruple_2(2))
  // 8

  // A function capture is a shorthand syntax for creating anonymous functions
  // that takes one argument and immediately calls another function with that
  // argument
  let quadruple_3 = multiply(4, _)
  io.debug(quadruple_3(4))
  // 16
}

// Generic functions are supported using type variables.
fn generic_twice(func: fn(value) -> value, argument: value) -> value {
  func(func(argument))
}

// In generic_twice value was the type variable.
// In generic_twice_decorator the_type is the type variable.
// As in any other variable you get to choose the name.
fn generic_twice_decorator(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argument: the_type) -> the_type { func(func(argument)) }
}

fn generic_typing_examples() {
  let double_integers = fn(a: Int) -> Int { a * 2 }
  let double_floats = fn(a: Float) -> Float { a *. 2.0 }
  io.debug(generic_twice(double_integers, 3))
  io.debug(generic_twice(double_floats, 3.0))

  let quadruple_integers = generic_twice_decorator(double_integers)
  let quadruple_floats = generic_twice_decorator(double_floats)
  io.debug(quadruple_integers(1))
  // 4
  io.debug(quadruple_floats(1.0))
  // 4.0
}

// Gleam's pipe operator |> takes the result of the expression on its left
// and passes it as an argument to the function on its right.
fn beloved_pipelines_demo() {
  // Let's be honest: you want to use Gleam just for this cool operator, right?
  ["hello", "world"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> io.debug

  // Match cleaner than this right?
  io.debug(
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hello", "world"], " "), ["!"]),
      ),
    ),
  )

  // Solution to the first problem of Project Euler:
  // URL: https://projecteuler.net/problem=1
  // Description: Find the sum of all the multiples of 3 and 5 below 1000.
  iterator.iterate(1, fn(n) { n + 1 })
  |> iterator.take(1000 - 1)
  |> iterator.filter(fn(n) { { n % 3 == 0 } || { n % 5 == 0 } })
  |> iterator.fold(from: 0, with: fn(acc, element) { element + acc })
  |> int.to_string
  |> fn(sum_as_text: String) {
    "Solution to Project Euler's problem #1: " <> sum_as_text
  }
  |> io.debug
  // Solution to Project Euler's problem #1: 233168
}

// Labels can be added before each argument
fn call_func_on_int_with_labels(
  func passed_func: fn(Int) -> Int,
  value n: Int,
) -> Int {
  passed_func(n)
}

// The label and the argument can have the same name
fn add_one(number number: Int) -> Int {
  number + 1
}

fn add_two_integers(first n: Int, second m: Int) -> Int {
  n + m
}

fn labels_in_function_calls() -> Int {
  // Since we are labelling the arguments we can switch the order
  // if we want to
  io.debug(call_func_on_int_with_labels(value: 8, func: double))
  io.debug(add_one(number: 1))
  // 2
  io.debug(string.contains(does: "theme", contain: "the"))
  // True
  // Unlabeled arguments must go first
  io.debug(add_two_integers(2, second: 2))
  // 4
}

fn showcase_flow_control() {
  // Use case if you want to use pattern-matching in order to
  // select which code to execute.
  // Gleam will make sure all possible values are covered
  // by performing exhaustiveness checks.
  // Otherwise you get compilation errors.
  let puppies = ["Bear", "Frisco", "Ranger"]
  let count = list.length(of: puppies)
  {
    "We have "
    <> int.to_string(count)
    <> " "
    <> // The underscore matches with any other value
    case count {
      1 -> "puppy"
      _ -> "puppies"
    }
  }
  |> io.debug

  // Gleam allows patterns in case expressions to also assign variables.
  {
    "Puppy count: "
    <> case list.length(puppies) {
      0 -> "None."
      1 -> "Just one."
      other -> "As many as " <> int.to_string(other) <> " puppies."
    }
  }
  |> io.debug

  // Consider BEAM languages are functional in design and Gleam is no exception
  // so there are no if, for or while constructs available.

  // Use pattern-matching for conditionals
  let answer = 42
  case answer == 42 {
    True -> {
      io.debug("This is the answer to the universe.")
    }
    False -> {
      io.debug("This is the answer to something else.")
    }
  }

  // Use recursion instead of looping
  from_one_to_ten(1)
}

// Recursive function
fn from_one_to_ten(n: Int) {
  io.debug(n)
  case n {
    10 -> Nil
    _ -> from_one_to_ten(n + 1)
  }
}

// In order to avoid memory exhaustion due to creating excessive
// stack frames when calling functions recursively, Gleam supports
// "tail call optimisation" which means that the compiler can reuse
// the stack frame for the current function if a function call is
// the last thing the function does.

pub fn fib(x: Int) -> Int {
  // The public function calls the private tail recursive function
  fib_loop(x, 1)
}

fn fib_loop(x: Int, accumulator: Int) -> Int {
  case x {
    1 -> accumulator

    // The last thing this function does is call itself
    // In the previous lesson the last thing it did was multiply two ints
    _ -> fib_loop(x - 1, accumulator + x)
  }
}

// Gleam supports pattern-matching the first element and the remainder
// of a list with the [x, ..y] pattern inside a case expression.
fn reverse_list(the_list: List(value)) -> List(value) {
  case the_list {
    [head, ..tail] -> list.concat([reverse_list(tail), [head]])
    [] -> []
  }
}

fn more_on_recursion() {
  io.debug(fib(10))
  // 55
  io.debug(reverse_list([1, 2, 3]))
}

fn more_on_pattern_matching() {
  // When pattern-matching on strings the <> operator match on strings
  // with a specific prefix and assigns the reminder to a variable
  io.debug(case "Hello, Lucy" {
    "Hello, " <> name -> "Greetings for " <> name
    _ -> "Potentially no greetings"
  })

  // Alternative patterns are supported so the same clause is used
  // for multiple values
  let month = 2
  let year = 2024
  let number_of_days = case month {
    2 ->
      case is_leap_year(year) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  io.debug("Number of days: " <> int.to_string(number_of_days))
  // 29

  // Guards in pattern-matching:
  // When using the if keyword an expression must evaluate to True
  // for the pattern to match.
  let list_starts_with = fn(the_list: List(value), the_value: value) -> Bool {
    case the_list {
      [head, ..] if head == the_value -> True
      _ -> False
    }
  }
  io.debug(list_starts_with([10, 20, 30], 10))
  // True
}

pub type Gender {
  Male
  Female
  Other
}

// Records:
// - Support variants
// - Each variant is similar to a struct with fields
pub type Shape {
  Rectangle(base: Float, height: Float)
  Triangle(base: Float, height: Float)
}

// Records with one variant resemble structs
pub type Point {
  Point(x: Float, y: Float)
}

fn showcase_types() {
  // Tuples:
  // - Can mix together elements of different types
  // - Their type is implicit e.g. #{1, "Hello"} is of type #{Int, String}
  // - Their elements can be accessed by numeric indexes
  let tuple_01 = #(1, "Ferris", "rustacean", True)
  let tuple_02 = #(1, "Lucy", "starfish", True)
  io.debug(tuple_01)
  io.debug(tuple_01.0)
  // 1
  io.debug(tuple_02.1)
  // Lucy
  let #(_, name, species, _) = tuple_01
  io.debug(name <> " the " <> species)

  // Pattern-matching with tuples including variable assignment
  case tuple_02 {
    #(_, name, _, True) -> io.debug(name <> " is a mascot.")
    #(_, name, _, False) -> io.debug(name <> " is not a mascot.")
  }

  // Using a custom type with pattern-matching
  let gender = Other
  io.debug(case gender {
    Male -> "Boy"
    Female -> "Girl"
    _ -> "Undetermined"
  })

  // Using records
  let rectangle_1 = Rectangle(base: 10.0, height: 20.0)
  io.debug(rectangle_1.height)
  // 10.3

  let point_1 = Point(x: 3.2, y: 4.3)
  io.debug(point_1)

  // Updating a record
  let point_2 = Point(..point_1, y: 5.7)
  io.debug(point_2)

  // In Gleam, values are not nullable.
  // Nil is the only value of its type.
  let some_var = Nil
  let result = io.println("Hello!")
  io.debug(some_var == result)
  // True
}

pub type Mineral {
  Gold
  Silver
  Copper
}

// Generic custom types with contained types as parameters
pub type Purity(inner_type) {
  Pure(inner_type)
  Impure(inner_type)
}

pub type Beverage {
  Water
  Juice
}

// Existing custom types from the gleam/option and gleam/result modules
// facilitate working with nullable values and handling potential errors
pub type Person {
  Person(name: String, nickname: Option(String))
}

pub type DiceError {
  DiceValueOutOfRange
}

fn checked_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(value)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn double_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 -> Ok(value * 2)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn more_on_types() {
  let mineral_sample_01: Purity(Mineral) = Pure(Gold)
  let mineral_sample_02 = Impure(Silver)
  io.debug(mineral_sample_01)
  io.debug(mineral_sample_02)

  // A glass can be empty or not
  let glass_01: Option(Beverage) = Some(Water)
  let glass_02 = None
  io.debug(glass_01)
  io.debug(glass_02)

  // A person can have a nickname or not
  let person_01 = Person(name: "John", nickname: Some("The Ripper"))
  let person_02 = Person(name: "Martin", nickname: None)
  io.debug(person_01)
  io.debug(person_02)

  // Working with functions that return values of type Result
  let dice_01 = 5
  case checked_dice_value(dice_01) {
    Ok(checked_value) ->
      io.debug("The value of " <> int.to_string(checked_value) <> " is OK.")
    Error(DiceValueOutOfRange) ->
      io.debug("The value of the dice is out of range")
  }

  // Let's attempt to double the value if the resulting value is still
  // a number in any of the sides of the dice.
  // Otherwise, let's put the max value.
  2
  |> checked_dice_value
  |> result.try(double_dice_value)
  |> result.unwrap(or: 6)
  |> io.debug
}

pub fn throw_dice_as_result() {
  Ok(int.random(6) + 1)
}

pub fn sum_dice_values(a: Int, b: Int) {
  Ok(a + b)
}

// Betting on first-class functions and pattern-matching
// can easily lead to tons of indentation
fn roll_two_dices_without_use() {
  result.try(throw_dice_as_result(), fn(first_dice) {
    result.try(throw_dice_as_result(), fn(second_dice) {
      result.map(sum_dice_values(first_dice, second_dice), fn(sum) { sum })
    })
  })
}

// The use expression still lets us write code that uses callbacks
// but cleans up excessive indentation:
// - A call to higher order function go the right side of the <- operator
// - The argument names for the callback function go on the left hand side of
//   the <- operator
// - All the remaining code in the enclosing {} block becomes the body of the
//   callback function.
fn roll_two_dices_with_use() {
  use first_dice <- result.try(throw_dice_as_result())
  use second_dice <- result.try(throw_dice_as_result())
  use sum <- result.map(sum_dice_values(first_dice, second_dice))
  // This is the remaining code in innermost callback function
  sum
}

fn more_on_callbacks() {
  io.debug(roll_two_dices_without_use())
  io.debug(roll_two_dices_with_use())
}

pub type DateTime

// External functions must annotate a return type
@external(erlang, "calendar", "local_time")
pub fn now() -> DateTime

fn showcase_externals() {
  io.debug(now())
  // #(#(2024, 4, 6), #(14, 4, 16))
}

fn showcase_panic() {
  // We can deliberately abort execution by using the panic keyword
  // in order to make our program crash immediately
  case 3 == 2 {
    True -> panic as "The equality operator is broken!"
    False -> "Equality operator works for integers"
  }
  // Calling a function that uses the todo keyword also crashes
  // homework()
}

pub fn homework() {
  todo
}
```

## Further reading

* [Gleam's official website](https://gleam.run/)
* [Language tour](https://tour.gleam.run/) - Includes live code editor
* [Official documentation](https://gleam.run/documentation/)
* [Gleam's awesome list](https://github.com/gleam-lang/awesome-gleam)
* [Exercism track for Gleam](https://exercism.org/tracks/gleam)

The official docs have cheatsheets for people familiar with:

* [Elixir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Elm](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Rust](https://gleam.run/cheatsheets/gleam-for-python-users)
