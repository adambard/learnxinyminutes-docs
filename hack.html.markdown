---
language: Hack
contributors:
    - ["Andrew DiMola", "https://github.com/AndrewDiMola"]
    - ["Stephen Holdaway", "https://github.com/stecman"]
    - ["David Lima", "https://github.com/davelima"]
filename: learnhack.hh
---

[Hack](https://hacklang.org/) lets you write code quickly, while also having safety features built in, like static typechecking.

To run Hack code, [install HHVM](https://docs.hhvm.com/hhvm/installation/introduction), the open-source virtual machine.

```php
/* ==================================
 *           READ THE DOCS!
 * ==================================
 */

/* For more information on the Hack language:
 * - About Hack: https://hacklang.org/
 * - Documentation: https://docs.hhvm.com/hack/
 */

/* ==================================
 *           A NOTE ON PHP
 * ==================================
 */

// The Hack language began as a superset of PHP.
// Since then, the languages have (largely) diverged.
// You may encounter the .php extension, which is no longer recommended.

/* ==================================
 *              COMMENTS
 * ==================================
 */

// Hack has single-line comments...

/* Multi-line comments...
 *
 */

/**
 * ... and a special syntax for doc comments.
 *
 * Use doc comments to summarize the purpose of a definition, function, class or method.
 */

/* ==================================
 *             NAMESPACES
 * ==================================
 */

// Namespaces contain definitions of classes, interfaces, traits, functions, and constants.

namespace LearnHackinYMinutes {

  /* ==================================
   *                TYPES
   * ==================================
   */

  function demo_hack_types(): void {

    // Hack has five primitive types: bool, int, float, string, and null.
    $is_helpful = true; // bool
    $int_value = 10; // int
    $precise_value = 2.0; // float
    $hello_world = "Hello World!"; // string
    $null_string = null; // null

    // Create a `shape` with the shape keyword, with a series of field names and values.
    $my_point = shape('x' => -3, 'y' => 6, 'visible' => true);

    // Create a `tuple` with the tuple keyword, with a series of two or more types as values.
    $apple_basket = tuple("apples", 25); // different types are OK

    // Use `arraykey` to represent either an integer or string.
    $the_answer = 42;
    $is_answer = process_key($the_answer);

    // Similarly, `num` represents either an int or float.
    $lucky_number = 7;
    $lucky_square = calculate_square($lucky_number);
  }

  function process_key(arraykey $the_answer): bool {
    if ($the_answer is int) {
      return true;
    } else {
      return false;
    } // true
  }

  function calculate_square(num $arg)[]: float {
    return ((float)$arg * $arg);
  }

  // Enums are limited to int or string (as an Arraykey), or other enum values.
  enum Permission: string {
    Read = 'R';
    Write = 'W';
    Execute = 'E';
    Delete = 'D';
  }

  // In contrast, an enum class can be of any value type!
  enum class Random: mixed {
    int X = 42;
    string S = 'foo';
  }

  /* ==================================
   *            HACK ARRAYS
   * ==================================
   */

  // The following line lets us use functions in the `C\` namespace.
  use namespace HH\Lib\C; // the `C` library operates on containers

  function demo_hack_arrays(): void {

    // vec: ordered
    $v = vec[1, 2, 3];
    $letters = vec['a', 'b', 'c'];

    $letters[0]; // returns 'a'
    $letters[] = 'd'; // appends 'd'

    // `inout` provides pass-by-reference behavior
    C\pop_back(inout $letters); // removes 'd'
    C\pop_front(inout $letters); // removes 'a'

    // keyset: ordered, without duplicates
    $k = keyset[1, 2, 3]; // values must be int or string
    $colors = keyset['red', 'blue', 'green'];

    // keyset keys are identical to their values
    $colors['blue']; // returns 'blue'.

    $colors[] = 'yellow'; // appends 'yellow'
    unset($colors['red']); // removes 'red'

    //  dict: ordered, by key-value
    $d = dict['a' => 1, 'b' => 3]; // keys must be int or string
    $alphabet = dict['a' => 1, 'b' => 2];

    $alphabet['a']; // indexing at 'a' returns `1`
    $alphabet['c'] = 3; // adds a new key-value pair of `c => 3`

    unset($alphabet['b']); // removes 'b'
  }

  /* ==================================
   *  THE HACK STANDARD LIBRARY (HSL)
   * ==================================
   */

  // The Hack Standard Library is a set of functions and classes for the Hack language.
  // Namespace use declarations are ideally at the top of your file but are placed here for instruction purposes.

  use namespace HH\Lib\Str; // The `Str` library operates on strings

  function demo_hack_standard_library(): void {

    $letters = vec['a', 'b', 'c'];
    $colors = keyset['red', 'blue', 'green'];
    $alphabet = dict['a' => 1, 'b' => 2];

    C\contains($letters, 'c'); // checks for a value; returns 'true'
    C\contains($colors, 'purple'); // checks for a value; returns 'false'
    C\contains_key($alphabet, 'a'); // checks for a key; returns 'true'
    C\contains($alphabet, 'd'); // checks for a value; returns 'false'

    Str\length("foo"); // returns `3`
    Str\join(vec['foo', 'bar', 'baz'], '!'); // returns `foo!bar!baz`
  }

  /* ==================================
   *           HELLO WORLD!
   * ==================================
   */

  use namespace HH\Lib\IO; // the `IO` library is a standard API for input / output

  <<__EntryPoint>> // required attribute for the typical entry/main function
  async function main(): Awaitable<
    void,
  > { // does not need to be named 'main' / is an asynchronous function
    await IO\request_output()->writeAllAsync(
      "Hello World!\n",
    ); // prints 'Hello World'!
  }

  /* ==================================
   *             FUNCTIONS
   * ==================================
   */

  // Functions are defined globally.
  // When a function is defined in a class, we refer to the function as a method.

  // Functions have return types (here: `int`) and must return a value of
  // that type or return no value when a void return type annotation was used.

  function add_one(int $x): int {
    return $x + 1;
  }

  // Functions can also have defined, default values.
  function add_value(int $x, int $y = 1): int {
    return $x + $y;
  }

  // Functions can be variadic (unspecified length of arguments).
  function sum_ints(int $val, int ...$vals): int {
    $result = $val;

    foreach ($vals as $v) {
      $result += $v;
    }
    return $result;
  }

  // Functions can also be anonymous (defined with the `==>` arrow).
  // $f = (int $x): int ==> $x + 1;

  /* ==================================
   *           PIPE OPERATOR
   * ==================================
   */

  // The pipe operator, `|>`, evaluates the result of a left-hand expression
  // and stores the result in `$$`, the predefined pipe variable.

  use namespace HH\Lib\Vec;

  function demo_pipe_operator(): void {

    Vec\sort(Vec\map(vec[2, 1, 3], $a ==> $a * $a)); // vec[1,4,9]

    // the same result, but using the pipe operator and pipe variable:
    $x = vec[2, 1, 3]
      |> Vec\map($$, $a ==> $a * $a) // $$ with value vec[2,1,3]
      |> Vec\sort($$); // $$ with value vec[4,1,9]
  }

  /* ==================================
   *             ATTRIBUTES
   * ==================================
   */

  // Hack provides built-in attributes that can change runtime or static type checking behavior.
  // For example, we used the `__EntryPoint` attribute earlier in the "Hello World!" example.

  // As another example, `__Memoize` caches the result of a function.
  <<__Memoize>>
  async function do_expensive_task(): Awaitable<string> {
    $site_contents = await \HH\Asio\curl_exec("http://hacklang.org");
    return $site_contents;
  }

  /* ==================================
   *             CONTEXTS
   * ==================================
   */

  // Hack functions are attached to different contexts and capabilities.
  // A context is a grouping of capabilities; that is, a grouping of permissions.

  // To declare allowed contexts (and capabilities), use the Context List `[]`.
  // If contexts are not defined, your function includes permissions defined in Hack's `defaults` context.

  // Because the context list is NOT defined, the `defaults` context is implicitly declared.
  async function implicit_defaults_context(): Awaitable<void> {
    await IO\request_output()->writeAllAsync(
      "Hello World!\n",
    ); // prints 'Hello World'!
  }

  // In the function below, the context list is defined to have the `defaults` context.
  // A function can have multiple contexts [context1, context2, ...].
  // `defaults` includes most of the capabilities defined by the Hack language.
  async function explicit_defaults_context()[defaults]: Awaitable<void> {
    await IO\request_output()->writeAllAsync("Hello World!\n");
  }

  // You can also specify zero contexts to create a pure function (no capabilities).
  async function empty_context()[]: Awaitable<void> {
    // The following line is an error, as the function does not have IO capabilities.
    // await IO\request_output()->writeAllAsync("Hello World!\n");
  }

  /* ==================================
   *             GENERICS
   * ==================================
   */

  // Generics allow classes or methods to be parameterized to any set of types.
  // That's pretty cool!

  // Hack typically passes by value: use `inout` to pass by reference.
  function swap<T>(inout T $input1, inout T $input2): void {
    $temp = $input1;
    $input1 = $input2;
    $input2 = $temp;
  }

  /* ==================================
   *             CLASSES
   * ==================================
   */

  // Classes provide a way to group functionality and state together.
  // To define a class, use the `class` keyword. To instantiate, use `new`.
  // Like other languages, you can use `$this` to refer to the current instance.

  class Counter {
    private int $i = 0;

    public function increment(): void {
      $this->i += 1;
    }

    public function get(): int {
      return $this->i;
    }
  }

  // Properties and Methods can be static (not requiring instantiation).
  class Person {
    public static function favoriteProgrammingLanguage(): string {
      return "Hack";
    }
  }

  function demo_hack_classes(): void {
    // Use `new` to instantiate a class.
    $c1 = new Counter();

    // To call a static property or method, use `::`
    $typical_person = tuple("Andrew", Person::favoriteProgrammingLanguage());
  }

  // Abstract class can be defined, but not instantiated directly.
  abstract class Machine {
    public function openDoors(): void {
      return;
    }
    public function closeDoors(): void {
      return;
    }
  }

  /* ==================================
   *             INTERFACES
   * ==================================
   */

  // A class can implement a set of requirements via an interface.
  // An interface is a set of method declarations and constants.

  interface Plane {
    // A constant is a named value. Once defined, the value cannot be changed.
    const MAX_SPEED = 300;
    public function fly(): void;
  }

  /* ==================================
   *             TRAITS
   * ==================================
   */

  // A trait defines properties and method declarations.
  // Traits are recommended when abstracting code for reuse.
  // Traits are included in code via the `use` keyword.

  trait Airplane {
    // Introduce a class or interface requirement with the following syntax:
    require extends Machine; // abstract class
    require implements Plane; // interface

    public function takeOff(): void {
      $this->openDoors();
      $this->closeDoors();
      $this->fly();
    }
  }

  class Spaceship extends Machine implements Plane {
    use Airplane;

    public function fly(): void {
      // fly like the wind
    }
  }

  /* ==================================
   *             KEEP READING!
   * ==================================
   */

  /*  This is a simplified guide!
   *  There's much more to learn, including:
   * - Asynchronous Operations: https://docs.hhvm.com/hack/asynchronous-operations/introduction
   * - Reified Generics: https://docs.hhvm.com/hack/reified-generics/reified-generics
   * - XHP: https://docs.hhvm.com/hack/XHP/setup
   * - ... and more!
   */
}
```

## More Information

Visit the [Hack language reference](http://docs.hhvm.com/hack/) to learn more about the Hack language.

For more information on HHVM, including installation instructions, visit the [official HHVM website](http://hhvm.com/).
