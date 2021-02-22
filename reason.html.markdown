---
language: reason
filename: reason.re
contributors:
  - ["Seth Corker", "https://sethcorker.com"]
---

Reason is a syntax over OCaml that is easier to get started for programmers who are familiar with C-style syntax like JavaScript. BuckleScript is part of the toolchain which compiles Reason to JavaScript so you can write statically typed code for anywhere that JavaScript runs.

```javascript
/* Comments start with slash-star, and end with star-slash */

/*----------------------------------------------
 * Variable and function declaration
 *----------------------------------------------
 * Variables and functions use the let keyword and end with a semi-colon
 * `let` bindings are immutable
 */

let x = 5;
/* - Notice we didn't add a type, Reason will infer x is an int */

/* A function like this, take two arguments and add them together */
let add = (a, b) => a + b;
/* - This doesn't need a type annotation either! */

/*----------------------------------------------
 * Type annotation
 *----------------------------------------------
 * Types don't need to be explicitly annotated in most cases but when you need
 * to, you can add the type after the name
 */

/* A type can be explicitly written like so */
let x: int = 5;

/* The add function from before could be explicitly annotated too */
let add2 = (a: int, b: int): int => a + b;

/* A type can be aliased using the type keyword */
type companyId = int;
let myId: companyId = 101;

/* Mutation is not encouraged in Reason but it's there if you need it
   If you need to mutate a let binding, the value must be wrapped in a `ref()`*/
let myMutableNumber = ref(120);

/* To access the value (and not the ref container), use `^` */
let copyOfMyMutableNumber = myMutableNumber^;

/* To assign a new value, use the `:=` operator */
myMutableNumber := 240;

/*----------------------------------------------
 * Basic types and operators
 *----------------------------------------------
 */

/* > String */

/* Use double quotes for strings */
let greeting = "Hello world!";

/* A string can span multiple lines */
let aLongerGreeting = "Look at me,
I'm a multi-line string
";

/* A quoted string can be used for string interpolation and special chars
   Use the `js` annotation for unicode */
let world = {js|🌍|js};

/* The `j` annotation is used for string interpolation */
let helloWorld = {j|hello, $world|j};

/* Concatenate strings with ++ */
let name = "John " ++ "Wayne";
let emailSubject = "Hi " ++ name ++ ", you're a valued customer";

/* > Char */

/* Use a single character for the char type */
let lastLetter = 'z';
/* - Char doesn't support Unicode or UTF-8 */

/* > Boolean */

/* A boolean can be either true or false */
let isLearning = true;

true && false;  /* - : bool = false;  Logical and */
true || true;   /* - : bool = true;   Logical or  */
!true;          /* - : bool = false;  Logical not */

/* Greater than `>`, or greater than or equal to `>=` */
'a' > 'b'; /* - bool : false */

/* Less than `<`, or less than or equal to `<=` */
1 < 5; /* - : bool = true */

/* Structural equal */
"hello" == "hello"; /* - : bool = true */

/* Referential equal */
"hello" === "hello"; /* - : bool = false */
/* - This is false because they are two different "hello" string literals */

/* Structural unequal */
lastLetter != 'a'; /* -: bool = true */

/* Referential unequal */
lastLetter !== lastLetter; /* - : bool = false */

/* > Integer */
/* Perform math operations on integers */

1 + 1;          /* - : int = 2  */
25 - 11;        /* - : int = 11 */
5 * 2 * 3;      /* - : int = 30 */
8 / 2;          /* - : int = 4  */

/* > Float */
/* Operators on floats have a dot after them */

1.1 +. 1.5;     /* - : float = 2.6  */
18.0 -. 24.5;   /* - : float = -6.5 */
2.5 *. 2.0;     /* - : float = 5.   */
16.0 /. 4.0;    /* - : float = 4.   */

/* > Tuple
 * Tuples have the following attributes
  - immutable
  - ordered
  - fix-sized at creation time
  - heterogeneous (can contain different types of values)
 A tuple is 2 or more values */

let teamMember = ("John", 25);

/* Type annotation matches the values */
let position2d: (float, float) = (9.0, 12.0);

/* Pattern matching is a great tool to retrieve just the values you care about
   If we only want the y value, let's use `_` to ignore the value */
let (_, y) = position2d;
y +. 1.0; /* - : float = 13. */

/* > Record */

/* A record has to have an explicit type */
type trainJourney = {
  destination: string,
  capacity: int,
  averageSpeed: float,
};

/* Once the type is declared, Reason can infer it whenever it comes up */
let firstTrip = {destination: "London", capacity: 45, averageSpeed: 120.0};

/* Access a property using dot notation */
let maxPassengers = firstTrip.capacity;

/* If you define the record type in a different file, you have to reference the
   filename, if trainJourney was in a file called Trips.re */
let secondTrip: Trips.trainJourney = {
  destination: "Paris",
  capacity: 50,
  averageSpeed: 150.0,
};

/* Records are immutable by default */
/* But the contents of a record can be copied using the spread operator */
let newTrip = {...secondTrip, averageSpeed: 120.0};

/* A record property can be mutated explicitly with the `mutable` keyword */
type breakfastCereal = {
  name: string,
  mutable amount: int,
};

let tastyMuesli = {name: "Tasty Muesli TM", amount: 500};

tastyMuesli.amount = 200;
/* - tastyMuesli now has an amount of 200 */

/* Punning is used to avoid redundant typing */
let name = "Just As Good Muesli";
let justAsGoodMuesli = {name, amount: 500};
/* - justAsGoodMuesli.name is now "Just As Good Muesli", it's equivalent
   to { name: name, amount: 500 } */

/* > Variant
   Mutually exclusive states can be expressed with variants */

type authType =
  | GitHub
  | Facebook
  | Google
  | Password;
/* - The constructors must be capitalized like so */
/* - Like records, variants should be named if declared in a different file */

let userPreferredAuth = GitHub;

/* Variants work great with a switch statement */
let loginMessage =
  switch (userPreferredAuth) {
  | GitHub => "Login with GitHub credentials."
  | Facebook => "Login with your Facebook account."
  | Google => "Login with your Google account"
  | Password => "Login with email and password."
  };

/* > Option
   An option can be None or Some('a) where 'a is the type */

let userId = Some(23);

/* A switch handles the two cases */
let alertMessage =
  switch (userId) {
  | Some(id) => "Welcome, your ID is" ++ string_of_int(id)
  | None => "You don't have an account!"
  };
/* - Missing a case, `None` or `Some`, would cause an error */

/* > List
  * Lists have the following attributes
   - immutable
   - ordered
   - fast at prepending items
   - fast at splitting

  * Lists in Reason are linked lists
 */

/* A list is declared with square brackets */
let userIds = [1, 4, 8];

/* The type can be explicitly set with list('a) where 'a is the type */
type idList = list(int);
type attendanceList = list(string);

/* Lists are immutable */
/* But the contents of a list can be copied using the spread operator */
let newUserIds = [101, 102, ...userIds];

/* > Array
 * Arrays have the following attributes
  - mutable
  - fast at random access & updates */

/* An array is declared with `[|` and ends with `|]` */
let languages = [|"Reason", "JavaScript", "OCaml"|];

/*----------------------------------------------
 * Function
 *----------------------------------------------
 */

/* Reason functions use the arrow syntax, the expression is returned */
let signUpToNewsletter = email => "Thanks for signing up " ++ email;

/* Call a function like this */
signUpToNewsletter("hello@reason.org");

/* For longer functions, use a block */
let getEmailPrefs = email => {
  let message = "Update settings for " ++ email;
  let prefs = ["Weekly News", "Daily Notifications"];

  (message, prefs);
};
/* - the final tuple is implicitly returned */

/* > Labeled Arguments */

/* Arguments can be labeled with the ~ symbol */
let moveTo = (~x, ~y) => {/* Move to x,y */};

moveTo(~x=7.0, ~y=3.5);

/* Labeled arguments can also have a name used within the function */
let getMessage = (~message as msg) => "==" ++ msg ++ "==";

getMessage(~message="You have a message!");
/* - The caller specifies ~message but internally the function can make use */

/* The following function also has explicit types declared */
let showDialog = (~message: string): unit => {
  () /* Show the dialog */;
};
/* - The return type is `unit`, this is a special type that is equivalent to
   specifying that this function doesn't return a value
   the `unit` type can also be represented as `()` */

/* > Currying
   Functions can be curried and are partially called, allowing for easy reuse */

let div = (denom, numr) => numr / denom;
let divBySix = div(6);
let divByTwo = div(2);

div(3, 24);     /* - : int = 8  */
divBySix(128);  /* - : int = 21 */
divByTwo(10);   /* - : int = 5  */

/* > Optional Labeled Arguments */

/* Use `=?` syntax for optional labeled arguments */
let greetPerson = (~name, ~greeting=?, ()) => {
  switch (greeting) {
  | Some(greet) => greet ++ " " ++ name
  | None => "Hi " ++ name
  };
};
/* - The third argument, `unit` or `()` is required because if we omitted it,
   the function would be curried so greetPerson(~name="Kate") would create
   a partial function, to fix this we add `unit` when we declare and call it */

/* Call greetPerson without the optional labeled argument */
greetPerson(~name="Kate", ());

/* Call greetPerson with all arguments */
greetPerson(~name="Marco", ~greeting="How are you today,");

/* > Pipe */
/* Functions can be called with the pipeline operator */

/* Use `->` to pass in the first argument (pipe-first) */
3->div(24);     /* - : int = 8 */
/* - This is equivalent to div(3, 24); */

36->divBySix;   /* - : int = 6 */
/* - This is equivalent to divBySix(36); */

/* Use `|>` to pass in the last argument (pipe-last) */
24 |> div(3);   /* - : int = 8 */
/* - This is equivalent to div(3, 24); */

36 |> divBySix; /* - : int = 6 */
/* - This is equivalent to divBySix(36); */

/* Pipes make it easier to chain code together */
let addOne = a => a + 1;
let divByTwo = a => a / 2;
let multByThree = a => a * 3;

let pipedValue = 3->addOne->divByTwo->multByThree; /* - : int = 6 */

/*----------------------------------------------
 * Control Flow & Pattern Matching
 *----------------------------------------------
 */

/* > If-else */
/* In Reason, `If` is an expression when evaluate will return the result */

/* greeting will be "Good morning!" */
let greeting = if (true) {"Good morning!"} else {"Hello!"};

/* Without an else branch the expression will return `unit` or `()` */
if (false) {
  showDialog(~message="Are you sure you want to leave?");
};
/* - Because the result will be of type `unit`, both return types should be of
   the same type if you want to assign the result. */

/* > Destructuring */
/* Extract properties from data structures easily */

let aTuple = ("Teacher", 101);

/* We can extract the values of a tuple */
let (name, classNum) = aTuple;

/* The properties of a record can be extracted too */
type person = {
  firstName: string,
  age: int,
};
let bjorn = {firstName: "Bjorn", age: 28};

/* The variable names have to match with the record property names */
let {firstName, age} = bjorn;

/* But we can rename them like so */
let {firstName: bName, age: bAge} = bjorn;

let {firstName: cName, age: _} = bjorn;

/* > Switch
   Pattern matching with switches is an important tool in Reason
   It can be used in combination with destructuring for an expressive and
   concise tool */

/* Lets take a simple list */
let firstNames = ["James", "Jean", "Geoff"];

/* We can pattern match on the names for each case we want to handle */
switch (firstNames) {
| [] => "No names"
| [first] => "Only " ++ first
| [first, second] => "A couple of names " ++ first ++ "," ++ second
| [first, second, third] =>
  "Three names, " ++ first ++ ", " ++ second ++ ", " ++ third
| _ => "Lots of names"
};
/* - The `_` is a catch all at the end, it signifies that we don't care what
   the value is so it will match every other case */

/* > When clause */

let isJohn = a => a == "John";
let maybeName = Some("John");

/* When can add more complex logic to a simple switch */
let aGreeting =
  switch (maybeName) {
  | Some(name) when isJohn(name) => "Hi John! How's it going?"
  | Some(name) => "Hi " ++ name ++ ", welcome."
  | None => "No one to greet."
  };

/* > Exception */

/* Define a custom exception */
exception Under_Age;

/* Raise an exception within a function */
let driveToTown = (driver: person) =>
  if (driver.age >= 15) {
    "We're in town";
  } else {
    raise(Under_Age);
  };

let evan = {firstName: "Evan", age: 14};

/* Pattern match on the exception Under_Age */
switch (driveToTown(evan)) {
| status => print_endline(status)
| exception Under_Age =>
  print_endline(evan.firstName ++ " is too young to drive!")
};

/* Alternatively, a try block can be used */
/* - With Reason exceptions can be avoided with optionals and are seldom used */
let messageToEvan =
  try (driveToTown(evan)) {
  | Under_Age => evan.firstName ++ " is too young to drive!"
  };

/*----------------------------------------------
 * Object
 *----------------------------------------------
 * Objects are similar to Record types but aren't as rigid
 * An object resembles a class
 */

/* An object may be typed like a record but contains a dot */
type surfaceComputer = {
  .
  color: string,
  capacity: int,
};
/* - A single dot signifies a closed object, an object that uses this type
   must have the exact shape */

let surfaceBook: surfaceComputer = {pub color = "blue"; pub capacity = 512};

/* But an object doesn't require a type */
let house = {
  /* A private property */
  val temp = ref(18.0);
  /* Public properties */
  pub temperature = temp;
  /* A private method only accessible from within house */
  pri setThermostat = v => temp := v;
  /* A public method that calls the private setThermostat method */
  pub arriveHome = () => this#setThermostat(22.0)
};

house#temperature; /* - : float = 18. */
house#arriveHome();
house#temperature; /* - : float = 22. */

/*----------------------------------------------
 * Module
 *----------------------------------------------
 * Modules are used to organize your code and provide namespacing.
 * Each file is a module by default
 */

/* Create a module */
module Staff = {
  type role =
    | Delivery
    | Sales
    | Other;
  type member = {
    name: string,
    role,
  };

  let getRoleDirectionMessage = staff =>
    switch (staff.role) {
    | Delivery => "Deliver it like you mean it!"
    | Sales => "Sell it like only you can!"
    | Other => "You're an important part of the team!"
    };
};

/* A module can be accessed with dot notation */
let newEmployee: Staff.member = {name: "Laura", role: Staff.Delivery};

/* Using the module name can be tiresome so the module's contents can be opened
   into the current scope with `open` */
open Staff;

let otherNewEmployee: member = {name: "Fred", role: Other};

/* A module can be extended using the `include` keyword, include copies
   the contents of the module into the scope of the new module */
module SpecializedStaff = {
  include Staff;

  /* `member` is included so there's no need to reference it explicitly */
  let ceo: member = {name: "Reggie", role: Other};

  let getMeetingTime = staff =>
    switch (staff) {
    | Other => 11_15 /* - : int = 1115; Underscores are for formatting only  */
    | _ => 9_30
    };
};
```

## Further Reading

- [Official Reason Docs](https://reasonml.github.io/docs/en/what-and-why)
- [Official BuckleScript Docs](https://bucklescript.github.io/docs/en/what-why)
- [Try Reason](https://reasonml.github.io/en/try)
- [Get Started with Reason by Nik Graf](https://egghead.io/courses/get-started-with-reason)
