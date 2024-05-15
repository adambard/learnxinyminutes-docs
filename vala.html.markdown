---
language: vala
contributors:
    - ["Milo Gilad", "https://github.com/Myl0g"]
filename: LearnVala.vala
---

In GNOME's own words, "Vala is a programming language that aims to bring modern programming language features to GNOME developers without imposing any additional runtime requirements and without using a different ABI compared to applications and libraries written in C."

Vala has aspects of Java and C#, so it'll be natural to those who know either.

[Read more here.](https://wiki.gnome.org/Projects/Vala)

```vala
// Single line comment

/* Multiline
Comment */

/**
* Documentation comment
*/

/* Data Types */

char character = 'a'
unichar unicode_character = 'u' // 32-bit unicode character

int i = 2; // ints can also have guaranteed sizes (e.g. int64, uint64)
uint j = -6; // Won't compile; unsigned ints can only be positive

long k;

short l;
ushort m;

string text = "Hello,"; // Note that the == operator will check string content

string verbatim = """This is a verbatim (a.k.a. raw) string. Special characters
(e.g. \n and "") are not interpreted. They may also be multiple lines long.""";

// String Templates allow for easy string formatting
string string_template = @"$text world"; // "$text" evaluates to "Hello,"

int test = 5;
int test2 = 10;
string template2 = @"$(test * test2) is a number."; // Expression evaluation

string template_slice = string_template[7:12]; // => "world"

// Most data types have methods for parsing.

bool parse_bool = bool.parse("false"); // => false
int parse_int = int.parse("-52"); // => -52
string parse_string = parse_int.to_string(); // => "-52"

/* Basic I/O */

stdout.printf(parse_string); // Prints to console
string input = stdin.read_line(); // Gets input from console

stderr.printf("Error message"); // Error printing

/* Arrays */

int[] int_array = new int[10]; // Array of ints with 10 slots
int better_int_array[10]; // Above expression, shortened
int_array.length; // => 10;

int[] int_array2 = {5, 10, 15, 20}; // Can be created on-the-fly

int[] array_slice = int_array2[1:3]; // Slice (copy of data)
unowned int[] array_slice_ref = int_array2[1:3]; // Reference to data

// Multi-dimensional Arrays (defined with a number of commas in the brackets)

int[,] multi_array = new int[6,4]; // 6 is the number of arrays, 4 is their size
int[,] multi_array2 = {{7, 4, 6, 4},
                       {3, 2, 4, 6},
                       {5, 9, 5, 1}}; // new int[3,4]
multi_array2[2,3] = 12; // 2 is the array, 3 is the index in the array
int first_d = multi_array2.length[0] // => 3
int second_d = multi_array2.length[1] // => 4

// Stacked arrays (e.g. int[][]) where array lengths vary are not supported.

// Multi-dimensional arrays cannot be sliced, nor can they be converted to one-
// dimensional.

int[] add_to_array = {};
add_to_array += 12; // Arrays can be dynamically added to

add_to_array.resize(20); // Array now has 20 slots

uint8[] chars = "test message".data;
chars.move(5, 0, 7);
stdout.printf((string) chars); // Casts the array to a string and prints it

/* Control Flow */

int a = 1;
int b = 2;
int[] foreach_demo = {2, 4, 6, 8};

while (b > a) { // While loop; checks if expression is true before executing
  b--;
}

do {
  b--;
}
while (b > a); // Do While loop; executes the code in "do" before while (b > a)

for (a = 0; a < 10; a++) { stdout.printf("%d\n", a); } // for loop

foreach (int foreach_demo_var in foreach_demo) {
  stdout.printf("%d\n", foreach_demo_var);
} // foreach works on any iterable collection

if (a == 0) {
  stdout.printf("%d\n", a);
} else if (a > 1) {
  stdout.printf("%d\n", a);
} else {
  stdout.printf("A is less than 0");
} // if-then-else

switch (a) {
  case 1:
    stdout.printf("A is 1\n");
    break;
  case 5:
  case 10:
    stdout.printf("A is 5 or 10\n");
    break;
  default:
    stdout.printf("???\n")
    break;
} // switch statement

/* Type Casting and Inference */

int cast_to_float = 10;
float casted_float = (float) cast_to_float; // static casting; no runtime checks

// For runtime checks, use dynamic casting.
// Dynamically casted objects must be the following:
// - Object's class is the same class as the desired type
// - Object's class is a subclass of the desired type
// - Desired class is an interface implemented by the object's class

float dyna_casted_float = cast_to_float as float // Won't compile

var inferred_string = "hello"; // Type inference

/* Methods (a.k.a. functions) */

int method_demo(string arg1, Object arg2) { // Returns int and takes args
    return 1;
}

// Vala methods cannot be overloaded.

void some_method(string text) { }
void some_method(int number) { }  // Won't compile

// To achieve similar functionality, use default argument values.

void some_better_method(string text, int number = 0) { }

some_better_method("text");
some_better_method("text", 12);

// varargs (variable-length argument lists) are also supported.

void method_with_varargs(int arg1, ...) {
    var varargs_list = va_list(); // gets the varargs list

    string arg_string = varargs_list.arg(); // gets arguments, one after another
    int int_vararg = varargs_list.arg();

    stdout.printf("%s, %d\n", arg_string, int_vararg)
}

string? ok_to_be_null(int? test_int) { } // "?" denotes possible null value

// Delegates

delegate void DelegateDemo(char char_a);

void delegate_match(char char_a) { // Matches DelegateDemo's signature
  stdout.printf("%d\n");
}

void call_delegate(DelegateDemo d, char char_b) { // Takes a delegate arg
  d(char_b) // calls delegate
}

void final_delegate_demo() {
  call_delegate(delegate_match); // Passes matching method as argument
}

// Lambdas (a.k.a. Anonymous Methods) are defined with "=>"

(a) => { stdout.printf("%d\n", a); } // Prints "a"

/* Namespaces */

namespace NamespaceDemo {
  // Allows you to organize variable names
  int namespace_int = 12;
}
namespace_int += 5; // Won't compile

using NamespaceDemo;
namespace_int += 5; // Valid

/* Structs and Enums */

struct Closet {
  public uint shirts; // Default access modifier is private
  public uint jackets;
}

Closet struct_init_1 = Closet(); // or Closet struct_init_1 = {};
Closet struct_init_2 = {15, 3};
var struct_init_3 = Closet() { // Type inference also works
  shirts = 15;
  jackets = 3;
}

enum HouseSize { // An example of an enum
  SMALL,
  MODERATE,
  BIG
}

/* Classes and Object-Oriented Programming */

class Message : GLib.Object { // Class Message extends GLib's Object
  private string sender; // a private field
  public string text {get; set;} // a public property (more on that later)
  protected bool is_digital = true; // protected (this class and subclasses)
  internal bool sent = false; // internal (classes in same package)

  public void send(string sender) { // public method
    this.sender = sender;
    sent = true;
  }

  public Message() { // Constructor
    // ...
  }

}

// Since method overloading isn't possible, you can't overload constructors.
// However, you can use named constructors to achieve the same functionality.

public class Calculator : GLib.Object {

    public Calculator() {
    }

    public Calculator.with_name(string name) {
    }

    public Calculator.model(string model_id, string name = "") {
      this.with_name(@"$model_id $name"); // Chained constructors with "this"
    }
    ~Calculator() { } // Only needed if you're using manual memory management
}

var calc1 = new Calculator.with_name("Temp");
var calc2 = new Calculator.model("TI-84");

// Signals (a.k.a. events or event listeners) are a way to execute multiple
// methods with the same signature at the same time.

public class SignalDemo : GLib.Object {
  public signal void sig_demo(int sig_demo_int); // Must be public

  public static int main(string[] args) {
    // main method; program does not compile without it

    var sig_demo_class = new SignalDemo(); // New instance of class

    sig_demo_class.sig_demo.connect((ob, sig_int) => { // Lambda used as handler
        stdout.printf("%d\n", sig_int); // "ob" is object on which it is emitted
      });

    sig_demo_class.sig_demo(27); // Signal is emitted

    return 0;
  }
}

// You may use the connect() method and attach as many handlers as you'd like.
// They'll all run at around the same time when the signal is emitted.

// Properties (getters and setters)

class Animal : GLib.Object {
  private int _legs; // prefixed with underscore to prevent name clashes

  public int legs {
    get { return _legs; }
    set { _legs = value; }
  }

  public int eyes { get; set; default = 5; } // Shorter way
  public int kingdom { get; private set; default = "Animalia"} // Read-only

  public static void main(string args[]) {
    rabbit = new Animal();

    // All GLib.Objects have a signal "notify" emitted when a property changes.

    // If you specify a specific property, replace all underscores with dashes
    // to conform to the GObject naming convention.

    rabbit.notify["eyes"].connect((s, p) => { // Remove the ["eyes"] for all
      stdout.printf("Property '%s' has changed!\n", p.name);
    });

    rabbit.legs = 2;
    rabbit.legs += 2;
    rabbit.eyes = 2;

  }
}

// Inheritance: Vala classes may inherit 1 class. Inheritance is not implicit.

class SuperDemo : GLib.Object {
  public int data1;
  protected int data2;
  internal int data3;
  private int data4;

  public static void test_method {  } // Statics can be called w/out an object
}
class SubDemo : SuperDemo {
  public static void main(string args[]) {
    stdout.printf((string) data1); // Will compile
    stdout.printf((string) data2); // Protected can be accessed by subclasses
    stdout.printf((string) data3); // Internal is accessible to package
    stdout.printf((string) data4); // Won't compile
  }
}

// Abstract Classes and Methods

public abstract class OperatingSystem : GLib.Object {
  public void turn_on() {
    stdout.printf("Booted successfully.\n");
  }
  public abstract void use_computer();
}

public class Linux : OperatingSystem {
  public override void use_computer() { // Abstract methods must be overridden
    stdout.printf("Beep boop\n");
  }
}

// Add default behavior to an abstract method by making it "virtual".

public abstract class HardDrive : GLib.Object {
  public virtual void die() {
    stdout.printf("CLICK-CLICK-CLICK\n");
  }
}
public class MyHD : HardDrive {
  public override void die() {
    return;
  }
}

// Interfaces: classes can implement any number of these.

interface Laptop { // May only contain abstracts or virtuals
  public abstract void turn_on();
  public abstract void turn_off();

  public abstract int cores; // Won't compile; fields cannot be abstract
  public abstract int cores {get; set;} // Will compile

  public virtual void keyboard() { // Virtuals are allowed (unlike Java/C#)
    stdout.printf("Clickity-clack\n");
  }
}

// The ability to use virtuals in Vala means that multiple inheritance is
// possible (albeit somewhat confined)

// Interfaces cannot implement interfaces, but they may specify that certain
// interfaces or classes must be also implemented (pre-requisites).

public interface CellPhone : Collection, GLib.Object {}

// You can get the type info of a class at runtime dynamically.

bool type_info = object is TypeName; // uses "is" to get a bool

Type type_info2 = object.get_type();
var type_name = type_info2.name();

Type type_info3 = typeof(Linux);
Linux type_demo = (Linux) Object.new(type_info3);

// Generics

class Computer<OperatingSystem> : GLib.Object {
  private OperatingSystem os;

  public void install_os(OperatingSystem os) {
    this.os = os;
  }
  public OperatingSystem retrieve_os() {
    return this.os;
  }
}

var new_computer = new Computer<Linux>();

/* Other Features */

// Assertions: crash if a statement is not true (at runtime)

bool is_true = true;
assert(is_true);

// Contract Programming

int contract_demo(int arg1, int arg2) {
  requires(arg1 > 0 && arg1 < 10) // Notice the lack of semicolon
  requires(arg2 >= 12)
  ensures(result >= 0)
}

// Error Handling

void error_demo(int int_ex) throws GError {
  if (int_ex != 1) {
    throw new GError("TEST MESSAGE");
  }
}
void error_demo2() {
  try {
    error_demo(0);
  } catch (GError ge) {
    stdout.printf("%s\n", ge.message);
  }
}

// Main Loop

void main() {

  var main_loop = new MainLoop();
  var time = new TimeoutSource(2000);

  time.set_callback(() => { // Executes the following lambda after 2000ms
      stdout.printf("2000ms have passed\n");
      main_loop.quit();
      return false;
  });

  time.attach(main_loop.get_context());

  loop.run();
}

// Pointers (manual memory management)

Object* pointer_obj = new Object(); // Creates Object instance and gives pointer

pointer_obj->some_method(); // Executes some_method
pointer_obj->some_data; // Returns some_data

delete pointer_obj;

int more = 57;
int* more_pointer = &more; // & = address-of
int indirection_demo = more_pointer*; // indirection

// Profiles: affect which Vala features are available and which libraries the
// C-code will use.
// - gobject (default)
// posix
// dova
// Use "--profile=whatever" when compiling.
```

* More [Vala documentation](https://valadoc.org/).
* [Alternate construction syntax](https://wiki.gnome.org/Projects/Vala/Tutorial#GObject-Style_Construction) similar to GObject
* More on [contract programming](http://en.wikipedia.org/wiki/Contract_programming)
* [Collections library](https://wiki.gnome.org/Projects/Vala/Tutorial#Collections)
* [Multithreading](https://wiki.gnome.org/Projects/Vala/Tutorial#Multi-Threading)
* Read about [building GUIs with GTK+ and Vala](http://archive.is/7C7bw).
* [D-Bus integration](https://wiki.gnome.org/Projects/Vala/Tutorial#D-Bus_Integration)
