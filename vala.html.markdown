---
language: vala
contributors:
    - ["Milo Gilad", "https://github.com/Myl0g"]
filename: LearnVala.vala
---

In GNOME's own words, "Vala is a programming language that aims to bring modern programming language features to GNOME developers without imposing any additional runtime requirements and without using a different ABI compared to applications and libraries written in C."

Vala has aspects of Java and C#, so it'll be natural to those who know either or.

[Read more here.](https://wiki.gnome.org/Projects/Vala)

```vala

// Single line comment

/* Multiline
Comment */

/**
* Documentation comment
*/

/* Data Types */

// Vala supports the data types supported by most other programming languages.

char character = 'a'
unichar unicode_character = 'u' // 32-bit unicode character

int i = 2; // ints can also have guaranteed sizes (e.g. int64, uint64)
uint j = -6; // Won't compile; unsigned ints can only be positive

long k;

short l;
ushort m;

string text = "Hello,"; // Note that the == operator will check string content

string verbatim = """This is a verbatim (a.k.a. raw) string. Special characters
(e.g. \n) are not interpreted. They may also be multiple lines long.""";

// String Templates allow for easy string formatting
string string_template = @"$text world"; // "$text" evaluates to "Hello"

int test = 5;
int test2 = 10;
string template2 = @"$(test * test2) is a number."; // Expression evaluation

string template_slice = string_template[7:12]; // => "world"

// Most data types have methods for parsing.

bool parse_bool = bool.parse("false"); // => false
int parse_int = int.parse("-52");
string parse_string = parse_int.to_string();

/* Basic I/O */

stdout.printf(parse_string); // Prints to console
string input = stdin.read_line(); // Gets input from console

stderr.printf("Error message"); // Error printing

/* Arrays */

int[] int_array = new int[10]; // Array of ints with 10 slots
int better_int_array[10]; // Shorter way of making int array with 10 slots
int_array.length; // => 10;

int[] int_array2 = {5, 10, 15, 20}; // Can be created on-the-fly

int[] array_slice = int_array2[1:3]; // Slice (copy of data)
unowned int[] array_slice_ref = int_array2[1:3]; // Reference to data

// Multi-dimensional Arrays (defined with a number of commas in the brackets)

int[,] multi_array = new int[6,4]; // 6 is the number of arrays, 4 is their size
int[,] multi_array2 = {{7, 4, 6, 4},
                       {3, 2, 4, 6},
                       {5, 9, 5, 1}};
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
print ((string) chars); // Casts the array to a string and prints "message"

/* Control Flow */

var a = 1;
var b = 2;
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
  break;
} else if (a > 1) {
  stdout.printf("%d\n", a);
} else {
  break;
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
// Dynamically casted objects must meet the following:
// - Object's class is the same class as the desired type
// - Object's class is a subclass of the desired type
// - Desired class is an interface implemented by the object's class

float dyna_casted_float = cast_to_float as float // Won't compile

var inferred_string = "hello"; // Type inference

/* Methods (a.k.a. functions) */

// Vala methods are C functions, and are bound by the same rules.

int method_demo(string arg1, Object arg2) { // Returns int and takes args
    return 1;
}

// Vala methods cannot be overloaded.

void some_method(string text) { }
void some_method(int number) { }  // Won't compile

// To achieve similar functionality, use default argument values.

void some_better_method(string text, int number = 0) { }

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

void delegate_match(char char_a) { // Matches the delegate's signature
  stdout.printf("%d\n");
}

void call_delegate(DelegateDemo d, char char_b) { // Takes a delegate arg
  d(char_b) // calls delegate
}

void final_delegate_demo() {
  call_delegate(delegate_match); // Passes matching method as argument
}

// Lambdas/Anonymous Methods are defined with "=>"

(a) => { stdout.printf("%d\n", a); } // Prints "a"

/* Namespaces */

namespace NamespaceDemo {
  // Allows you to organize variable names
  int namespace_int = 12;
}
namespace_int += 5; // Won't compile

using NamespaceDemo;
namespace_int += 5; // Valid

/* Structs */

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

/* Classes and Object-Oriented Programming */

class Message : GLib.Object { // Class Message extends GLib's Object
  private string sender; // a private field
  public string text {get; set;} // a public property
  internal bool sent = false; // internal (classes in same package) field

  public void send(string message_sender) { // public method
    sender = message_sender;
    sent = true;
  }

  public Message() { // Constructor
    // ...
  }

}

interface InterfaceDemo { // Can be used as a mixin
  // ...
}

// Since method overloading isn't possible, you can use named constructors
// to get the same functionality.

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

enum HouseSize {
  SMALL,
  MODERATE,
  BIG
}

```

* Read about building GUIs with GTK+ and Vala [here](http://archive.is/7C7bw).
