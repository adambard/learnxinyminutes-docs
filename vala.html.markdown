---
language: vala
contributors:
    - ["Milo Gilad", "https://github.com/Myl0g"]
filename: LearnVala.vala
---

In GNOME's own words, "Vala is a programming language that aims to bring modern programming language features to GNOME developers without imposing any additional runtime requirements and without using a different ABI compared to applications and libraries written in C."

Vala has aspects of Java and C#, so it'll be familiar to those who know either or.

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


struct Closet {
  public uint shirts;
  public uint jackets;
}

enum HouseSize {
  SMALL,
  MODERATE,
  BIG
}

```
