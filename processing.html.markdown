---
language: "Processing"
filename: learnprocessing.pde
contributors:
    - ["Phone Thant Ko", "http://github.com/phonethantko"]
---
## Introduction

Processing is a programming language for creation of digital arts and multimedia content, allowing non-programmers to
learn fundamentals of computer programming in a visual context.  
While the language is based on Java language,
its syntax has been largely influenced by both Java and Javascript syntaxes. [See more here](https://processing.org/reference/)  
The language is statically typed, and also comes with its official IDE to compile and run the scripts.

```processing
/* ---------
   Comments
   ---------
*/

// Single-line comment starts with //

/*
   Since Processing is based on Java,
   the syntax for its comments are the same as Java (as you may have noticed above)!
   Multi-line comments are wrapped as seen here.
*/

/* ---------------------------------------
   Writing and Running Processing Programs
   ---------------------------------------
 */

// In Processing, your program's entry point is a function named setup() with a void return type.
// Note! The syntax looks strikingly similar to that of C++.
void setup() {
  // This prints out the classic output "Hello World!" to the console when run.
  println("Hello World!"); // Another language with a semi-column trap, ain't it?
}

// Normally, we put all the static codes inside the setup() method as the name suggest since it only runs once.
// It can range from setting the background colours, setting the canvas size.
// You will see more of them throughout this document.

// If you want to run the codes indefinitely, it has to be placed in draw() method.
// draw() must exist if you want the code to run continuously and obviously, there can only be one draw() method.
int i = 0;
void draw() {
  // This block of code loops forever until stopped
  print(i);
  i++; // Increment Operator!
}

// Now that we know how to write the working script and how to run it,
// we will proceed to explore what data types and collections are supported in Processing.

/* -----------------------
  Datatypes & collections
  ------------------------
*/

// According to Processing References, Processing supports 8 primitive datatypes as follows.

boolean booleanValue = true; // Boolean
byte byteValueOfA = 23; // Byte
char charValueOfA = 'A'; // Char
color colourValueOfWhiteM = color(255, 255, 255); // Colour (Specified using color() method)
color colourValueOfWhiteH = #FFFFFF; // Colour (Specified using hash value)
int intValue = 5; // Integer (Number without decimals)
long longValue = 2147483648L; // "L" is added to the number to mark it as a long
float floatValue = 1.12345; // Float (32-bit floating-point numbers)
double doubleValue = 1.12345D; // Double (64-bit floating-point numbers)

// NOTE!
// Although datatypes "long" and "double" work in the language,
// processing functions do not use these datatypes, therefore
// they need to be converted into "int" and "float" datatypes respectively,
// using (int) and (float) syntax before passing into a function.

// There is a whole bunch of default composite datatypes available for use in Processing.
// Primarily, I will brief through the most commonly used ones to save time.

// String
// While char datatype uses '', String datatype uses "" - double quotes.
String sampleString = "Hello, Processing!";
// String can be constructed from an array of char datatypes as well. We will discuss array very soon.
char source = {'H', 'E', 'L', 'L', 'O'};
String stringFromSource = new String(source); // HELLO
// As in Java, strings can be concatenated using the "+" operator.
print("Hello " + "World!"); // Hello World!

// Array
// Arrays in Processing can hold any datatypes including Objects themselves.
// Since arrays are similar to objects, they must be created with the keyword "new".
int[] intArray = new int[5];
int[] intArrayWithValues = {1, 2, 3}; // You can also populate with data.

// ArrayList
// Functions are similar to those of array; arraylists can hold any datatypes.
// The only difference is arraylists resize dynamically,
// as it is a form of resizable-array implementation of the Java "List" interface.
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// Object
// Since it is based on Java, Processing supports object-oriented programming.
// That means you can basically define any datatypes of your own and manipulate them to your needs.
// Of course, a class has to be defined before for the object you want.
// Format --> ClassName InstanceName
SomeRandomClass myObject // then instantiate later
//or
SomeRandomClass myObjectInstantiated = new SomeRandomClass(); // Assuming we have nothing to pass into the constructor

// Processing comes up with more collections (eg. - Dictionaries and Lists) by default,
// for the simplicity sake, I will leave them out of discussion here.

/* -----------
  Maths
  ------------
*/
// Arithmetic
1 + 1 // 2
2 - 1 // 0
2 * 3 // 6
3 / 2 // 1
3.0 / 2 // 1.5
3.0 % 2 // 1.0

// Processing also comes with a set of functions that simplify mathematical operations.
float f = sq(3); // f = 9.0
float p = pow(3, 3); // p = 27.0
int a = abs(-13) // a = 13
int r1 = round(3.1); // r1 = 3
int r2 = round(3.7); // r2 = 4
float sr = sqrt(25); // sr = 5.0

// Vectors
// Processing provides an easy way to implement vectors in its environment using PVector class.
// It can describe a two or three dimensional vector and comes with a set of methods which are useful for matrices operations.
// You can find more information on PVector class and its functions here. (https://processing.org/reference/PVector.html)

// Trigonometry
// Processing also supports trigonometric operations by supplying a set of functions.
// sin(), cos(), tan(), asin(), acos(), atan() and also degrees() and radians() for convenient conversion.
// However, a thing to note is those functions take angle in radians as the parameter so it has to be converted beforehand.
float one = sin(PI/2); // one = 1.0
// As you may have noticed, there exists a set of constants for trigonometric uses; PI, HALF_PI, QUARTER_PI and so on...

```
Processing is easy to learn and is particularly useful to create multimedia contents (even in 3D) without
having to type a lot of codes. It is so simple that you can read through the code and get a rough idea of
the program flow.  
However, that does not apply when you introduce external libraries, packages and even your own classes.
(Trust me! Processing projects can get really large)  

## What's Next?

Here, I have compiled some useful resources:  

 - [Processing Website](http://processing.org)
 - [Processing Sketches](http://openprocessing.org)
