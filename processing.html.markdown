---
language: "Processing"
filename: learnprocessing.pde
contributors:
    - ["Phone Thant Ko", "http://github.com/phonethantko"]
---
## Introduction

Processing is a programming language for creation of digital arts and multimedia content, allowing non-programmers to
learn fundamentals of computer programming in a visual context.  
While the language is based off on Java language,
its syntax has been largely influenced by both Java and Javascript syntaxes. [See more here](https://processing.org/reference/)  
The language also comes with its official IDE to compile and run the scripts.

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

// Normally, we put all the static codes inside the setup() method as the name suggests.
// It can range from setting the background colours, setting the canvas size.
// You will see more of them throughout this document.

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
