---
language: processing
filename: learnprocessing.pde
contributors:
    - ["Phone Than Ko", "http://github.com/phonethantko"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
---

## Introduction

Processing is a programming language for creation of digital arts and
multimedia content, allowing non-programmers to learn fundamentals of computer
programming in a visual context.

While the language is based on Java language, its syntax has been largely
influenced by both Java and Javascript syntaxes. [See more here](https://processing.org/reference/)

The language is statically typed, and also comes with its official IDE to
compile and run the scripts.

```
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

// In Processing, the program entry point is a function named setup() with a
// void return type.
// Note! The syntax looks strikingly similar to that of C++.
void setup() {
  // This prints out the classic output "Hello World!" to the console when run.
  println("Hello World!"); // Another language with a semi-column trap, aint it?
}

// Normally, we put all the static codes inside the setup() method as the name
// suggest since it only runs once.
// It can range from setting the background colours, setting the canvas size.
background(color); // setting the background colour
size(width,height,[renderer]); // setting the canvas size with optional
// parameter defining renderer
// You will see more of them throughout this document.

// If you want to run the codes indefinitely, it has to be placed in draw()
// method.
// draw() must exist if you want the code to run continuously and obviously,
// there can only be one draw() method.
int i = 0;
void draw() {
  // This block of code loops forever until stopped
  print(i);
  i++; // Increment Operator!
}

// Now that we know how to write the working script and how to run it,
// we will proceed to explore what data types and collections are supported in
// Processing.

/* ------------------------
   Datatypes & collections
   ------------------------
*/

// According to Processing References, Processing supports 8 primitive
// datatypes as follows.

boolean booleanValue = true; // Boolean
byte byteValueOfA = 23; // Byte
char charValueOfA = 'A'; // Char
color colourValueOfWhiteM = color(255, 255, 255); // Colour (Specified using
// color() method)
color colourValueOfWhiteH = #FFFFFF; // Colour (Specified using hash value)
int intValue = 5; // Integer (Number without decimals)
long longValue = 2147483648L; // "L" is added to number to mark it as a long
float floatValue = 1.12345; // Float (32-bit floating-point numbers)
double doubleValue = 1.12345D; // Double (64-bit floating-point numbers)

// NOTE!
// Although datatypes "long" and "double" work in the language,
// processing functions do not use these datatypes, therefore
// they need to be converted into "int" and "float" datatypes respectively,
// using (int) and (float) syntax before passing into a function.

// There is a whole bunch of default composite datatypes available for use in
// Processing.
// Primarily, I will brief through the most commonly used ones to save time.

// String
// While char datatype uses '', String datatype uses "" - double quotes.
String sampleString = "Hello, Processing!";
// String can be constructed from an array of char datatypes as well. We will
// discuss array very soon.
char source = {'H', 'E', 'L', 'L', 'O'};
String stringFromSource = new String(source); // HELLO
// As in Java, strings can be concatenated using the "+" operator.
print("Hello " + "World!"); // Hello World!

// Array
// Arrays in Processing can hold any datatypes including Objects themselves.
// Since arrays are similar to objects, they must be created with the keyword
// "new".
int[] intArray = new int[5];
int[] intArrayWithValues = {1, 2, 3}; // You can also populate with data.

// ArrayList
// Functions are similar to those of array; arraylists can hold any datatypes.
// The only difference is arraylists resize dynamically, as it is a form of
// resizable-array implementation of the Java "List" interface.
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// Object
// Since it is based on Java, Processing supports object-oriented programming.
// That means you can basically define any datatypes of your own and manipulate
// them to your needs.
// Of course, a class has to be defined before for the object you want.
// Format --> ClassName InstanceName
SomeRandomClass myObject // then instantiate later
//or
SomeRandomClass myObjectInstantiated = new SomeRandomClass();

// Processing comes up with more collections (eg. - Dictionaries and Lists) by
// default, for the simplicity sake, I will leave them out of discussion here.

/* ------------
   Maths
   ------------
*/

// Arithmetic
1 + 1 // 2
2 - 1 // 1
2 * 3 // 6
3 / 2 // 1
3.0 / 2 // 1.5
3.0 % 2 // 1.0

// Processing also comes with a set of functions that simplify mathematical
// operations.
float f = sq(3); // f = 9.0
float p = pow(3, 3); // p = 27.0
int a = abs(-13); // a = 13
int r1 = round(3.1); // r1 = 3
int r2 = round(3.7); // r2 = 4
float sr = sqrt(25); // sr = 5.0

// Vectors
// Processing provides an easy way to implement vectors in its environment
// using PVector class. It can describe a two or three dimensional vector and
// comes with a set of methods which are useful for matrices operations.
// You can find more information on PVector class and its functions here.
// (https://processing.org/reference/PVector.html)

// Trigonometry
// Processing also supports trigonometric operations by supplying a set of
// functions. sin(), cos(), tan(), asin(), acos(), atan() and also degrees()
// and radians() for convenient conversion.
// However, those functions take angle in radians as the parameter so it has
// to be converted beforehand.
float one = sin(PI/2); // one = 1.0
// As you may have noticed, there exists a set of constants for trigonometric
// uses;
// PI, HALF_PI, QUARTER_PI and so on...

/* -------------
   Control Flow
   -------------
*/

// Conditional Statements
// If Statements - The same syntax as if statements in Java.
if (author.getAppearance().equals("hot")) {
  print("Narcissism at its best!");
} else {
  // You can check for other conditions here.
  print("Something is really wrong here!");
}
// A shortcut for if-else statements can also be used.
int i = 3;
String value = (i > 5) ? "Big" : "Small"; // "Small"

// Switch-case structure can be used to check multiple conditions concisely.
// It is important to use the break statement. If the `break`-statement does 
// not exist the program executes all the following cases after a case was true.
int value = 2;
switch(value) {
  case 0:
    print("Nought!"); // This does not get executed.
    break; // Jumps to the next statement
  case 1:
    print("Getting there..."); // This again does not get executed.
    break;
  case 2:
    print("Bravo!"); // This line gets executed.
    break;
  default:
    print("Not found!"); // This line gets executed if our value was some other value.
    break;
}

// Iterative statements
// For Statements - Again, the same syntax as in Java
for(int i = 0; i < 5; i++){
  print(i); // prints from 0 to 4
}

// While Statements - Again, nothing new if you are familiar with Java syntax.
int j = 3;
while(j > 0) {
  print(j);
  j--; // This is important to prevent from the code running indefinitely.
}

// loop()| noLoop() | redraw() | exit()
// These are more of Processing-specific functions to configure program flow.
loop(); // allows the draw() method to run forever while
noLoop(); // only allows it to run once.
redraw(); // runs the draw() method once more.
exit(); // This stops the program. It is useful for programs with draw()
// running continuously.
```

## Drawing with Processing

Since you will have understood the basics of the language by now, we will now
look into the best part of Processing - DRAWING.

```
/* ------
   Shapes
   ------
*/

// 2D Shapes

// Point
point(x, y); // In 2D space
point(x, y, z); // In 3D space
// Draws a point in the coordinate space.

// Line
line(x1, y1, x2, y2); // In 2D space
line(x1, y1, z1, x2, y2, z2); // In 3D space
// Draws a line connecting two points defined by (x1, y1) and (x2, y2).

// Triangle
triangle(x1, y1, x2, y2, x3, y3);
// Draws a triangle connecting three points defined by coordinate parameters.

// Rectangle
rect(a, b, c, d, [r]); // With optional parameter defining the radius of all corners
rect(a, b, c, d, [tl, tr, br, bl]); // With optional set of parameters defining
// radius of each corner
// Draws a rectangle with {a, b} as a top left coordinate and c and d as width
// and height respectively.

// Quad
quad(x, y, x2, y2, x3, y3, x4, y4);
// Draws a quadrilateral with parameters defining coordinates of each corner
// point.

// Ellipse
ellipse(x, y, width, height);
// Draws an eclipse at point {x, y} with width and height specified.

// Arc
arc(x, y, width, height, start, stop, [mode]);
// While the first four parameters are self-explanatory,
// start and end defined the angles the arc starts and ends (in radians).
// Optional parameter [mode] defines the filling;
// PIE gives pie-like outline, CHORD gives the chord-like outline and OPEN is
// CHORD without strokes

// Curves
// Processing provides two implementation of curves; using curve() and bezier().
// Since I plan to keep this simple I wont be discussing any further details.
// However, if you want to implement it in your sketch, here are the references:
// (https://processing.org/reference/curve_.html)
// (https://processing.org/reference/bezier_.html)

// 3D Shapes

// 3D space can be configured by setting "P3D" to the renderer parameter in
// size() method.
size(width, height, P3D);
// In 3D space, you will have to translate to the particular coordinate to
// render the 3D shapes.

// Box
box(size);  // Cube with same length defined by size
box(w, h, d); // Box with width, height and depth separately defined

// Sphere
sphere(radius); // Its size is defined using the radius parameter
// Mechanism behind rendering spheres is implemented by tessellating triangles.
// That said, how much detail being rendered is controlled by function
// sphereDetail(res)
// More information here: (https://processing.org/reference/sphereDetail_.html)

// Irregular Shapes
// What if you wanted to draw something thats not made available by Processing
// functions?
// You can use beginShape(), endShape(), vertex(x,y) to define shapes by
// specifying each point. More information here:
// (https://processing.org/reference/beginShape_.html)
// You can also use custom made shapes using PShape class:
// (https://processing.org/reference/PShape.html)

/* ---------------
   Transformations
   ---------------
*/

// Transformations are particularly useful to keep track of the coordinate
// space and the vertices of the shapes you have drawn. Particularly;
// matrix stack methods; pushMatrix(), popMatrix() and translate(x,y)
pushMatrix(); // Saves the current coordinate system to the stack
// ... apply all the transformations here ...
popMatrix(); // Restores the saved coordinate system
// Using them, the coordinate system can be preserved and visualized without
// causing any conflicts.

// Translate
translate(x, y); // Translates to point{x, y} i.e. - setting origin to that point
translate(x, y, z); // 3D counterpart of the function

// Rotate
rotate(angle); // Rotate the amount specified by the angle parameter
// It has 3 3D counterparts to perform rotation, each for every dimension,
// namely: rotateX(angle), rotateY(angle), rotateZ(angle)

// Scale
scale(s); // Scale the coordinate system by either expanding or contracting it.

/* --------------------
   Styling and Textures
   --------------------
*/

// Colours
// As I have discussed earlier, the background colour can be configured using
// background() function. You can define a color object beforehand and then
// pass it to the function as an argument.
color c = color(255, 255, 255); // WHITE!
// By default, Processing uses RGB colour scheme but it can be configured to
// HSB using colorMode(). Read more here:
// (https://processing.org/reference/colorMode_.html)
background(c); // By now, the background colour should be white.
// You can use fill() function to select the colour for filling the shapes.
// It has to be configured before you start drawing shapes so the colours gets
// applied.
fill(color(0, 0, 0));
// If you just want to colour the outlines of the shapes then you can use
// stroke() function.
stroke(255, 255, 0, 200); // stroke colour set to yellow with transparency
// set to a lower value.

// Images
// Processing can render images and use them in several ways. Mostly stored as
// PImage datatype.
filter(shader); // Processing supports several filter functions for image manipulation.
texture(image); // PImage can be passed into arguments for texture-mapping the shapes.
```

If you want to take things further, there are more things Processing is powered
for. Rendering models, shaders and whatnot. There's too much to cover in a
short documentation, so I will leave them out here. Should you be interested,
please check out the references.

```
// Before we move on, I will touch a little bit more on how to import libraries
// so you can extend Processing functionality to another horizon.

/* -------
   Imports
   -------
*/

// The power of Processing can be further visualized when we import libraries
// and packages into our sketches.
// Import statement can be written as below at the top of the source code.
import processing.something.*;
```

## DTC?

Down To Code? Let's get our hands dirty!

Let us see an example from openprocessing to visualize how much Processing is
capable of within few lines of code.

Copy the code below into your Processing IDE and see the magic.

```
// Disclaimer: I did not write this program since I currently am occupied with
// internship and this sketch is adapted from openprocessing since it shows
// something cool with simple codes.
// Retrieved from: (https://www.openprocessing.org/sketch/559769)

float theta;
float a;
float col;
float num;

void setup() {
  size(600,600);
}

void draw() {
  background(#F2F2F2);
  translate(width/2, height/2);
  theta = map(sin(millis()/1000.0), -1, 1, 0, PI/6);

  float num=6;
  for (int i=0; i<num; i++) {
    a =350;
    rotate(TWO_PI/num);
    branch(a);
  }

}

void branch(float len) {
  col=map(len, 0, 90, 150, 255);
  fill(col, 0, 74);
  stroke (col, 0, 74);
  line(0, 0, 0, -len);
  ellipse(0, -len, 3, 3);
  len *= 0.7;

  if (len>30) {
    pushMatrix();
    translate(0, -30);
    rotate(theta);
    branch(len);
    popMatrix();

    pushMatrix();
    translate(0, -30);
    rotate(-theta);
    branch(len);
    popMatrix();

  }
}
```

Processing is easy to learn and is particularly useful to create multimedia
contents (even in 3D) without having to type a lot of codes. It is so simple
that you can read through the code and get a rough idea of the program flow.

However, that does not apply when you introduce external libraries, packages
and even your own classes. (Trust me! Processing projects can get real humongous...)

## Some useful resources

 - [Processing Website](http://processing.org)
 - [Processing Sketches](http://openprocessing.org)
