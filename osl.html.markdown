---
language: osl
filename: learnosl.osl
contributors:
  - ["Preetham Pemmasani", "https://github.com/Preetham-ai"]
---

OSL (Open Shading Language) is a programming language designed for creating shaders to describe how surfaces react to light in computer graphics.

[Read more here.](https://raw.githubusercontent.com/imageworks/OpenShadingLanguage/master/src/doc/osl-languagespec.pdf)

```c


// Single-line comments start with //

/* Multi line comments are preserved. */

// Statements can be terminated by ;
divide(1,2);

///////////////
// 1. Basics //
///////////////

// Declating variables
float Num = 3.00;  // Scalar floating-point data (numbers)
int _num = 3; // Integer data
float c[3] = {0.1, 0.2, 3.14}; // Array

// Math works as you would expect
3 + 1;   // 4
74 - 3;   // 71
20 * 2; // 40
75/3;  // 25.0

// And modulo division only works with integers
10 % 2; // 0
31 % 4; // 1

// Bitwise operations only works with integers
- 0 // 1 (Unary Negation)
~ 00100011 // 11011100 (bitwise Compliment)
1 << 2; // 4 (shift Left)
12 >> 1; // 3 (shift Right)
1 & 0; // 0 (bitwise AND)
1 | 0; // 1 (bitwise OR)
1 ^ 1; // 0 (bitwise XOR)

// We also have booleans
true;
false;

// Booleans can't be compared to integers
true = 1 // Error
false = 0 // Error

// Negation uses the ! symbol
!0; // 1
!1; // 0
!2; // 0
//... and so on

// Relation Operators are defined like:
0 == 0 // true (equal to)
0 != 1 // true (not equal to)
5 < 3 // false (less then)
3 <= 3 // true (less than or equal to)
69 > 69 // false (greater than)
99 >= 52 // true (greater than or equal)


// Functions are same as C and C++
float sum(float a, float b){
	return a+b;
}

int subtract(int a, int b){
	return a-b;
}

sum(2,3); // 5

////////////////
// 2. Shaders //
////////////////

// Shaders extend the functionality of a renderer with custom behavior of materials and light
// Shader's syntax is similar to the main function in C
// The inputs and the outputs should be initialized to default types
shader multiply(float a = 0.0,
				float b = 0.0, 
				output float c = 0.0){
    c = a*b;
}

// Double brackets[[ ]] is used to classify metadata of a shader
surface plastic
	[[ string help = "Realistic wood shader" ]]
(
	color Plastic = color (0.7, 0.5, 0.3) [[ string help = "Base color" ]],
	float Reflectivity = 0.5 [[ float min = 0, float max = 1 ]],
){...}

///////////////////////////////////////
// Metadata Types
///////////////////////////////////////

[[ string label = "IOR" ]] // Display-name in UI of the parameter
[[ string help = "Change Refractive Index" ]] // Info about the parameter
[[ string help = "widget" // Gives widgets to input the parameter
	string widget = "number" ]] // input float or int
	string widget = "string" ]] // String input
	string widget = "boolean" ]] // yes/no (or) 1/0
	string widget = "popup", options = "smooth|rough" ]] // Drop-down list
	// enum Drop-down list can also be made
	string widget = "mapper", options = "smooth:0|rough:1" ]]
	string widget = "filename" ]] // Input files externally
	string widget = "null" ]] // null input

[[ float min = 0.0 ]] // Minimum value of parameter
[[ float max = 0.5 ]] // Maximum value of parameter
[[ int slider = 3.0   // Adds a slider as an input
	int slidermin = -1]] // minimum value of the slider
	int slidermax = 3]] // maximum value of the slider
	int slidercenter = 2]] // origin value of the slider

[[ float sensitivity = 0.5 ]] // step size for incrementing the parameter
[[ string URL = www.example.com/ ]] // URL of shader's documentation 

	

// There are different types of shaders

/* Surface shaders determine the basic material properties of a surface and 
how it reacts to light */
// Light shaders are a type of SURFACE shaders used for emissive objects.
// Displacement shaders alter the geometry using position and normals.
// Volume shaders adds a medium like air/smoke/dust into the scene.

volume multiply(float a = 0.0, float b = 0.0, output float c = 0.0){
    c = 2*a+b; 
}

////////////////////////////////////////
// 3. Data Types and Global Variables //
////////////////////////////////////////

// Data Types

// 1. int (Integer)
int x = -12; // Minimum size of 32-bits
int new2 = 0x01cf // Hexadecimal can also be specified

///////////////////////////////////////
// Order of Evaluation
///////////////////////////////////////

// From top to bottom, top has higher precedence
//-----------------------------------//
//        Operators                  //
//-----------------------------------//
// int++, int--                      //
// ++ int --int - ~ !                //
// * / %                             //
// + -                               //
// << >>                             //
// < <= > >=                         //
// == !=                             //
// &                                 //
// ^                                 //
// |                                 //
// &&                                //
// ||                                //
// ?:                                //
// = += -= *= /=             		 //
//-----------------------------------//

// 2. float (Floating-point number)
float A = 2.3; // minimum  IEEE 32-bit float
float Z = -4.1e2

// Order of evaluation is similar to int.
// Operations like ( ~ ! % << >> ^ | & && || ) aren't available in float

// 3. color (Red, Green, Blue)
color p = color(0,0,0) // black
color q = color(1) // white ( same as color(1,1,1) )
color r = color("rgb", 0.23, 0.1, 0.8) // explicitly specify in RGB
color s = color("hsv", 0.23, 0.1, 0.8) // specify in HSV
// HSV stands for (Hue, Saturation, Luminance)
// HSL stands for (Hue, Saturation, Lightness)
// YIQ, XYZ and xyY formats can also be used

// Global Variables
// Contains info that the renderer knows
// These variables need not be declared

point P // Position of the point you are shading
vector I // Incident ray direction from viewing position to shading position
normal N // Normal of the surface at P
normal Ng // Normal of the surface at P irrespective of bump mapping
float u // UV 2D x - parametric coordinate of geometry
float v // UV 2D y - parametric coordinate of geometry
vector dPdu // change of P with respect to u tangent to the surface
vector dPdv // change of P with respect to v tangent to the surface
float time // Current time
float dtime // Time covered
vector dPdtime // change of P with respect to time

/////////////////////
// 4. Control flow //
/////////////////////

// Conditionals in OSL are just like in C or C++.

// If/Else
if (5>2){
	int x = s;
	int l = x;
}
else{
	int x = s + l; 
}

// 'while' loop
int i = 0;
while (i < 5) {
    i += 1;
    printf("Current value of i: %d\n", i);
}

// 'do-while' loop is where test happens after the body of the loop
int i = 0;
do {
    printf("Current value of i: %d\n", i);
    i += 1;
} while (i < 5);

// 'for' loop
for (int i = 0; i < 5; i += 1) {
    printf("Current value of i: %d\n", i);
}
```