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
color Blue; // Initializing a variable
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

// Shaders explain the custom behavior of materials and light
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

// 1. The void type indicates a function that doesn't return any value

// 2. int (Integer)
	int x = -12; // Minimum size of 32-bits
	int new2 = 0x01cf; // Hexadecimal can also be specified

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

// 3. float (Floating-point number)
	float A = 2.3; // minimum  IEEE 32-bit float
	float Z = -4.1e2;

	// Order of evaluation is similar to int.
	// Operations like ( ~ ! % << >> ^ | & && || ) aren't available in float

// 4. string
	// The syntax is similar to C
	string new = "Hello World";
	// some Special characters:
	/*
	'\"'; // double quote
	'\n'; // newline character
	'\t'; // tab character (left justifies text)
	'\v'; // vertical tab
	'\\'; // back slash
	'\r'; // carriage return
	'\b'; // backspace character
	*/

	// Strings are concatenated with whitespace
	"Hello " "world!"; // "Hello world!"
	// concat function can also be used
	string concat ("Hello ","World!");

	// printf function is same as C
	int i = 18
	printf("I am %d years old",i);

	// String functions can alse be used
	int strlen (string s) // gives the length of the string
	int startswith (string s, "the") // gives 1 if string starts with suffix
	int endswith (string s, "the") // gives 1 if string ends with suffix

// 5. color (Red, Green, Blue)
	color p = color(0,0,0); // black
	color q = color(1); // white ( same as color(1,1,1) )
	color r = color("rgb", 0.23, 0.1, 0.8); // explicitly specify in RGB
	color s = color("hsv", 0.23, 0.1, 0.8); // specify in HSV
	// HSV stands for (Hue, Saturation, Luminance)
	// HSL stands for (Hue, Saturation, Lightness)
	// YIQ, XYZ and xyY formats can also be used
	// We can also access the indivudual values of (R,G,B)
	float Red = p[0]; // access the red component  
	float Green = p[1]; // access the green component  
	float Blue = p[2]; // access the blue component

	// They can also be accessed like this
	float Red = p.r; // access the red component  
	float Green = p.g; // access the green component  
	float Blue = p.b; // access the blue component

	// Math operators work like this with decreasing precedence
	color C = (3,2,3) * (1,0,0); // (3, 0, 0)
	color D = (1,1,1) * 255; // (255, 255, 255)
	color E = (25,5,125) / 5; // (5, 1, 25)
	color F = (30,40,50) / (3,4,5); // (10, 10, 10)
	color A = (1,2,3) + (1,0,0); // (2, 2, 3)
	color B = (1,2,3) - (1,0,0); // (0, 2, 3)
	// Operators like ( - == != ) are also used

	// Color Functions
	color blackbody (float 1500) // Gives color based on temperature 
	float luminance (color A) // gives luminance as 0.2126R+0.7152G+0.0722B
	color wavelength color (float 700) // Gives color based on wavelength
	color transformc ("hsl", "rgb") //converts rgb to hsl and etc

// 6. point (x,y,z) is position of a point in the 3D space
// 7. vector (x,y,z) has length and direction but no position
// 8. normal (x,y,z) is a special vector perpendicular to a surface
	L = point(0.5, 0.5, 0.5);
	M = vector(1, 1, 1);
	N = normal(0, 0, 1);

	// These 3 types can be assigned to a coordinate system
	L = point("object", 0.5, 0.5, 0.5); // relative to local space
	M = vector("common", 0.5, 0.5, 0.5); // relative to world space
	// There's also ("shader", "world", "camera", "screen", "raster", "NDC")

	float x = L[0]; // access the x-component  
	float y = L[1]; // access the y-component  
	float z = L[2]; // access the z-component

	// They can also be accessed like this
	float x = M.x; // access the x-component
	float y = M.y; // access the y-component  
	float z = M.z; // access the z-component

	// Operators are the same as color and have the same precedence

// 9. matrix
	// Used for transforming vectors between different coordinate systems.
	// They are usually 4x4 (or) 16 floats
	matrix zero = 0; // makes a 4x4 zero matrix
	matrix ident = 1; // makes a 4x4 identity matrix
	matrix m = 7; // Maked a 4x4 scalar matrix with scaling factor of 7
	float x = m[1][1]; // 7
	
	// matrices can be constructed using floats in row-major order
	matrix new = matrix (m00, m01, m02, m03, m10, m11, m12, m13,
					     m20, m21, m22, m23, m30, m31, m32, m33);


// 10. array 
	// Arrays in OSL are similar to C
	float a[5]; // initialize array a with size 5
	int b[3] = {90,80,70}; // declare array with size 3
	int len = arraylength(b); // 3
	int f = b[1]; // 80
	float anotherarray[3] = b; // arrays can be copied if same type

// 11. struct (Structures)
	// Structures in OSL are similar to C and C++.
	struct RGBA { // Defining a structure
		color rgb;
		float alpha;
	};

	
	RGBA col; // Declare a structure
	RGBA b = { color(.1,.2,.3), 1 }; // Can also be declared like this
	
	r.rgb = color (1, 0, 0); // Assign to one field
	color c = r.rgb; // Read from a structure field



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