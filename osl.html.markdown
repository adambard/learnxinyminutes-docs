---
language: osl
filename: learnosl.osl
contributors:
  - ["Preetham Pemmasani", "https://github.com/Preetham-ai"]
---

OSL (Open Shading Language) is a programming language designed by Sony for Arnold Renderer used for creating shaders.

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
color Blue; // Initializing a variable
int _num = 3;
float Num = 3.00;
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
true == 1 // Error
false == 0 // Error

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

	float a = dot ((1,2,3), (1,2,3)) // Dot Product
	vector b = cross ((1,2,3), (1,2,3)) // Cross Product
	float l = length(b) // length of vector
	vector normalize (vector b) //  Normalizes the vector

	float distance (point P0, point P1) //Finds the distance between two points
	float distance (point P0, point P1, point Q) /* Perpendicular distance from
	Q to line joining P0 and P1 */



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

	// matrix transformations are easy to implement
	matrix a = matrix ("shader", 1); // converted shader to common
	matrix m = matrix ("object", "world"); // converted object to world

	// Operations that can be used with decreasing precedence are:
	// ( - * / == !=)

	float determinant (matrix M) // returns the determinant of the matrix
	float transpose (matrix M) // returns the transpose of the matrix

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

// 12. closure
	// Used to store data that aren't considered during Shader's execution
	// Can't be manipulated, nor read
	// A null closure can always be assigned
	
	closure color oren nayar diffuse_bsdf(normal N, color alb, float roughness)
	closure color burley diffuse_bsdf (normal N, color alb, float roughness)
	closure color dielectric_bsdf (normal N, vector U, color reflection tint,
		color transmission tint, float roughness x, float roughness y,
		float ior, string distribution)
	closure color conductor_bsdf (normal N, vector U, float roughness x,
		float roughness y, color ior, color extinction, string distribution)
	closure color generalized schlick_bsdf (normal N, vector U,
		color reflection tint, color transmission tint,
		float roughness x, float roughness y, color f0, color f90,
		float exponent, string distribution)
	closure color translucent_bsdf (normal N, color albedo)
	closure color transparent_bsdf ()
	closure color subsurface bssrdf ()
	closure color sheen_bsdf (normal N, color albedo, float roughness)

	// Also exist for Volumetrics

	closure color anisotropic_vdf(color albedo, color extinction,
		float anisotropy)
	closure color medium vdf (color albedo, float transmission depth,
		color transmission color, float anisotropy, float ior, int priority)

	closure color uniform edf (color emittance) // Emission closure
	closure color holdout () // Hides objects beneath it

	// BSDFs can be layered using this closure
	closure color layer (closure color top, closure color base)



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

/////////////////////
// 5. Functions //
/////////////////////

// Math Constants
	M_PI // π
	M_PI_35 // π/35
	m_E // e
	M_LN2 // ln 2
	M_SQRT2 // √2
	M_SQRT1_2 // √(1/2)

// Geometry Functions
	
	vector reflect (vector I, vector N)
	vector faceforward (vector N, vector I) // Tells the direction of vector
	vector faceforward (vector N, vector I, vector Nref) // Using a reference
	vector reflect (vector I, vector N) // gives Reflection vector along normal
	vector refract (vector I, vector N, float IOR) // gives refracted vector
	void fresnel (vector I, normal N, float eta,
				output float Kr, output float Kt,
				output vector R, output vector T);
	/* Computes the Reflection (R) and Transmission (T) vectors, along with the
	scaling factors for reflected (Kr) and transmitted (Kt) light. */

	// Rotating a point along a given axis 
	point rotate (point Q, float angle, vector axis) 
	// Rotating a point along a line made by 2 points
	point rotate (point Q, float angle, point P0, point P1)
	vector calculatenormal (point p) // Calculates normal of surface at point p

	// Transforming units is easy
	float transformu ("cm", float x) // converts to cm
	float transformu ("cm", "m", float y) // converts cm to m

// Displacement Functions
	void displace (float 5); // Displace by 5 amp units
	void bump (float 10); // Bump by 10 amp units

// Pattern Generations
	type step (type edge, type x) // Returns 1 if x ≥ edge, else 0
	type linearstep (type edge0, type edge1, type x) /* Linearstep Returns 0 if
	x ≤ edge0, and 1 if x ≥ edge1, with linear interpolation */
	type linearstep (type edge0, type edge1, type x) /* smoothstep Returns 0 if
	x ≤ edge0, and 1 if x ≥ edge1, with Hermite interpolation */

	type noise (type noise (string noisetype, float u, float v, ...)) // noise
	/* some noises are ("perlin", "snoise", "uperlin", "noise", "cell", "hash"
	"simplex", "usimplex", "gabor", etc) */

	type pnoise (string noisetype, float u, float uperiod) // periodic noise
	type pnoise (string noisetype, float u, float v, float uperiod, float vperiod)
	type snoise (float u, float v)
	type psnoise (float u, float v, float uperiod, float vperiod)
	type cellnoise (float u, float v)
	type hashnoise (float u, float v)
	// The type may be of float, color, point, vector, or normal.
	int hash (float u, float v)

	// Splines can be created in two ways
	type spline (string basis, float x, int nknots, type y[])
	type spline (string basis, float x, type y0, type y1, ... type y(n−1))
	/* basis is the type of interpolation ranges from "catmull-rom", "bezier",
	"bspline", "hermite", "linear", or "constant" */

	// InverseSplines also exist
	float splineinverse (string basis, float v, float y0, ... float yn−1)
	float splineinverse (string basis, float v, int nknots, float y[])

// Calculus Operators
	type Dx (type f), Dy (type f), Dz (type f)
	// The type can be of float, vector and color
	// Returns partial derivative of f with respect to x, y and z
	vector Dx (point a), Dy (point a), Dz (point a)

	float area (point p) // gives the surface area at the position p 

	float filterwidth (float x) // gives the changes of x in adjacent samples

// Texture Functions
	// lookup for a texture at coordinates (x,y)
	type texture (string filename, float x, float y, ...params...)
	// 3D lookup for a texture at coordinates (x,y)
	type texture3d (string filename, point p, ...params...)
	// parameters are ("blur","width","wrap","fill","alpha","interp", ...)
	
// Light Functions

	float surfacearea () // Returns the surface area of area light covers
	int backfacing () // Outputs 1 if the normals are backfaced, else 0
	int raytype (string name) // returns 1 if the ray is a particular raytype 
	int trace (point pos, vector dir, ...) // Trace ray from pos in a direction
	// Parameters are ("mindist", "mindist", "shade", "traceset")

// Lookup Functions
	// Regex can be used to search inside strings
	int regex search (string subject, string re) // search for subject in re
	int regex match (string subject, string re)

	// Dictionaries exist either as a string or a file
	int dict find (string dictionary, string query)
```
### Further reading

* [Blender Docs for OSL](https://docs.blender.org/manual/en/latest/render/shader_nodes/osl.html)
* [C4D Docs for OSL](https://docs.otoy.com/cinema4d//OpenShadingLanguageOSL.html)
* Open Shading Language on [Github](https://github.com/AcademySoftwareFoundation/OpenShadingLanguage)
* [Official OSL Documentation](https://open-shading-language.readthedocs.io/en/main/)