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
	//--------------------------//
	//        Operators         //
	//--------------------------//
	// int++, int--             //
	// ++ int --int - ~ !       //
	// * / %                    //
	// + -                      //
	// << >>                    //
	// < <= > >=                //
	// == !=                    //
	// &                        //
	// ^                        //
	// |                        //
	// &&                       //
	// ||                       //
	// ?:                       //
	// = += -= *= /=            //
	//--------------------------//

// 3. float (Floating-point number)
	float A = 2.3; // minimum  IEEE 32-bit float
	float Z = -4.1e2; // Z = -4.1 * 10^2

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
	string concat ("Hello ","World!"); // "Hello world!"

	// printf function is same as C
	int i = 18;
	printf("I am %d years old",i); // I am 18 years old

	// String functions can alse be used
	int strlen (string s); // gives the length of the string
	int len = strlen("Hello, World!"); // len = 13

	// startswith returns 1 if string starts with prefix, else returns 0
	int starts = startswith("The quick brown fox", "The"); // starts = 1

	// endswith returns 1 if string starts with suffix, else returns 0
	int ends = endswith("The quick brown fox", "fox"); // ends will be 1

// 5. color (Red, Green, Blue)
	color p = color(0,1,2); // black
	color q = color(1); // white ( same as color(1,1,1) )
	color r = color("rgb", 0.23, 0.1, 0.8); // explicitly specify in RGB
	color s = color("hsv", 0.23, 0.1, 0.8); // specify in HSV
	// HSV stands for (Hue, Saturation, Luminance)
	// HSL stands for (Hue, Saturation, Lightness)
	// YIQ, XYZ and xyY formats can also be used
	// We can also access the indivudual values of (R,G,B)
	float Red = p[0]; // 0 (access the red component)
	float Green = p[1]; // 1 (access the green component)
	float Blue = p[2]; // 2 (access the blue component)

	// They can also be accessed like this
	float Red = p.r; // 0 (access the red component)  
	float Green = p.g; // 1 (access the green component) 
	float Blue = p.b; // 2 (access the blue component)

	// Math operators work like this with decreasing precedence
	color C = (3,2,3) * (1,0,0); // (3, 0, 0)
	color D = (1,1,1) * 255; // (255, 255, 255)
	color E = (25,5,125) / 5; // (5, 1, 25)
	color F = (30,40,50) / (3,4,5); // (10, 10, 10)
	color A = (1,2,3) + (1,0,0); // (2, 2, 3)
	color B = (1,2,3) - (1,0,0); // (0, 2, 3)
	// Operators like ( - == != ) are also used

	// Color Functions
	color blackbody (1500) // Gives color based on temperature (in Kelvin)
	float luminance (0.5, 0.3, 0.8) // 0.37 gives luminance cd/m^2
	// Luminance is calculated by 0.2126R+0.7152G+0.0722B
	color wavelength color (700) // (1, 0, 0) Gives color based on wavelength
	color transformc ("hsl", "rgb") // converts one system to another

// 6. point (x,y,z) is position of a point in the 3D space
// 7. vector (x,y,z) has length and direction but no position
// 8. normal (x,y,z) is a special vector perpendicular to a surface
	// These Operators are the same as color and have the same precedence
	L = point(0.5, 0.6, 0.7);
	M = vector(30, 100, 70);
	N = normal(0, 0, 1);

	// These 3 types can be assigned to a coordinate system
	L = point("object", 0.5, 0.6, 0.7); // relative to local space
	M = vector("common", 30, 100, 70); // relative to world space
	// There's also ("shader", "world", "camera", "screen", "raster", "NDC")

	float x = L[0]; // 0.5 (access the x-component)  
	float y = L[1]; // 0.6 (access the y-component)  
	float z = L[2]; // 0.7 (access the z-component)

	// They can also be accessed like this
	float x = M.x; // 30 (access the x-component)
	float y = M.y; // 100 (access the y-component)
	float z = M.z; // 70 (access the z-component)

	float a = dot ((1,2,3), (1,2,3)); // 14 (Dot Product)
	vector b = cross ((1,2,3), (1,2,3)); // (0,0,0) (Cross Product)
	float l = length(L); // 1.085 (length of vector)
	vector normalize (vector L); // (0.460, 0.552, 0.644) Normalizes the vector

	point p0 = point(1, 2, 3);
	point p1 = point(4, 5, 6);
	point Q = point(0, 0, 0);

	// Finding distance between two points
	float len = distance(point(1, 2, 3), point(4, 5, 6)); // 5.196
	// Perpendicular distance from Q to line joining P0 and P1
	float distance (point P0, point P1, point Q); // 2.45


// 9. matrix
	// Used for transforming vectors between different coordinate systems.
	// They are usually 4x4 (or) 16 floats
	matrix zero = 0; // makes a 4x4 zero matrix
	/* 0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0 */
	
	matrix ident = 1; // makes a 4x4 identity matrix
	/* 1.0, 0.0, 0.0, 0.0,
	   0.0, 1.0, 0.0, 0.0,
	   0.0, 0.0, 1.0, 0.0,
	   0.0, 0.0, 0.0, 1.0 */
		
	matrix m = 7; // Maked a 4x4 scalar matrix with scaling factor of 7
	/* 7.0, 0.0, 0.0, 0.0,
	   0.0, 7.0, 0.0, 0.0,
	   0.0, 0.0, 7.0, 0.0,
	   0.0, 0.0, 0.0, 7.0 */
	
	float x = m[1][1]; // 7
	
	// matrices can be constructed using floats in row-major order
	// matrices are usually 4x4 with 16 elements
	matrix myMatrix = matrix(1.0, 0.0, 0.0, 0.0,    // Row 1
                             0.0, 2.0, 0.0, 0.0,    // Row 2
                             0.0, 0.0, 3.0, 0.0,    // Row 3
                             0.0, 0.0, 0.0, 4.0);	// Row 4

	// matrix transformations are easy to implement
	matrix a = matrix ("shader", 1); // converted shader to common
	matrix m = matrix ("object", "world"); // converted object to world

	// Operations that can be used with decreasing precedence are:
	// ( - * / == !=)

	float determinant (matrix M) // 24 (returns the determinant of the matrix)
	float transpose (matrix M) // returns the transpose of the matrix
	/* 1.0, 0.0, 0.0, 0.0,
       0.0, 2.0, 0.0, 0.0,
       0.0, 0.0, 3.0, 0.0,
       0.0, 0.0, 0.0, 4.0 */

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

	
	RGBA col; // Declaring a structure
	RGBA b = { color(0.1, 0.2, 0.3), 1 }; // Can also be declared like this

	r.rgb = color (1, 0, 0); // Assign to one field
	color c = r.rgb; // Read from a structure field

// 12. closure
	// Closure is used to store data that aren't considered when it executes.
	// It cannot be manipulated or read.
	// A null closure can always be assigned.
	// OSL currently only supports color as their closure.

	// A few examples of closures are:

	// Diffuse BSDF closures:
	closure color oren_nayar_diffuse_bsdf(normal N, color alb, float roughness)
	closure color burley_diffuse_bsdf(normal N, color alb, float roughness);

	// Dielectric BSDF closure:
	closure color dielectric_bsdf(normal N, vector U, color reflection_tint,
	    color transmission_tint, float roughness_x, float roughness_y,
	    float ior, string distribution);

	// Conductor BSDF closure:
	closure color conductor_bsdf(normal N, vector U, float roughness_x,
	    float roughness_y, color ior, color extinction, string distribution);

	// Generalized Schlick BSDF closure:
	closure color generalized_schlick_bsdf(normal N, vector U,
	    color reflection_tint, color transmission_tint,
	    float roughness_x, float roughness_y, color f0, color f90,
	    float exponent, string distribution);

	// Translucent BSDF closure:
	closure color translucent_bsdf(normal N, color albedo);

	// Transparent BSDF closure:
	closure color transparent_bsdf();

	// Subsurface BSSRDF closure:
	closure color subsurface_bssrdf();

	// Sheen BSDF closure:
	closure color sheen_bsdf(normal N, color albedo, float roughness);

	// Anisotropic VDF closure: (Volumetric)
	closure color anisotropic_vdf(color albedo, color extinction,
	    float anisotropy);

	// Medium VDF closure: (Volumetric)
	closure color medium_vdf(color albedo, float transmission_depth,
	    color transmission_color, float anisotropy, float ior, int priority);

	closure color uniform edf(color emittance); // Emission closure
	closure color holdout(); // Holdout Hides objects beneath it

	// BSDFs can be layered using this closure
	closure color layer (closure color top, closure color base);



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
	vector N = vector(0.1, 1, 0.2); // Normal vector
	vector I = vector(-0.5, 0.2, 0.8); // Incident vector

	// Faceforward tells the direction of vector
	vector facing_dir = faceforward(N, I); // facing_dir = (-0.5, 0.2, 0.8)

	// faceforward with three arguments
	vector ref = vector(0.3, -0.7, 0.6); // Reference normal
	facing_dir = faceforward(N, I, ref); // facing_dir = (0.5, -0.2, -0.8)

	// reflect gives the reflected vector along normal
	vector refl = reflect(I, N); // refl = (-0.7, -0.4, 1.4)\

	// refract gives the refracted vector along normal
	float ior = 1.5; // Index of refraction
	vector refr = refract(I, N, ior); // refr = (-0.25861, 0.32814, 0.96143)

	/* Fresnel computes the Reflection (R) and Transmission (T) vectors, along
	with the scaling factors for reflected (Kr) and transmitted (Kt) light. */
	float Kr, Kt;
	vector R, T;
	fresnel(I, N, ior, Kr, Kt, R, T);
/* Kr = 0.03958, Kt = 0.96042
	R = (-0.19278, -0.07711, 0.33854)
	T = (-0.25861, 0.32814, 0.96143) */

	// Rotating a point along a given axis
	point Q = point(1, 0, 0);
	float angle = radians(90); // 90 degrees
	vector axis = vector(0, 0, 1);
	point rotated_point = rotate(Q, angle, axis);
	// rotated_point = point(0, 1, 0)

	// Rotating a point along a line made by 2 points
	point P0 = point(0, 0, 0);
	point P1 = point(1, 1, 0);
	angle = radians(45); // 45 degrees
	Q = point(1, 0, 0);
	rotated_point = rotate(Q, angle, P0, P1);
	// rotated_point = point(0.707107, 0.707107, 0)

	// Calculating normal of surface at point p
	point p1 = point(1, 0, 0); // Point on the sphere of radius 1
	vector normal1 = calculatenormal(p1);
	// normal1 = vector(1, 0, 0)

	// Transforming units is easy
	float transformu ("cm", float x) // converts to cm
	float transformu ("cm", "m", float y) // converts cm to m

// Displacement Functions
	void displace (float 5); // Displace by 5 amp units
	void bump (float 10); // Bump by 10 amp units


// Noise Generation

	type noise (type noise (string noisetype, float u, float v, ...)); // noise
	type noise (string noisetype, point p,...); // point instead of coordinates
	/* some noises are ("perlin", "snoise", "uperlin", "noise", "cell", "hash"
	"simplex", "usimplex", "gabor", etc) */
	
	// Noise Names

	// 1. Perlin Noise (perlin, snoise):
	// Creates smooth, swirling noise often used for textures.
	// Range: [-1, 1] (signed)
	color cloud_texture = noise("perlin", P);

	// 2. Simplex Noise (simplex, usimplex):
	// Similar to Perlin noise but faster.
	// Range: [-1, 1] (signed) for simplex, [0, 1] (unsigned) for usimplex
	float bump_amount = 0.2 * noise("simplex", P * 5.0);

	// 3. UPerlin Noise (uperlin, noise):
	// Similar to peril
	// Range: [0, 1] (unsigned)
	color new_texture = noise("uperlin", P);
	
	// 4. Cell Noise (cell):
	// Creates a blocky, cellular and constant values within each unit block
	// Range: [0, 1] (unsigned)
	color new_texture = noise("cell", P);

	// 5. Hash Noise (hash):
	// Generates random, uncorrelated values at each point.
	// Range: [0, 1] (unsigned)
	color new_texture = noise("hash", P);

	// Gabor Noise (gabor) 
	// Gabor Noise is advanced version of Perin noies and gives more control
	// Range: [-1, 1] (signed)
	// Gabor Noise Parameters

	// Anisotropic (default: 0)
	// Controls anisotropy:
	// 0: Isotropic (equal frequency in all directions)
	// 1: Anisotropic with user-defined direction vector (defaults to (1,0,0))
	/* 2: Hybrid mode,anisotropic along direction vector but radially isotropic
	perpendicularly. */

	// Direction (default: (1,0,0))
	// Specifies the direction of anisotropy (used only if anisotropic is 1).

	// bandwidth (default: 1.0)
	// Controls the frequency range of the noise.

	// impulses (default: 16)
	// Controls the number of impulses used per cell, affecting detail level.

	// do_filter (default: 1)
	// Enables/disables antialiasing (filtering).

	result = noise(
        "gabor",
        P,
        "anisotropic", anisotropic,
        "direction", direction,
        "bandwidth", bandwidth,
        "impulses", impulses,
        "do_filter", do_filter
    );

	// Specific noises can also be used instead of passing them as types
	// pnoise is periodic noise
	float n1 = pnoise("perlin", 0.5, 1.0);
	// 2D periodic noise with Gabor type
	float n2 = pnoise("gabor", 0.2, 0.3, 2.0, 3.0);
	// 2D non-periodic simplex noise
	float n3 = snoise(0.1, 0.7);
	// 2D periodic simplex noise
	type psnoise (float u, float v, float uperiod, float vperiod);
	float n4 = psnoise(0.4, 0.6, 0.5, 0.25);
	// 2D cellular noise
	float n5 = cellnoise(0.2, 0.8);
	// 2D hash noise
	int n6 = hash(0.7, 0.3);

// Step Function
	// Step Functions are used to compare input and threshold

	// The type may be of float, color, point, vector, or normal.
	type step (type edge, type x); // Returns 1 if x ≥ edge, else 0
	color checker = step(0.5, P);  // P is a point on the surface
	/* Pixels with P values below 0.5 will be black, those above or equal will
	be white */
	float visibility = step(10, distance(P, light_position));
	// Light is fully visible within 10 units, completely invisible beyond

	type linearstep (type edge0, type edge1, type x); /* Linearstep Returns 0
	if x ≤ edge0, and 1 if x ≥ edge1, with linear interpolation */
	color gradient = linearstep(0, 1, P);
	// P is a point on the surface between 0 and 1
	// Color will graduate smoothly from black to white as P moves from 0 to 1
	float fade = linearstep(0.85, 1, N.z);  // N.z is the z-component
	// Object edges with normals close to vertical (N.z near 1) will fade out
	
	type smoothstep (type edge0, type edge1, type x); /* smoothstep Returns 0
	if x ≤ edge0, and 1 if x ≥ edge1, with Hermite interpolation */
	float soft_mask = smoothstep(0.2, 0.8, noise(P));  /* noise(P) is a noisy
	value between 0 and 1. soft_mask will vary smoothly between 0 and 1 based
	on noise(P), with a smoother curve than linearstep */

// Splines
	// Splines are smooth curves based on a set of control points

	/* The type of interpolation ranges from "catmull-rom", "bezier",
	"bspline", "hermite", "linear", or "constant" */

	// Spline with knot vector
	float[] knots = {0, 0, 0, 0.25, 0.5, 0.75, 1, 1, 1};
	point[] controls = {point(0),point(1, 2, 1),point(2, 1, 2),point(3, 3, 1)};
	spline curve1 = spline("bezier", 0.5, len(knots), controls);
	// curve1 is a Bezier spline evaluated at u = 0.5

	// Spline with control points
	spline curve2 = spline("catmull-rom", 0.25, point(0, 0, 0), point(1, 2, 1),
	                       point(2, 1, 2), point(3, 3, 1));
	// curve2 is a Catmull-Rom spline evaluated at u = 0.25

	// Constant spline with a single float value
	float value = 10;
	u = 0.1;
	spline curve5 = spline("constant", u, value);
	// curve5 is a constant spline with value 10 evaluated at u = 0.1

	// Hermite spline with point and vector controls
	point q0 = point(0, 0, 0), q1 = point(3, 3, 3);
	vector t0 = vector(1, 0, 0), t1 = vector(-1, 1, 1);
	u = 0.75;
	spline curve3 = spline("hermite", u, q0, t0, q1, t1);
	// curve3 is a Hermite spline evaluated at u = 0.75

	// Linear spline with float controls
	float f0 = 0, f1 = 1, f2 = 2, f3 = 3;
	u = 0.4;
	spline curve4 = spline("linear", u, f0, f1, f2, f3);
	// curve4 is a linear spline evaluated at u = 0.4
	
	// InverseSplines also exist
	
	// Inverse spline with control values
	float y0 = 0, y1 = 1, y2 = 2, y3 = 3;
	float v = 1.5;
	float u1 = splineinverse("linear", v, y0, y1, y2, y3);
	// u1 = 0.5 (linear interpolation between y1 and y2)

	// Inverse spline with knot vector
	float[] knots = {0, 0, 0, 0.25, 0.5, 0.75, 1, 1, 1};
	float[] values = {0, 1, 4, 9};
	v = 6;
	float u2 = splineinverse("bezier", v, len(knots), values);
	// u2 = 0.75 (Bezier spline inverse evaluated at v = 6)

	// Inverse spline with constant value
	v = 10;
	float u3 = splineinverse("constant", v, 10);
	// u3 = 0 (since the constant spline always returns 10)

	// Inverse spline with periodic values
	float y4 = 0, y5 = 1, y6 = 0;
	v = 0.5;
	float u4 = splineinverse("periodic", v, y4, y5, y6);
	// u4 = 0.75 (periodic spline inverse evaluated at v = 0.5)



// Calculus Operators
	// Partial derivative of f with respect to x, y and z using Dx, Dy, Dz
	float a = 3.14;
	float dx = Dx(a); // partial derivative of a with respect to x

	point p = point(1.0, 2.0, 3.0);
	vector dp_dx = Dx(p); // partial derivative of p with respect to x

	vector dv_dy = Dy(N); // partial derivative of normal with respect to y

	color c = color(0.5, 0.2, 0.8);
	color dc_dz = Dz(c); // partial derivative of c with respect to z
	

	float area (point p) // gives the surface area at the position p 

	float filterwidth (float x) // gives the changes of x in adjacent samples

// Texture Functions
	// lookup for a texture at coordinates (x,y)
	color col1 = texture("texture.png", 0.5, 0.2);
	// Lookup color at (0.5, 0.2) in texture.png

	// 3D lookup for a texture at coordinates (x,y)
	color col3 = texture3d("texture3d.vdb", point(0.25, 0.5, 0.75));

	// parameters are ("blur","width","wrap","fill","alpha","interp", ...)
	color col2 = texture("texture.png",1.0,0.75,"blur",0.1,"wrap", "periodic");
	// Lookup color at (1.0, 0.75) with blur 0.1 and periodic wrap mode
	
// Light Functions

	float surfacearea (); // Returns the surface area of area light covers
	int backfacing (); // Outputs 1 if the normals are backfaced, else 0
	int raytype (string name); // returns 1 if the ray is a particular raytype

	// Tracing a ray from a position in a direction
	point pos = point(0, 0, 0); // Starting position of the ray
	vector dir = vector(0, 0, 1); // Direction of the ray
	int hit = trace(pos, dir); // returns 1 if it hits, else 0
```

### Further reading

* [Blender Docs for OSL](https://docs.blender.org/manual/en/latest/render/shader_nodes/osl.html)
* [C4D Docs for OSL](https://docs.otoy.com/cinema4d//OpenShadingLanguageOSL.html)
* Open Shading Language on [GitHub](https://github.com/AcademySoftwareFoundation/OpenShadingLanguage)
* [Official OSL Documentation](https://open-shading-language.readthedocs.io/en/main/)