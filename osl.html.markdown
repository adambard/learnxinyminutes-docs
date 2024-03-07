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
dothis();

///////////////
// 1. Basics //
///////////////
 
float num = 3.00;  // Scalar floating-point data (numbers)
int num = 3; // Integer data

// Math works as you would expect
3 + 1;   // 4
74 - 3;   // 71
20 * 2; // 40
75/3;  // 25.0

// And modulo division only works with integers
10 % 2; // 0
31 % 4; // 1

// Bitwise operations only works with integers
- 0 // 1 (Negation)
~ 00100011 // 11011100 (bitwise Compliment)
1 << 2; // 4 (shift Left)
12 >> 1; // 3 (shift Right)
1 & 0; // 0 (bitwise AND)
1 | 0; // 1 (bitwise OR)
1 ^ 1; // 0 (bitwise XOR)

// We also have booleans
true;
false;

// Negation uses the ~ symbol
~true; // false
~false; // true

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

// Shader (similar to main function in C)
// The inputs should be initialized
shader multiply(float a = 0.0, float b = 0.0, output float c = 0.0){
    c = a*b;
}

// Functions can be used inside shader.

/////////////////////////
// 2. Global Variables //
/////////////////////////

// Contains information that the renderer knows about the point being shaded
//These variables need not be declared

point P // Position of the point you are shading
vector I // Incident ray direction from viewing position to shading position
normal N
normal Ng
float u
float v
vector dPdu
vector dPdv
point Ps
float time 
float dtime
vector dPdtime
closure color Ci

/////////////////////
// 4. Control flow //
/////////////////////

// Conditionals in OSL are just like in C or C++.

// If/Else
if (5>2){
	x = s;
	l = x;
}
else{
	x = s + l; 
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