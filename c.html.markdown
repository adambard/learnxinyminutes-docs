---
language: c
author: Adam Bard
author_url: http://adambard.com/
---

Ah, C. Still the language of modern high-performance computing.

C is the lowest-level language most programmers will ever use, but
it more than makes up for it with raw speed. Just be aware of its manual
memory management and C will take you as far as you need to go.

```c
// Single-line comments start with //
/*
Multi-line comments look like this.
*/

// Import headers with #include
#include <stdlib.h>
#include <stdio.h>

// Declare function signatures in advance in a .h file, or at the top of
// your .c file.
void function_1();
void function_2();

// Your program's entry point is a function called
// main with an integer return type.
int main(){

// print output using printf, for "print formatted"
// %d is an integer, \n is a newline
printf("%d\n", 0); // => Prints 0
// All statements must end with a semicolon

///////////////////////////////////////
// Types
///////////////////////////////////////

// Variables must always be declared with a type.

// 32-bit integer
int x_int = 0;

// 16-bit integer
short x_short = 0;

// 8-bit integer, aka 1 byte
char x_char = 0;
char y_char = 'y'; // Char literals are quoted with ''

long x_long = 0; // Still 32 bytes for historical reasons
long long x_long_long = 0; // Guaranteed to be at least 64 bytes

// 32-bit floating-point decimal
float x_float = 0.0;

// 64-bit floating-point decimal
double x_double = 0.0;

// Integer types may be unsigned
unsigned char ux_char;
unsigned short ux_short;
unsigned int ux_int;
unsigned long long ux_long_long;

// Arrays must be initialized with a concrete size.
char my_char_array[20]; // This array occupies 1 * 20 = 20 bytes
int my_int_array[20]; // This array occupies 4 * 20 = 80 bytes


// You can initialize an array to 0 thusly:
char my_array[20] = {0};

// Indexing an array is like other languages -- or,
// rather, other languages are like C
my_array[0]; // => 0

// Arrays are mutable; it's just memory!
my_array[1] = 2;
printf("%d\n", my_array[1]); // => 2

// Strings are just lists of chars terminated by a null (0x00) byte.
char a_string[20] = "This is a string";

/*
You may have noticed that a_string is only 16 chars long.
Char #17 is a null byte, 0x00 aka \0. 
Chars #18, 19 and 20 have undefined values.
*/

printf("%d\n", a_string[16]);

///////////////////////////////////////
// Operators
///////////////////////////////////////

int i1 = 1, i2 = 2; // Shorthand for multiple declaration
float f1 = 1.0, f2 = 2.0;

// Arithmetic is straightforward
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5, but truncated towards 0)

f1 / f2; // => 0.5, plus or minus epsilon

// Modulo is there as well
11 % 3; // => 2

// Comparison operators are probably familiar, but
// there is no boolean type in c. We use ints instead.
// 0 is false, anything else is true
3 == 2; // => 0 (false)
3 != 2; // => 1 (true)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// Logic works on ints
!3; // => 0 (Logical not)
!0; // => 1
1 && 1; // => 1 (Logical and)
0 && 1; // => 0
0 || 1; // => 1 (Logical or)
0 || 0; // => 0

// Bitwise operators!
~0x0F; // => 0xF0 (bitwise negation)
0x0F & 0xF0; // => 0x00 (bitwise AND)
0x0F | 0xF0; // => 0xFF (bitwise OR)
0x04 ^ 0x0F; // => 0x0B (bitwise XOR)
0x01 << 1; // => 0x02 (bitwise left shift (by 1))
0x02 >> 1; // => 0x01 (bitwise right shift (by 1))

///////////////////////////////////////
// Control Structures
///////////////////////////////////////

if(0){
  printf("I am never run\n");
}else if(0){
  printf("I am also never run\n");
}else{
  printf("I print\n");
}

// While loops exist
int ii = 0;
while(ii < 10){
    printf("%d, ", ii++); // ii++ increments ii in-place, after using its value.
} // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

int kk = 0;
do{
    printf("%d, ", kk);
}while(++kk < 10); // ++kk increments kk in-place, before using its value
// => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// For loops too
int jj;
for(jj=0; jj < 10; jj++){
    printf("%d, ", jj);
} // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

///////////////////////////////////////
// Typecasting
///////////////////////////////////////

// Everything in C is stored somewhere in memory. You can change
// the type of a variable to choose how to read its data

int x_hex = 0x01; // You can assign vars with hex literals

// Casting between types will attempt to preserve their numeric values
printf("%d\n", x_hex); // => Prints 1
printf("%d\n", (short) x_hex); // => Prints 1
printf("%d\n", (char) x_hex); // => Prints 1

// Types will overflow without warning
printf("%d\n", (char) 257); // => 1 (Max char = 255)
printf("%d\n", (short) 65537); // => 1 (Max short = 65535)

///////////////////////////////////////
// Pointers
///////////////////////////////////////

// You can retrieve the memory address of your variables,
// then mess with them.

int x = 0;
printf("%p\n", &x); // Use & to retrive the address of a variable
// (%p formats a pointer)
// => Prints some address in memory;

int x_array[20]; // Arrays are a good way to allocate a contiguous block of memory
int xx;
for(xx=0; xx<20; xx++){
    x_array[xx] = 20 - xx;
} // Initialize x_array to 20, 19, 18,... 2, 1

// Pointer types end with *
int* x_ptr = x_array;
// This works because arrays are pointers to their first element.

// Put a * in front to de-reference a pointer and retrieve the value,
// of the same type as the pointer, that the pointer is pointing at.
printf("%d\n", *(x_ptr)); // => Prints 20
printf("%d\n", x_array[0]); // => Prints 20

// Pointers are incremented and decremented based on their type
printf("%d\n", *(x_ptr + 1)); // => Prints 19
printf("%d\n", x_array[1]); // => Prints 19

// Array indexes are such a thin wrapper around pointer
// arithmatic that the following works:
printf("%d\n", 0[x_array]); // => Prints 20;
printf("%d\n", 2[x_array]); // => Prints 18;

// The above is equivalent to:
printf("%d\n", *(0 + x_ptr));
printf("%d\n", *(2 + x_ptr));

// You can give a pointer a block of memory to use with malloc
int* my_ptr = (int*) malloc(sizeof(int) * 20);
for(xx=0; xx<20; xx++){
    *(my_ptr + xx) = 20 - xx;
} // Initialize memory to 20, 19, 18, 17... 2, 1 (as ints)

// Dereferencing memory that you haven't allocated gives
// unpredictable results
printf("%d\n", *(my_ptr + 21)); // => Prints who-knows-what?

// When you're done with a malloc'd block, you need to free it
free(my_ptr);

// Strings can be char arrays, but are usually represented as char
// pointers:
char* my_str = "This is my very own string";

printf("%d\n", *my_str); // 84 (The ascii value of 'T')

function_1();
} // end main function

///////////////////////////////////////
// Functions
///////////////////////////////////////

// Function declaration syntax:
// <return type> <function name>(<args>)

int add_two_ints(int x1, int x2){
    return x1 + x2; // Use return a return a value
}

/*
Pointers are passed-by-reference (duh), so functions
can mutate their values.

Example: in-place string reversal
*/

// A void function returns no value
void str_reverse(char* str_in){
    char tmp;
    int ii=0, len = strlen(str_in); // Strlen is part of the c standard library
    for(ii=0; ii<len/2; ii++){
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // ii-th char from end
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/

///////////////////////////////////////
// User-defined types and structs
///////////////////////////////////////

// Typedefs can be used to create type aliases
typedef int my_type;
my_type my_type_var = 0;

// Structs are just collections of data
struct rectangle {
    int width;
    int height;
};


void function_1(){

    struct rectangle my_rec;

    // Access struct members with .
    my_rec.width = 10;
    my_rec.height = 20;

    // You can declare pointers to structs
    struct rectangle* my_rec_ptr = &my_rec;

    // Use dereferencing to set struct pointer members...
    (*my_rec_ptr).width = 30;

    // ... or use the -> shorthand
    my_rec_ptr->height = 10; // Same as (*my_rec_ptr).height = 10;
}

// You can apply a typedef to a struct for convenience
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

```

## Further Reading

Best to find yourself a copy of [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language)

Another good resource is [Learn C the hard way](http://c.learncodethehardway.org/book/)

Other than that, Google is your friend.
