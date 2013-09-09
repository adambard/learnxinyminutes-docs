---
language: c
filename: learnc.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
    - ["Árpád Goretity", "http://twitter.com/H2CO3_iOS"]

---

Ah, C. Still **the** language of modern high-performance computing.

C is the lowest-level language most programmers will ever use, but
it more than makes up for it with raw speed. Just be aware of its manual
memory management and C will take you as far as you need to go.

```c
// Single-line comments start with // - only available in C99 and later.

/*
Multi-line comments look like this. They work in C89 as well.
*/

// Import headers with #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// (File names between <angle brackets> are headers from the C standard library.)
// For your own headers, use double quotes instead of angle brackets:
#include "my_header.h"

// Declare function signatures in advance in a .h file, or at the top of
// your .c file.
void function_1();
void function_2();

// Your program's entry point is a function called
// main with an integer return type.
int main() {
    // print output using printf, for "print formatted"
    // %d is an integer, \n is a newline
    printf("%d\n", 0); // => Prints 0
    // All statements must end with a semicolon

    ///////////////////////////////////////
    // Types
    ///////////////////////////////////////

    // ints are usually 4 bytes 
    int x_int = 0;

    // shorts are usually 2 bytes
    short x_short = 0;

    // chars are guaranteed to be 1 byte
    char x_char = 0;
    char y_char = 'y'; // Char literals are quoted with ''

    // longs are often 4 to 8 bytes; long longs are guaranteed to be at least
    // 64 bits
    long x_long = 0;
    long long x_long_long = 0; 

    // floats are usually 32-bit floating point numbers
    float x_float = 0.0;

    // doubles are usually 64-bit floating-point numbers
    double x_double = 0.0;

    // Integral types may be unsigned.
    unsigned short ux_short;
    unsigned int ux_int;
    unsigned long long ux_long_long;

    // sizeof(T) gives you the size of a variable with type T in bytes
    // sizeof(obj) yields the size of the expression (variable, literal, etc.).
    printf("%zu\n", sizeof(int)); // => 4 (on most machines with 4-byte words)


    // If the argument of the `sizeof` operator an expression, then its argument
    // is not evaluated (except VLAs (see below)).
    // The value it yields in this case is a compile-time constant.
    int a = 1;
    // size_t is an unsiged integer type of at least 2 bytes used to represent the size of an object.
    size_t size = sizeof(a++); // a++ is not evaluated
    printf("sizeof(a++) = %zu where a = %d\n", size, a);
    // prints "sizeof(a++) = 4 where a = 1" (on a 32-bit architecture)

    // Arrays must be initialized with a concrete size.
    char my_char_array[20]; // This array occupies 1 * 20 = 20 bytes
    int my_int_array[20]; // This array occupies 4 * 20 = 80 bytes
                          // (assuming 4-byte words)


    // You can initialize an array to 0 thusly:
    char my_array[20] = {0};

    // Indexing an array is like other languages -- or,
    // rather, other languages are like C
    my_array[0]; // => 0

    // Arrays are mutable; it's just memory!
    my_array[1] = 2;
    printf("%d\n", my_array[1]); // => 2

    // In C99 (and as an optional feature in C11), variable-length arrays (VLAs)
    // can be declared as well. The size of such an array need not be a compile
    // time constant:
    printf("Enter the array size: "); // ask the user for an array size
    char buf[0x100];
    fgets(buf, sizeof buf, stdin);

    // strtoul parses a string to an unsigned integer
    size_t size = strtoul(buf, NULL, 10);
    int var_length_array[size]; // declare the VLA
    printf("sizeof array = %zu\n", sizeof var_length_array);

    // A possible outcome of this program may be:
    // > Enter the array size: 10
    // > sizeof array = 40

    // Strings are just arrays of chars terminated by a NUL (0x00) byte,
    // represented in strings as the special character '\0'.
    // (We don't have to include the NUL byte in string literals; the compiler
    //  inserts it at the end of the array for us.)
    char a_string[20] = "This is a string";
    printf("%s\n", a_string); // %s formats a string

    printf("%d\n", a_string[16]); // => 0
    // i.e., byte #17 is 0 (as are 18, 19, and 20)

    // If we have characters between single quotes, that's a character literal.
    // It's of type `int`, and *not* `char` (for historical reasons).
    int cha = 'a'; // fine
    char chb = 'a'; // fine too (implicit conversion from int to char)

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
    // Floating-point numbers and calculations are not exact

    // Modulo is there as well
    11 % 3; // => 2

    // Comparison operators are probably familiar, but
    // there is no boolean type in c. We use ints instead.
    // (Or _Bool or bool in C99.)
    // 0 is false, anything else is true. (The comparison 
    // operators always yield 0 or 1.)
    3 == 2; // => 0 (false)
    3 != 2; // => 1 (true)
    3 > 2; // => 1
    3 < 2; // => 0
    2 <= 2; // => 1
    2 >= 2; // => 1

    // C is not Python - comparisons don't chain.
    int a = 1;
    // WRONG:
    int between_0_and_2 = 0 < a < 2;
    // Correct:
    int between_0_and_2 = 0 < a && a < 2;

    // Logic works on ints
    !3; // => 0 (Logical not)
    !0; // => 1
    1 && 1; // => 1 (Logical and)
    0 && 1; // => 0
    0 || 1; // => 1 (Logical or)
    0 || 0; // => 0

    // Bitwise operators!
    ~0x0F; // => 0xF0 (bitwise negation, "1's complement")
    0x0F & 0xF0; // => 0x00 (bitwise AND)
    0x0F | 0xF0; // => 0xFF (bitwise OR)
    0x04 ^ 0x0F; // => 0x0B (bitwise XOR)
    0x01 << 1; // => 0x02 (bitwise left shift (by 1))
    0x02 >> 1; // => 0x01 (bitwise right shift (by 1))

    // Be careful when shifting signed integers - the following are undefined:
    // - shifting into the sign bit of a signed integer (int a = 1 << 32)
    // - left-shifting a negative number (int a = -1 << 2)
    // - shifting by an offset which is >= the width of the type of the LHS:
    //   int a = 1 << 32; // UB if int is 32 bits wide

    ///////////////////////////////////////
    // Control Structures
    ///////////////////////////////////////

    if (0) {
      printf("I am never run\n");
    } else if (0) {
      printf("I am also never run\n");
    } else {
      printf("I print\n");
    }

    // While loops exist
    int ii = 0;
    while (ii < 10) {
        printf("%d, ", ii++); // ii++ increments ii in-place
                              // after yielding its value ("postincrement").
    } // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    int kk = 0;
    do {
        printf("%d, ", kk);
    } while (++kk < 10); // ++kk increments kk in-place, and yields
                         // the already incremented value ("preincrement")
    // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // For loops too
    int jj;
    for (jj=0; jj < 10; jj++) {
        printf("%d, ", jj);
    } // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // branching with multiple choices: switch()
    switch (some_integral_expression) {
    case 0: // labels need to be integral *constant* epxressions
        do_stuff();
        break; // if you don't break, control flow falls over labels
    case 1:
        do_something_else();
        break;
    default:
        // if `some_integral_expression` didn't match any of the labels
        fputs("error!\n", stderr);
        exit(-1);
        break;
    }
    

    ///////////////////////////////////////
    // Typecasting
    ///////////////////////////////////////

    // Every value in C has a type, but you can cast one value into another type
    // if you want (with some constraints).

    int x_hex = 0x01; // You can assign vars with hex literals

    // Casting between types will attempt to preserve their numeric values
    printf("%d\n", x_hex); // => Prints 1
    printf("%d\n", (short) x_hex); // => Prints 1
    printf("%d\n", (char) x_hex); // => Prints 1

    // Types will overflow without warning
    printf("%d\n", (unsigned char) 257); // => 1 (Max char = 255 if char is 8 bits long)

    // For determining the max value of a `char`, a `signed char` and an `unisigned char`,
    // respectively, use the CHAR_MAX, SCHAR_MAX and UCHAR_MAX macros from <limits.h>

    // Integral types can be cast to floating-point types, and vice-versa.
    printf("%f\n", (float)100); // %f formats a float
    printf("%lf\n", (double)100); // %lf formats a double
    printf("%d\n", (char)100.0);

    ///////////////////////////////////////
    // Pointers
    ///////////////////////////////////////

    // A pointer is a variable declared to store a memory address. Its declaration will
    // also tell you the type of data it points to. You can retrieve the memory address 
    // of your variables, then mess with them.

    int x = 0;
    printf("%p\n", (void *)&x); // Use & to retrieve the address of a variable
    // (%p formats an object pointer of type void *)
    // => Prints some address in memory;


    // Pointers start with * in their declaration
    int *px, not_a_pointer; // px is a pointer to an int
    px = &x; // Stores the address of x in px
    printf("%p\n", (void *)px); // => Prints some address in memory
    printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
    // => Prints "8, 4" on a typical 64-bit system

    // To retrieve the value at the address a pointer is pointing to,
    // put * in front to de-reference it.
    // Note: yes, it may be confusing that '*' is used for _both_ declaring a
    // pointer and dereferencing it.
    printf("%d\n", *px); // => Prints 0, the value of x

    // You can also change the value the pointer is pointing to.
    // We'll have to wrap the de-reference in parenthesis because
    // ++ has a higher precedence than *.
    (*px)++; // Increment the value px is pointing to by 1
    printf("%d\n", *px); // => Prints 1
    printf("%d\n", x); // => Prints 1

    // Arrays are a good way to allocate a contiguous block of memory
    int x_array[20];
    int xx;
    for (xx = 0; xx < 20; xx++) {
        x_array[xx] = 20 - xx;
    } // Initialize x_array to 20, 19, 18,... 2, 1

    // Declare a pointer of type int and initialize it to point to x_array
    int* x_ptr = x_array;
    // x_ptr now points to the first element in the array (the integer 20). 
    // This works because arrays often decay into pointers to their first element.
    // For example, when an array is passed to a function or is assigned to a pointer,
    // it decays into (implicitly converted to) a pointer.
    // Exceptions: when the array is the argument of the `&` (address-od) operator:
    int arr[10];
    int (*ptr_to_arr)[10] = &arr; // &arr is NOT of type `int *`!
                                  // It's of type "pointer to array" (of ten `int`s).
    // or when the array is a string literal used for initializing a char array:
    char arr[] = "foobarbazquirk";
    // or when it's the argument of the `sizeof` or `alignof` operator:
    int arr[10];
    int *ptr = arr; // equivalent with int *ptr = &arr[0];
    printf("%zu %zu\n", sizeof arr, sizeof ptr); // probably prints "40, 4" or "40, 8"


    // Pointers are incremented and decremented based on their type
    // (this is called pointer arithmetic)
    printf("%d\n", *(x_ptr + 1)); // => Prints 19
    printf("%d\n", x_array[1]); // => Prints 19

    // You can also dynamically allocate contiguous blocks of memory with the
    // standard library function malloc, which takes one argument of type size_t
    // representing the number of bytes to allocate (usually from the heap, although this
    // may not be true on e. g. embedded systems - the C standard says nothing about it).
    int *my_ptr = malloc(sizeof(*my_ptr) * 20);
    for (xx = 0; xx < 20; xx++) {
        *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
    } // Initialize memory to 20, 19, 18, 17... 2, 1 (as ints)

    // Dereferencing memory that you haven't allocated gives
    // "unpredictable results" - the program is said to invoke "undefined behavior"
    printf("%d\n", *(my_ptr + 21)); // => Prints who-knows-what? It may even crash.

    // When you're done with a malloc'd block of memory, you need to free it, 
    // or else no one else can use it until your program terminates
    // (this is called a "memory leak"):
    free(my_ptr);

    // Strings are arrays of char, but they are usually represented as a
    // pointer-to-char (which is a pointer to the first element of the array).
    // It's good practice to use `const char *' when referring to a string literal,
    // since string literals shall not be modified (i. e. "foo"[0] = 'a' is ILLEGAL.)
    const char *my_str = "This is my very own string literal";
    printf("%c\n", *my_str); // => 'T'

    // This is not the case if the string is an array
    // (potentially initialized with a string literal)
    // that resides in writable memory, as in:
    char foo[] = "foo";
    foo[0] = 'a'; // this is legal, foo now contains "aoo"

    function_1();
} // end main function

///////////////////////////////////////
// Functions
///////////////////////////////////////

// Function declaration syntax:
// <return type> <function name>(<args>)

int add_two_ints(int x1, int x2)
{
    return x1 + x2; // Use return to return a value
}

/*
Functions are pass-by-value, but you can make your own references
with pointers so functions can mutate their values.

Example: in-place string reversal
*/

// A void function returns no value
void str_reverse(char *str_in)
{
    char tmp;
    int ii = 0;
    size_t len = strlen(str_in); // `strlen()` is part of the c standard library
    for (ii = 0; ii < len / 2; ii++) {
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

// Structs are just collections of data, the members are allocated sequentially,
// in the order they are written:
struct rectangle {
    int width;
    int height;
};

// It's not generally true that
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// due to potential padding between the structure members (this is for alignment
// reasons). [1]

void function_1()
{
    struct rectangle my_rec;

    // Access struct members with .
    my_rec.width = 10;
    my_rec.height = 20;

    // You can declare pointers to structs
    struct rectangle *my_rec_ptr = &my_rec;

    // Use dereferencing to set struct pointer members...
    (*my_rec_ptr).width = 30;

    // ... or even better: prefer the -> shorthand for the sake of readability
    my_rec_ptr->height = 10; // Same as (*my_rec_ptr).height = 10;
}

// You can apply a typedef to a struct for convenience
typedef struct rectangle rect;

int area(rect r)
{
    return r.width * r.height;
}

// if you have large structs, you can pass them "by pointer" to avoid copying
// the whole struct:
int area(const rect *r)
{
    return r->width * r->height;
}

///////////////////////////////////////
// Function pointers 
///////////////////////////////////////
/*
At runtime, functions are located at known memory addresses. Function pointers are
much like any other pointer (they just store a memory address), but can be used 
to invoke functions directly, and to pass handlers (or callback functions) around.
However, definition syntax may be initially confusing.

Example: use str_reverse from a pointer
*/
void str_reverse_through_pointer(char *str_in) {
    // Define a function pointer variable, named f. 
    void (*f)(char *); // Signature should exactly match the target function.
    f = &str_reverse; // Assign the address for the actual function (determined at runtime)
    // f = str_reverse; would work as well - functions decay into pointers, similar to arrays
    (*f)(str_in); // Just calling the function through the pointer
    // f(str_in); // That's an alternative but equally valid syntax for calling it.
}

/*
As long as function signatures match, you can assign any function to the same pointer.
Function pointers are usually typedef'd for simplicity and readability, as follows:
*/

typedef void (*my_fnp_type)(char *);

// Then used when declaring the actual pointer variable:
// ...
// my_fnp_type f; 

```

## Further Reading

Best to find yourself a copy of [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language)
It is *the* book about C, written by the creators of C. Be careful, though - it's ancient and it contains some
inaccuracies (well, ideas that are not considered good anymore) or now-changed practices.

Another good resource is [Learn C the hard way](http://c.learncodethehardway.org/book/).

If you have a question, read the [compl.lang.c Frequently Asked Questions](http://c-faq.com).

It's very important to use proper spacing, indentation and to be consistent with your coding style in general.
Readable code is better than clever code and fast code. For a good, sane coding style to adopt, see the
[Linux kernel coding stlye](https://www.kernel.org/doc/Documentation/CodingStyle).

Other than that, Google is your friend.

[1] http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member
