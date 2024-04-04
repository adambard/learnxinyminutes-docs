---
name: Sing
category: language
language: Sing
filename: learnsing.sing
contributors:
    - ["Maurizio De Girolami", "https://github.com/mdegirolami"]
---

The purpose of sing is to provide a simple, safe, fast language that 
can be a good replacement for c++ for high performance applications.

Sing is an easy choice because it compiles to human-quality readable c++.

Because of that, if you work for a while with Sing and, at any time, you discover you don't like Sing anymore, you lose nothing of your work
because you are left with nice and clean c++ code. 

In some way you can also think Sing as a tool to write c++ in a way that enforces some best practices.

```go
/* Multi- line comment. 
    /* It can be nested */ 
    Use it to remark-out part of the code.
    It leaves no trace in the intermediate c++ code. 
    (sing translates into nice human readable c++)
*/

// Single line comment, can be placed only before a statement or declaration...
// ...or at the right of the first line of a statement or declaration.
// single line comments are kept into c++.
//
// here we declare if we need to use public declarations from other files. 
// (in this case from files 'sio', 'sys')
requires "sio";
requires "sys";

//
// A sing function declaration.
// All the declarations can be made public with the 'public' keyword.
// All the declarations start with a keyword specifying the type of declaration
// (in this case fn for function) then follows the name, the arguments and the
// return type.
//
// Each argument starts with a direction qualifyer (in, out, io) which tells if
// the argument is an input, an output or both...
// ...then follows the argument name and the type.
public fn singmain(in argv [*]string) i32
{
    // print is from the sio file and sends a string to the console
    sio.print("Hello World\n");

    // type conversions are allowed in the form of <newtype>(expression).
    sio.print(string(sum(5, 10)) + "\n");

    // For clarity you can specify after an argument its name separated by ':'.
    var result i32;
    recursive_power(10:base, 3:exponent, result);

    // referred here to avoid a 'not used' error.
    learnTypes();

    // functions can only return a single value of some basic type.
    return(0);
}

// You can have as many arguments as you want, comma separated. 
// You can also omit the 'in' direction qualifyer (it is the default).
fn sum(arg1 i32, arg2 i32) i32
{
    // as 'fn' declares a function, 'let' declares a constant.
    // With constants, if you place an initializer, you can omit the type.
    let the_sum = arg1 + arg2;

    return(the_sum);
}

// Arguments are passed by reference, which means that in the function body you
// use the argument names to refer to the passed variables.
// Example: all the functions in the recursion stack access the same 'result'
// variable, supplied by the singmain function. 
fn recursive_power(base i32, exponent i32, out result i32) void
{
    if (exponent == 0) {
        result = 1;
    } else {
        recursive_power(base, exponent - 1, result);
        result *= base;
    }
}

//**********************************************************
//
// TYPES
//
//**********************************************************
fn learnTypes() void
{
    // the var keyword declares mutable variables 
    // in this case an UTF-8 encoded string
    var my_name string;

    // ints of 8..64 bits size
    var int0 i8; 
    var int1 i16; 
    var int2 i32; 
    var int3 i64; 

    // uints
    var uint0 u8;
    var uint1 u16;
    var uint2 u32;
    var uint3 u64;

    // floats
    var float0 f32;
    var float1 f64;

    // complex
    var cmplx0 c64;
    var cmplx1 c128;

    cmplx0 = 0;
    cmplx1 = 0;

    // and of course...
    var bool0 bool;

    // type inference: by default constants are i32, f32, c64
    let an_int32 = 15;
    let a_float32 = 15.0;
    let a_complex = 15.0 + 3i;
    let a_string = "Hello !";
    let a_bool = false;

    // To create constant of different types use a conversion-like syntax:
    // NOTE: this is NOT a conversion. Just a type specification
    let a_float64 = f64(5.6);

    // in a type definition [] reads as "array of"
    // in the example []i32 => array of i32.
    var intarray []i32 = {1, 2, 3};

    // You can specify a length, else the length is given by the initializer
    // the last initializer is replicated on the extra items
    var sizedarray [10]i32 = {1, 2, 3};

    // Specify * as the size to get a dynamic array (can change its length)
    var dyna_array [*]i32;

    // you can append items to a vector invoking a method-like function on it.
    dyna_array.push_back(an_int32);

    // getting the size of the array. sys.validate() is like assert in c
    sys.validate(dyna_array.size() == 1); 

    // a map that associates a number to a string. 
    // "map(x)..." reads "map with key of type x and value of type..." 
    var a_map map(string)i32;

    a_map.insert("one", 1);
    a_map.insert("two", 2);
    a_map.insert("three", 3);
    let key = "two";

    // note: the second argument of get_safe is the value to be returned 
    // when the key is not found.
    sio.print("\nAnd the value is...: " + string(a_map.get_safe(key, -1)));

    // string concatenation
    my_name = "a" + "b";
}

// an enum type can only have a value from a discrete set. 
// can't be converted to/from int !
enum Stages {first, second, last}

// you can refer to enum values (to assign/compare them)
// specifying both the typename and tagname separated with the '.' operator
var current_stage = Stages.first;


//**********************************************************
//
// POINTERS
//
//**********************************************************

// This is a factory for a dynamic vector.
// In a type declaration '*' reads 'pointer to..'
// so the return type is 'pointer to a vector of i32'
fn vectorFactory(first i32, last i32) *[*]i32
{
    var buffer [*]i32;

    // fill
    for (value in first : last) {
        buffer.push_back(value);
    }

    // The & operator returns the address of the buffer.
    // You can only use & on local variables
    // As you use & on a variable, that variable is allocated on the HEAP.
    return(&buffer);
}

fn usePointers() void
{
    var bufferptr = vectorFactory(0, 100);

    // you don't need to use the factory pattern to use pointers.
    var another_buffer [*]i32;
    var another_bufferptr = &another_buffer;

    // you can dereference a pointer with the * operator
    // sys.validate is an assertion (causes a signal if the argument is false)
    sys.validate((*bufferptr)[0] == 0);

    /* 
    // as all the pointers to a variable exit their scope the variable is
    // no more accessible and is deleted (freed)
    */
}

//**********************************************************
//
// CLASSES
//
//**********************************************************

// This is a Class. The member variables can be directly initialized here
class AClass {
public:
    var public_var = 100;       // same as any other variable declaration  
    fn is_ready() bool;         // same as any other function declaration 
    fn mut finalize() void;     // destructor (called on object deletion)
private:
    var private_var string; 

    // Changes the member variables and must be marked as 'mut' (mutable)
    fn mut private_fun(errmsg string) void;    
}

// How to declare a member function
fn AClass.is_ready() bool
{
    // inside a member function, members can be accessed thrugh the 
    // this keyword and the field selector '.'
    return(this.public_var > 10);
}

fn AClass.private_fun(errmsg string) void
{
    this.private_var = errmsg;
}

// using a class
fn useAClass() void
{
    // in this way you create a variable of type AClass.
    var instance AClass;

    // then you can access its members through the '.' operator.
    if (instance.is_ready()) {
        instance.public_var = 0;
    }
}

//**********************************************************
//
// INTERFACES
//
//**********************************************************

// You can use polymorphism in sing defining an interface...
interface ExampleInterface {
    fn mut eraseAll() void;
    fn identify_myself() void;
} 

// and then creating classes which implement the interface
// NOTE: you don't need (and cannot) re-declare the interface functions
class Implementer1 : ExampleInterface {
private:
    var to_be_erased i32 = 3;
public:    
    var only_on_impl1 = 0;
}

class Implementer2 : ExampleInterface {
private:
    var to_be_erased f32 = 3;
}

fn Implementer1.eraseAll() void
{
    this.to_be_erased = 0;
}

fn Implementer1.identify_myself() void
{
    sio.print("\nI'm the terrible int eraser !!\n");
}

fn Implementer2.eraseAll() void
{
    this.to_be_erased = 0;
}

fn Implementer2.identify_myself() void
{
    sio.print("\nI'm the terrible float eraser !!\n");
}

fn interface_casting() i32
{
    // upcasting is automatic (es: *Implementer1 to *ExampleInterface)
    var concrete Implementer1;
    var if_ptr *ExampleInterface = &concrete; 

    // you can access interface members with (guess what ?) '.'
    if_ptr.identify_myself();

    // downcasting requires a special construct 
    // (see also below the conditional structures)
    typeswitch(ref = if_ptr) {  
        case *Implementer1: return(ref.only_on_impl1);
        case *Implementer2: {}
        default: return(0);
    }

    return(1);
}

// All the loop types
fn loops() void
{
    // while: the condition must be strictly of boolean type
    var idx = 0;
    while (idx < 10) {
        ++idx;
    }

    // for in an integer range. The last value is excluded
    // 'it' is local to the loop and must not be previously declared
    for (it in 0 : 10) {
    }

    // reverse direction
    for (it in 10 : 0) {
    }

    // configurable step. The loop stops when it's >= the final value
    for (it in 0 : 100 step 3) {
    }

    // with an auxiliary counter. 
    // The counter start always at 0 and increments by one at each iteration
    for (counter, it in 3450 : 100 step -22) {
    } 

    // value assumes in turn all the values from array
    var array [*]i32 = {0, 10, 100, 1000};
    for (value in array) {
    }

    // as before with auxiliary counter
    for (counter, value in array) {
    }
}

// All the conditional structures
interface intface {}
class c0_test : intface {public: fn c0stuff() void;}
class delegating : intface {}

fn conditionals(in object intface, in objptr *intface) void
{
    let condition1 = true;
    let condition2 = true;
    let condition3 = true;
    var value = 30;

    // condition1 must be a boolean.
    if (condition1) {
        ++value;    // conditioned statement
    } 

    // you can chain conditions with else if
    if (condition1) {
        ++value;
    } else if (condition2) {
        --value;
    } 

    // a final else runs if any other condition is false
    if (condition1) {
        ++value;
    } else if (condition2) {
        --value;
    } else {
        value = 0;
    }

    // based on the switch value selects a case statement
    switch (value) {
        case 0: sio.print("value is zero"); // a single statement !
        case 1: {}                          // do nothing
        case 2:                             // falls through
        case 3: sio.print("value is more than one");
        case 4: {                           // a block is a single statement !
            value = 0;
            sio.print("how big !!");
        }
        default: return;                    // if no one else matches
    }

    // similar to a switch but selects a case based on argument type.
    // - object must be a function argument of type interface.
    // - the case types must be classes implementing the object interface.
    // - in each case statement, ref assumes the class type of that case.
    typeswitch(ref = object) {
        case c0_test: ref.c0stuff();
        case delegating: {}
        default: return;
    }

    // - object must be an interface pointer.
    // - the case types must be pointers to classes implementing the objptr interface.
    // - in each case statement, ref assumes the class pointer type of that case.
    typeswitch(ref = objptr) {
        case *c0_test: {
            ref.c0stuff();
            return;
        }
        case *delegating: {}
        default: sio.print("unknown pointer type !!");
    } 
}
```

## Further Reading

[official Sing web site](https://mdegirolami.wixsite.com/singlang).

If you want to play with sing you are recommended to download the vscode plugin. Please
follow the instructions at [Getting Started](https://mdegirolami.wixsite.com/singlang/copy-of-interfacing-sing-and-c-2)
