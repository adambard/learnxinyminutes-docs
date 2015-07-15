---
language: chapel
filename: learnchapel.chpl
contributors:
    - ["Ian J. Bertolacci", "http://www.cs.colostate.edu/~ibertola/"]
lang: en
---
What is Chapel?
===============
You can read all about chapel at [Cray's official Chapel website](http://chapel.cray.com).
In short, Chapel is an open-source, high-productivity, parallel-programming language in development at Cray Inc., and is designed to run on multi-core PCs as well as multi-kilocore supercomputers.

Your input, questions, and discoveries are important to the developers!
-----------------------------------------------------------------------
Chapel is currently in-development so there are occasional hiccups with performance and language features.
The more information you give the Chapel development team about issues you encounter with the language, the better the language gets.
Feel free to email the team and other developers through the [sourceforge email lists](https://sourceforge.net/p/chapel/mailman).

If you're really interested in the development of the compiler or contributing to the project, 
[check out the master Github repository](https://github.com/chapel-lang/chapel).

Installing the Compiler
-----------------------
Chapel can be built and installed on your average 'nix machine (and cygwin).
[Download the latest release version](https://github.com/chapel-lang/chapel/releases/)
and its as easy as 
 1. ```tar -xvf chapel-1.11.0.tar.gz```
 2. ```cd chapel-1.11.0```
 3. ```make```
 4. ```source util/setchplenv.bash # or .sh or .csh or .fish```

You will need to ```source util/setchplenv.EXT``` from the chapel directory every time your terminal starts so its suggested that you drop that command in a script that will get executed on startup (like .bashrc).

Chapel is easily installed with Brew for OS X
 1. ```brew update```
 2. ```brew install chapel```

Who is this tutorial for?
-------------------------
This tutorial is for people who want to learn the ropes of chapel without having to hear about what fiber mixture the ropes are, or how they were braided, or how the braid configurations differ between one another.
It won't teach you how to develop amazingly performant code, and it's not exhaustive. 
Refer to the [language specification](http://chapel.cray.com/language.html) and the [library documentation](http://chapel.cray.com/docs/latest/) for more details.

Occasionally check here back to see if more topics have been added.

```chapel
// Comments are C-family style
// one line comment
/*
  multi-line comment
*/

// Basic printing
write( "Hello, " );
writeln( "World!" );
// write and writeln can take a list of things to print.
// each thing is printed right next to each other, so include your spacing!
writeln( "There are ", 3, " commas (\",\") in this line of code" );
// Different output channels
stdout.writeln( "This goes to standard output (just like plain writeln() does)");
stderr.writeln( "This goes to standard error" );

// Variables
// Variables dont have to be explicitly typed as long as 
// the compiler can figure out the type that it will hold.
var myVar = 10; // 10 is an int, so myVar is implicitly an int
myVar = -10;
// var anError; // this would be a compile time error.

// We can (and should) explicitly type things
var mySecondVar: real; // define mySecondVar as a real
var myThirdVar: real = -1.234;
mySecondVar = myThirdVar;

// There are a number of basic types.
var myInt: int = -1000; // signed ints
var myUint: uint = 1234; // unsigned ints
var myReal: real = 9.876; // floating point numbers
var myImag: imag = 5.0i; // imaginary numbers
var myCplx: complex = 10 + 9i; // complex numbers
myCplx = myInt + myImag ; // another way to form complex numbers
var myBool: bool = false; // booleans
var myStr: string = "Some string..."; // strings

// Some types can have sizes
var my8Int: int(8) = 10; // 8 bit (one byte) sized int;
var my64Real: real(64) = 1.516; // 64 bit (8 bytes) sized real

// Typecasting
var intFromReal = myReal : int;
var intFromReal2: int = myReal : int;

// consts are constants, they cannot be changed after set in runtime
const almostPi: real = 11.0/7.0;
// params are constants whose value must be known statically at compile time
// Like consts, they cannot be changed during runtime
param compileTimeConst: int = 16;

// The config modifier allows values to be set at the command line
// and is much easier that the usual getOpts debacle 
// config vars and consts can be changed through the command line at run time
config var varCmdLineArg: int = -123; 
config const constCmdLineArg: int = 777;
// set with --VarName=Value or --VarName Value at run time

// config params can be set at compile time
config param paramCmdLineArg: bool = false;
writeln( varCmdLineArg, ", ", constCmdLineArg, ", ", paramCmdLineArg );
//set config with --set paramCmdLineArg=value at compile time

// Math operators
var a: int, thisInt = 1234, thatInt = 5678;
a = thisInt + thatInt;  // Addition
a = thisInt * thatInt;  // Multiplication
a = thisInt - thatInt;  // Subtraction
a = thisInt / thatInt;  // division
a = thisInt ** thatInt; // exponentiation
a = thisInt % thatInt;  // remainder (modulo)

// Logical Operators
var b: bool, thisBool = false, thatBool = true;
b = thisBool && thatBool; // logical and
b = thisBool || thatBool; // logical or
b = !thisBool;            // logical negation

// Relational Operators
b = thisInt > thatInt;           // greater-than
b = thisInt >= thatInt;          // greater-than-or-equal-to
b = thisInt < a && a <= thatInt; // less-than, and, less-than-or-equal-to
b = thisInt != thatInt;          // not-equal-to
b = thisInt == thatInt;          // equal-to

// Bitwise operations
a = thisInt << 10;     // left-bit-shift by 10 bits;
a = thatInt >> 5;      // right-bit-shift by 5 bits;
a = ~thisInt;          // bitwise-negation
a = thisInt ^ thatInt; // bitwise exclusive-or

// Compound assignment operations
a += thisInt;          // addition-equals ( a = a + thisInt;)
a *= thatInt;          // times-equals ( a = a * thatInt; )
b &&= thatBool;        // logical-and-equals ( b = b && thatBool; )
a <<= 3;               // left-bit-shift-equals ( a = a << 10; )
// and so on...
// Unlike other C family languages there are no 
// pre/post-increment/decrement operators like
//  ++j, --j, j++, j-- 

// Swap operator
var old_this = thisInt;
var old_that = thatInt;
thisInt <=> thatInt; // Swap the values of thisInt and thatInt
writeln( (old_this == thatInt) && (old_that == thisInt) );

// We can also define operator overloads, 
// which we'll cover with procedures.

// Tuples
// tuples can be of the same type
var sameTup: 2*int = (10,-1);
// or different types
var diffTup: (int,real,complex) = (5, 1.928, myCplx);
// Accessed using array bracket notation
// However, tuples are all 1-indexed
writeln( "(", sameTup[1], ",", sameTup[2], ")" );
writeln( diffTup );
// Tuples can also be written into.
diffTup[1] = -1;
// you can expand tuples as well
var (tupInt, tupReal, tupCplx) = diffTup;
writeln( diffTup == (tupInt, tupReal, tupCplx) );
// Can also be used to easily write a collection 
// of variables as a list (common in debugging)
writeln( (a,b,thisInt,thatInt,thisBool,thatBool) );


// Type aliasing
type chroma = int;        // type of a single hue
type RGBColor = 3*chroma; // type representing a full color 
var black: RGBColor = ( 0,0,0 );
var white: RGBColor = ( 255, 255, 255 );



// If-Then statements
// if-thens dont require parentheses around the condition 
// as they do in C (however, we will use them)
// A single line body can use the 'then' keyword instead of 
// braces and else statements can be written similarly

if 10 < 100 then
  writeln( "All is well" );

if -1 < 1 then
  writeln( "Continuing to believe reality" );
else
  writeln( "Send mathematician, something's wrong" );


if ( 10 > 100 ) {
  writeln( "Universe broken. Please reboot universe." );
}

if ( a % 2 == 0 ) {
  writeln( a, " is even." );
} else {
  writeln( a, " is odd." );
}

if ( a % 3 == 0 ) {
  writeln( a, " is even divisible by 3." );
} else if ( a % 3 == 1 ){
  writeln( a, " is divided by 3 with a remainder of 1." );
} else {
  writeln( b, " is divided by 3 with a remainder of 2." );
}

// Ternary: if-then-else in a statement
var maximum = if ( thisInt < thatInt ) then thatInt else thisInt;

// Select statements are much like switch statements in other languages
// However, Select statements dont cascade like in C or Java
var inputOption = "anOption";
select( inputOption ){
  when "anOption" do writeln( "Chose 'anOption'" );
  when "otherOption" {
    writeln( "Chose 'otherOption'" );
    writeln( "Which has a body" );
  }
  otherwise { 
    writeln( "Any other Input" );
    writeln( "the otherwise case doesn't need a do if the body is one line" );
    writeln( "Oh, and when statements dont cascade like the case statements" );
    writeln( "of other languages" );
  }
}

// While loops and Do-While loops are basically the same in every language.
var j: int = 1;
var jSum: int = 0;
while( j <= 1000 ){
  jSum += j;
  j += 1; // there are no ++j, --j, j++, j--, operators
}
writeln( jSum );

// basic Do-While loop
do{
  jSum += j;
  j += 1;
}while( j <= 10000 );
writeln( jSum );


// For loops are much like those in python in that they iterate over a range.
// Ranges themselves are types, and can be stuffed into variables 
// (more about that later)
for i in 1..10 do write( i , ", ") ;
writeln();

var iSum: int = 0;
for i in 1..1000 {
  iSum += i;
}
writeln( iSum );

for x in 1..10 {
  for y in 1..10 {
    write( (x,y), "\t" );
  }
  writeln();
}

// Ranges and Domains
// For-loops and arrays both use ranges and domains to 
// define an index set that can be iterated over.
// Ranges are single dimensional
// Domains can be multi-dimensional and can 
// represent indicies of different types as well.
// They are types, and can be assigned into variables;
var range1to10: range = 1..10;  // // 1, 2, 3, ... , 10
var range2to11 = 2..11; // 2, 3, 4, ..., 11

//ranges can be unbounded 
var range1toInf: range(boundedType=BoundedRangeType.boundedLow) = 1.. ; // 1, 2, 3, 4, 5, ...
var rangeNegInfto1 = ..1; // ..., -4, -3, -2, -1, 0, 1
// Note: the range(boundedType= ... ) is only 
// necessary if we explicitly type the variable

// Ranges can be strided using the 'by' operator.
var range2to10by2: range(stridable=true) = 2..10 by 2; // 2, 4, 6, 8, 10
var reverse2to10by2 = 10..2 by -2; // 10, 8, 6, 4, 2
// Note: the range(stridable=true) is only 
// necessary if we explicitly type the variable

// The end point of a range can be determined using the count (#) operator
var rangeCount: range = -5..#12; // range from -5 to 6

// Can mix operators
var rangeCountBy: range(stridable=true) = -5..#12 by 2; // -5, -3, -1, 1, 3, 5
writeln( rangeCountBy );

// Can query properties of the range
// Print the first index, last index, number of indices, 
// stride, and ask if 2 is include in the range
writeln( ( rangeCountBy.first, rangeCountBy.last, rangeCountBy.length, 
           rangeCountBy.stride, rangeCountBy.member( 2 ) ) );

for i in rangeCountBy{
  write( i, if i == rangeCountBy.last then "\n" else ", " );
}

// Rectangular domains are similarly defined using range notation
var domain1to10: domain(1) = {1..10};        // 1D domain from 1..10;
var twoDimensions: domain(2) = {-2..2,0..2}; // 2D domain over product of ranges
var thirdDim: range = 1..16;
var threeDims: domain(3) = {thirdDim, 1..10, 5..10}; // using a range variable

// Can iterate over the indices as tuples
for idx in twoDimensions do
  write( idx , ", ");
writeln();

// Or can deconstruct the tuple
for (x,y) in twoDimensions {
  write( "(", x, ", ", y, ")", ", " );
}
writeln();

// Associative domains act like sets
var stringSet: domain(string); // empty set of strings
stringSet += "a";
stringSet += "b";
stringSet += "c";
stringSet += "a"; // redundant add "a"  
stringSet -= "c"; // remove "c"
writeln( stringSet ); 


// Array are similar to those of other languages.
// Their sizes are defined using domains that represent their indices
var intArray: [1..10] int;
var intArray2: [{1..10}] int; //equivalent

// Accessed using bracket notation
for i in 1..10 do
  intArray[i] = -i;
writeln( intArray );
// we cannot access intArray[0] because it exists outside 
// of the index set, {1..10}, we defined it to have
// intArray[11] is illegal for the same reason.

var realDomain: domain(2) = {1..5,1..7};
var realArray: [realDomain] real;
var realArray2: [1..5,1..7] real;   // equivalent 
var realArray3: [{1..5,1..7}] real; // equivalent

for i in 1..5 {
  // use the range from 2nd dimension of the domain
  for j in realDomain.dim(2) {
    realArray[i,j] = -1.61803 * i + 0.5 * j;  // access using index list
    var idx: 2*int = (i,j);                   // note: 'index' is a keyword
    realArray[idx] = - realArray[(i,j)];      // index using tuples
  }
}

// arrays have domains as members that we can iterate over
for idx in realArray.domain {  // again, idx is a 2*int tuple 
  realArray[idx] = 1 / realArray[idx[1],idx[2]]; // access by tuple and list   
}

writeln( realArray );

// can also iterate over the values of an array
var rSum: real = 0;
for value in realArray {
  rSum += value; // read a value
  value = rSum;  // write a value
}
writeln( rSum, "\n", realArray );

// Using associative domains we can create associative arrays (dictionaries)
var dictDomain: domain(string) = { "one", "two" };
var dict: [dictDomain] int = [ "one" => 1, "two" => 2 ];
dict["three"] = 3;
writeln( dict );

// Chapel procedures have similar syntax to other languages functions.
proc fibonacci( n : int ) : int {
  if ( n <= 1 ) then return n;
  return fibonacci( n-1 ) + fibonacci( n-2 );
}

// input parameters can be untyped (a generic procedure)
proc doublePrint( thing ): void {
  write( thing, " ", thing, "\n");
}

// return type can be inferred (as long as the compiler can figure it out)
proc addThree( n ) {
  return n + 3;
}

doublePrint( addThree( fibonacci( 20 ) ) );

// Can also take 'unlimited' number of parameters
proc maxOf( x ...?k ) { 
  // x refers to a tuple of one type, with k elements
  var maximum = x[1];
  for i in 2..k do maximum = if (maximum < x[i]) then x[i] else maximum;
  return maximum;
}
writeln( maxOf( 1, -10, 189, -9071982, 5, 17, 20001, 42 ) );

// the ? operator is called the query operator, and is used to take 
// undetermined values (like tuple or array sizes, and generic types).

// Taking arrays as parameters.
// The query operator is used to determine the domain of A.
// this is important to define the return type (if you wanted to)
proc invertArray( A: [?D] int ): [D] int{
  for a in A do a = -a;
  return A;
}

writeln( invertArray( intArray ) );

// Procedures can have default parameter values, and
// the parameters can be named in the call, even out of order
proc defaultsProc( x: int, y: real = 1.2634 ): (int,real){
  return (x,y);
}

writeln( defaultsProc( 10 ) );
writeln( defaultsProc( x=11 ) );
writeln( defaultsProc( x=12, y=5.432 ) );
writeln( defaultsProc( y=9.876, x=13 ) );

// We can query the type of arguments to make safer generic procedures
// Here we define a procedure that takes two arguments of
// the same type, yet we dont define what that type is.
proc genericProc( arg1 : ?valueType, arg2 : valueType ): void {
  select( valueType ){
    when int do writeln( arg1, " and ", arg2, " are ints" );
    when real do writeln( arg1, " and ", arg2, " are reals" );
    otherwise writeln( arg1, " and ", arg2, " are somethings!" );
  }
}

genericProc( 1, 2 );
genericProc( 1.2, 2.3 );
genericProc( 1.0+2.0i, 3.0+4.0i );

// We can also enforce a form of polymorphism with the 'where' clause
// This allows the compiler to decide which function to use.
// Note: that means that all information needs to be known at compile
// time. Hence, we use params here to assert that the arguments must
// be known at compile time.
proc whereProc( param N : int ): void
 where ( N > 0 ) {
  writeln( "N is greater than 0" );  
}

proc whereProc( param N : int ): void
 where ( N < 0 ) {
  writeln( "N is less than 0" );  
}

whereProc( 10 );
whereProc( -1 );
// whereProc( 0 ) would result in a compiler error because there 
// are no functions that satisfy the where clause's condition.
// We could have defined a whereProc without a where clause that would
// then have been called. 

// Operator definitions are through procedures as well
// we can define the unary operators:
// + - ! ~
// and the binary operators:
// + - * / % ** == <= >= < > << >> & | ˆ by 
// += -= *= /= %= **= &= |= ˆ= <<= >>= <=>

// boolean exclusive or operator
proc ^( left : bool, right : bool ): bool {
  return (left || right) && !( left && right );
}

writeln( true  ^ true  );
writeln( false ^ true  );
writeln( true  ^ false );
writeln( false ^ false );

// Define a * operator on any two types that returns a tupe of those types
proc *( left : ?ltype, right : ?rtype): ( ltype, rtype ){
  return (left, right );
}

writeln( 1 * "a" ); // uses our * operator
writeln( 1 * 2 );   // uses the original * operator

/*
Note: You could break everything if you get careless with your overloads.
This here will break everything. Don't do it.
proc +( left: int, right: int ): int{
  return left - right;
} 
*/

// Classes are similar to those in C++ and Java.
// They currently lack privatization
class MyClass {
  // Member variables
  var memberInt : int; 
  var memberBool : bool = true; 

  // Classes have default constructors that dont need to be coded (see below)
  // Our explicitly defined constructor
  proc MyClass( val : real ){
    this.memberInt = ceil( val ): int;
  }

  // Our explicitly defined destructor
  proc ~MyClass( ){
    writeln( "MyClass Destructor called ", (this.memberInt, this.memberBool) );
  }

  // Class methods
  proc setMemberInt( val: int ){
    this.memberInt = val;
  }
  
  proc setMemberBool( val: bool ){
    this.memberBool = val;
  }

  proc getMemberInt( ): int{ 
    return this.memberInt;
  }

  proc getMemberBool(): bool {
    return this.memberBool;
  }
  
}

// Construct using default constructor, using default values
var myObject = new MyClass( 10 );
    myObject = new MyClass( memberInt = 10 ); // equivalent
writeln( myObject.getMemberInt() );
// ... using our values
var myDiffObject = new MyClass( -1, true );
    myDiffObject = new MyClass( memberInt = -1, 
                                memberBool = true ); // equivalent
writeln( myDiffObject );

// Construct using written constructor
var myOtherObject = new MyClass( 1.95 );
    myOtherObject = new MyClass( val = 1.95 ); // equivalent
writeln( myOtherObject.getMemberInt() );

// We can define an operator on our class as well but 
// the definition has to be outside the class definition
proc +( A : MyClass, B : MyClass) : MyClass {
  return new MyClass( memberInt = A.getMemberInt() + B.getMemberInt(),
                      memberBool = A.getMemberBool() || B.getMemberBool() );
}

var plusObject = myObject + myDiffObject;
writeln( plusObject );

// destruction
delete myObject;
delete myDiffObject;
delete myOtherObject;
delete plusObject;

// Classes can inherit from one or more parent classes
class MyChildClass : MyClass {
  var memberComplex: complex;
}

// Generic Classes
class GenericClass {
  type classType;
  var classDomain: domain(1);
  var classArray: [classDomain] classType;
  
  // Explicit constructor
  proc GenericClass( type classType, elements : int ){
    this.classDomain = {1..#elements};
  }
  
  // Copy constructor
  // Note: We still have to put the the type as an argument, but we can 
  // default to the type of the other object using the query (?) operator
  // Further, we can take advantage of this to allow our copy constructor
  // to copy classes of different types
  proc GenericClass( other : GenericClass(?otherType), 
                     type classType = otherType ) {
    this.classDomain = other.classDomain;
    // Copy and cast
    [ idx in this.classDomain ] this[ idx ] = other[ idx ] : classType; 
  }
  
  // Define bracket notation on a GenericClass object
  // i.e. objVar[ i ] or objVar( i )
  proc this( i : int ) ref : classType {
    return this.classArray[ i ];
  }
  
  // Define an iterator for the class.
  // i.e. for i in objVar do ....
  iter these() ref : classType {
    for i in this.classDomain do
      yield this[i];
  }
  
}

var realList = new GenericClass( real, 10 );
// We can assign to the member array of the object using the bracket 
// notation that we defined ( proc this( i: int ){ ... }  )
for i in realList.classDomain do realList[i] = i + 1.0;
// We can iterate over the values in our list with the iterator 
// we defined ( iter these(){ ... } )
for value in realList do write( value, ", " );
writeln();

// Make a copy of realList using the copy constructor
var copyList = new GenericClass( realList );
for value in copyList do write( value, ", " );
writeln();

// make a copy of realList and change the type, also using the copy constructor
var copyNewTypeList = new GenericClass( realList, int );
for value in copyNewTypeList do write( value, ", " );
writeln();


// A task is some work that will be done separately from the current
// task, and (if there are any available) in its own thread.

// a sync statement will ensure that the progress of the 
// main task will not progress until the children have synced back up.
sync {
// a begin statement will spin the body off into one new task
  begin {
    var a = 0;
    for i in 1..1000 do a += 1;
    writeln( "Done: ", a);
  }
  writeln( "spun off a task!");
}
writeln( "Back together" );

proc printFibb( n: int ){
  writeln( "fibonacci(",n,") = ", fibonacci( n ) );
}

// a cobegin statement will spin each 
// statement of the body into one new task
cobegin {
  printFibb( 20 );
  printFibb( 10 );
  printFibb( 5 );
  { 
    // this is a nested statement body and thus is a single statement
    // to the parent statement and is executed by a single task
    writeln( "this gets" );
    writeln( "executed as" );
    writeln( "a whole" );
  }
}
// Notice here that the prints from each statement may happen in any order.

// Coforall loop will create a new task for EACH iteration
var num_tasks = 10; // Number of tasks we want
coforall taskID in 1..#num_tasks {
  writeln( "Hello from task# ", taskID );
}
// Again we see that prints happen in any order.
// NOTE! coforall should be used only for creating tasks!
// Using it to iterating over a structure is very a bad idea!

// forall loops are another parallel loop, but only create a smaller number 
// of tasks, specifically --dataParTasksPerLocale=number of task
forall i in 1..100 {
  write( i, ", ");
}
writeln();
// Here we see that there are sections that are in order, followed by 
// a section that would not follow ( e.g. 1, 2, 3, 7, 8, 9, 4, 5, 6, )
// this is because each task is taking on a chunk of the range 1..10
// (1..3, 4..6, or 7..9) doing that chunk serially, but each task happens
// in parallel.
// Your results may depend on your machine and configuration

// For both the forall and coforall loops, the execution of the parent task
// will not continue until all the children sync up.

// forall loops are particularly useful for parallel iteration over arrays
// Lets run an experiment to see how much faster a parallel loop is
use Time; // Import the Time module to use Timer objects
var timer: Timer; 
var myBigArray: [{1..4000,1..4000}] real; // large array we will write into
// Serial Experiment
timer.start(); // start timer
for (x,y) in myBigArray.domain { // serial iteration
  myBigArray[x,y] = (x:real) / (y:real);
}
timer.stop(); // stop timer
writeln( "Serial: ", timer.elapsed() ); // print elapsed time
timer.clear(); // clear timer for parallel loop

// Parallel Experiment
timer.start(); // start timer
forall (x,y) in myBigArray.domain { // parallel iteration
  myBigArray[x,y] = (x:real) / (y:real);
}
timer.stop(); // stop timer
writeln( "Parallel: ", timer.elapsed() ); // print elapsed time
timer.clear();
// you may have noticed that (depending on how many cores you have) that
// the parallel loop went faster than the serial loop

// A succinct way of writing a forall loop over an array:
// iterate over values
[ val in myBigArray ] val = 1 / val; 
// or iterate over indicies
[ idx in myBigArray.domain ] myBigArray[idx] = -myBigArray[idx]; 
```
