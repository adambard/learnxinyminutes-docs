---
language: hdl
filename: learnhdl.hdl
contributors:
  - ["Jack Smith", "https://github.com/JSmithTech2019"]
---

HDL (hardware description language) is a specialized language used to describe the structure/behavior of real world circuits. 

It is used by circuit designers to simulate circuits and logic prior to wiring and fabricating a hardware circuit.

HDL allows circuit designers to simulate circuits at a high level without being connected to specific components.

## Basic building blocks & introduction to the language---
This programming language is built by simulating hardware chips and wiring. Normal programming functions are replaced with specialized chips that are added to the current wiring design. Every base chip must be written as it's own file and imported to be used in the current chip, though they may be reused as often as desired.

```verilog
// Single line comments start with two forward slashes.

/*
 * Multiline comments can be written using '/*' and 'star/'.
 * These are often used as comments.
 *
 * Note that they cannot be nested and will end at the first 'star/'.
 */

////////////////////////////////////////////////////
// 1. Chips & Components
////////////////////////////////////////////////////
/* 
 * Unlike other languages HDL creates an individual chip (function) per file
 * These are defined with a name, input arguments, output arguments
 * and finally the parts/logic of that specific chip.
 */

// Note CHIP is capitalized, the chip name does not need to be.
CHIP Ex {
    IN  a,  // Single bit (0 or 1) variable.
        c[16];  // 16 bit variable bus of single bit values.

    OUT out[16],  // 16 bit variable bus output.
        carry;  // Single bit output variable

    PARTS:
    // The functional components of the chip.
}

// Lines are ended with semicolons but can be continued using commas. The
// whitespace is ignored.



////////////////////////////////////////////////////
// 2. Inputs, Outputs, & Variables
////////////////////////////////////////////////////
/*
 * Variables and IO are treated as pins/wires and can carry a single bit
 * of data (0 or 1).
 */

// Hardware works on low level 0's and 1's, in order to use a constant
// high or low we use the terms true and false.
a=false; // This is a 0 value.
b=true; // This is a 1 value.

// Inputs and outputs can be defined as single bits
IN a, b; // Creates two single bit inputs

// They can also be defined as busses act as arrays where each
// index can contain a single bit value.
OUT c[16]; // Creates a 16 bit output array.

// Bussed values can be accessed using brackets
a[0] // The first indexed value in the bus a.
a[0..3] // The first 4 values in the a bus.
// Values can also be passed in entirety. For example if the function 
// foo() takes an 8 bit input bus and outputs a 2 bit bus:
foo(in=a[0..7], out=c); // C is now a 2 bit internal bus


// Note that internally defined busses cannot be subbussed!
// To access these elements, output or input them separately:
foo(in[0]=false, in[1..7]=a[0..6], out[0]=out1, out[1]=out2);
// out1 and out2 can then be passed into other circuits within the design.



////////////////////////////////////////////////////
// Combining Subsystems
////////////////////////////////////////////////////
/*
 * HDL relies heavily on using smaller "building block" chips to then be
 * added into larger and more complex designs. Creating the smaller components
 * and then adding them to the larger circuit allows for fewer lines of code
 * as well as reduction in total rewriting of code.
 */

// We are writing the function AND that checks if inputs I and K are both one.
// To implement this chip we will use the built in NAND gate as well as design
// a custom NOT gate to invert a single input.

// First we construct the Negation (not) chip. We will use the logically
// complete gate NAND that is built in for this task.
CHIP Not {
    IN i; // Not gates only take one single bit input.
    OUT o; // The negated value of a.

    PARTS:
    // Add the input to the built in chip, which then sends output to the NOT
    // output. This effectively negates the given value.
    Nand(a=i, b=i, out=o);
}

// By using the built in NAND gate we were able to construct a NOT gate
// that works like a real world hardware logic chip. Now we must construct
// the AND gate using these two gate primitives.

// We define a two input, single output AND gate:
CHIP And {
    IN i, k; // Two single bit inputs.
    OUT o; // One single bit output.

    PARTS:
    // Insert I and K into the nand gate and store the output in an internal
    // wire called notOut.
    Nand(a=i,b=k,out=notOut);

    // Use the not gate we constructed to invert notOut and send to the AND
    // output.
    Not(in=notOut,out=o);
}

// Easy! Now we can use Nand, And, and Not gates in higher level circuits.
// Many of these low level components are built in to HDL but any chip can
// be written as a submodule and used in larger designs.
```

## Test Files
When working with the nand2tetris hardware simulator chips written using HDL will
then be processed against test and comparison files to test functionality of the
simulated chip versus the expected output. To do this a test file will be loaded
into the hardware simulator and run against the simulated hardware.

```verilog
// First the chip the test file is written for is loaded
load <chip name>.hdl

// We set the output file for the simulated chip output as well as the comparison
// file that it will be tested against. We also specify what the output is
// expected to look like. In this case there will be two output columns, each
// will be buffered by a single space on either side and 4 binary values in
// the center of each column.
output-file <chip name>.out,
compare-to <chip name>.cmp,
output-list in%B1.4.1 out%B1.4.1;

// Then we set initial values for inputs to the chip. For example
set enable1 1, // set input enable1 to 1
set enable2 0, // set input enable2 to 0

// The clock is also controlled in the test file using tick and tock. Tick is a
// positive pulse and tock takes the clock back to 0. Clock cycles can be run
// multiple times in a row with no other changes to inputs or outputs.
tick,
tock,

// Finally we output the first expected value (from the test file) which is then
// compared with the first line of real output from our HDL circuit. This output
// can be viewed in the <chip name>.out file.
output;

// An example of <chip name>, a chip that takes in a 4 bit value as input and
// adds 1 to that value could have the following as test code:

// Set the input value to 0000, clock pulse, compare output from cmp file to actual out.
set in %B0000,
tick,
tock,
output;

// Set the input value to 0110, clock pulse, compare output from cmp file to actual out.
set in %B0110,
tick,
tock,
output;

// The expected output for case 1 should be 0001 and case 2 expects 0111, lets
// learn a little more about comparison files before finalizing our lesson.
```

## Comparison Files
Now lets take a look at comparison files, the files that hold what the test file
compares with the actual output of an HDL chip in the hardware simulator!

```verilog
// Like the <chip name> example above, the structure of the comparison file
// would look something like this
|  in  | out  |
| 0000 | 0001 |
| 0110 | 0111 |

// Notice how the input values specified in the test case are equivalent to the
// `in` column of the comparison file, and that the space buffer is 1 on either side.

// If the output from the HDL code we not this, such as the output below, then the
// test will fail and the user will know that the simulated chip is not correctly designed.
|  in  | out  |
| 0000 | 0001 |
| 0110 | 0110 | // Error! The chip did not add 1 here, something went wrong.
```

This is incredibly useful as it allows designers to simulate chip logic prior to
fabricating real life hardware and identify problems in their designs. Be warned that
errors in the test or comparison files can lead to both false positives and also
the more damaging false negatives so ensure that the logic is sound behind the test
creation.


Good luck and happy coding!

## Resources

* [From Nand To Tetris](https://www.nand2tetris.org)

## Further Reading

* [Hardware Description Language](https://en.wikipedia.org/wiki/Hardware_description_language)

* [HDL Programming Fundamentals](https://www.electronicdesign.com/products/hdl-programming-fundamentals)
