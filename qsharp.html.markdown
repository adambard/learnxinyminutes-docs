---
language: Q#
contributors:
    - ["Vincent van Wingerden", "https://github.com/vivanwin"]
    - ["Mariia Mykhailova", "https://github.com/tcNickolas"]
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
filename: LearnQSharp.qs
---

Q# is a high-level domain-specific language which enables developers to write quantum algorithms. Q# programs can be executed on a quantum simulator running on a classical computer and (in future) on quantum computers.

```c#
// Single-line comments start with //


/////////////////////////////////////
// 1. Quantum data types and operators

// The most important part of quantum programs is qubits. 
// In Q# type Qubit represents the qubits which can be used.
// This will allocate an array of two new qubits as the variable qs.
using (qs = Qubit[2]) {

    // The qubits have internal state that you cannot access to read or modify directly.
    // You can inspect the current state of your quantum program 
    // if you're running it on a classical simulator.
    // Note that this will not work on actual quantum hardware!
    DumpMachine();

    // If you want to change the state of a qubit
    // you have to do this by applying quantum gates to the qubit.
    H(qs[0]);    // This changes the state of the first qubit 
                // from |0⟩ (the initial state of allocated qubits) 
                // to (|0⟩ + |1⟩) / sqrt(2).
    // qs[1] = |1⟩; - this does NOT work, you have to manipulate a qubit by using gates.

    // You can apply multi-qubit gates to several qubits.
    CNOT(qs[0], qs[1]);

    // You can also apply a controlled version of a gate: 
    // a gate that is applied if all control qubits are in |1⟩ state.
    // The first argument is an array of control qubits, 
    // the second argument is the target qubit.
    Controlled Y([qs[0]], qs[1]); 

    // If you want to apply an anti-controlled gate 
    // (a gate that is applied if all control qubits are in |0⟩ state), 
    // you can use a library function.
    ApplyControlledOnInt(0, X, [qs[0]], qs[1]);

    // To read the information from the quantum system, you use measurements.
    // Measurements return a value of Result data type: Zero or One.
    // You can print measurement results as a classical value.
    Message($"Measured {M(qs[0])}, {M(qs[1])}");
}


/////////////////////////////////////
// 2. Classical data types and operators

// Numbers in Q# can be stored in Int, BigInt or Double.
let i = 1;            // This defines an Int variable i equal to 1
let bi = 1L;          // This defines a BigInt variable bi equal to 1
let d = 1.0;          // This defines a Double variable d equal to 1

// Arithmetic is done as expected, as long as the types are the same
let n = 2 * 10;                // = 20
// Q# does not have implicit type cast, 
// so to perform arithmetic on values of different types, 
// you need to cast type explicitly
let nd = IntAsDouble(2) * 1.0; // = 20.0

// Boolean type is called Bool
let trueBool = true;
let falseBool = false;

// Logic operators work as expected
let andBool = true and false;
let orBool = true or false;
let notBool = not false;

// Strings
let str = "Hello World!";

// Equality is ==
let x = 10 == 15; // is false

// Range is a sequence of integers and can be defined like: start..step..stop
let xi = 1..2..7; // Gives the sequence 1,3,5,7

// Assigning new value to a variable:
// by default all Q# variables are immutable;
// if the variable was defined using let, you cannot reassign its value.

// When you want to make a variable mutable, you have to declare it as such, 
// and use the set word to update value
mutable xii = true;
set xii = false;

// You can create an array for any data type like this
let xiii = new Double[10];

// Getting an element from an array 
let xiv = xiii[8];

// Assigning a new value to an array element
mutable xv = new Double[10];
set xv w/= 5 <- 1;


/////////////////////////////////////
// 3. Control flow

// If structures work a little different than most languages
if (a == 1) {
    // ...
} elif (a == 2) {
    // ... 
} else {
    // ...
}

// Foreach loops can be used to iterate over an array
for (qubit in qubits) {
    X(qubit);
}

// Regular for loops can be used to iterate over a range of numbers
for (index in 0 .. Length(qubits) - 1) {
    X(qubits[index]);
}

// While loops are restricted for use in classical context only
mutable index = 0;
while (index < 10) {
    set index += 1;
}

// Quantum equivalent of a while loop is a repeat-until-success loop.
// Because of the probabilistic nature of quantum computing sometimes
// you want to repeat a certain sequence of operations 
// until a specific condition is achieved; you can use this loop to express this.
repeat {
    // Your operation here
}
until (success criteria) // This could be a measurement to check if the state is reached
fixup {
    // Resetting to the initial conditions, if required
}


/////////////////////////////////////
// 4. Putting it all together

// Q# code is written in operations and functions
operation ApplyXGate(source : Qubit) : Unit {
    X(source);
}

// If the operation implements a unitary transformation, you can define 
// adjoint and controlled variants of it. 
// The easiest way to do that is to add "is Adj + Ctl" after Unit. 
// This will tell the compiler to generate the variants automatically.
operation ApplyXGateCA (source : Qubit) : Unit is Adj + Ctl {
    X(source);
}

// Now you can call Adjoint ApplyXGateCA and Controlled ApplyXGateCA.


// To run Q# code, you can put @EntryPoint() before the operation you want to run first
@EntryPoint()
operation XGateDemo() : Unit {
    using (q = Qubit()) {
        ApplyXGate(q);
    }
}

// Here is a simple example: a quantum random number generator. 
// We will generate a classical array of random bits using quantum code.
@EntryPoint()
operation QRNGDemo() : Unit {
    mutable bits = new Int[5];                // Array we'll use to store bits
    using (q = Qubit()) {                     // Allocate a qubit
        for (i in 0 .. 4) {                   // Generate each bit independently
            H(q);                             // Hadamard gate sets equal superposition
            let result = M(q);                // Measure qubit gets 0|1 with 50/50 prob
            let bit = result == Zero ? 0 | 1; // Convert measurement result to integer
            set bits w/= i <- bit;            // Write generated bit to an array
        }
    }
    Message($"{bits}");                       // Print the result
}
```


## Further Reading

The [Quantum Katas][1] offer great self-paced tutorials and programming exercises to learn quantum computing and Q#. 

[Q# Documentation][2] is official Q# documentation, including language reference and user guides.

[1]: https://github.com/microsoft/QuantumKatas
[2]: https://docs.microsoft.com/quantum/
