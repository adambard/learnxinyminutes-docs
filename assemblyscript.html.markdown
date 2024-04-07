---
language: Assemblyscript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Steve Huguenin-Elie", "https://github.com/StEvUgnIn"]
    - ["Sebastian Speitel", "https://github.com/SebastianSpeitel"]
    - ["Max Graey", "https://github.com/MaxGraey"]
filename: learnassemblyscript.ts
---

__AssemblyScript__ compiles a variant of __TypeScript__ (basically JavaScript with types) to __WebAssembly__ using __Binaryen__. It generates lean and mean WebAssembly modules while being just an `npm install` away.

This article will focus only on AssemblyScript extra syntax, as opposed to [TypeScript](/docs/typescript) and [JavaScript](/docs/javascript).

To test AssemblyScript's compiler, head to the
[Playground](https://bit.ly/asplayground) where you will be able
to type code, have auto completion and directly see the emitted WebAssembly.

```ts
// There are many basic types in AssemblyScript,
let isDone: boolean = false;
let name: string = "Anders";

// but integer type come as signed (sized from 8 to 64 bits)
let lines8: i8 = 42;
let lines16: i16 = 42;
let lines32: i32 = 42;
let lines64: i64 = 42;

// and unsigned (sized from 8 to 64 bits),
let ulines8: u8 = 42;
let ulines16: u16 = 42;
let ulines32: u32 = 42;
let ulines64: u64 = 42;

// and float has two sizes possible (32/64).
let rate32: f32 = 1.0
let rate64: f64 = 1.0

// But you can omit the type annotation if the variables are derived
// from explicit literals
let _isDone = false;
let _lines = 42;
let _name = "Anders";

// Use const keyword for constants
const numLivesForCat = 9;
numLivesForCat = 1; // Error

// For collections, there are typed arrays and generic arrays
let list1: i8[] = [1, 2, 3];
// Alternatively, using the generic array type
let list2: Array<i8> = [1, 2, 3];

// For enumerations:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;

// Functions imported from JavaScript need to be declared as external
// @ts-ignore decorator
@external("alert")
declare function alert(message: string): void;

// and you can also import JS functions in a namespace
declare namespace window {
  // @ts-ignore decorator
  @external("window", "alert")
  function alert(message: string): void;
}

// Lastly, "void" is used in the special case of a function returning nothing
export function bigHorribleAlert(): void {
  alert("I'm a little annoying box!"); // calling JS function here
}

// Functions are first class citizens, support the lambda "fat arrow" syntax

// The following are equivalent, the compiler does not offer any type
// inference for functions yet, and same WebAssembly will be emitted.
export function f1 (i: i32): i32 { return i * i; }
// "Fat arrow" syntax
let f2 = (i: i32): i32 => { return i * i; }
// "Fat arrow" syntax, braceless means no return keyword needed
let f3 = (i: i32): i32 => i * i;

// Classes - members are public by default
export class Point {
  // Properties
  x: f64;

  // Constructor - the public/private keywords in this context will generate
  // the boiler plate code for the property and the initialization in the
  // constructor.
  // In this example, "y" will be defined just like "x" is, but with less code
  // Default values are also supported

  constructor(x: f64, public y: f64 = 0) {
    this.x = x;
  }

  // Functions
  dist(): f64 { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Static members
  static origin: Point = new Point(0, 0);
}

// Classes can be explicitly marked as extending a parent class.
// Any missing properties will then cause an error at compile-time.
export class PointPerson extends Point {
  constructor(x: f64, y: f64, public name: string) {
    super(x, y);
  }
  move(): void {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); //y will be 0

// Inheritance
export class Point3D extends Point {
  constructor(x: f64, y: f64, public z: f64 = 0) {
    super(x, y); // Explicit call to the super class constructor is mandatory
  }

  // Overwrite
  dist(): f64 {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Namespaces, "." can be used as separator for sub namespaces
export namespace Geometry {
  class Square {
    constructor(public sideLength: f64 = 0) {
    }
    area(): f64 {
      return Math.pow(this.sideLength, 2);
    }
  }
}

let s1 = new Geometry.Square(5);

// Generics
// AssemblyScript compiles generics to one concrete method or function per set 
// of unique contextual type arguments, also known as [monomorphisation]. 
// Implications are that a module only includes and exports concrete functions 
// for sets of type arguments actually used and that concrete functions can be 
// shortcutted with [static type checks] at compile time, which turned out to 
// be quite useful.
// Classes
export class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

export class Pair<T> {
  item1: T;
  item2: T;
}

// And functions
export function pairToTuple <T>(p: Pair<T>): Tuple<T, T> {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple<string>({ item1: "hello", item2: "world" });

// Including references to a TypeScript-only definition file:
/// <reference path="jquery.d.ts" />

// Template Strings (strings that use backticks)
// String Interpolation with Template Strings
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// Multiline Strings with Template Strings
let multiline = `This is an example
of a multiline string`;

let numbers: Array<i8> = [0, 1, 2, 3, 4];
let moreNumbers: Array<i8> = numbers;
moreNumbers[5] = 5; // Error, elements are read-only
moreNumbers.push(5); // Error, no push method (because it mutates array)
moreNumbers.length = 3; // Error, length is read-only
numbers = moreNumbers; // Error, mutating methods are missing

// Type inference in Arrays
let ints = [0, 1, 2, 3, 4]  // will infer as Array<i32>
let floats: f32[] = [0, 1, 2, 3, 4]  // will infer as Array<f32>
let doubles = [0.0, 1.0, 2, 3, 4]  // will infer as Array<f64>
let bytes1 = [0 as u8, 1, 2, 3, 4]  // will infer as Array<u8>
let bytes2 = [0, 1, 2, 3, 4]  as u8[] // will infer as Array<u8>
let bytes3: u8[] = [0, 1, 2, 3, 4] // will infer as Array<u8>
```

## Further Reading
 * [AssemblyScript Official website] (https://www.assemblyscript.org/)
 * [AssemblyScript source documentation] https://github.com/AssemblyScript/website/tree/main/src)
 * [Source Code on GitHub] (https://github.com/AssemblyScript/assemblyscript)
