---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Worajedt Sitthidumrong", "https://bitbucket.org/wrj"]
filename: learntypescript.ts
lang: th-th
---

TypeScript เป็นภาษาที่มีเป้าหมายเพื่อทำให้การพัฒนาซอฟต์แวร์ขนาดใหญ่ด้วย JavaScript ทำได้ง่ายขึ้น โดยที่ TypeScript ได้เพิ่มแนวคิดที่พบทั่วไป อาทิ classes, modules, interfaces, generics และ static typing (ไม่บังคับ) เข้าไปในภาษา JavaScript ดังนั้น TypeScript ก็เลยเป็น Super Set ของ JavaScript อีกที โค้ด JavaScript ทุกส่วน ก็คือโค้ดที่ทำงานได้ถูกต้องใน TypeScript ทำให้เราเพิ่ม TypeScript เข้าไปใช้ในโปรเจคการพัฒนาของเราได้ไม่ยากเลย เพราะ TypeScript คอมไพล์ผลลัพธ์ออกมาเป็น JavaScript ในท้ายสุดอยู่ดี

บทความนี้จะเน้นเฉพาะ syntax ส่วนขยายของ TypeScript ซึ่งจะไม่รวมกับที่มีใน  [JavaScript](/docs/javascript).

การทดสอบเขียน TypeScript เริ่มได้ง่าย ๆ โดยเข้าไปที่
[Playground] (http://www.typescriptlang.org/Playground) ซึ่งคุณจะเขียนโค้ดพร้อม autocomplete และเห็นเลยว่ามันจะแปลงมาเป็นผลลัพธ์แบบ JavaScript อย่างไร

```ts
// TypeScript มี data type พื้นฐาน 3 แบบ
let isDone: boolean = false;
let lines: number = 42;
let name: string = "Anders";

// แต่เราก็สามารถละการบอกชนิดได้ โดยชนิดตัวแปรก็จะปรับชนิดของเขาจากข้อมูลที่กำหนดให้โดยตรง
let isDone = false;
let lines = 42;
let name = "Anders";

// ถ้าไม่รู้ ก็กำหนดเป็นชนิด "Any" ได้
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // โอเค ตอนนี้เป็น Boolean แน่ ๆ

// ใช้ const สำหรับสร้าง ค่าคงที่
const numLivesForCat = 9;
numLivesForCat = 1; // Error

// สำหรับ collections มี typed arrays และ generic arrays
// ก็คือ อะเรย์บอกชนิด และ อะเรย์เจเนอริก ตามลำดับ
let list: number[] = [1, 2, 3];
// ในอีกทางหนึ่ง สร้างเป็นอะเรย์ชนิด generic array
let list: Array<number> = [1, 2, 3];

// และสำหรับ enumerations:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;

// สุดท้าย, "void" ใช้เมื่อเป็นกรณีพิเศษที่ฟังก์ชันไม่ส่งค่ากลับ
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// ฟังก์ชั่น (Functions) เป็นสิ่งที่มีความสำคัญมาเป็นอันดับหนึ่ง รองรับการใช้ "fat arrow" ในการสร้าง lambda function และ type inference

// สไตล์ต่อไปนี้มีค่าเท่ากันกับบรรทัดที่ยกตัวอย่างด้านล่าง เพราะคอมไพเลอร์จะมองเหมือนกัน และได้ JavaScript แบบเดียวกัน
let f1 = function (i: number): number { return i * i; }
// อนุมานชนิดที่ส่งกลับ หรือ type inferred
let f2 = function (i: number) { return i * i; }
// เขียนแบบ "Fat arrow" แต่บอกชนิดส่งกลับ
let f3 = (i: number): number => { return i * i; }
// เขียนแบบ "Fat arrow" แต่อนุมานชนิดส่งกลับ
let f4 = (i: number) => { return i * i; }
// เขียนแบบ "Fat arrow" อนุมานชนิดส่งกลับ พร้อมกับไม่มีวงเล็บ แปลว่าไม่ต้องมี return keyword ด้วย
let f5 = (i: number) => i * i;

// Interfaces นั้นเป็นเหมือนเราออกแบบโครงสร้าง คุณสมบัติต่าง ๆ ตอนเอาไปใช้ จะต้องเป็นไปตาม interface นั้น ๆ
interface Person {
  name: string;
  // Optional properties กำหนดด้วย "?"
  age?: number;
  // และมี function พร้อมชนิดได้ใน interface
  move(): void;
}

// Object that implements the "Person" interface
// Can be treated as a Person since it has the name and move properties
let p: Person = { name: "Bobby", move: () => { } };
// Objects that have the optional property:
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// Is not a person because age is not a number
let invalidPerson: Person = { name: "Bobby", age: true };

// Interfaces can also describe a function type
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Only the parameters' types are important, names are not important.
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// Classes - members are public by default
class Point {
  // Properties
  x: number;

  // Constructor - the public/private keywords in this context will generate
  // the boiler plate code for the property and the initialization in the
  // constructor.
  // In this example, "y" will be defined just like "x" is, but with less code
  // Default values are also supported

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Functions
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Static members
  static origin = new Point(0, 0);
}

// Classes can be explicitly marked as implementing an interface.
// Any missing properties will then cause an error at compile-time.
class PointPerson implements Person {
    name: string
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); //y will be 0

// Inheritance
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // Explicit call to the super class constructor is mandatory
  }

  // Overwrite
  dist() {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Modules, "." can be used as separator for sub modules
module Geometry {
  export class Square {
    constructor(public sideLength: number = 0) {
    }
    area() {
      return Math.pow(this.sideLength, 2);
    }
  }
}

let s1 = new Geometry.Square(5);

// Local alias for referencing a module
import G = Geometry;

let s2 = new G.Square(10);

// Generics
// Classes
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// Interfaces
interface Pair<T> {
  item1: T;
  item2: T;
}

// And functions
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// Including references to a definition file:
/// <reference path="jquery.d.ts" />

// Template Strings (strings that use backticks)
// String Interpolation with Template Strings
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// Multiline Strings with Template Strings
let multiline = `This is an example
of a multiline string`;

// READONLY: New Feature in TypeScript 3.1
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // Error, p1.x is read-only

var p2 = { name: "John", age: 60 };
var p3: Person = p2; // Ok, read-only alias for p2
p3.x = 35; // Error, p3.x is read-only
p2.x = 45; // Ok, but also changes p3.x because of aliasing

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // Assignment permitted in constructor
    this.model = "Unknown Model"; // Assignment permitted in constructor
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // Error, elements are read-only
moreNumbers.push(5); // Error, no push method (because it mutates array)
moreNumbers.length = 3; // Error, length is read-only
numbers = moreNumbers; // Error, mutating methods are missing
```

## อ่านเพิ่มเติมที่
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications] (https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Source Code on GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
