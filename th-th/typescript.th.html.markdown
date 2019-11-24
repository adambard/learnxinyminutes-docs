---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Worajedt Sitthidumrong", "https://bitbucket.org/wrj"]
filename: learntypescript-th.ts
lang: th-th
---

TypeScript เป็นภาษาที่มีเป้าหมายเพื่อทำให้การพัฒนาซอฟต์แวร์ขนาดใหญ่ด้วย JavaScript ทำได้ง่ายขึ้น โดยที่ TypeScript ได้เพิ่มแนวคิดที่พบทั่วไป อาทิ classes, modules, interfaces, generics และ static typing (ไม่บังคับ) เข้าไปในภาษา JavaScript ดังนั้น TypeScript ก็เลยเป็น Super Set ของ JavaScript อีกที โค้ด JavaScript ทุกส่วน ก็คือโค้ดที่ทำงานได้ถูกต้องใน TypeScript ทำให้เราเพิ่ม TypeScript เข้าไปใช้ในโปรเจคการพัฒนาของเราได้ไม่ยากเลย เพราะ TypeScript คอมไพล์ผลลัพธ์ออกมาเป็น JavaScript ในท้ายสุดอยู่ดี

บทความนี้จะเน้นเฉพาะ syntax ส่วนขยายของ TypeScript ซึ่งจะไม่รวมกับที่มีใน  [JavaScript](/docs/javascript)

การทดสอบเขียน TypeScript เริ่มได้ง่าย ๆ โดยเข้าไปที่
[Playground](http://www.typescriptlang.org/Playground) ซึ่งคุณจะเขียนโค้ดพร้อม autocomplete และเห็นเลยว่ามันจะแปลงมาเป็นผลลัพธ์แบบ JavaScript อย่างไร

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

// Interfaces นั้นเป็นเหมือนเราออกแบบโครงสร้าง คุณสมบัติต่าง ๆ ตอนเอาไปใช้ จะต้องเป็นไปตาม interface นั้น ๆ เหมือนกับเป็นการกำหนดสเป็คของ "ชนิดข้อมูล"
interface Person {
  name: string;
  // Optional properties กำหนดด้วย "?"
  age?: number;
  // และมี function พร้อมชนิดได้ใน interface
  move(): void;
}

// Object นี้ implements "Person" interface ทำให้มันเป็นชนิด Person และมันก็มี property name และ function move() 
let p: Person = { name: "Bobby", move: () => { } };
// Objects นี้เป็นชนิด Person ด้วย และมี optional property หรือ age?: นั่นเอง
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// ไม่ใช่ Person เพราะ age: ต้องเป็น number เท่านั้น ตามข้อกำหนดใน interface Person
let invalidPerson: Person = { name: "Bobby", age: true };

// Interfaces ยังนำมาใช้ในลักษณะของ function ได้อีกด้วย
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// เฉพาะชนิด parameters เท่านั้นที่สำคัญ ชื่อของมันไม่จำเป็นต้องเหมือน
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// สมาชิกใน class จะเข้าถึงได้แบบ public เป็นค่าปริยาย
class Point {
  // Properties
  // ตั้งค่า Properties ของ class นี้
  x: number;

  // Constructor
  // เราใส่ public/private keywords ตรงนี้ได้ มีผลเหมือนกันกับกำหนด x ด้านบน
  // ในตัวอย่าง y มีการกำหนดเช่นเดียวกับ x แต่พิมพ์สั้นกว่า
  // สังเกตว่า มีการกำหนดค่าปริยายให้ parameters ได้ด้วย

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Functions
  dist() { return Math.sqrt(this.x*this.x + this.y*this.y); }

  // Static members
  static origin = new Point(0, 0);
}

// Classes สามารถระบุชนิด interface ที่ต้องการได้ตรง ๆ ด้วยเช่นโค้ดด้านล่าง
// แต่อะไรที่จะ implement มานั้น ถ้าไม่ได้กำหนดไว้ใน constructor ก็จะเกิดข้อผิดพลาดตอนคอมไพล์
class PointPerson implements Person {
    name: string  // ตรงนี้จะผิด แก้ไขโดยการไปสร้างตัวรับค่าเข้ามาผ่านทาง constructor
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); //y เป็น 0 เพราะกำหนดค่าปริยายไว้ให้แล้ว

// Inheritance การสืบทอดคุณสมบัติ
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // เราจะต้องเรียกใช้ super class constructor โดยตรง
  }

  // Overwrite ฟังก์ชั่นที่มีอยู่เดิมใน Point
  dist() {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Modules ใช้เป็นกลุ่มของ class เราใช้ "." เป็นตัวแบ่ง sub modules
// อย่างกรณีนี้จะเป็น Module.Class เช่น Geometry.Square
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

// เราทำให้เรียกใช้ง่ายขึ้นโดยการใช้ alias มาอ้างชื่อ module แบบเดียวกับบางภาษา เช่น Python
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

// และ functions
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// เราเรียกใช้ไฟล์ definition แบบนี้:
/// <reference path="jquery.d.ts" />

// Template Strings ( คือ strings ที่ใช้ backticks ครอบ — "`" ปุ่มบนซ้ายคีย์บอร์ด )
// แทรกข้อความใน String ด้วย Template Strings
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// Strings หลายบรรทัดก็ทำได้ใน Template Strings
let multiline = `This is an example
of a multiline string`;

// READONLY: ความสามารถใหม่ใน TypeScript 3.1
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // Error แน่นอน เพราะ p1.age ถูกกำหนดเป็น read-only

var p2 = { name: "John", age: 60 }; // สังเกตว่า p2 ไม่ได้กำหนดเป็น Person
var p3: Person = p2; // ทำได้ เป็น read-only alias ของ p2 และกำหนดเป็น Person
p3.x = 35; // Error p3.x ก็เป็น read-only
p2.x = 45; // Ok ทำได้แต่ก็จะเปลี่ยนค่า p3.x ด้วย เพราะ p3 เป็น alias ของ p2

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make  = "Unknown Make";   // อนุญาตให้กำหนดค่าได้ใน constructor แม้ว่าจะ read-only
    this.model = "Unknown Model"; // อนุญาตให้กำหนดค่าได้ใน constructor แม้ว่าจะ read-only
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5;     // Error, สมาชิกอะเรย์เป็น read-only แปลว่า ห้ามแก้ไข
moreNumbers.push(5);    // Error, push method ใช้ไม่ได้ เพราะมันจะไปแก้ไข read-only array
moreNumbers.length = 3; // Error, เพราะ length ก็ต้อง read-only
numbers = moreNumbers;  // Error, method ที่ทำให้อะเรย์เปลี่ยนได้จะไม่อนุญาต

// Tagged Union Types สำหรับโมเดลสเตท ที่อาจจะมีได้หลายๆ สเตท
type State = 
  | { type: "loading" }
  | { type: "success", value: number }
  | { type: "error", message: string };

ประกาศ const state: State;
if (state.type === "success") {
  console.log(state.value);
} else if (state.type === "error") {
  console.error(state.message);
}

// Iterators และ Generators

// ประโยคแบบ for..of
// การทำซ้ำกับ ลิสต์ของค่าในออปเจ็คต์
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "string", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// ประโยคแบบ for..in
// การทำซ้ำกับ ลิสต์ของคีย์ในออปเจ็คต์
for (const i in list) {
   console.log(i); // 0, 1, 2
}


```

## อ่านเพิ่มเติมที่
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications] (https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Source Code on GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
