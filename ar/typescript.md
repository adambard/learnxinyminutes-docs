---
name: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Kiwimoe", "https://github.com/kiwimoe"]
filename: learntypescript.ts
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">
تايب سكريبت لغة تهدف إلى تسهيل تطوير تطبيقات جافاسكريبت واسعة النطاق. تضيف مفاهيم شائعة مثل الصفوف والوحدات والواجهات والأنواع العامة والكتابة الثابتة (الاختيارية). وهي مجموعة فوق جافاسكريبت: كل شيفرة جافاسكريبت صالحة في تايب سكريبت فيمكن دمجها بسهولة في أي مشروع. مُصرّف تايب سكريبت يُخرج جافاسكريبت.
</p>

<p dir="rtl">
يركّز هذا المقال على صياغة تايب سكريبت الإضافية فقط، وليس على
<a href="../javascript/">جافاسكريبت</a>.
</p>

<p dir="rtl">
لتجربة المُصرّف، افتح
<a href="https://www.typescriptlang.org/play">الملعب (Playground)</a>
حيث يمكنك كتابة الشيفرة والإكمال التلقائي ومشاهدة جافاسكريبت المُصدَر.
</p>

```ts
// في تايب سكريبت ثلاثة أنواع أساسية
let isDone: boolean = false;
let lines: number = 42;
let name: string = "Anders";

// يمكن حذف التعليق النوعي إذا استُنتجت الأنواع من قيم حرفية صريحة
let isDone = false;
let lines = 42;
let name = "Anders";

// عند تعذّر الاستنتاج يوجد النوع "any"
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // حسناً، من الواضح أنه منطقي

// استخدم const للثوابت
const numLivesForCat = 9;
numLivesForCat = 1; // خطأ

// للمجموعات: مصفوفات مُعلَّمة نوعياً والمصفوفة العامة
let list: number[] = [1, 2, 3];
// أو باستخدام الصياغة العامة للمصفوفة
let list: Array<number> = [1, 2, 3];

// للتعدادات:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;
console.log(Color[c]); // "Green"

// أخيراً، "void" للدالة التي لا تُرجع قيمة
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// الدوال مواطنة من الدرجة الأولى، وتدعم سهم لامدا والاستنتاج النوعي

// التالي متكافئ: المُصرّف يستنتج نفس التوقيع ويُصدِر نفس جافاسكريبت
let f1 = function (i: number): number { return i * i; }
// نوع الإرجاع مُستنتَج
let f2 = function (i: number) { return i * i; }
// صياغة السهم السمين
let f3 = (i: number): number => { return i * i; }
// سهم مع نوع إرجاع مُستنتَج
let f4 = (i: number) => { return i * i; }
// بدون أقواس معناه لا حاجة لكلمة return
let f5 = (i: number) => i * i;

// الدوال يمكنها قبول أكثر من نوع
function f6(i: string | number): void {
  console.log("The value was " + i);
}

// الواجهات هيكلية: أي كائن يملك الخصائص يتوافق مع الواجهة
interface Person {
  name: string;
  // خصائص اختيارية بعلامة "?"
  age?: number;
  // وطبعاً الدوال
  move(): void;
}

// كائن يحقق واجهة "Person"
let p: Person = { name: "Bobby", move: () => { } };
// كائن يملك الخاصية الاختيارية:
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// ليس شخصاً صالحاً لأن age ليس رقماً
let invalidPerson: Person = { name: "Bobby", age: true };

// الواجهة يمكن أن تصف نوع دالة
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// أنواع المعاملات مهمة، الأسماء غير مهمة.
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// الأصناف — الأعضاء عامون افتراضياً
class Point {
  // خصائص
  x: number;

  // الباني — كلمتا public/private هنا تولّدان الشفرة الرتيبة للخاصية والتهيئة في الباني.
  // في هذا المثال "y" يُعرَّف مثل "x" بأقل شيفرة
  // القيم الافتراضية مدعومة أيضاً

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // دوال
  dist(): number { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // أعضاء ثابتة
  static origin = new Point(0, 0);
}

// يمكن تعليم الصنف صراحةً أنه يحقق واجهة
class PointPerson implements Person {
    name: string
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); // y ستكون 0

// الوراثة
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // استدعاء باني الفئة الأب إلزامي
  }

  // إعادة تعريف
  dist(): number {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// الوحدات — النقطة "." فاصل للوحدات الفرعية
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

// اسم مستعار محلي للوحدة
import G = Geometry;

let s2 = new G.Square(10);

// الأنواع العامة
// أصناف
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// واجهات
interface Pair<T> {
  item1: T;
  item2: T;
}

// ودوال
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// تضمين ملف تعريفات:
/// <reference path="jquery.d.ts" />

// القوالب النصية (backticks)
// استيفاء النص في القوالب
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// نص متعدد الأسطر بالقوالب
let multiline = `This is an example
of a multiline string`;

// للقراءة فقط (readonly) — ميزة TypeScript 3.1
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // خطأ، p1.age للقراءة فقط

var p2 = { name: "John", age: 60 };
var p3: Person = p2; // مقبول، اسم مستعار للقراءة فقط لـ p2
p3.age = 35; // خطأ
p2.age = 45; // مقبول، لكنه يغيّر p3.age أيضاً بسبب الاسم المستعار

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // التعيين مسموح في الباني
    this.model = "Unknown Model";
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // خطأ، العناصر للقراءة فقط
moreNumbers.push(5); // خطأ، لا push (لأنه يعدّل المصفوفة)
moreNumbers.length = 3; // خطأ، الطول للقراءة فقط
numbers = moreNumbers; // خطأ، دوال التعديل ناقصة

// اتحاد مُوسوم لنمذجة حالة بأشكال متعددة
type State =
  | { type: "loading" }
  | { type: "success", value: number }
  | { type: "error", message: string };

declare const state: State;
if (state.type === "success") {
  console.log(state.value);
} else if (state.type === "error") {
  console.error(state.message);
}

// أنواع قوالب نصية حرفية
// لبناء أنواع سلسلة معقّدة
type OrderSize = "regular" | "large";
type OrderItem = "Espresso" | "Cappuccino";
type Order = `A ${OrderSize} ${OrderItem}`;

let order1: Order = "A regular Cappuccino";
let order2: Order = "A large Espresso";
let order3: Order = "A small Espresso"; // خطأ

// المكررات والمولّدات

// حلقة for...of
// تمرّ على قيم المكرَّر
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "string", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// حلقة for...in
// تمرّ على مفاتيح الكائن
for (const i in list) {
   console.log(i); // "0", "1", "2"
}

// تأكيد النوع (Type assertion)

let foo = {} // foo كائن فارغ
foo.bar = 123 // خطأ: الخاصية 'bar' غير موجودة على `{}`
foo.baz = 'hello world' // خطأ

// النوع المُستنتَج `{}` فيمنع إضافة bar وbaz. بتأكيد النوع يمرّ:

interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // تأكيد النوع هنا
foo.bar = 123;
foo.baz = 'hello world'
```

<h2 dir="rtl">قراءة إضافية</h2>

<ul dir="rtl">
<li><a href="https://www.typescriptlang.org/">الموقع الرسمي لتايب سكريبت</a></li>
<li><a href="https://github.com/microsoft/TypeScript">الشيفرة على GitHub</a></li>
<li><a href="https://learntypescript.dev/">Learn TypeScript</a></li>
</ul>
