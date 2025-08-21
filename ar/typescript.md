---
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Kiwimoe", "https://github.com/kiwimoe"]
translators:
    - ["Ahmed Soliman", "https://github.com/ahmedsliman"]
filename: learntypescript.ts
---

<div dir="rtl">

يعد **TypeScript** لغة تهدف إلى تسهيل تطوير التطبيقات واسعة النطاق المكتوبة بلغة JavaScript.
يضيف TypeScript مفاهيم شائعة مثل **الفئات (Classes)** و**الوحدات (Modules)** و**النماذج (Interfaces)** و**الأنواع القابلة للتعميم (Generics)** و**الأنواع الثابتة الاختيارية (Optional Static Typing)** إلى JavaScript.

إنه **امتداد (Superset)** للغة JavaScript: فجميع شيفرات JavaScript صالحة في TypeScript، وبالتالي يمكن دمجها بسهولة في أي مشروع.
يقوم مترجم TypeScript بإنتاج شيفرة JavaScript مباشرة.

سيركّز هذا المقال فقط على **ميزات اللغة الإضافية (Extra Syntax)** في TypeScript، على عكس [JavaScript](../javascript/).

لاختبار مترجم **TypeScript**، توجّه إلى [بيئة الاختبار التفاعلية](https://www.typescriptlang.org/play) حيث يمكنك كتابة الشيفرة، والاستفادة من الإكمال التلقائي، ومشاهدة شيفرة JavaScript الناتجة مباشرة.

</div>

```ts
// في TypeScript توجد 3 أنواع أساسية من البيانات
let isDone: boolean = false;   // نوع منطقي (صح/خطأ)
let lines: number = 42;        // رقم
let name: string = "Anders";   // نص (سلسلة حروف)

// يمكن الاستغناء عن تعريف النوع إذا كانت القيمة واضحة
let isDone = false;
let lines = 42;
let name = "Anders";

// إذا لم يكن بالإمكان تحديد النوع، نستخدم النوع "Any"
let notSure: any = 4;
notSure = "ربما نص بدلًا من ذلك";
notSure = false; // مقبول، الآن قيمة منطقية

// نستخدم const لتعريف الثوابت (قيم لا تتغير)
const numLivesForCat = 9;
numLivesForCat = 1; // خطأ: لا يمكن تغيير قيمة ثابتة

// عند التعامل مع المجموعات (collections)، يمكن استخدام مصفوفات بأنواع محددة
let list: number[] = [1, 2, 3];
// أو باستخدام النوع العام للمصفوفات (Generic Array)
let list: Array<number> = [1, 2, 3];

// بالنسبة لقوائم القيم المحددة مسبقًا (Enums):
enum Color { Red, Green, Blue };
let c: Color = Color.Green;
console.log(Color[c]); // "Green"

// أخيرًا، نستخدم النوع "void" مع الدوال التي لا تُرجع أي قيمة
function bigHorribleAlert(): void {
  alert("أنا صندوق إزعاج صغير!");
}

// الدوال تعتبر عناصر أساسية (first-class citizens)،
// وتدعم صياغة "fat arrow"،
// ويستفيد المترجم من إستنتاج النوع تلقائيًا (type inference).

// الأمثلة التالية متكافئة؛ المترجم سيستنتج نفس شكل الدالة (signature)
let f1 = function (i: number): number { return i * i; }
// نوع القيمة المرجعة محدد صراحة

let f2 = function (i: number) { return i * i; }
// نوع القيمة المرجعة يُستدل عليه تلقائيًا

let f3 = (i: number): number => { return i * i; }
// صياغة "fat arrow" مع تحديد نوع القيمة المرجعة

let f4 = (i: number) => { return i * i; }
// صياغة "fat arrow" مع استدلال تلقائي لنوع القيمة المرجعة

let f5 = (i: number) => i * i;
// صياغة "fat arrow" بدون أقواس تعني عدم الحاجة لكلمة return

// يمكن للدوال قبول أكثر من نوع للمعاملات
function f6(i: string | number): void {
  console.log("القيمة كانت " + i);
}

// الواجهات (Interfaces) في TypeScript تعتمد على البنية (structural)
// أي كائن يمتلك الخصائص المطلوبة يوافق الواجهة
interface Person {
  name: string;
  // خصائص اختيارية، تُعلّم بعلامة "?"
  age?: number;
  // وطبعًا الدوال
  move(): void;
}

// كائن يطبق واجهة "Person"
// يمكن اعتباره Person لأنه يحتوي على خصائص name و move
let p: Person = { name: "Bobby", move: () => { } };

// كائنات تحتوي على الخاصية الاختيارية:
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };

// ليس Person لأن age ليست رقمًا
let invalidPerson: Person = { name: "Bobby", age: true };

// يمكن للواجهات أيضًا وصف نوع الدوال
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// فقط أنواع المعاملات مهمة، أسماء المعاملات ليست مهمة
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// الفئات (Classes) - الدوال عامة (public) افتراضيًا
class Point {
  // الخصائص
  x: number;

  // المُنشئ (Constructor) - الكلمات المفتاحية public/private هنا
  // تولد الشيفرة اللازمة لتعريف الخاصية وتهيئتها في المُنشئ
  // في هذا المثال، "y" سيُعرف مثل "x" لكن بأقل شيفرة
  // يمكن أيضًا تحديد قيم افتراضية

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  //الدوال
  dist(): number { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // الدوال أو الخصائص المشتركة (Static members)
  static origin = new Point(0, 0);
}

// يمكن للفئة أن تُعلن صراحة أنها تنفذ واجهة برمجية (interface).
// أي خاصية ناقصة ستؤدي إلى خطأ أثناء الترجمة (compile-time).
class PointPerson implements Person {
    name: string
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); // ستكون قيمة y هي 0

// الوراثة
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // الاستدعاء الصريح لمُنشئ الفئة الأساسية (super) إلزامي
  }

  // استبدال الوظيفة (Overwrite)
  dist(): number {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// الوحدات (Modules)، يمكن استخدام "." لتحديد الوحدات الفرعية
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

// إعادة تسمية وحدة (module) محليًا لتسهيل استخدامها
import G = Geometry;

let s2 = new G.Square(10);

// الأنواع القابلة للتعميم (Generics)

// الفئات (Classes)
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// الواجهات أو النماذج (Interfaces)
interface Pair<T> {
  item1: T;
  item2: T;
}

// والدوال أيضًا
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// تضمين ملفات التعريف المرجعية:
/// <reference path="jquery.d.ts" />

// السلاسل النصية القابلة للقالب (Template Strings) باستخدام backticks
// إدراج القيم داخل النصوص
let name = 'Tyrone';
let greeting = `مرحبًا ${name}، كيف حالك؟`
// سلاسل متعددة الأسطر باستخدام Template Strings
let multiline = `هذا مثال
لسلسلة نصية متعددة الأسطر`;

// الخصائص للقراءة فقط (Read-Only): ميزة جديدة في TypeScript 3.1
interface Person {
  readonly name: string; // لا يمكن تغيير الاسم بعد تعريفه
  readonly age: number;  // لا يمكن تغيير العمر بعد تعريفه
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // خطأ، الخاصية p1.age لا يمكن تعديلها

var p2 = { name: "John", age: 60 };
var p3: Person = p2; // صحيح، p3 مرجع ثابت للخصائص
p3.age = 35; // خطأ، p3.age لا يمكن تعديلها
p2.age = 45; // صحيح، يغير أيضًا p3.age بسبب الربط المرجعي

class Car {
  readonly make: string;   // علامة تجارية لا يمكن تعديلها خارج المُنشئ
  readonly model: string;  // طراز السيارة لا يمكن تعديله خارج المُنشئ
  readonly year = 2018;    // سنة الصنع ثابتة

  constructor() {
    this.make = "Unknown Make";   // مسموح بالتعيين داخل المُنشئ
    this.model = "Unknown Model"; // مسموح بالتعيين داخل المُنشئ
  }
}

// مصفوفة قابلة للتغيير
let numbers: Array<number> = [0, 1, 2, 3, 4];
// مصفوفة للقراءة فقط، لا يمكن تعديل محتواها
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // خطأ، العناصر ثابتة
moreNumbers.push(5); // خطأ، لا يمكن إضافة عناصر
moreNumbers.length = 3; // خطأ، الطول ثابت
numbers = moreNumbers; // خطأ، طرق التغيير غير موجودة

// حالات الاتحاد المميزة (Tagged Union Types) لتحديد حالة متغيرة يمكن أن تكون في أحد الأشكال المحددة مسبقًا
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

// أنواع النصوص التي من الممكن أن تحتوي أجزاء متغيرة
// تُستخدم لإنشاء أنواع نصوص معقدة
type OrderSize = "regular" | "large";
type OrderItem = "Espresso" | "Cappuccino";
type Order = `A ${OrderSize} ${OrderItem}`;

let order1: Order = "A regular Cappuccino";
let order2: Order = "A large Espresso";
let order3: Order = "A small Espresso"; // خطأ

// المكررات والدوال التي تولد القيم عند الطلب (Iterators and Generators)

// جملة for..of
// للتكرار على قائمة القيم في الكائن الجاري التكرار عليه
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "نص", خطأ
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// جملة for..in
// للتكرار على قائمة المفاتيح (keys) في الكائن الجاري التكرار عليه
for (const i in list) {
   console.log(i); // "0", "1", "2"
}

// تأكيد النوع (Type Assertion)

let foo = {} // إنشاء foo ككائن فارغ
foo.bar = 123 // خطأ: الخاصية 'bar' غير موجودة في هذا الكائن
foo.baz = 'hello world' // خطأ: الخاصية 'baz' غير موجودة في هذا الكائن

// بما أن النوع المستنتج لـ foo هو `{}` (كائن بلا خصائص)،
// لا يمكن إضافة bar و baz إليه. لكن باستخدام تأكيد النوع،
// يمكن تمرير الكود التالي بنجاح:

interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // استخدام تأكيد النوع هنا
foo.bar = 123;
foo.baz = 'hello world'
```

<div dir="rtl">

## اقرأ أكثر

* [الموقع الرسمي لـ TypeScript](https://www.typescriptlang.org/)
* [الشيفرة المصدرية على GitHub](https://github.com/microsoft/TypeScript)
* [تعلّم TypeScript](https://learntypescript.dev/)

</div>
