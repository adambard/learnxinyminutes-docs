---
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Kiwimoe", "https://github.com/kiwimoe"]
translators:
    - ["Woody Chang", "https://github.com/kazettique"]
---

TypeScript 是為開發大型 JavaScript 應用程式而設計的語言。它為 JavaScript 導入某些程式語言常見的一些概念，諸如：類別（class）、模組（module）、介面（interface）、泛型（generic type）和靜態型別（static type）。TypeScript 是 JavaScript 的「超集」（superset）：意即建立在 JavaScript 的基礎上，所有 JavaScript 語法皆可在 TypeScript 中使用。因此，TypeScript 可以無縫導入到任何 JavaScript 專案中。TypeScript 編譯器最終會編譯成 JavaScript 程式碼。

本文將只專注於 TypeScript 的額外語法，其他請參考 [JavaScript 的指南](../javascript/)

要測試 TypeScript 的編譯器，請前往 [Playground](https://www.typescriptlang.org/play)，在那裡你可以輸入程式碼，獲得自動完成（autocomplete）功能，並查看編譯過的 JavaScript 程式碼。

```ts
// TS 基本型別有三種
let isDone: boolean = false;
let lines: number = 42;
let name: string = "Anders";

// 當變數有賦值時，也可以省略型別定義
let isDone = false;
let lines = 42;
let name = "Anders";

// 若無法確定型別，則可以定義為 `any`
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // 布林值也屬於 `any` 型別

// 以 `const` 關鍵字定義常數
const numLivesForCat = 9;
numLivesForCat = 1; // 報錯，常數初始化之後，無法指定新值

// 關於集合類型的資料，有型別化陣列（typed array）和泛型陣列（generic array）
let list: number[] = [1, 2, 3];
// 或使用泛型陣列類型
let list: Array<number> = [1, 2, 3];

// 列舉型別：
enum Color { Red, Green, Blue };
let c: Color = Color.Green;
console.log(Color[c]); // "Green"

// `void` 用於函式不回傳任何值的特殊情況
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// 函式是一等公民，支援 lambda「胖箭頭」 `=>` 語法，並使用型別推斷

// 以下幾種函式寫法是等效的，編譯器會生成相同的 JavaScript 程式碼
// 一般的函式
let f1 = function (i: number): number { return i * i; }
// 自動推斷回傳型別
let f2 = function (i: number) { return i * i; }
// 使用胖箭頭語法
let f3 = (i: number): number => { return i * i; }
// 胖箭頭語法（自動推斷回傳型別）
let f4 = (i: number) => { return i * i; }
// 胖箭頭語法（自動推斷回傳型別、省略函式的括號與 `return` 關鍵字）
let f5 = (i: number) => i * i;

// 函式的參數也可以同時定義多種型別的連集
function f6(i: string | number): void {
  console.log("The value was " + i);
}

// 介面是結構化的，任何擁有這些屬性的物件都要符合該介面的定義
interface Person {
  name: string;
  // 以問號（`?`）來表示選填的屬性
  age?: number;
  // 當然也可以包含函式
  move(): void;
}

// 實作 `Person` 介面的物件
// 可被視為一個 `Person` 物件，因為它具有 `name` 和 `move` 屬性
let p: Person = { name: "Bobby", move: () => { } };
// 包含選填屬性的物件：
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// 此物件非 `Person` 物件，因為 `age` 屬性非數字
let invalidPerson: Person = { name: "Bobby", age: true };

// 介面也可以描述一個函式的型別
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// 函式的型別定義著重於各個參數以及回傳值的型別，而函式名稱並不重要
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// 類別的屬性，其存取權限預設為公開（public）
class Point {
  // 定義屬性
  x: number;

  // 在建構函式種使用 `public`、`private` 關鍵字，會實例化的時候自動生成屬性
  // 以此為例，`y` 如同 `x` 定義其屬性，並於實例化時賦值，但寫法更為簡潔，同時支援預設值
  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // 函式，在類別中，又稱為方法（method）
  dist(): number {
    return Math.sqrt(this.x * this.x + this.y * this.y);
  }

  // 靜態成員（static member）
  static origin = new Point(0, 0);
}

// 類別可以被明確標記為實作某個介面。
// 任何缺少的屬性或方法都會在編譯時引發錯誤。
class PointPerson implements Person {
    name: string
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); // y 值將預設為 0

// 類別的繼承
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // 必須明確呼叫父類別的建構函式，使用 `super` 關鍵字
  }

  // 複寫父類別的方法
  dist(): number {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// 模組，以 `.` 語法存取子模組
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

// 引用模組，可以在本地使用別名命名並使用之
import G = Geometry;

let s2 = new G.Square(10);

// 泛用型別，泛型（generic type）
// 在類別使用泛型
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// 在介面使用泛型
interface Pair<T> {
  item1: T;
  item2: T;
}

// 在函式使用泛型
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// 引用型別定義檔：
/// <reference path="jquery.d.ts" />

// 樣板字串（template string）（使用反引號「`」的字串）
// 以樣板字串進行字串內插（interpolation）
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// 多行的樣板字串
let multiline = `This is an example
of a multiline string`;

// 唯讀存取子：TypeScript 3.1 的新語法
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // 錯誤，`p1.age` 為唯讀屬性

var p2 = { name: "John", age: 60 };
var p3: Person = p2; // 正確，`p2` 的唯讀別名
p3.age = 35; // 錯誤，`p3.age` 為唯讀屬性
p2.age = 45; // 正確，但因為 `p3` 參照可 `p2`，因此 `p3.age` 將會被修改

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // 唯讀屬性在建構函式被允許賦值
    this.model = "Unknown Model"; // 唯讀屬性在建構函式被允許賦值
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // 錯誤，陣列的成員為唯讀
moreNumbers.push(5); // 錯誤，無 `push` 方法（因為 `push` 方法會改變陣列的值）
moreNumbers.length = 3; // 錯誤，`length` 為唯讀
numbers = moreNumbers; // 錯誤，修改陣列的方法並不存在於唯讀陣列

// 可以使用聯合型別（union type）來定義不同的資料型別
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

// 樣板實字（template literal）型別
// 可以用來建立複雜的字串型別
type OrderSize = "regular" | "large";
type OrderItem = "Espresso" | "Cappuccino";
type Order = `A ${OrderSize} ${OrderItem}`;

let order1: Order = "A regular Cappuccino";
let order2: Order = "A large Espresso";
let order3: Order = "A small Espresso"; // 錯誤

// 迭代器（iterator）與產生器（generator）

// for..of 陳述式
// 循覽物件的每個成員「值」（value）
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "string", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// for..in 陳述式
// 循覽物件的每個成員的「鍵」（key）
for (const i in list) {
   console.log(i); // 0, 1, 2
}

// 型別斷言（assertion）
let foo = {} // 建立一個名為 `foo` 的空物件
foo.bar = 123 // 錯誤，`bar` 屬性並不存在於 `{}`
foo.baz = 'hello world' // 錯誤：`baz` 屬性並不存在於 `{}`

// 因為 `foo` 的推斷型別是 `{}`（一個無任何屬性的物件），所以不允許新增 `bar`、`baz` 及其他任何名稱的屬性。然而，通過型別斷言，以下程式碼將能夠通過 TS 的檢查：
interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // 這裡使用型別斷言
foo.bar = 123;
foo.baz = 'hello world'
```

## 延伸閱讀

* [TypeScript官網](https://www.typescriptlang.org/)
* [TypeScript原始碼](https://github.com/microsoft/TypeScript)
* [Learn TypeScript](https://learntypescript.dev/)
