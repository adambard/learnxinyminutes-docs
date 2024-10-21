---
language: TypeScript
lang: uk-ua
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Kiwimoe", "https://github.com/kiwimoe"]
translators:
    - ["Illia Piskurov", "https://github.com/illia-piskurov"]
filename: learntypescript-ua.ts
---

TypeScript - це мова, яка має на меті полегшити розробку великомасштабних додатків, написаних на JavaScript.
TypeScript додає загальні концепції, такі як класи, модулі, інтерфейси, узагальнене програмування та
(необов'язково) статичну типізацію до JavaScript. Вона є надмножиною JavaScript: увесь код JavaScript є дійсним
кодом TypeScript, тому його можна легко додати до будь-якого проекту. Компілятор TypeScript генерує JavaScript.

Ця стаття концентрується тільки на синтаксисі TypeScript, на відміну від статті про [JavaScript](../javascript-ua/).

Для тестування компілятора TypeScript перейдіть по посиланню в [пісочницю](https://www.typescriptlang.org/Playground).
Там ви маєте змогу написати код (з підтримкою автодоповнення) та одразу побачити згенерованний JavaScript код.

```ts
// TypeScript має 3 базові типи
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Андерс";

// Тип «any» для випадків, коли заздалегідь невідомий тип змінної
var notSure: any = 4;
notSure = "або, можливо, рядок";
notSure = false; // або логічний тип

// Для колекцій є типізовані та узагальнені масиви
var list: number[] = [1, 2, 3];
// Альтернатива з використанням узагальненого масиву
var list: Array<number> = [1, 2, 3];

// Перелік:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// Зрештою, «void» використовується для позначення того, що функція нічого не повертає
function bigHorribleAlert(): void {
  alert("Я маленьке надокучливе віконечко!");
}

// Функції — це об'єкти першого класу. Вони підтримують лямбда-синтаксис (=>)
// і використовують виведення типів (type inference)

// Наступні рядки коду є еквівалентними, компілятором передбачається
// однакова сигнатура, на виході генерується однаковий JavaScript-код
var f1 = function(i: number): number { return i * i; }
// Передбачається тип, що повертається
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Передбачається тип, що повертається
var f4 = (i: number) => { return i * i; }
// Передбачається тип, що повертається, в однорядковій функції ключове слово «return» не потрібне
var f5 = (i: number) =>  i * i;

// Інтерфейси є структурними; усе, що має властивості, сумісне з інтерфейсом
interface Person {
  name: string;
  // Опціональні властивості, позначені символом «?»
  age?: number;
  // І, звісно, функції
  move(): void;
}

// Об'єкт, який реалізує інтерфейс «Person»
// До нього можна звертатися, як до «Person», оскільки він має властивості «name» і «move»
var p: Person = { name: "Боббі", move: () => {} };
// Об'єкти, які можуть мати опціональні властивості:
var validPerson: Person = { name: "Боббі", age: 42, move: () => {} };
// Це не «Person», оскільки «age» не є числовим значенням
var invalidPerson: Person = { name: "Боббі", age: true };

// Інтерфейси можуть також описувати функціональний тип
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Важливі лише типи параметрів, імена — ні.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Класи. Члени класу за замовчуванням є публічними
class Point {
    // Властивості
    x: number;

    // Конструктор — ключові слова public/private у цьому контексті згенерують
    // шаблонний код для властивості та для ініціалізації в конструкторі
    // У цьому прикладі «y» буде визначено так само, як і «x», але з меншим обсягом коду
    // Значення за замовчуванням також підтримуються

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Функції
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Статичні члени
    static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); // y буде дорівнювати 0

// Наслідування
class Point3D extends Point {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Явний виклик конструктора базового класу обов'язковий
    }

    // Перевизначення
    dist() {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Модулі, знак «.» може використовуватися як роздільник для позначення підмодулів
module Geometry {
  export class Square {
    constructor(public sideLength: number = 0) {
    }
    area() {
      return Math.pow(this.sideLength, 2);
    }
  }
}

var s1 = new Geometry.Square(5);

// Локальний псевдонім для посилання на модуль
import G = Geometry;

var s2 = new G.Square(10);

// Узагальнене програмування
// Класи
class Tuple<T1, T2> {
    constructor(public item1: T1, public item2: T2) {
    }
}

// Інтерфейси
interface Pair<T> {
    item1: T;
    item2: T;
}

// І функції
var pairToTuple = function<T>(p: Pair<T>) {
    return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"Привіт", item2:"Світ"});

// Включення посилання на файл визначення:
/// <reference path="jquery.d.ts" />

// Шаблонні рядки (Template Strings) (рядки, які використовують зворотні лапки)
// Інтерполяція рядків за допомогою шаблонних рядків
let name = 'Тайрон';
let greeting = `Привіт ${name}, як ся маєш?`
// Багаторядкові рядки за допомогою шаблонних рядків
let multiline = `Це приклад
багаторядкового рядку`;

// READONLY: Нова функція у TypeScript 3.1
// Члени, які доступні лише для читання
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Тайрон", age: 42 };
p1.age = 25; // Помилка, p1.age є тільки для читання

var p2 = { name: "Джон", age: 60 };
var p3: Person = p2; // Ок, тільки для читання через псевдонім p2
p3.age = 35; // Помилка, p3.age є тільки для читання
p2.age = 45; // Ок, але також змінює p3.age через псевдонім

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // Присвоєння дозволено у конструкторі
    this.model = "Unknown Model"; // Присвоєння дозволено у конструкторі
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // Помилка, елементи тільки для читання
moreNumbers.push(5); // Помилка, метод push відсутній (оскільки він змінює масив)
moreNumbers.length = 3; // Помилка, довжина тільки для читання
numbers = moreNumbers; // Помилка, відсутні методи для зміни масиву

// Tagged Union Types для моделювання стану, що може мати одну з кількох форм
type State =
  | { type: "завантаження" }
  | { type: "успіх", value: number }
  | { type: "помилка", message: string };

declare const state: State;
if (state.type === "успіх") {
  console.log(state.value);
} else if (state.type === "помилка") {
  console.error(state.message);
}

// Template Literal Types
// Використовуються для створення складних типів рядків
type OrderSize = "Звичайне" | "Велике";
type OrderItem = "Еспресо" | "Капучино";
type Order = `${OrderSize} ${OrderItem}`;

let order1: Order = "Звичайне Капучино";
let order2: Order = "Велике Еспресо";
let order3: Order = "Маленьке Капучино"; // Помилка

// Ітератори та Генератори

// Оператор for..of
// Ітерація по списку значень об'єкта, що перебирається
let arrayOfAnyType = [1, "рядок", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "рядок", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// Оператор for..in
// Ітерація по списку ключів об'єкта, що перебирається
for (const i in list) {
   console.log(i); // 0, 1, 2
}

// Type Assertion (Приведення типів)

let foo = {} // Створення порожнього об'єкта foo
foo.bar = 123 // Помилка: властивість 'bar' не існує на `{}` 
foo.baz = 'привіт світ' // Помилка: властивість 'baz' не існує на `{}` 

// Оскільки передбачуваний тип foo є `{}` (об'єкт з 0 властивостями),
// не дозволяється додавати bar і baz до нього. Однак за допомогою приведення типу,
// наступне пройде без помилок:

interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // Приведення типу
foo.bar = 123;
foo.baz = 'привіт світ'
```

## Для подальшого читання

* [Офіційний веб-сайт TypeScript](https://www.typescriptlang.org/)
* [Вихідний код на GitHub](https://github.com/microsoft/TypeScript)
* [Вивчення TypeScript](https://learntypescript.dev/)
