---
language: TypeScript
lang: ru-ru
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Fadil Mamedov", "https://github.com/fadilmamedov"]
    - ["Andre Polykanine", "https://github.com/Oire"]
filename: learntypescript-ru.ts
---

TypeScript — это язык программирования, целью которого является лёгкая разработка широкомасштабируемых JavaScript-приложений.
TypeScript добавляет в Javascript общие концепции, такие, как классы, модули, интерфейсы, обобщённое программирование и (опционально) статическую типизацию.  
Это надмножество языка JavaScript: весь JavaScript-код является валидным TypeScript-кодом, следовательно, может быть добавлен бесшовно в любой проект. 
Компилятор TypeScript генерирует JavaScript-код.

Эта статья концентрируется только на синтаксисе TypeScript, в противовес статье о [JavaScript](javascript-ru/).

Для тестирования компилятора TypeScript пройдите по ссылке в [песочницу](http://www.typescriptlang.org/Playground). 
Там вы можете написать код (с поддержкой автодополнения) и сразу же увидеть сгенерированный JavaScript код.

```js
// В TypeScript есть 3 базовых типа
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Андерс";

// Тип «any» для случаев, когда заранее неизвестен тип переменной
var notSure: any = 4;
notSure = "а может быть, строка";
notSure = false; // а теперь логический тип

// Для коллекций есть типизированные массивы и обобщённые массивы
var list: number[] = [1, 2, 3];
// Как альтернатива, использование обобщённого массива
var list: Array<number> = [1, 2, 3];

// Перечисления:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// Наконец, «void» используется для обозначения того, что функция ничего не возвращает
function bigHorribleAlert(): void {
  alert("Я маленькое надоедливое окошко!");
}

// Функции — это объекты первого класса. Они поддерживают лямбда-синтаксис (=>)
// и используют вывод типов (type inference)

// Следующие строки кода являются эквивалентными, компилятором предполагается
// одинаковая сигнатура, на выходе генерируется одинаковый JavaScript-код
var f1 = function(i: number): number { return i * i; }
// Предполагается возвращаемый тип
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Предполагается возвращаемый тип
var f4 = (i: number) => { return i * i; }
// Предполагается возвращаемый тип, в однострочной функции ключевое слово «return» не нужно
var f5 = (i: number) =>  i * i;

// Интерфейсы являются структурными; всё, что имеет свойства, совместимо с интерфейсом
interface Person {
  name: string;
  // Опциональные свойства, помеченные символом «?»
  age?: number;
  // И, конечно, функции
  move(): void;
}

// Объект, который реализует интерфейс «Person»
// К нему можно обращаться, как к «Person», так как он имеет свойства «name» и «move»
var p: Person = { name: "Бобби", move: () => {} };
// Объекты, которые могут иметь опциональные свойства:
var validPerson: Person = { name: "Бобби", age: 42, move: () => {} };
// Это не «Person», поскольку «age» не является числовым значением
var invalidPerson: Person = { name: "Бобби", age: true };

// Интерфейсы могут также описывать функциональный тип
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Важны только типы параметров, имена — нет.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Классы. Члены класса по умолчанию являются публичными
class Point {
  	// Свойства
    x: number;

    // Конструктор — ключевые слова public/private в данном контексте сгенерируют
    // шаблонный код для свойства и для инициализации в конструкторе
    // В данном примере «y» будет определён так же, как и «x», но меньшим количеством кода
    // Значения по умолчанию также поддерживаются

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Функции
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Статические члены
    static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y будет равен 0

// Наследование
class Point3D extends Point {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Явный вызов конструктора базового класса обязателен
    }

    // Перегрузка
    dist() {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Модули, знак «.» может быть использован как разделитель для обозначения подмодулей
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

// Локальный псевдоним для ссылки на модуль
import G = Geometry;

var s2 = new G.Square(10);

// Обобщённое программирование
// Классы
class Tuple<T1, T2> {
    constructor(public item1: T1, public item2: T2) {
    }
}

// Интерфейсы
interface Pair<T> {
    item1: T;
    item2: T;
}

// И функции
var pairToTuple = function<T>(p: Pair<T>) {
    return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// Включение ссылки на файл определения:
/// <reference path="jquery.d.ts" />

```

## Для дальнейшего чтения
 * [Официальный веб-сайт TypeScript](http://www.typescriptlang.org/)
 * [Спецификация языка TypeScript (pdf)](http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg — Introducing TypeScript на Channel 9](http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Исходный код на GitHub](https://github.com/Microsoft/TypeScript)
 * [Definitely Typed — репозиторий определений типов](http://definitelytyped.org/)
