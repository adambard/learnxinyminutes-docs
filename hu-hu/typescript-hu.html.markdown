---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Tamás Diószegi", "https://github.com/ditam"]
filename: learntypescript-hu.ts
lang: hu-hu
---

A TypeScript nyelv a JavaScript nyelven írt nagy méretű alkalmazások fejlesztését kívánja megkönnyíteni.
A TypeScript olyan, más nyelvekből ismert gyakori fogalmakat ad hozzá a JavaScripthez, mint például osztályok, interfészek, generikusság, és (opcionális) statikus típusosság.
A JavaScript egy befoglaló halmazát képzi: minden JavaScript kód érvényes TypeScript kód, így könnyen hozzáadható meglévő projektekhez. A TypeScript fordító kimenetként JavaScript kódot állít elő.

Ez a dokumentum a TypeScript által hozzáadott új szintaxissal foglalkozik, nem pedig a [Javascripttel](../javascript/).

Hogy kipróbáld a TypeScript fordítót, látogass el a [Játszótérre avagy Playground-ra](http://www.typescriptlang.org/Playground) ahol kódot írhatsz automatikus kódkiegészítéssel, és közvetlenül láthatod az előállított JavaScript kódot.

```js
// 3 alapvető típus létezik TypeScriptben
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

// Amikor nem lehet a típust előre tudni, használható az "Any" típus
var notSure: any = 4;
notSure = "talán mégis sztring lesz";
notSure = false; // tévedtem, mégis boolean

// Kollekciókból létezik típusos és generikus tömb
var list: number[] = [1, 2, 3];
// ugyanez a generikus típus használatával
var list: Array<number> = [1, 2, 3];

// Enumerált típusok:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// Végül, "void" használható a visszatérési értékkel nem bíró függvényeknél
function bigHorribleAlert(): void {
  alert("Kis idegesítő doboz vagyok!");
}

// A függvények elsőrangú (first-class) típusok, használható a vastag nyilas
// lambda szintaxis,
// a compiler pedig kikövetkezteti a típusokat (inferred types)

// A következők egyenértékűek, ugyanaz a szignatúra kerül kikövetkeztetésre, és
// így ugyanaz a JavaScript kód lesz előállítva
var f1 = function(i: number): number { return i * i; }
// Következtetett visszatérési értékkel
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Következtetett visszatérési értékkel
var f4 = (i: number) => { return i * i; }
// Következtetett visszatérési értékkel, 
// ebben az egysoros formában nem szükséges a return kulcsszó
var f5 = (i: number) =>  i * i;

// Az interfészek szerkezeti alapon működnek, vagyis minden objektum, ahol 
// jelen vannak a megfelelő mezők kompatibilis az interfésszel
interface Person {
  name: string;
  // Az opcionális tagokat "?" jelöli
  age?: number;
  // És persze függvények is:
  move(): void;
}

// Egy objektum, ami megvalósítja a "Person" interfészt
// Tekinthető Personnek, hiszen van name és move mezője
var p: Person = { name: "Bobby", move: () => {} };
// Egy objektum, ahol az opcionális mező is jelen van:
var validPerson: Person = { name: "Bobby", age: 42, move: () => {} };
// Ez viszont nem Person, mert az age mező típusa nem szám!
var invalidPerson: Person = { name: "Bobby", age: true };

// Az interfészekkel függvény típusok is leírhatóak:
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Csak a paraméterek típusai számítanak, a neveik nem.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Osztályok - a mezők alapértelmezésben publikusak
class Point {
    // Mezők
    x: number;

    // Konstruktor - a public/private kulcsszavak ebben a kontextusban
    // legenerálják a mezőkhöz szükséges kódot a konstruktorban.
    // Ebben a példában az "y" ugyanúgy definiálva lesz, mint az "x", csak 
    // kevesebb kóddal.
    // Alapértelmezett (default) értékek is megadhatóak.

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Metódusok
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Statikus mezők
    static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y itt 0 lesz

// Öröklés
class Point3D extends Point {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Szükséges az ősosztály konstruktorának explicit hívása
    }

    // Felülírás
    dist() {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Modulok
// ("." használható az almodulok számára)
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

// Új lokális név definiálása a module számára
import G = Geometry;

var s2 = new G.Square(10);

// Generikus típusok
// Osztályok
class Tuple<T1, T2> {
    constructor(public item1: T1, public item2: T2) {
    }
}

// Interfészek
interface Pair<T> {
    item1: T;
    item2: T;
}

// és függvények
var pairToTuple = function<T>(p: Pair<T>) {
    return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// definíciós fájl hivatkozása:
/// <reference path="jquery.d.ts" />

```

## További források
 * [TypeScript hivatalos weboldala] (http://www.typescriptlang.org/)
 * [TypeScript nyelv specifikációja (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Forráskód GitHubon] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - típusdefiníciók gyűjteménye] (http://definitelytyped.org/)
