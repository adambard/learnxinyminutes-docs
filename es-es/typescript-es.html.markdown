---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
filename: learntypescript-es.ts
translators:
    - ["Damaso Sanoja", "https://github.com/damasosanoja"]
lang: es-es
---

TypeScript es un lenguaje cuyo objetivo es facilitar el desarrollo de aplicaciones a gran escala escritas en JavaScript.
TypeScript añade conceptos comunes como clases, módulos, interfaces, genéricos y (opcionalmente) tipeo estático a JavaScript.
Es un superset de JavaScript: todo el código JavaScript es código válido en TypeScript de manera que se puede integrar fácilmente a cualquier proyecto . El compilador TypeScript emite JavaScript.

Este artículo se enfocará solo en la sintáxis extra de TypeScript, y no en [JavaScript] (../javascript/).

Para probar el compilador de TypeScript, diríjase al [Área de Pruebas] (http://www.typescriptlang.org/Playground) donde podrá tipear código, y ver como se auto-completa al tiempo que ve el código emitido JavaScript.

```js
// Existen 3 tipos básicos en TypeScript
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

// Cuando es imposible de saber, tenemos el tipo "Any"
var notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // okey, definitivamente un boolean

// Para colecciones, hay matrices de tipos y matrices genéricas
var list: number[] = [1, 2, 3];
// Alternativamente, usando la matriz genérica
var list: Array<number> = [1, 2, 3];

// Para enumeradores:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// Finalmente, "void" es usado para el caso especial de una función que no retorna nada
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// Las funciones son ciudadanos de primera clase, soportan la sintáxis lambda "fat arrow" y
// usan el tipo inferencia

// Lo siguiente es equivalante, la misma firma será inferida por el
// compilador, y el mismo JavaScript será emitido
var f1 = function(i: number): number { return i * i; }
// Retorna tipo inferido
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Retorna tipo inferido
var f4 = (i: number) => { return i * i; }
// Retorna tipo inferido, one-liner significa que no es necesario que regresen palabras claves
var f5 = (i: number) =>  i * i;

// Las interfaces son estructurales, todo lo que tenga las propiedades cumple con
// la interfase
interface Person {
  name: string;
  // Propiedades opcionales, marcadas con un "?"
  age?: number;
  // Y por supuesto funciones
  move(): void;
}

// Objeto que implementa la interfase "Persona"
// Puede ser tratada como Persona ya que posee las propiedades name y move
var p: Persona = { name: "Bobby", move: () => {} };
// Objetos que tienen propiedades opcionales:
var validPersona: Persona = { name: "Bobby", age: 42, move: () => {} };
// No es una persona porque su edad no es un número
var invalidPersona: Persona = { name: "Bobby", age: true };

// Las interfases también pueden describir un tipo de función
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Solo los tipos de parámetros son importantes, los nombres no son importantes.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Clases - los miembros son públicos por defecto
class Point {
  // Properties
    x: number;

    // Constructor - las palabras clave public/private en este contexto generarán
    // un código boiler plate para la propiedad y la inicialización en el
    // constructor.
    // En este ejemplo, "y" debe ser definida al igual que "x" lo es, pero con menos código
    // También son soportados valores por defecto

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Funciones
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Miembros estáticos
    static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y será 0

// Herencia
class Point3D extends Point {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Un llamado explícito al constructor de la super clase es indispensable
    }

    // Sobrescribir
    dist() {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Módulos, los "." pueden ser usados como separadores para los submódulos
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

// Un alias local para referirse a un módulo
import G = Geometry;

var s2 = new G.Square(10);

// Genéricos
// Clases
class Tuple<T1, T2> {
    constructor(public item1: T1, public item2: T2) {
    }
}

// Interfases
interface Pair<T> {
    item1: T;
    item2: T;
}

// Y funciones
var pairToTuple = function<T>(p: Pair<T>) {
    return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// Incluyendo referencias a un archivo de definición:
/// <reference path="jquery.d.ts" />

```

## Para mayor información
 * [Sitio Oficial de TypeScript] (http://www.typescriptlang.org/)
 * [Especificaciones del lenguaje TypeScript (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introduciendo TypeScript en Canal 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Código fuente en GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repositorio para definiciones de tipo] (http://definitelytyped.org/)
