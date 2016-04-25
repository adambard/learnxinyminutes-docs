---
lang: pt-br
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Will Mendes", "http://willmendesneto.github.io"]
filename: learntypescript.ts
---

TypesScript é uma linguagem que visa facilitar o desenvolvimento de grandes aplicações escritas em JavaScript.
TypeScript adiciona conceitos conhecidos como classes, módulos, interfaces, generics e tipagem estática (sendo esta opcional) ao seu JavaScript.
É um superset para o JavaScript: todo o código JavaScript é validado pelo TypeScript para que possa ser adicionados diretamente a qualquer projeto. O compilador TypeScript gera JavaScript.

Este artigo abordará somente na sintaxe extra do TypeScript, ao contrário do  [JavaScript] (../javascript/).

Para testar o compilador TypeScript, visite o [Playground] (http://www.typescriptlang.org/Playground) onde você vai ser capaz de escrever código TypeScript e ver diretamente o JavaScript gerado.

```js
// Existem 3 tipos básicos no TypeScript
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

// Quando é impossível saber, existe o tippo "Any"
var notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // ok, definitivamente um boolean

// Para coleções, existem arrays tipados e arrays genéricos
var list: number[] = [1, 2, 3];
// Uma outra alternativa, utilizando o array do tipo genérico
var list: Array<number> = [1, 2, 3];

// Para enumerações:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// Por último, "void" é usado em casos especiais quando uma função não possui retorno
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// Funções são first class citizens, suportando sintaxe lambda "fat arrow" e
// Usando inferência de tipo

// A seguir, são equivalentes, a mesma assinatura será inferida pelo
// Compilador, e o mesmo JavaScript será gerado
var f1 = function(i: number): number { return i * i; }
// Retorna o tipo inferido
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Retorna o tipo inferido
var f4 = (i: number) => { return i * i; }
// Retorna o tipo inferido, em uma linha "one-liner" significa que não é necessário nenhum retorno de palavra-chave
var f5 = (i: number) =>  i * i;

// Interfaces é qualquer estrutura que tenha  propriedades compatíveis com
// a interface
interface Person {
  name: string;
  // Propriedades opcionais, marcadas com o sinal "?"
  age?: number;
  // E claro, funções
  move(): void;
}

// Objeto que implementa a interface "Person"
// Pode ser tratado com uma pessoa desde que ele tenha o nome e a propriedade "move"
var p: Person = { name: "Bobby", move: () => {} };
// Objetos que possuem propriedade opcional
var validPerson: Person = { name: "Bobby", age: 42, move: () => {} };
// Não é uma pessoa porque a idade não é um númetros
var invalidPerson: Person = { name: "Bobby", age: true };

// Interfaces podem também descrever um tipo de função
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Somente os tipos de parâmetros são importantes, nomes não são importantes.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Classes - membros são públicos por padrão
class Point {
  // Propiedades
    x: number;

    // Constructor - the public/private keywords in this context will generate
    // the boiler plate code for the property and the initialization in the
    // constructor.
    // In this example, "y" will be defined just like "x" is, but with less code
    // Default values are also supported

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Funções
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Membros estácos
    static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y será 0

// Herança
class Point3D extends Point {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Chamada explícita para a super classe contrutora é obrigatório
    }

    // Overwrite
    dist() {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Módulos, "." podem ser usados como separadores para submódulos
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

// alias local referenciando um módulo
import G = Geometry;

var s2 = new G.Square(10);

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

// E funções
var pairToTuple = function<T>(p: Pair<T>) {
    return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// Incluindo referências para um arquivo definido
/// <reference path="jquery.d.ts" />

```

## Leitura adicional
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Source Code on GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
