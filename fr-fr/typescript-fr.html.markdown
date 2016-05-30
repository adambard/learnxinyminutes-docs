---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Alois de Gouvello", "https://github.com/aloisdg"]
filename: learntypescript-fr.ts
lang: fr-fr
---

TypeScript est un langage visant à faciliter le développement d'applications larges et scalables, écrites en JavaScript.
TypeScript ajoute des concepts classiques comme les classes, les modules, les interfaces, les génériques et le typage statique (optionnel) à JavaScript.
C'est une surcouche de JavaScript : tout le code JavaScript est valide en TypeScript ce qui permet de l'ajouter de façon transparente à n'importe quel projet. Le code TypeScript est transcompilé en JavaScript par le compilateur.

Cet article se concentrera seulement sur la syntaxe supplémentaire de TypeScript, plutôt que celle de [JavaScript] (../javascript/).

Pour tester le compilateur de TypeScript, rendez-vous au [Playground] (http://www.typescriptlang.org/Playground) où vous pourrez coder, profiter d'une autocomplétion et accéder directement au rendu JavaScript.

```js
// Il y a 3 types basiques en TypeScript
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

// Si nous ne pouvons pas déterminer le type, on utilise `Any`
var notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // ok, définitivement un booléen

// Pour les collections, il y a les tableaux typés et les tableaux génériques
var list: number[] = [1, 2, 3]; // Un tableaux typé
var list: Array<number> = [1, 2, 3]; // un tableau générique

// Pour les énumeration
enum Color { Red, Green, Blue };
var c: Color = Color.Green;

// Enfin, `void` est utilisé dans le cas spécifique
// d'une fonction ne retournant rien
function bigHorribleAlert(): void {
  alert("Je suis une petite boîte ennuyeuse !");
}

// Les fonctions sont des entités de première classe. Le langage supporte
// les expressions lambda et utilise l'inférence de type

// Les fonctions ci-dessous sont équivalentes, une signature identique
// sera inférée par le compilateur, et le même JavaScript sera généré
var f1 = function(i: number): number { return i * i; }
// Retourne un type inféré
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Retourne un type inféré
var f4 = (i: number) => { return i * i; }
// Retourne un type inféré, ici le mot clé `return` n'est pas nécessaire
var f5 = (i: number) =>  i * i;

// Les interfaces sont structurées, tout les objets qui ont ces propriétés
// sont compatible avec l'interface
interface Person {
  name: string;
  // Les propriétés optionnelles sont identifiées avec un "?"
  age?: number;
  // Et bien sûr, les fonctions
  move(): void;
}

// Un objet implémentant l'interface "Person" peut être traité comme 
// une Person car il a les propriétés "name" et "move"
var p: Person = { name: "Bobby", move: () => {} };
// Des objets implémentants la propriété optionnelle :
// valide car "age" est un nombre
var validPerson: Person = { name: "Bobby", age: 42, move: () => {} };
// invalide car "age" n'est pas un nombre
var invalidPerson: Person = { name: "Bobby", age: true };

// Les interfaces peuvent aussi décrire un type de fonction
interface SearchFunc {
  (source: string, subString: string): boolean;
}

// Seul les types des paramètres sont importants. Les noms ne le sont pas.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Les membres des classes sont publiques par défaut.
class Point {
  // Propriétés
  x: number;

  // Constructeur - Les mots clés "public" et "private" dans ce contexte
  //  génèrent le code de la propriété et son initialisation dans le
  // constructeur. Ici, "y" sera défini de la même façon que "x",
  // mais avec moins de code. Les valeurs par défaut sont supportées.
  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Fonctions
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Membres statiques
  static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); // y sera 0

// Héritage
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    // Un appel explicite au constructeur de la super classe
    // est obligatoire.
    super(x, y);
  }

  // Redéfinition
  dist() {
    var d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Modules, "." peut être utilisé comme un séparateur de sous modules.
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

// Alias local pour référencer un module
import G = Geometry;

var s2 = new G.Square(10);

// Génériques
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

// Et fonctions
var pairToTuple = function<T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// Inclure des références à un fichier :
/// <reference path="jquery.d.ts" />

```

## Lectures complémentaires
 * [Site officiel de TypeScript] (http://www.typescriptlang.org/)
 * [Spécification du langage TypeScript (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Code source sur GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
