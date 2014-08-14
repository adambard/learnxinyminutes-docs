---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]]
---

TypeScript is a language that aims at easing development of large scale applications written in JavaScript.
TypeScript adds common concepts such as classes, modules, interfaces, generics and (optional) static typing to JavaScript.
It is a superset of JavaScript: all JavaScript code is valid TypeScript code so it can be added seamlessly to any project. The TypeScript compiler emitts JavaScript.

This article will focus only on TypeScript extra syntax, as oposed to [JavaScript] (../javascript/).

To test TypeScript's compiler, head to the [Playground] (http://www.typescriptlang.org/Playground) where you will be able to type code, have auto completion and directly see the emitted JavaScript.

```ts
//There are 3 basic types in TypeScript
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

//When it's impossible to know, there is the "Any" type
var notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // okay, definitely a boolean

//For collections, there are typed arrays and generic arrays
var list: number[] = [1, 2, 3];
//Alternatively, using the generic array type
var list: Array<number> = [1, 2, 3];

//For enumerations:
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

//Lastly, "void" is used in the special case of a function not returning anything
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

//Functions are first class citizens, have a shortened definition and can leverage the strong type inference
//All examples are equivalent, the same signature will be infered by the compiler and the same JavaScript will be emitted
var f1 = function(i: number) : number { return i * i; }
var f2 = function(i: number) { return i * i; } //Return type infered #TODO bug!
var f3 = (i : number) : number => { return i * i; }
var f4 = (i: number) => { return i * i; } //Return type infered
var f5 = (i: number) =>  i * i; //Return type infered, one-liner means no return keyword needed

//Interfaces are structural, anything that has the properties is compliant with the interface (duck typing)
interface Person {
  name: string;
  //Optional properties, marked with a "?"
  age?: number;
}
//Object that implements the "Person" interface
var p : Person = { name: "Bobby" }; //Can be treated as a Person since it has the name and age properties
//Objects that have the optional property:
var validPerson : Person = { name: "Bobby", age: 42 };
var invalidPerson : Person = { name: "Bobby", age: true }; //Is not a person because age is not a number

//Interfaces can also define method signatures:
interface PersonWhoCanTalk {
  sayHello(otherPersonsName: string): void;
}

//And also indexers, both with number and string
interface PersonWhoCanBeIndexed {
  [index: number]: string;
}
//TODO

//Interfaces can also describe a function type
interface SearchFunc {
  (source: string, subString: string): boolean;
}
//Only the parameters' types are important, names are not important.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

//Classes
class Point {
  //Properties
	x: number;
	
	//Constructor - the public/private keywords are shortcuts to generate the code for a property and its initialization
	//Equivalent to "x" in this case
	//Default values are also supported
	constructor(x: number, public y: number = 0) {
		this.x = x;
	}
	
	//Functions
	dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }
	
	//Static members
	static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y will be 0

//Inheritance
class Point3D extends Point {
	constructor(x: number, y: number, public z: number = 0) {
		super(x, y); //Explicit call to the super class constructor is mandatory
	}
	
	/Overwrite
	dist() {
		var d = super.dist();
		return Math.sqrt(d * d + this.z * this.z);
	}
}

//Modules

//Generics

//Including references to a definition file
/// <reference path="jquery.d.ts" />

```

## Further Reading
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Source Code on GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
