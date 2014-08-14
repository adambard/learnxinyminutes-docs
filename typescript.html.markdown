---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]]
---

TypeScript is a language that aims at easing development of large scale applications written in JavaScript.
TypeScript adds common concepts such as classes, modules, interfaces, generics and (optional) static typing to JavaScript.
It is a superset of JavaScript: all JavaScript code is valid TypeScript code so it can be added seamlessly to any project. In turn, the TypeScript compiler transform the code to JavaScript.

This article will focus only on TypeScript added syntax, everything else is plain [JavaScript] (../javascript/).

To test TypeScript's compiler, head to the [Playground] (http://www.typescriptlang.org/Playground) where you will be able to type code, have auto completion and directly see the resulting JavaScript.

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
//Or, using the generic array type
var list: Array<number> = [1, 2, 3];

//For enumerations
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

//Lastly, "void" is used in the special case of a function not returning anything
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

//Interfaces are structural, anything that has the properties is compliant with the interface.
//In the bellow example, any object that has a name which is a string and an age which is a number is a Person.
//This is called "duck typing".
interface Person {
  name: string;
  age: number;
  
  //Interfaces also support optional properties
  phone?: number;
}

//Interfaces can also describe a function type, to describe a function signature
interface SearchFunc {
  (source: string, subString: string): boolean;
}
//The type can then be used for functions, and the compiler will be able to check that types are compliants
//Note that only the parameters' types are important, names are not important.
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  var result = source.search(subString);
  if (result == -1) {
    return false;
  }
  else {
    return true;
  }
}


```



## Further Reading
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
