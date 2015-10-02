---
language: javascript
contributors:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
filename: javascript.js
---

JavaScript foi criada por Brendan Eich, funcionário da Netscape, em 1995. Ela
foi originalmente criada para ser uma linguagem de script para websites,
complementando o uso de Java para aplicações web mais complexas, mas a sua
integração com páginas web e seu suporte nativo nos browsers fez com que
ela se tornasse mais comum que Java no frontend web.

Javascript não é somente limitado a browsers web, no entanto: existe o Node.js,
que é um projeto que fornece um interpretador baseado no motor V8 do Google 
Chrome e está se tornando cada vez mais famoso.


Feedback são muito apreciados! Você me encontrar em
[@adambrenecki](https://twitter.com/adambrenecki), ou
[adam@brenecki.id.au](mailto:adam@brenecki.id.au).

```js
// Comentários são como em C. Comentários de uma linha começam com duas barras,
/* e comentários de múltplas linhas começam com barra-asterisco
   e fecham com asterisco-barra */

// comandos podem ser terminados com  ;
facaAlgo();

// ... mas eles não precisam ser, assim como o ponto-e-vírgula é automaticamente
// inserido quando há uma nova linha, exceto alguns casos.
facaAlgo()

// Porque esses casos podem causar resultados inesperados, vamos continuar 
// a usar ponto-e-vírgula neste guia.

///////////////////////////////////
// 1. Números, Strings e Operadores

// Javascript tem um tipo de número (que é o 64-bit IEEE 754 double).
// Doublas tem uma mantissa 52-bit, que é suficiente para guardar inteiros
// acima de 9✕10¹⁵ precisamente.
3; // = 3
1.5; // = 1.5

// A aritmética básica funciona seria de esperar.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Inclusive divisão desigual.
5 / 2; // = 2.5

// Operadores Bitwise também funcionam; quando você faz uma operação bitwise
// seu float é convertido para um int de até 32 bits.
1 << 2; // = 4

// A precedência é aplicada com parênteses.
(1 + 3) * 2; // = 8

// There are three special not-a-real-number values:
// Existem três especiais valores não-é-número-real:
Infinity; // resultado de 1/0
-Infinity; // resultado de -1/0
NaN; // resultado de 0/0

// Existe também o tipo booleano.
true;
false;

// Strings são criados com ' ou ".
'abc';
"Olá, mundo";

// Negation uses the ! symbol
// Negação usa o símbolo !
!true; // = false
!false; // = true

// Igualdade é ===
1 === 1; // = true
2 === 1; // = false

// Desigualdade é !==
1 !== 1; // = false
2 !== 1; // = true

// Mais comparações
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Strings são concatenadas com +
"Olá " + "mundo!"; // = "Olá mundo!"

// e comparadas com < e >
"a" < "b"; // = true

// A coerção de tipos é feita para comparações com dois iguais...
"5" == 5; // = true
null == undefined; // = true

// ...a menos que use ===
"5" === 5; // = false
null === undefined; // = false 

// ...que irá resultar num comportamento estranho...
13 + !0; // 14
"13" + !0; // '13true'

// Você pode acessar caracteres de uma String usando o `charAt`
"Isto é uma String".charAt(0);  // = 'I'

// ...ou usar `substring` para pegar pedaços maiores.
"Olá mundo".substring(0, 3); // = "Olá"

// `length` é uma propriedade, portanto não use ().
"Olá".length; // = 3

// Existe também o `null` e o `undefined`.
null;      // usado para indicar um valor não considerado
undefined; // usado para indicar um valor que não é a atualmente definido
           // (entretando `undefined` é usado como um próprio valor

// false, null, undefined, NaN, 0 and "" são valores falsy;
// qualquer outro valor é truthy
// Note que 0 é falsy e "0" é truthy, até mesmo 0 == "0".

///////////////////////////////////
// 2. Variáveis, Arrays e Objetos

// Variáveis são declarados com a palavra-chave `var`. O Javascript é 
// dinâmicamente tipado, portanto você não precisa especificar o tipo.
// Atribuições usam um simples caracter de `=`.
var someVar = 5;

// se você deixar de colocar a palavra-chave var, você não receber um erro...
someOtherVar = 10;

// ...mas sua variável será criada no escopo global, não no escopo em que você
// definiu ela.

// Variáveis declaradas sem receberem um valor são definidas como `undefined`.
var someThirdVar; // = undefined

// Existe um shorthad para operações matemáticas em variáveis:
someVar += 5; // equivalente a someVar = someVar + 5; someVar é 10 agora
someVar *= 10; // agora someVar é 100

// e um para adição e subtração de 1
someVar++; // now someVar is 101
someVar--; // back to 100

// Arrays são listas ordenadas de valores, de qualquer tipo.
var myArray = ["Olá", 45, true];

// Seus membros podem ser acessados usando a sintaxe de colchetes.
// O indíce de um Array começa pelo 0.
myArray[1]; // = 45

// Arrays são mutáveis e de tamanho variável.
myArray.push("World");
myArray.length; // = 4

// Adicionar/modificar em um índice específico
myArray[3] = "Hello";

// Objetos de Javascript são equivalentes aos dicionários ou maps de outras
// linguagens: uma coleção não ordenada de pares chave-valor.
var myObj = {chave1: "Olá", chave2: "Mundo"};

// Chaves são strings, mas as aspas não são necessárias se elas são
// identificadores válidos no Javascript. Valores podem ser de qualquer tipo.
var myObj = {myKey: "myValue", "my other key": 4};

// Atributos de objetos também podem ser acessados com a sintaxe de colchetes.
myObj["my other key"]; // = 4

// ... ou usando a sintaxe de ponto, passando a chave que é um identificador
// válido.
myObj.myKey; // = "myValue"

// Objetos são mutáveis, valores podem ser modificados e novas chaves 
// adicionadas.
myObj.myThirdKey = true;

// Se você tentar acessar um valor que não foi determinado ainda, você irá
// receber `undefined`.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Lógica e Estruturas de Controle

// A sintaxe para essa seção é quase idêntica a maioria das linguagens.

// The `if` structure works as you'd expect.
// A estrutura `if` funciona como deveria ser.
var count = 1
if (count == 3){
    // executa se count é 3
} else if (count == 4){
    // executa se count é 4
} else {
    // executa se count não é 3 nem 4
}

// Como se faz `while`.
while (true){
    // Um loop infinito!
}

// Os loops do-while são como os loops de while, exceto quando eles sempre
// executam pelo menos uma vez.
do {
    input = getInput();
} while (!isValid(input))

// The `for` loop is the same as C and Java:
// initialisation; continue condition; iteration.

// O loop `for` é o mesmo de C e Java:
// inicialização, condição de continuar; iteração
for (var i = 0; i < 5; i++){
    // vai rodar cinco vezes
}

// && é o `e` lógico , || é o `ou` lógico
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (cor == "red" || cor == "blue"){
    // cor é vermelha OU azul
}

// && e || "pequeno circuito", é útil para determinar valores padrões.
var name = otherName || "padrão";

// O `switch` checa pela igualdade com `===`.
// Use `break` após cada `case`
grade = 'B';
switch (grade) {
  case 'A':
    console.log("Great job");
    break;
  case 'B':
    console.log("OK job");
    break;
  case 'C':
    console.log("You can do better");
    break;
  default:
    console.log("Oy vey");
    break;
}


///////////////////////////////////
// 4. Functions, Scope and Closures

// JavaScript functions are declared with the `function` keyword.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Note that the value to be returned must start on the same line as the
// `return` keyword, otherwise you'll always return `undefined` due to
// automatic semicolon insertion. Watch out for this when using Allman style.
function myFunction()
{
    return // <- semicolon automatically inserted here
    {
        thisIsAn: 'object literal'
    }
}
myFunction(); // = undefined

// JavaScript functions are first class objects, so they can be reassigned to
// different variable names and passed to other functions as arguments - for
// example, when supplying an event handler:
function myFunction(){
    // this code will be called in 5 seconds' time
}
setTimeout(myFunction, 5000);
// Note: setTimeout isn't part of the JS language, but is provided by browsers
// and Node.js.

// Function objects don't even have to be declared with a name - you can write
// an anonymous function definition directly into the arguments of another.
setTimeout(function(){
    // this code will be called in 5 seconds' time
}, 5000);

// JavaScript has function scope; functions get their own scope but other blocks
// do not.
if (true){
    var i = 5;
}
i; // = 5 - not undefined as you'd expect in a block-scoped language

// This has led to a common pattern of "immediately-executing anonymous
// functions", which prevent temporary variables from leaking into the global
// scope.
(function(){
    var temporary = 5;
    // We can access the global scope by assiging to the "global object", which
    // in a web browser is always `window`. The global object may have a
    // different name in non-browser environments such as Node.js.
    window.permanent = 10;
})();
temporary; // raises ReferenceError
permanent; // = 10

// One of JavaScript's most powerful features is closures. If a function is
// defined inside another function, the inner function has access to all the
// outer function's variables, even after the outer function exits.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Inner functions are put in the local scope by default, as if they were
    // declared with `var`.
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout is asynchronous, so the sayHelloInFiveSeconds function will
    // exit immediately, and setTimeout will call inner afterwards. However,
    // because inner is "closed over" sayHelloInFiveSeconds, inner still has
    // access to the `prompt` variable when it is finally called.
}
sayHelloInFiveSeconds("Adam"); // will open a popup with "Hello, Adam!" in 5s

///////////////////////////////////
// 5. More about Objects; Constructors and Prototypes

// Objects can contain functions.
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// When functions attached to an object are called, they can access the object
// they're attached to using the `this` keyword.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// What this is set to has to do with how the function is called, not where
// it's defined. So, our function doesn't work if it isn't called in the
// context of the object.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Inversely, a function can be assigned to the object and gain access to it
// through `this`, even if it wasn't attached when it was defined.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// We can also specify a context for a function to execute in when we invoke it
// using `call` or `apply`.

var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// The `apply` function is nearly identical, but takes an array for an argument
// list.

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// This is useful when working with a function that accepts a sequence of
// arguments and you want to pass an array.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// But, `call` and `apply` are only temporary. When we want it to stick, we can
// use `bind`.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` can also be used to partially apply (curry) a function.

var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16

// When you call a function with the `new` keyword, a new object is created, and
// made available to the function via the this keyword. Functions designed to be
// called like that are called constructors.

var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Every JavaScript object has a 'prototype'. When you go to access a property
// on an object that doesn't exist on the actual object, the interpreter will
// look at its prototype.

// Some JS implementations let you access an object's prototype on the magic
// property `__proto__`. While this is useful for explaining prototypes it's not
// part of the standard; we'll get to standard ways of using prototypes later.
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// This works for functions, too.
myObj.myFunc(); // = "hello world!"

// Of course, if your property isn't on your prototype, the prototype's
// prototype is searched, and so on.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// There's no copying involved here; each object stores a reference to its
// prototype. This means we can alter the prototype and our changes will be
// reflected everywhere.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// We mentioned that `__proto__` was non-standard, and there's no standard way to
// change the prototype of an existing object. However, there are two ways to
// create a new object with a given prototype.

// The first is Object.create, which is a recent addition to JS, and therefore
// not available in all implementations yet.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// The second way, which works anywhere, has to do with constructors.
// Constructors have a property called prototype. This is *not* the prototype of
// the constructor function itself; instead, it's the prototype that new objects
// are given when they're created with that constructor and the new keyword.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6
myNewObj2.getMyNumber(); // = 6

// Built-in types like strings and numbers also have constructors that create
// equivalent wrapper objects.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Except, they aren't exactly equivalent.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // This code won't execute, because 0 is falsy.
}
if (Number(0)){
    // This code *will* execute, because Number(0) is truthy.
}

// However, the wrapper objects and the regular builtins share a prototype, so
// you can actually add functionality to a string, for instance.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"

// This fact is often used in "polyfilling", which is implementing newer
// features of JavaScript in an older subset of JavaScript, so that they can be
// used in older environments such as outdated browsers.

// For instance, we mentioned that Object.create isn't yet available in all
// implementations, but we can still use it with this polyfill:
if (Object.create === undefined){ // don't overwrite it if it exists
    Object.create = function(proto){
        // make a temporary constructor with the right prototype
        var Constructor = function(){};
        Constructor.prototype = proto;
        // then use it to create a new, appropriately-prototyped object
        return new Constructor();
    }
}
```

## Further Reading

The [Mozilla Developer
Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) provides
excellent documentation for JavaScript as it's used in browsers. Plus, it's a
wiki, so as you learn more you can help others out by sharing your own
knowledge.

MDN's [A re-introduction to
JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
covers much of the concepts covered here in more detail. This guide has quite
deliberately only covered the JavaScript language itself; if you want to learn
more about how to use JavaScript in web pages, start by learning about the
[Document Object
Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Learn Javascript by Example and with Challenges](http://www.learneroo.com/modules/64/nodes/350) is a variant of this reference with built-in challenges. 

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) is an in-depth
guide of all the counter-intuitive parts of the language.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/) is a classic guide / reference book. 

In addition to direct contributors to this article, some content is adapted
from Louie Dinh's Python tutorial on this site, and the [JS
Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
on the Mozilla Developer Network.
