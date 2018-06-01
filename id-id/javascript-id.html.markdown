---
language: javascript
contributors:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
filename: javascript.js
translator:
    - ["Ganesha Danu Enastika", "http://github.com/blinfoldking"]
---

Javascript dibuat oleh Netscape (Brendan Eich) pada 1995. pada awalnya 
diciptakan sebagai scripting language yang sederhana untuk pengembangan web,
dan melengkapi penggunaan dari Java untuk aplikasi web yang lebih kompleks, tapi 
akibar dari keeratan integrasinya dengan laman web dan built-in support dalam browser
menyebabkan javascript lebih banyak digunakan dibandingkan java dalam frontend dari
sebuah web

Javascript tidak hanya terbatas pada web browser, melalui Node.js,  sebuah project
yang menyediakan runtime terpisah dari browser menggunakan Google's Chrome v8 Javascript
Engine, yang mana semakin populer

Javascript memiliki syntax mirip dengan C, jika anda pernah menggunakan bahasa
seperti C ataupun Java, banyak sekali syntax dasar yang akan terasa familiar. Meskipun memiliki
persamaan dalam sytax dan nama, permodelan object dalam javascript sangat berbeda dengan Java


```js
// Comment sebaris dimulai dengan garsi miring sebanyak dua kali
/* Comment banyak baris dimulai dan diakhiri dengan 
garsi miring bintang dan bintang garsi miring */

// sebuah statment dapat diakhiri dengan ;
lakukanSesuatu();

// ... tapi hal ini tidak diwajibkan, titik koma dengan sendiri nya akan ditambahkan dengan otomatis
// untuk setiap baris baru, kecuali pada keadaan tertentu.
lakukanSesuatu()

// untuk menghindari hal tak terduga kita akan tetap
// menggunakan titik koma untuk guide ini

///////////////////////////////////
// 1. Angka, String dan operator

// JavaScript hanya memiliki satu tipe data angka (yang mmana adalah 64-bit IEEE 754 double).
// Doubles memiliki 52-bit mantissa, yang mana cukup untuk menampung banyak integer
// hingga 9✕10¹⁵ .
3; // = 3
1.5; // = 1.5

// Artimatik seperti pada umumnya.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// bahkan untuk penjumlahan ganjil.
5 / 2; // = 2.5

// dan pembagian modulo.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Operasi Bitwise juga dapat  dilakukan; ketika kamu melakukan operasi bitwise pada float
// akan  di konversi menjadi signed int *hingga* 32 bits.
1 << 2; // = 4

// Presedensi ditentukan dengan kurung.
(1 + 3) * 2; // = 8

// Ada tiga jenis angka dengan tipe non-real:
Infinity; // dihasilkan dari misalnya 1/0
-Infinity; // dihasilkan dari misalnya -1/0
NaN; // dihasilkan dari misalnya 0/0, memiliki arti 'Not a Number'

// Terdapat juga tipe data boolean
true;
false;

// String ditandai dengan ' atau ".
'abc';
"Halo, dunia";

// Negasi disimbolkan dengan!
!true; // = false
!false; // = true

// Persamaan dengan ===
1 === 1; // = true
2 === 1; // = false

// Pertidaksamaan dengan !==
1 !== 1; // = false
2 !== 1; // = true

// Pembanding lainnya
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Strings disatukan dengan +
"Halo " + "dunia!"; // = "Halo dunia!"

// ... bekerja tidak hanya pada string
"1, 2, " + 3; // = "1, 2, 3"
"Halo " + ["dunia", "!"]; // = "Halo dunia,!"

// string dapat dibandingkan dengan < dan >
"a" < "b"; // = true

// type data berbeda dapat dibandingkan dengan ==...
"5" == 5; // = true
null == undefined; // = true

// ...kecuali menggunakan ===
"5" === 5; // = false
null === undefined; // = false

// ...yang dapat menyebabkan ke hasil yang aneh...
13 + !0; // 14
"13" + !0; // '13true'

// anda dapat mengakses sebuah karakter dalam string menggunakan `charAt`
"Ini String".charAt(0);  // = 'I'

// ...atau menggunakan `substring` untuk mendapatkan penggalan yang lebih  besar.
"Halo dunia".substring(0, 4); // = "Halo"

// `length` adalah sebuah property, jangan menggunakan ().
"Halo".length; // = 4

// terdapat `null` dan `undefined`.
null;      // menandakan sesuatu yang tidak memiliki value
undefined; // menandakan sesuatu yang belum/atau tidak ada (meskipun
           // `undefined` dengan sendirinya adalah sebuah nilai)

// false, null, undefined, NaN, 0 dan "" bernilai false; selain itu bernilai true.
// NB : 0 bernilai salah dan "0" bernilai benar, meskipun 0 == "0".

///////////////////////////////////
// 2. Variables, Arrays and Objects

// sebuah variable dideklarasikan dengan keyword `var`. JavaScript memiliki
// type data dinamis, sehingga anda tidak perlu menentukan tipe datanya. 
// Assignment dilakukan menggunakan '='.
var someVar = 5;

// jika anda lupa akan 'var', anda tidak akan mendapat error...
someOtherVar = 10;

// ...tapi variable yang anda dapat diakses secara global (global scope), tidak pada
// scope yang seharusnya.

// Variabel yang dideklarasikan tanpa di assign memiliki tipe data undefined.
var someThirdVar; // = undefined

// jika ingin mendeklarasikan beberapa variable, hanya perlu menggunakan
// pembatas koma
var someFourthVar = 2, someFifthVar = 4;

// terdapat beberapa singkatan dalam operasi matematika:
someVar += 5; // sama saja dengan someVar = someVar + 5; someVar = 10
someVar *= 10; // someVar = 100

// singkatan yang lebih pendek untuk penjumlahan dan pengurangan dengan 1
someVar++; // sekarang someVar adalah 101
someVar--; // kembali menjadi 100

// Array adalah himpunan terurut dari object apapun.
var myArray = ["Halo", 45, true];

// isi dari sebuah array dapat diakses dengan menggunakan [].
// array memiliki indeks dari 0.
myArray[1]; // = 45

// array dapat diubah dan memiliki panjang.
myArray.push("World");
myArray.length; // = 4

// menambahkan atau merubah isi array
myArray[3] = "Halo";

// menambahkan atau membuang isi array pada awal atau akhir array
myArray.unshift(3); // menambahkan pada awal array
someVar = myArray.shift(); // membuang array pada posisi awal dan me-return-kannya
myArray.push(3); // menambahkan array pada posisi akhir
someVar = myArray.pop(); // membuang array pada posisi akhir dan me-return-kannya

// menyatukan semua array dengan ';'
var myArray0 = [32,false,"js",12,56,90];
myArray0.join(";") // = "32;false;js;12;56;90"

// mengakses subarray dari indeks 1 (include) ke 4 (exclude)
myArray0.slice(1,4); // = [false,"js",12]

// membuang 4 elemen mulai dari indeks 2, dan menambahkan string
// "hi","wr" dan "ld"; me-return subarray
myArray0.splice(2,4,"hi","wr","ld"); // = ["js",12,56,90]
// myArray0 === [32,false,"hi","wr","ld"]

// JavaScript memiliki objek yang mirip dengan "dictionaries" atau "maps" pada
// language lain: sebuah koleksi key-value pair.
var myObj = {key1: "Hello", key2: "World"};

// Key adalah string, tapi kutip tidak diperlukan selama valid sebagai
// JavaScript identifier. Values bisa bertipe apa saja.
var myObj = {myKey: "myValue", "my other key": 4};

// atribut dari object dapat diakses menggunakan [],
myObj["my other key"]; // = 4

// ... atau menggunakan titik, jika terdapat  key sebagai indentifier yang valid.
myObj.myKey; // = "myValue"

// Object dapat diubah; value dapat diubah dan menambahkan key baru.
myObj.myThirdKey = true;

// jika kamu mencoba mengakses value yang tidak ada, kamu akan mendapat 
//data berupa undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Struktur logik dan kontrol

// Struktur 'if' seperti pada umumnya.
var count = 1;
if (count == 3){
    // mengevaluasi apakah count bernilai 3
} else if (count == 4){
    // mengevaluasi apakah count bernilai 4
} else {
    // mengevaluasi apakah count tidak bernilai 3 atau 4
}

// Begitu juga dengan `while`.
while (true){
    // Sebuah infinite loop!
}

// Do-while loops seperti while loops, namu struktur ini berjalan setidaknya sekali.
var input;
do {
    input = getInput();
} while (!isValid(input));

// `for` loop sama seperti yang ada di C and Java:
// kondisi awal; kondisi berjalan; iterasi (perubahan).
for (var i = 0; i < 5; i++){
    // akan berjalan 5 kali
}

// keluar dari loop yang sudah di beri 'label' seperti Java
outer:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break outer;
            // keluar  dari loop yang terluar (yang diberi label outer) bukan yang di dalam
        }
    }
}

// for/in statement dapat melakukan perulangan di dalam properti sebuah objek.
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
} // description = 'Paul Ken 18 '

// && adalah operasi logis and, || operasi logis or
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // 'color' dapat berupa red atau blue
}

// && dan || disebut "short circuit", yang berguna untuk menentukan nilai default
var name = otherName || "default";

// `switch` statement mengecek persamaan dengan `===`.
// Gunakan 'break' untuk setiap kasus (case)
// atau kasus (case) setelahnya yang benar akan dieksekusi juga.
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
function myFunction(){
    return // <- semicolon automatically inserted here
    {thisIsAn: 'object literal'};
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

// Another function provided by browsers is setInterval
function myFunction(){
    // this code will be called every 5 seconds
}
setInterval(myFunction, 5000);

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
    // We can access the global scope by assigning to the "global object", which
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
        return "Hello dunia!";
    }
};
myObj.myFunc(); // = "Hello dunia!"

// When functions attached to an object are called, they can access the object
// they're attached to using the `this` keyword.
myObj = {
    myString: "Hello dunia!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello dunia!"

// What this is set to has to do with how the function is called, not where
// it's defined. So, our function doesn't work if it isn't called in the
// context of the object.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Inversely, a function can be assigned to the object and gain access to it
// through `this`, even if it wasn't attached when it was defined.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// We can also specify a context for a function to execute in when we invoke it
// using `call` or `apply`.

var anotherFunc = function(s){
    return this.myString + s;
};
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

var product = function(a, b){ return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// When you call a function with the `new` keyword, a new object is created, and
// made available to the function via the `this` keyword. Functions designed to be
// called like that are called constructors.

var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Unlike most other popular object-oriented languages, JavaScript has no
// concept of 'instances' created from 'class' blueprints; instead, JavaScript
// combines instantiation and inheritance into a single concept: a 'prototype'.

// Every JavaScript object has a 'prototype'. When you go to access a property
// on an object that doesn't exist on the actual object, the interpreter will
// look at its prototype.

// Some JS implementations let you access an object's prototype on the magic
// property `__proto__`. While this is useful for explaining prototypes it's not
// part of the standard; we'll get to standard ways of using prototypes later.
var myObj = {
    myString: "Hello dunia!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// This works for functions, too.
myObj.myFunc(); // = "hello dunia!"

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

// The for/in statement allows iteration over properties of an object,
// walking up the prototype chain until it sees a null prototype.
for (var x in myObj){
    console.log(myObj[x]);
}
///prints:
// Hello dunia!
// 43
// [Function: myFunc]

// To only consider properties attached to the object itself
// and not its prototypes, use the `hasOwnProperty()` check.
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
///prints:
// Hello world!

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
myNewObj2.myNumber = 6;
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
if (new Number(0)){
   // This code will execute, because wrapped numbers are objects, and objects
   // are always truthy.
}

// However, the wrapper objects and the regular builtins share a prototype, so
// you can actually add functionality to a string, for instance.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
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
    };
}
```

## Further Reading

The [Mozilla Developer Network][1] provides excellent documentation for
JavaScript as it's used in browsers. Plus, it's a wiki, so as you learn more you
can help others out by sharing your own knowledge.

MDN's [A re-introduction to JavaScript][2] covers much of the concepts covered
here in more detail. This guide has quite deliberately only covered the
JavaScript language itself; if you want to learn more about how to use
JavaScript in web pages, start by learning about the [Document Object Model][3].

[Learn Javascript by Example and with Challenges][4] is a variant of this
reference with built-in challenges.

[JavaScript Garden][5] is an in-depth guide of all the counter-intuitive parts
of the language.

[JavaScript: The Definitive Guide][6] is a classic guide and reference book.

[Eloquent Javascript][8] by Marijn Haverbeke is an excellent JS book/ebook with
attached terminal

[Eloquent Javascript - The Annotated Version][9] by Gordon Zhu is also a great
derivative of Eloquent Javascript with extra explanations and clarifications for
some of the more complicated examples.

[Javascript: The Right Way][10] is a guide intended to introduce new developers
to JavaScript and help experienced developers learn more about its best practices.

[Javascript:Info][11] is a modern javascript tutorial covering the basics (core language and working with a browser)
as well as advanced topics with concise explanations.


In addition to direct contributors to this article, some content is adapted from
Louie Dinh's Python tutorial on this site, and the [JS Tutorial][7] on the
Mozilla Developer Network.


[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: http://www.learneroo.com/modules/64/nodes/350
[5]: http://bonsaiden.github.io/JavaScript-Garden/
[6]: http://www.amazon.com/gp/product/0596805527/
[7]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[8]: http://eloquentjavascript.net/
[9]: http://watchandcode.com/courses/eloquent-javascript-the-annotated-version
[10]: http://jstherightway.org/
[11]: https://javascript.info/
