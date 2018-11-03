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
// 4. Functions (Fungsi), Scope(Ruang Lingkup) dan Closures

// Fungsi JavaScript dideklarasikan dengan keyword `function`.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Perhatikan bahwa nilai yang dikembalikan haruslah satu bari dengan
// keyword `return`, atau fungsinya akan mengembalikan `undefined` bedasarkan
// penyisipan semicolon (;) secara otomatis.
// Hati-hati akan hal ini jika menggunakan style allman.
function myFunction(){
    return // <- semicolon disisipkan disini
    {thisIsAn: 'object literal'};
}
myFunction(); // = undefined

// fungsi di JavaScript adalah first class object. jadi dapat dimasukkan kedalam
// nama variable yang berbeda dan menjadi argumen dari fungsi lain - untuk
// contoh, ketika menyuplai fungsi kedalam event handler:
function myFunction(){
    // code ini akan dijalankan dalam 5 detik
}
setTimeout(myFunction, 5000);
// Note: setTimeout bukan bagian dari JS language, tapi disediakan browsers
// dan Node.js.

// fungsi lain yang disediakan browser, setInterval
function myFunction(){
    // code ini akan dijalankan setiap 5 detik
}
setInterval(myFunction, 5000);

// Function object bahkan tidak perlu diberi nama - kamu dapat menulis
// sebuah anonymous function secara langsung kedalam sebuah argument
setTimeout(function(){
    // code ini akan djalankan dalam 5 detik
}, 5000);

// fungsi di JavaScript memiliki scope (ruang lingkup); fungsi memiliki tapi jenis
//blok (statement) yang lain tidak
if (true){
    var i = 5;
}
i; // = 5 - tidak undefined seperti bahasa lain yang memiliki scope pada blok

// Hal inilah yang menyebabkan penggunaan "immediately-executing anonymous
// functions (function anonym yang langsung dieksekusi)", yang 
// mecegah variable sementara 'bocor' ke scope global
(function(){
    var temporary = 5;
    // Kita dapat mengakses scope global dengan memasukkannya kedalam
    // "global object", yang mana di dalam web browser selalu disebu
    // `window`. global object dapat memiliki nama yang berbeda
    // pada non-browser environment seperti Node.js.
    window.permanent = 10;
})();
temporary; // memiliki ReferenceError
permanent; // = 10

// Salah satu fitur paling berguna JavaScript adalah closures. Jika sebuah fungsi
// didefinisikan dalam sebuah fungsi, fungsi yang di dalam dapat mengakses semua
// variable yang ada di luar, bahkan juka fungsi yang di luar sudah dikeluarkan.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Fungsi tedalam masuk kedalam local scope secara default, sebagaimana
    // dideklarasikan dengan `var`.
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout dalah fungsi asynchronous, jadi fungsi sayHelloInFiveSeconds akan
    // keluar sesegera mungkin, dan setTimeout akan memanggil inner setelah. Namun,
    // karena inner "ditutup setelah" sayHelloInFiveSeconds, inner masih
    // memiliki akses pada variable `prompt`ketika dipanggil.
}
sayHelloInFiveSeconds("Adam"); // will open a popup with "Hello, Adam!" in 5s

///////////////////////////////////
// 5. Lebih lanjut tentang object; Constructor dan Prototype

// Object dapat menyimpan sebuah fungsi.
var myObj = {
    myFunc: function(){
        return "Hello dunia!";
    }
};
myObj.myFunc(); // = "Hello dunia!"

// Ketika sebuah fungsi dalam objek dipanggil, fungsi tersebut dapat mengakses objeknya
// dengan menggunakan `this` keyword.
myObj = {
    myString: "Hello dunia!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello dunia!"

// Apa yang terjadi disini adalah bagaimana sebuah fungsi dipanggil, tidak pada 
// dimana fungsi tersebut di definisikan. Jadi, fungsi kita tidak dapat bekerja jika
// dipanggil diluar konteks dari object-nya.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Kebalikannya, sebuah sebuah fungsi dapat diisikan pada sebuah object dan mendapatkan akses ke objeknya
// melalui `this`, bahkan jika fungsi tersebut tidak berada didalamnya ketika didefinisikan.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// Kita juga dapat menspesifikasikan conteks dari sebuah fungsi dengan mengeksekusi nya dengan
// menggunakan `call` atau `apply`.

var anotherFunc = function(s){
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// fungsi `apply` hampir sama, tapi menerima sebuah array sebagai argument.

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Hal ini sangat berguna ketika bekerja dengan sebuah fungsi yang menerima serangkaian 
// argument dan kamu ingin memasukan sebuah array.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// tapi, `call` dan `apply` bersifat sementara. Ketika kita ingin terus menggunakannya, kita dapat
// menggunakan `bind`.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` juga dapat digunakan sebagian pada sebuah fungsi (curry).

var product = function(a, b){ return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Ketika sebuah fungsi dipanggil dengan keyword `new`, sebuah object baru dibuat, dan
// tersedia dengan menggunakan keyword `this`. Fungsi ini di desain untuk
// dipanggil layaknya sebuah constructor.

var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Tidak seperti kebanyakan bahasa berorientasi objek yang lain, JavaScript tika punya 
// konsep 'instances' yang dibuat dari rancangan 'class'; sebagai gantinya, JavaScript
// menggabungkan instantiation dan inheritance dalam satu konsep: 'prototype'.

// Setiap JavaScript object memiliki sebuah 'prototype'. Ketika mengakses sebuah properti 
// pada sebuah object yang tidak ada object yang sesungguhnya, interpreter akan
// menganggapnya sebagai prototype-nya.

// Beberapa implementasi JS mengizinkanmu untuk mengakses prototype sebuah object dengan menggunakan magic
// property yaitu `__proto__`. Meskipun ini berguna untuk  menjelas kan prototype 
// tapi bukan bagian dari standar; kita akan mengakses prototype dengan standar yang benar nanti.
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

// Berfungsi untuk fungsi juga.
myObj.myFunc(); // = "hello dunia!"

// Tentu saja, jika sebuah property tidak berada dalam prototype mu, prototype
// dalam prototype akan dicari, dan seterusnya.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Tidak ada penyalinan disini; setiap object menyimpan sebuah refrensi ke
// prototype-nya. Hal ini berarti kita dapat mengubah sebuah prototype dan pengubahan ini akan
// berlaku dimanapun.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// for/in statement membuat pengulangan pada property dari sebuah object,
// menelusuri setiap prototype hingga sebuah prototype.
for (var x in myObj){
    console.log(myObj[x]);
}
///prints:
// Hello dunia!
// 43
// [Function: myFunc]

// Jika mempertimbangkan untuk menyimpan sebuah property pada object secara langsung
// dan tidak pada prototype, gunakan `hasOwnProperty()`.
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
///prints:
// Hello world!

// Kita menyebutkan bahwa `__proto__` dalah cara non-standard, dan tidak ada cara standar 
// mengubah prototype dari object yang sudah ada. Namun, ada dua cara
// menciptakan object baru dengan prototype tertentu.

// Cara pertama adalah Object.create, yang merupakan tambahan baru di JS, maka dari itu
// Belum tersedia pada semua implementasi.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// Cara kedua, yang berkerja dimanapun, tida ada hubungannya dengan prototype.
// Constructor memiliki sebuah properti dengan nama prototype. Ini *bukan* prototype dari
// constructor function itu sendiri; melainkan, Sebuah prototype yang yang diberikan
// pada object baru ketika mereka diciptakan dengan constructor nya dan keyword new.
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

// Tipe data bawaan seperti string dan number juga memiliki constructor yang membuat
// object pembungkus yang sama.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Bedanya, mereka tidak sebenarnya sama.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Bagian ini tidak akan dieksekusi karena 0 itu bersifat false.
}
if (new Number(0)){
   // Bagian ini akan dieksekusi, karena sebuah angka dalam object pembungkus adalah object, dan object
   // bersifat true.
}

// Meski begitu, object pembungkus dan objek biasa berbagi prototype yang sama, jadi
// kamu dapat menambahkan fungsionalitas tambahan untuk string, misalnya.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// Hal ini biasa digunakan pada "polyfilling", yang mengimplementasikan 
// fitu baru dari JavaScript pada javascript lama JavaScript, sehingga mereka dapat
// digunakan pada environment ataupun browser lama.

// Sebagai contoh, kita telah menyebutkan Object.create tidak tersedia di semua 
// implementasi, tapi kita dapat tetap menggunakannya dengan polyfill:
if (Object.create === undefined){ // tidak menimpa jika sudah ada
    Object.create = function(proto){
        // membuat constructor sementara dengan prototype yang tepat
        var Constructor = function(){};
        Constructor.prototype = proto;
        // kemudia digunakan untuk membuat object dengan prototype yang benar
        return new Constructor();
    };
}
```

## Further Reading

[Mozilla Developer Network][1] menyediakan dokumentasi yang sangat baik sebagaimana
JavaScript digunakan pada browsernya. Plus, sebagai wiki, jadi kamu dapat membantu
banyak orang dengan membagi pengetahuanmu.

[A re-introduction to JavaScript][2] milik MDN menjelaskan banyak konsep
yang dijelaskan disini dengan lebih detail. Panduan ini hanya menjelaskan tentang
Javascript itu sendiri; Jika anda ingin belajar lebih lanjut tentang penggunaan
JavaScript dalam web pages, Mulai lah belajar tentang [Document Object Model][3].

[Learn Javascript by Example and with Challenges][4] adalah variasi dari
refrensi ini dengan tambahan tantangan di dalamnya.

[JavaScript Garden][5] adalah penjelasan lebih dalam mengenai konsep bahasa ini
yang tidak intuitif.

[JavaScript: The Definitive Guide][6] Refrensi klasik untuk javascript.

[Eloquent Javascript][8] oleh Marijn Haverbeke adalah sebuah buku (elektronik) JS yang sangat bagu dengan
sebuah terminal

[Eloquent Javascript - The Annotated Version][9] oleh Gordon Zhu juga merupakan
variasi yang bagus dari Eloquent Javascript dengan tambahan penjelasan dan klarifikasi untuk
beberapa contoh yang lebih rumit.

[Javascript: The Right Way][10] sebuah panduan yang ditujukan untuk developer baru
JavaScript dan membantu developer yang berpengalaman untuk mempelajari lebih lanjut tentang best practic yang ada.


[Javascript:Info][11] adalah sebuah tutorial modern dari dasar-dasar javascript (bahasa inti dan bekerja dengan browser)
juga dengan konsep lebih lanjut dengan penjelasan yang lengkap.


Sebagai tambahan kepada kontributor langsung pada artikel ini, beberapa konten diadaptasi dari
Tutorial Python oleh Louie Dinh pada situs ini, dan [JS Tutorial][7] pada
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
