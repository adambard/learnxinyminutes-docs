---
language: javascript
contributors:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
filename: javascript-ms.js
translators:
    - ["abdalim", "https://github.com/abdalim"]
lang: ms-my
---

Javascript dicipta oleh Brendan Eich dari Netscape pada 1995. Pada awalnya, ia
dicipta sebagai bahasa skrip yang ringkas untuk laman web, melengkapi penggunaan
Java untuk aplikasi web yang lebih rumit, namun begitu, integrasi rapat pada
halaman web dan sokongan tersedia dalam pelayar web telah menyebabkan ia menjadi
lebih kerap digunakan berbanding Java pada bahagian hadapan laman web.

Namun begitu, Javascript tidak terhad pada pelayar web; Node.js, sebuah projek
yang menyediakan 'runtime' berdiri sendiri untuk enjin V8 Google Chrome sedang
kian mendapat sambutan yang hangat.

```js
// Komentar adalah seperti dalam C. Komentar sebaris bermula dengan dua sengkang
/* dan komentar banyak baris bermula dengan sengkang-bintang
   dan berakhir dengan bintang-sengkang */

// Pernyataan boleh ditamatkan dengan ';'
doStuff();

// ... tetapi ia tidak wajib, kerana koma bertitik secara automatik akan
// dimasukkan dimana tempat yang ada baris baru, kecuali dalam kes - kes
// tertentu.
doStuff()

// Disebabkan kes - kes itu boleh menyebabkan hasil yang tidak diduga, kami
// akan sentiasa menggunakan koma bertitik dalam panduan ini.

///////////////////////////////////
// 1. Nombor, String dan Operator

// Javascript mempunyai satu jenis nombor (iaitu 64-bit IEEE 754 double).
// Double mempunyai 52-bit mantissa, iaitu ia cukup untuk menyimpan integer
//    sehingga 9✕10¹⁵ secara tepatnya.
3; // = 3
1.5; // = 1.5

// Sebahagian aritmetic asas berfungsi seperti yang anda jangkakan.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Termasuk pembahagian tidak rata.
5 / 2; // = 2.5

// Dan pembahagian modulo.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Operasi bitwise juga boleh digunakan; bila anda melakukan operasi bitwise,
// float anda akan ditukarkan kepada int bertanda *sehingga* 32 bit.
1 << 2; // = 4

// Keutamaan ditekankan menggunakan kurungan.
(1 + 3) * 2; // = 8

// Terdapat tiga nilai nombor-tidak-nyata istimewa
Infinity; // hasil operasi seperti 1/0
-Infinity; // hasil operasi seperti -1/0
NaN; // hasil operasi seperti 0/0, bermaksud 'Bukan Sebuah Nombor'

// Terdapat juga jenis boolean
true;
false;

// Talian dicipta dengan ' atau ''.
'abc';
"Hello, world";

// Penafian menggunakan simbol !
!true; // = tidak benar
!false; // = benar

// Sama ialah ===
1 === 1; // = benar
2 === 1; // = tidak benar

// Tidak sama ialah !==
1 !== 1; // = tidak benar
2 !== 1; // = benar

// Lagi perbandingan
1 < 10; // = benar
1 > 10; // = tidak benar
2 <= 2; // = benar
2 >= 2; // = benar

// Talian disambungkan dengan +
"Hello " + "world!"; // = "Hello world!"

// dan dibandingkan dengan < dan >
"a" < "b"; // = benar

// Paksaan jenis dilakukan untuk perbandingan menggunakan dua sama dengan...
"5" == 5; // = benar
null == undefined; // = benar

// ...melainkan anda menggunakan ===
"5" === 5; // = tidak benar
null === undefined; // = tidak benar

// ...yang boleh menghasilkan keputusan yang pelik...
13 + !0; // 14
"13" + !0; // '13true'

// Anda boleh akses huruf dalam perkataan dengan `charAt`
"This is a string".charAt(0);  // = 'T'

// ...atau menggunakan `substring` untuk mendapatkan bahagian yang lebih besar.
"Hello world".substring(0, 5); // = "Hello"

// `length` adalah ciri, maka jangan gunakan ().
"Hello".length; // = 5

// Selain itu, terdapat juga `null` dan `undefined`.
null;      // digunakan untuk menandakan bukan-nilai yang disengajakan
undefined; // digunakan untuk menandakan nilai yang tidak wujud pada waktu ini (walaupun `undefined` adalah nilai juga)

// false, null, undefined, NaN, 0 dan "" adalah tidak benar; semua selain itu adalah benar.
// Peringatan, 0 adalah tidak benar dan "0" adalah benar, walaupun 0 == "0".

///////////////////////////////////
// 2. Pembolehubah, Array dan Objek

// Pembolehubah digunakan dengan kata kunci 'var'. Javascript ialah sebuah
// bahasa aturcara yang jenisnya dinamik, maka anda tidak perlu spesifikasikan
// jenis pembolehubah. Penetapan menggunakan satu '=' karakter.
var someVar = 5;

// jika anda tinggalkan kata kunci var, anda tidak akan dapat ralat...
someOtherVar = 10;

// ...tetapi pembolehubah anda akan dicipta di dalam skop global, bukan di
// dalam skop anda menciptanya.

// Pembolehubah yang dideklarasikan tanpa ditetapkan sebarang nilai akan
// ditetapkan kepada undefined.
var someThirdVar; // = undefined

// jika anda ingin mendeklarasikan beberapa pembolehubah, maka anda boleh
// menggunakan koma sebagai pembahagi
var someFourthVar = 2, someFifthVar = 4;

// Terdapat cara mudah untuk melakukan operasi - operasi matematik pada
// pembolehubah:
someVar += 5; // bersamaan dengan someVar = someVar +5; someVar sama dengan 10 sekarang
someVar *= 10; // sekarang someVar bernilai 100

// dan cara lebih mudah untuk penambahan atau penolakan 1
someVar++; // sekarang someVar ialah 101
someVar--; // kembali kepada 100

// Array adalah senarai nilai yang tersusun, yang boleh terdiri daripada
// pembolehubah pelbagai jenis.
var myArray = ["Hello", 45, true];

// Setiap ahli array boleh diakses menggunakan syntax kurungan-petak.
// Indeks array bermula pada sifar.
myArray[1]; // = 45

// Array boleh diubah dan mempunyai panjang yang tidak tetap dan boleh ubah.
myArray.push("World");
myArray.length; // = 4

// Tambah/Ubah di index yang spesifik
myArray[3] = "Hello";

// Objek javascript adalah sama dengan "dictionaries" atau "maps" dalam bahasa
// aturcara yang lain: koleksi pasangan kunci-nilai yang tidak mempunyai
// sebarang susunan.
var myObj = {key1: "Hello", key2: "World"};

// Kunci adalah string, tetapi 'quote' tidak diperlukan jika ia adalah pengecam
// javascript yang sah. Nilai boleh mempunyai sebarang jenis.
var myObj = {myKey: "myValue", "my other key": 4};

// Ciri - ciri objek boleh juga diakses menggunakan syntax subskrip (kurungan-
// petak),
myObj["my other key"]; // = 4

// ... atau menggunakan syntax titik, selagi kuncinya adalah pengecam yang sah.
myObj.myKey; // = "myValue"

// Objek adalah boleh diubah; nilai boleh diubah dan kunci baru boleh ditambah.
myObj.myThirdKey = true;

// Jika anda cuba untuk akses nilai yang belum ditetapkan, anda akan mendapat
// undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Logik dan Struktur Kawalan

// Syntax untuk bahagian ini adalah hampir sama dengan Java.

// Struktur `if` berfungsi seperti yang anda jangkakan.
var count = 1;
if (count == 3){
    // dinilai jika count ialah 3
} else if (count == 4){
    // dinilai jika count ialah 4
} else {
    // dinilai jika count bukan 3 atau 4
}

// Sama juga dengan `while`.
while (true){
    // Sebuah ulangan yang tidak terhingga!
    // An infinite loop!
}

// Ulangan do-while adalah sama dengan ulangan while, kecuali ia akan diulang
// sekurang-kurangnya sekali.
var input;
do {
    input = getInput();
} while (!isValid(input))

// Ulangan `for` adalah sama dengan C dan Java:
// Persiapan; kondisi untuk bersambung; pengulangan.
for (var i = 0; i < 5; i++){
    // akan berulang selama 5 kali
}

// Pernyataan ulangan For/In akan mengulang setiap ciri seluruh jaringan
// 'prototype'
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
}

// Jika anda cuma mahu mengambil kira ciri - ciri yang ditambah pada objek it
// sendiri dan bukan 'prototype'nya, sila gunakan semakan hasOwnProperty()
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    if (person.hasOwnProperty(x)){
        description += person[x] + " ";
    }
}

// for/in tidak sepatutnya digunakan untuk mengulang sebuah Array di mana
// indeks susunan adalah penting.
// Tiada sebarang jaminan bahawa for/in akan mengembalikan indeks dalam
// mana - mana susunan

// && adalah logikal dan, || adalah logikal atau
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // warna adalah sama ada 'red' atau 'blue'
}

// && dan || adalah "lintar pintas", di mana ia berguna untuk menetapkan
// nilai asal.
var name = otherName || "default";


// Pernyataan `switch` menyemak persamaan menggunakan `===`.
// gunakan pernyataan `break` selepas setiap kes
// atau tidak, kes - kes selepas kes yang betul akan dijalankan juga.
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
// 4. Functions, Skop dan Closures

// Function javascript dideklarasikan dengan kata kunci `function`.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Perhatikan yang nilai yang dikembalikan mesti bermula pada baris yang sama
// dengan kata kunci `return`, jika tidak, anda akan sentiasa mengembalikan
// `undefined` disebabkan kemasukan 'semicolon' secara automatik. Sila berjaga -
// jaga dengan hal ini apabila menggunakan Allman style.
function myFunction(){
    return // <- semicolon dimasukkan secara automatik di sini
    {thisIsAn: 'object literal'}
}
myFunction(); // = undefined

// Function javascript adalah objek kelas pertama, maka ia boleh diberikan
// nama pembolehubah yang lain dan diberikan kepada function yang lain sebagai
// input - sebagai contoh, apabila membekalkan pengendali event:
function myFunction(){
    // kod ini akan dijalankan selepas 5 saat
}
setTimeout(myFunction, 5000);
// Nota: setTimeout bukan sebahagian daripada bahasa JS, tetapi ia disediakan
// oleh pelayar web dan Node.js.

// Satu lagi function yang disediakan oleh pelayar web adalah setInterval
function myFunction(){
    // kod ini akan dijalankan setiap 5 saat
}
setInterval(myFunction, 5000);

// Objek function tidak perlu dideklarasikan dengan nama - anda boleh menulis
// function yang tidak bernama didalam input sebuah function lain.
setTimeout(function(){
    // kod ini akan dijalankan dalam 5 saat
}, 5000);

// Javascript mempunyai skop function; function mempunyai skop mereka
// tersendiri tetapi blok tidak.
if (true){
    var i = 5;
}
i; // = 5 - bukan undefined seperti yang anda jangkakan di dalam bahasa blok-skop

// Ini telah menyebabkan corak biasa iaitu "immediately-executing anonymous
// functions", yang mengelakkan pembolehubah sementara daripada bocor ke
// skop global.
(function(){
    var temporary = 5;
    // Kita boleh akses skop global dengan menetapkan nilai ke "objek global",
    // iaitu dalam pelayar web selalunya adalah `window`. Objek global mungkin
    // mempunyai nama yang berlainan dalam alam bukan pelayar web seperti Node.js.
    window.permanent = 10;
})();
temporary; // akan menghasilkan ralat ReferenceError
permanent; // = 10

// Salah satu ciri terhebat Javascript ialah closure. Jika sebuah function
// didefinisikan di dalam sebuah function lain, function yang di dalam akan
// mempunyai akses kepada semua pembolehubah function yang di luar, mahupun
// selepas function yang di luar tersebut selesai.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Function dalam diletakkan di dalam skop lokal secara asal, seperti
    // ia dideklarasikan dengan `var`.
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout adalah tak segerak atau asinkroni, maka function sayHelloInFiveSeconds akan selesai serta merta, dan setTimeout akan memanggil
    // inner selepas itu. Walaubagaimanapun, disebabkan inner terletak didalam
    // sayHelloInFiveSeconds, inner tetap mempunyai akses kepada pembolehubah
    // `prompt` apabila ia dipanggil.
}
sayHelloInFiveSeconds("Adam"); // akan membuka sebuah popup dengan "Hello, Adam!" selepas 5s

///////////////////////////////////
// 5. Lagi tentang Objek, Constructor dan Prototype

// Objek boleh mengandungi function.
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// Apabila function sesebuah object dipanggil, ia boleh mengakses objek asalnya
// dengan menggunakan kata kunci `this`.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// Nilai sebenar yang ditetapkan kepada this akan ditentukan oleh bagaimana
// sesebuah function itu dipanggil, bukan dimana ia didefinisikan. Oleh it,
// sesebuah function tidak akan berfungsi jika ia dipanggil bukan pada konteks
// objeknya.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Sebaliknya, sebuah function boleh ditetapkan kepada objek dan mendapat akses
// kepada objek itu melalui `this`, walaupun ia tidak ditetapkan semasa ia
// didefinisikan.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// Kita juga boleh menentukan konteks untuk sebuah function dijalankan apabila
// ia dipanggil menggunakan `call` atau `apply`.

var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// Function `apply` adalah hampir sama, tetapi ia mengambil sebuah array
// sebagai senarai input.

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Ini sangat berguna apabila menggunakan sebuah function yang menerima senarai
// input dan anda mahu menggunakan sebuah array sebagai input.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Tetapi, `call` dan `apply` adalah hanya sementara, sebagaimana hidup ini.
// Apabila kita mahu ia kekal, kita boleh menggunakan `bind`.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` boleh juga digunakan untuk menggunakan sebuah function tidak
// sepenuhnya (curry).

var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Apabila anda memanggil sebuah function dengan kata kunci `new`, sebuah
// objek baru akan dicipta dan dijadikan tersedia kepada function itu melalui
// kata kunci `this`. Function yang direka bentuk untuk dipanggil sebegitu rupa
// dikenali sebagai constructors.

var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Setiap objek JavaScript mempunyai `prototype`. Apabila anda akses sesuatu
// ciri sebuah objek yang tidak wujud dalam objek sebenar itu, interpreter akan
// mencari ciri itu didalam `prototype`nya.

// Sebahagian implementasi JS membenarkan anda untuk akses prototype sebuah
// objek pada ciri istimewa `__proto__`. Walaupun ini membantu dalam menerangkan
// mengenai prototypes, ia bukan sebahagian dari piawai; kita akan melihat
// cara - cara piawai untuk menggunakan prototypes nanti.
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

// Ini berfungsi untuk function juga.
myObj.myFunc(); // = "hello world!"

// Sudah pasti, jika ciri anda bukan pada prototype anda, prototype kepada
// prototype anda akan disemak, dan seterusnya.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Tiada penyalinan terlibat disini; setiap objek menyimpan rujukan kepada
// prototypenya sendiri. Ini bermaksud, kita boleh mengubah prototypenya dan
// pengubahsuaian itu akan dilihat dan berkesan dimana sahaja.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// Kami menyatakan yang `__proto__` adalah bukan piawai, dan tiada cara rasmi
// untuk mengubah prototype sesebuah objek. Walaubagaimanapun, terdapat dua
// cara untuk mencipta objek baru dengan sesebuah prototype.

// Yang pertama ialah Object.create, yang merupakan tambahan terbaru pada JS,
// dan oleh itu tiada dalam semua implementasi buat masa ini.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// Cara kedua, yang boleh digunakan dimana sahaja, adalah berkaitan dengan
// constructor. Constructors mempunyai sebuah ciri yang dipanggil prototype.
// Ini *bukan* prototype constructor terbabit; tetapi, ia adalah prototype yang
// diberikan kepada objek baru apabila ia dicipta menggunakan constructor dan
// kata kunci new.
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

// Jenis yang terbina sedia seperti string dan nombor juga mempunyai constructor
// yang mencipta objek pembalut yang serupa.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Kecuali, mereka sebenarnya tak sama sepenuhnya.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Kod ini tidak akan dilaksanakan, kerana 0 adalah tidak benar.
}

// Walaubagaimanapun, pembalut objek dan jenis terbina yang biasa berkongsi
// prototype, maka sebagai contoh, anda sebenarnya boleh menambah fungsi
// kepada string.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"

// Fakta ini selalu digunakan dalam "polyfilling", iaitu melaksanakan fungsi
// baru JavaScript didalam subset JavaScript yang lama, supaya ia boleh
// digunakan di dalam persekitaran yang lama seperti pelayar web yang lama.

// Sebagai contoh, kami menyatakan yang Object.create belum lagi tersedia
// di semua implementasi, tetapi kita masih boleh menggunakannya dengan polyfill:
if (Object.create === undefined){ // jangan ganti jika ia sudah wujud
    Object.create = function(proto){
        // buat satu constructor sementara dengan prototype yang betul
        var Constructor = function(){};
        Constructor.prototype = proto;
        // kemudian gunakannya untuk mencipta objek baru yang diberikan
        // prototype yang betul
        return new Constructor();
    }
}
```
## Bacaan Lanjut

[Mozilla Developer Network][1] menyediakan dokumentasi yang sangat baik untuk
JavaScript kerana ia digunakan di dalam pelayar - pelayar web. Tambahan pula,
ia adalah sebuah wiki, maka, sambil anda belajar lebih banyak lagi, anda boleh
membantu orang lain dengan berkongsi pengetahuan anda.

[A re-introduction to JavaScript][2] oleh MDN meliputi semua konsep yang
diterangkan di sini dengan lebih terperinci. Panduan ini menerangkan bahasa
aturcara JavaScript dengan agak mudah; jika anda mahu belajar lebih lanjut
tentang menggunakan JavaScript didalam laman web, mulakan dengan mempelajari
tentang [Document Object Model][3].

[Learn Javascript by Example and with Challenges][4] adalah variasi panduan ini
dengan cabaran yang tersedia pakai.

[JavaScript Garden][5] pula adalah panduan yang lebih terperinci mengenai
semua bahagian bahasa aturcara ini yang bertentangan dengan naluri atau
kebiasaan.

[JavaScript: The Definitive Guide][6] adalah panduan klasik dan buku rujukan.

Selain daripada penyumbang terus kepada artikel ini, sebahagian kandungannya
adalah adaptasi daripada tutorial Python Louie Dinh di dalam laman web ini,
dan [JS Tutorial][7] di Mozilla Developer Network.


[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: http://www.learneroo.com/modules/64/nodes/350
[5]: http://bonsaiden.github.io/JavaScript-Garden/
[6]: http://www.amazon.com/gp/product/0596805527/
[7]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
