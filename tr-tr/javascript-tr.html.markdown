---
language: javascript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
filename: javascript-tr.js
translators:
    - ["Emre Kayık", "http://github.com/emrekayik"]
lang: tr-tr
---

JavaScript, 1995 yılında Netscape'ten Brendan Eich tarafından oluşturuldu. Başlangıçta web sitelerinde kullanılmak üzere tasarlanmış basit bir betik dili olarak düşünülmüştü, fakat daha karmaşık web uygulamaları için Java kullanımını tamamlamak için tasarlanmıştı. Ancak, web sayfalarıyla sıkı bir entegrasyon ve tarayıcıların içinde desteklemesi nedeniyle, Java'dan çok daha yaygın olarak kullanılmaya başlandı.

JavaScript sadece web tarayıcılarıyla sınırlı değildir: Node.js, Google Chrome'un V8 JavaScript motoru için bağımsız bir çalışma zamanı sağlayan bir proje olarak giderek daha popüler hale geliyor.

JavaScript'in C benzeri bir sözdizimi var, bu yüzden C veya Java gibi dilleri kullandıysanız, temel sözdizimin çoğu zaten tanıdık olacak. Buna rağmen ve isim benzerliğine rağmen, JavaScript'in nesne modeli Java'nınkinden önemli ölçüde farklıdır.

```javascript
// Tek satırlık yorumlar iki eğik çizgi ile başlar.
/* Çok satırlı yorumlar başlangıçta eğik çizgi-yıldız işareti ile başlayıp, sonunda yıldız-eğik çizgi işareti ile biter. */

// İfadeler ; (noktalı virgül) ile sonlandırılabilir.
doStuff();

// ... ancak noktalı virgüller otomatik olarak eklendiğinden zorunlu değildir.
// belirli durumlar hariç, herhangi bir satırbaşı olduğu yerde eklenir.
doStuff()

//  Bu durumlar beklenmedik sonuçlara neden olabileceğinden, bu kılavuzda noktalı virgül kullanmaya devam edeceğiz.

///////////////////////////////////
// 1. Sayılar, Dizeler ve Operatörler

// JavaScript'te sadece bir sayı tipi vardır ve bu 64-bit IEEE 754 double olarak adlandırılır.
// Double sayılar, yaklaşık olarak 9✕10¹⁵ kadar kesin bir şekilde tamsayıları saklamak için yeterli olan 52-bitlik bir mantis yapısına sahiptir.
3; // = 3
1.5; // = 1.5

// Bazı temel aritmetik işlemler beklediğiniz gibi çalışır.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Bölme işleminin sonucu tam sayı olmasa da çalışır.
5 / 2; // = 2.5

// Modüler bölme işlemi.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Bitsel işlemler de çalışır; bitsel bir işlem gerçekleştirdiğinizde, 
// float sayınız 32 bitlik bir tamsayıya dönüştürülür.
1 << 2; // = 4

// Öncelik parantezlerle belirtilir.
(1 + 3) * 2; // = 8

// Gerçek sayı olmayan üç özel değer vardır:
Infinity; // örneğin sonuç 1/0
-Infinity; // örneğin sonuç -1/0
NaN; // örneğin sonuç 0/0, 'Not a Number' anlamına gelir(Bir Sayı Değil)

// Ayrıca bir boolean türü de vardır.
true;
false;

// Dizeler tek(') veya çift(") tırnaklarla tanımlanabilir.
'abc';
"Merhaba, dünya!";

// Değili için ünlem(!) sembolü kullanılır.
!true; // = false
!false; // = true

// Eşitlik ===
1 === 1; // = true
2 === 1; // = false

// Eşitsizlik !==
1 !== 1; // = yanlış
2 !== 1; // = doğru

// Daha fazla karşılaştırma
// Sırasıyla: büyüklük, küçüklük, büyüklük veya eşitlik, küçüklük veya eşitlik.
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Dizeler + ile birleştirilir
"Merhaba " + "dünya!"; // = "Merhaba dünya!"

// ... dizelerden daha fazlası ile çalışır
"1, 2, " + 3; // = "1, 2, 3"
"Merhaba " + ["dünya", "!"]; // = "Merhaba dünya,!"

// küçüktür(<) ve büyüktür(>) ile karşılaştırılır
"a" < "b"; // = true

//  Çift eşittir(==) ile karşılaştırmalar için tür dönüşümü yapılır.
"5" == 5; // = true
null == undefined; // = true

// ... üç eşittir(===) kullanmadığınız sürece
"5" === 5; // = false
null === undefined; // = false

// ...bu da bazı garip davranışlara neden olabilir...
13 + !0; // 14
"13" + !0; // '13true'

// Bir dize içindeki karakterlere `charAt` ile erişebilirsiniz
// 0 ilk karakteri ifade eder.
"Bu bir metindir".charAt(0); // = 'B'

// ... veya daha büyük parçalar elde etmek için `substring` kullanın.
// 0. karakterden başlayarak 5 karakter alır.
"Merhaba dünya".substring(0, 5); // = "Merhaba"

// `length` bir özelliktir, bu nedenle () kullanmayın.
// metnin uzunluğunu verir.
"Merhaba".length; // = 5

// Ayrıca `null` ve `undefined` da vardır.
null; // kasıtlı olarak değer olmadığını belirtmek için kullanılır
undefined; // bir değerin şu anda mevcut olmadığını belirtmek için kullanılır (ancak
           // `undefined` aslında bir değerin kendisidir)

// false, null, undefined, NaN, 0 ve "" yanlış; diğer her şey doğrudur.
// 0 == "0" olmasına rağmen 0'ın yanlış ve "0"'ın doğru olduğuna dikkat edin.

///////////////////////////////////
// 2. Değişkenler, Diziler ve Nesneler

// // Değişkenler `var` anahtar sözcüğü ile bildirilir.  
// JavaScript, dinamik tiplemede olduğu için tür belirtmeniz gerekmez. 
// Atamalarda tek bir `=` kullanılır.
var someVar = 5;

// Eğer var anahtar kelimesini kullanmazsanız, hata almazsınız...
someOtherVar = 10;

// ...ancak değişkeniniz, tanımlandığı kapsamda değil, global kapsamda oluşturulacaktır.

// Atanmadan bildirilen değişkenler tanımsız(undefined) olarak tanımlanır.
var someThirdVar; // = undefined

// Birkaç değişken bildirmek istiyorsanız, virgül(,) kullanabilirsiniz.
var someFourthVar = 2, someFifthVar = 4;

// Değişkenler üzerinde matematik işlemleri yapmak için kısaltma vardır:
someVar += 5; // eşdeğeri someVar = someVar + 5; someVar şimdi 10
someVar *= 10; // şimdi someVar 100 oldu.

// ... 1 eklemek veya çıkarmak için daha da kısa bir yol olarak ++ ve -- kullanılabilir.
someVar++; // şimdi someVar 101 oldu
someVar--; // 100'e geri dön

// Diziler, herhangi bir türdeki değerlerin listeleridir.
var myArray = ["Merhaba", 45, true];

// İçindekilere köşeli parantez([]) kullanılarak erişilebilir.
// Dizi indisleri sıfırdan başlar.
myArray[1]; // = 45


// Diziler değiştirilebilir, değişken eklenebilir 
// ayrıca diziler uzunluğa sahiptir.
myArray.push("Dünya");
myArray.length; // = 4

// Belirli bir indeksteki değeri ekle/değiştir
myArray[3] = "Merhaba";

// Dizinin başından veya sonundan eleman ekleme ve çıkarma
myArray.unshift(3); // İlk eleman olarak ekleme yap
someVar = myArray.shift(); // İlk elemanı çıkar ve değerini döndür
myArray.push(3); // Son eleman olarak ekleme yap
someVar = myArray.pop(); // Son elemanı çıkar ve değerini döndür

// Bir dizinin tüm elemanlarını noktalı virgül ile birleştirme
var myArray0 = [32, false, "js", 12, 56, 90];
myArray0.join(";"); // = "32;false;js;12;56;90"

// Dizi 1'den (dahil) 4'e (hariç) kadar olan öğelerin alt dizisini al
myArray0.slice(1,4); // = [false, "js", 12]

// Dizide 2'den başlayarak 4 öğeyi kaldırın ve dizeleri buraya yerleştirin
// "hi", "wr" ve "ld"; kaldırılan alt diziyi döndür
myArray0.splice(2,4, "hi", "wr", "ld"); // = ["js",12,56,90]
// myArray0 === [32,false, "hi", "wr", "ld"]

// JavaScript'in nesneleri diğer dillerdeki "dictionaries" veya "maps" ile eşdeğerdir diller:
// anahtar-değer çiftlerinden oluşan sırasız bir koleksiyon.
var myObj = {anahtar1: "Merhaba", anahtar2: "Dünya"};

// Anahtarlar dizelerdir, ancak geçerli bir anahtarlarsa tırnak işaretlerine gerek yoktur.
// Değerler herhangi bir türde olabilir.
var myObj = {anahtar: "myValue", "baska anahtar": 4};

// Nesne niteliklerine alt simge sözdizimi kullanılarak da erişilebilir,
myObj["baska anahtar"]; // = 4

// ... veya anahtarın geçerli bir tanımlayıcı olması koşuluyla nokta sözdizimini kullanarak.
myObj.anahtar; // = "myValue"

// Nesneler değiştirilebilir; değerler değiştirilebilir ve yeni anahtarlar eklenebilir.
myObj.ucuncuAnahtar = true;

// Henüz ayarlanmamış bir değere erişmeye çalışırsanız, undefined alırsınız.
myObj.dorduncuAnahtar; // = undefined

///////////////////////////////////
// 3. Mantık ve Kontrol Yapıları

// `if` yapısı bildiğiniz gibi çalışır.
var count = 1;
if (count == 3){
    // sayı 3 ise çalışır
} else if (count == 4){
    // sayı 4 ise çalışır
} else {
    // 3 veya 4 değilse çalışır
}

// While da aynı şekilde
while (true){
    // sonsuza kadar çalışır
}

// Do-while döngüleri while döngüleri gibidir, ancak her zaman en az bir kez çalışırlar.
var input;
do {
    input = getInput();
} while (!isValid(input));

// `for` döngüsü C ve Java ile aynıdır:
// değişkeni belirleme; devam koşulu; yineleme.
for (var i = 0; i < 5; i++){
    // 5 kez çalışacak
}
// Etiketli döngülerden çıkmak Java'ya benzer
outer:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break outer;
            // sadece iç döngü yerine dış döngüden çıkar
        }
    }
}

// for/in deyimi bir nesnenin özellikleri üzerinde yineleme yapılmasını sağlar.
var description = "";
var person = {fname: "Paul", lname: "Ken", age:18};
for (var x in person){
    description += kişi[x] + " ";
} // description = 'Paul Ken 18 '

// for/of ifadesi, yapılandırılmış iterable nesnelerin (String, Array, örneğin Array benzeri arguments 
// veya NodeList nesneleri, TypedArray, Map ve Set, ve kullanıcı tanımlı iterables) 
// üzerinde yinelemeyi sağlar.
var myPets = "";
var pets = ["cat", "dog", "hamster", "hedgehog"];
for (var pet of pets){
    myPets += pet + " ";
} // myPets = 'cat dog hamster hedgehog '

// ve (&&) mantıksal operatörü, veya (||) mantıksal operatörü
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // renk ya kırmızı ya da mavidir
}

// ve(&&) ve veya(||) kısa yolları, varsayılan değerleri ayarlamak için kullanışlıdır.
var name = otherName || "default";

// switch ifadesi === operatörü ile eşitliği kontrol eder. 
// Doğru durumdan sonra her bir durum için 'break' kullanmalısınız, 
// aksi takdirde doğru durumdan sonraki tüm durumlar da çalıştırılacaktır.
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
// 4. Fonksiyonlar, Kapsam ve Kapanışlar

// JavaScript fonksiyonları `function` anahtar sözcüğü ile bildirilir.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// // Döndürülecek değerin `return` anahtar sözcüğü ile aynı satırda başlaması gerektiğine dikkat edin, 
// aksi takdirde her zaman `undefined` döndürürsünüz. otomatik noktalı virgül eklenir. 
//Allman stilini kullanırken buna dikkat edin.
function myFunction(){
    return // <- noktalı virgül otomatik olarak buraya eklenir
    {thisIsAn: 'object literal'};
}
myFunction(); // = undefined

// JavaScript fonksiyonları birinci sınıf nesnelerdir, bu nedenle farklı değişken adlarına yeniden atanabilir 
// ve diğer fonksiyonlara argüman olarak aktarılabilirler 
// örneğin, bir olay işleyicisi sağlarken:
function myFunction(){
    // bu kod 5 saniye içinde çağrılacaktır
}
setTimeout(myFunction, 5000);
// Not: setTimeout JS dilinin bir parçası değildir, ancak tarayıcılar ve Node.js tarafından sağlanır.

// Tarayıcılar tarafından sağlanan bir başka işlev de setInterval'dir
function myFunction(){
    // bu kod her 5 saniyede bir çağrılacaktır
}
setInterval(myFunction, 5000);

// Fonksiyon nesnelerinin bir isimle bildirilmesine bile gerek yoktur - şöyle yazabilirsiniz
// anonim bir fonksiyon tanımını doğrudan başka bir fonksiyonun argümanlarına dönüştürür.
setTimeout(function(){
    // bu kod 5 saniye içinde çağrılacaktır
}, 5000);

// JavaScript fonksiyon kapsamına sahiptir; fonksiyonlar kendi kapsamlarını alır ancak diğer bloklar kapsam oluşturmaz. 
// Eğer şöyle bir kod yazarsanız:
if (true){
    var i = 5;
}
i; // = blok kapsamındaki bir dilde beklediğiniz gibi 'undefined' değil 5 değerini alırsınız.

// Bu durum, geçici değişkenlerin global değişkenlere sızmasını önleyen "hemen çalıştırılan anonim fonksiyonlar(immediately-executing anonymous functions)" modelinin yaygınlaşmasına yol açmıştır.
(function(){
    var temporary = 5;
    // Global kapsama, bir web tarayıcısında her zaman `window` olan "global nesneye" atama yaparak erişebiliriz. 
    // Global nesne, Node.js gibi tarayıcı dışı ortamlarda farklı bir ada sahip olabilir.
    window.permanent = 10;
})();
temporary; // ReferenceError yükseltir
permanent; // = 10

// JavaScript'in en güçlü özelliklerinden biri, kapanışlardır (closures). 
// Bir fonksiyon başka bir fonksiyonun içinde tanımlanmışsa, içteki fonksiyon dıştaki fonksiyonun tüm değişkenlerine erişebilir 
// ve dıştaki fonksiyon sonlandıktan sonra bile erişimi devam eder.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Varsayılan olarak, sanki `var` ile tanımlanmış gibi iç fonksiyonlar yerel kapsama yerleştirilir.
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout işlevi asenkron olduğundan, sayHelloInFiveSeconds işlevi hemen çıkar ve setTimeout daha sonra inner'ı çağırır.
    // Ancak, inner sayHelloInFiveSeconds üzerine kapanış yaptığı için, sonunda çağrıldığında bile hala `prompt` değişkenine erişimi vardır.
}
sayHelloInFiveSeconds("Adam"); // "Hello, Adam!" ifadesini 5 saniye sonra açan bir pencere açacak.

///////////////////////////////////
// 5. Nesneler Hakkında Daha Fazla Bilgi; Yapılandırıcılar ve Prototipler

// Nesneler fonksiyonlar içerebilir.
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// Bir nesneye bağlı fonksiyonlar çağrıldığında, 'this' anahtar kelimesini kullanarak bağlı oldukları nesnelere erişebilirler.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// 'this' anahtar kelimesinin neye ayarlandığı, fonksiyonun nerede tanımlandığına değil, nasıl çağrıldığına bağlıdır. 
// Bu nedenle, fonksiyonumuz nesne bağlamında çağrılmazsa çalışmaz.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Tersine bir fonksiyon nesneye atandığında ve tanımlandığı sırada bağlı değilse bile, this anahtar kelimesi aracılığıyla erişebilir.
var myOtherFunc = function(){
return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// 'call' veya 'apply' kullanarak bir fonksiyonu çağırdığımızda, onu yürütmek için bir bağlam belirtebiliriz.

var anotherFunc = function(s){
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// JavaScript'teki 'apply' fonksiyonu, 'call' fonksiyonuna neredeyse benzer, ancak argüman listesi için bir dizi alır.
anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Bu, bir dizi argüman kabul eden bir işlevle çalışırken ve bir dizi geçirmek istediğinizde kullanışlıdır.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (hata!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Ancak call ve apply işlevleri yalnızca geçici olarak bir işlevin bağlamını değiştirmek için kullanılır. 
// Kalıcı olarak bir işlevin bağlamını değiştirmek istediğimizde, bind işlevini kullanabiliriz.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// 'bind', bir işlevin bağlamını kalıcı olarak değiştirirken aynı zamanda kısmi uygulama (currying) yapmak için de kullanılabilir.

var product = function(a, b){ return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Bir işlevi 'new' anahtar kelimesi ile çağırdığınızda, yeni bir nesne oluşturulur ve bu nesne, 
// işlevin 'this' anahtar kelimesi aracılığıyla kullanılabilir hale getirilir. 
//Bu şekilde çağrılmak için tasarlanmış işlevlere yapıcılar (constructors) denir.
var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Diğer popüler nesne yönelimli dillerin çoğunun aksine, JavaScript'te 'sınıf' planlarından oluşturulan 'örnekler' kavramı yoktur; 
// bunun yerine JavaScript örnekleme ve kalıtımı tek bir kavramda birleştirir: 'prototip'.

// Her JavaScript nesnesinin bir prototipi vardır. 
// Gerçek nesnede bulunmayan bir özelliğe erişmeye çalıştığınızda yorumlayıcı, bu özelliği prototipinde arar.

// Bazı JavaScript uygulamaları, nesnenin prototipine sihirli __proto__ özelliği üzerinden erişmenize izin verir. 
// Bu, prototipleri açıklamak için faydalıdır, ancak standartın bir parçası değildir; 
// daha sonra prototipleri kullanmanın standart yollarına geleceğiz.
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// Bu fonksiyonlar için de geçerlidir.
myObj.myFunc(); // = "hello world!"

// Tabii ki, eğer özelliğiniz prototipinizde yoksa, prototipin prototipi aranır ve bu böyle devam eder.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Burada kopyalama işlemi yoktur; her nesne, prototipine bir referans saklar. 
// Bu, prototipi değiştirebileceğimiz ve değişikliklerimizin her yerde yansıtılacağı anlamına gelir.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// for/in ifadesi, bir nesnenin özelliklerinin (properties) üzerinde yineleme yapmamızı sağlar 
// ve boş bir prototip görene kadar prototip zincirini takip eder.
for (var x in myObj){
    console.log(myObj[x]);
}
///prints:
// Hello world!
// 43
// [Function: myFunc]
// true

// Sadece nesneye kendisine bağlı özellikleri ve prototiplerini değil, 
// yalnızca nesnenin kendi özelliklerini ele almak için hasOwnProperty() kontrolünü kullanabilirsiniz.
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
///prints:
// Hello world!

// __proto__'nun standart olmadığını ve mevcut bir nesnenin prototipini değiştirmenin standart bir yolu olmadığını belirttik. 
// Ancak, belirli bir prototipe sahip yeni bir nesne oluşturmanın iki yolu vardır.

// İlk yöntem, JS'ye yakın zamanda eklenen ve henüz tüm uygulamalarda mevcut olmayan Object.create() işlevini kullanmaktır.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// İkinci yöntem, herhangi bir yerde çalışabilen yapıcılar (constructors) ile ilgilidir. 
// Yapıcılarda prototype adlı bir özellik vardır. Bu, yapıcı işlevinin prototipi değildir; 
// bunun yerine, yeni nesnelerin oluşturulduğunda bu yapıcı ve 'new' anahtar kelimesi kullanılarak verilen prototiptir.
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

// Dize ve sayı gibi yerleşik türler de, eşdeğer sarma nesnelerini oluşturan yapıcılara sahiptir.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Ancak, bunlar tam olarak eşdeğer değillerdir.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Bu kod çalışmayacak, çünkü 0 yanlıştır (falsy).
}
if (new Number(0)){
   // Sarmal sayılar nesneler olduğu için ve nesneler her zaman doğru (truthy) olduğu için, bu kod çalışacaktır.
}

// Ancak, sarma nesneleri ve normal yerleşik türler bir prototip paylaşırlar, bu nedenle, örneğin bir dizeye işlevsellik ekleyebilirsiniz.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// Bu gerçek sıklıkla "polyfilling" olarak adlandırılan bir teknikte kullanılır. Bu teknik, JavaScript'in daha yeni özelliklerini, 
// örneğin eski tarayıcılarda kullanılabilecek şekilde, daha eski bir alt kümesinde uygulamak için kullanılır.

// Örneğin, tüm uygulamalarda henüz mevcut olmayan Object.create'i, aşağıdaki polyfill ile kullanabiliriz:
if (Object.create === undefined){ // varsa üzerine yazma
    Object.create = function(proto){
        // İstenen prototipte geçici bir yapıcı oluşturun
        var Constructor = function(){};
        Constructor.prototype = proto;
        // Daha sonra, uygun şekilde prototipleştirilmiş yeni bir nesne oluşturmak için kullanın
        return new Constructor();
    };
}

// ES6 Eklemeleri

// "let" anahtar kelimesi, var anahtar kelimesinin aksine bir sözlüksel(leksikal) kapsamda değişken tanımlamanızı sağlar.
let name = "Emre";

// let ile tanımlanan değişkenlere yeni değerler atanabilir.
name = "Ahmet";

// 'const' anahtar kelimesi, let ike benzer şekilde leksikal bir kapsamda değişken tanımlamanızı sağlar. 
// Ancak, bir kez değer atandıktan sonra, bu değer tekrar atanamaz!!

const pi = 3.14;

pi = 4.13; // Bu yapılmaz, hata verir.

// ES6'da, "lambda syntax" olarak bilinen yeni bir işlev sözdizimi vardır. 
// Bu, değişkenlerle const ve let ile tanımlanan gibi leksikal kapsamda fonksiyon tanımlanmasını sağlar.

const isEven = (number) => {
    return number % 2 === 0;
};

isEven(7); // false

// Geleneksel sözdizimine sahip bu işlevin "eşdeğeri" bildiğiniz gibi şu şekilde gösterilir:
function isEven(number) {
    return number % 2 === 0;
};

// "eşdeğer" kelimesini çift tırnak içine aldım çünkü lambda sözdizimi kullanarak tanımlanan bir fonksiyon, tanımlamadan önce çağrılamaz.
// Aşağıdaki, geçersiz kullanım örneğidir:
add(1, 8);

const add = (firstNumber, secondNumber) => {
    return firstNumber + secondNumber;
};
```

## Daha fazlası için
[Mozilla Developer Network (MDN)][1], tarayıcılarda kullanılan JavaScript için mükemmel bir belgelendirme sağlar. 
Ayrıca, bir wiki olduğundan, daha fazla öğrendikçe kendi bilginizi paylaşarak diğerlerine yardımcı olabilirsiniz.

MDN'nin [JavaScript'e Yeniden Giriş][2] rehberi, programlamanın temellerine hakim olan 
ve JavaScript dilinin kendisi hakkında daha fazla bilgi edinmek isteyenler için harika bir kaynaktır. 
Bu rehber, burada ele aldığımız konuları daha detaylı bir şekilde ele almaktadır.

[Learn Javascript by Example and with Challenges][4] bunun bir çeşididir
yerleşik zorluklarla birlikte referans.

[JavaScript Garden][5] dilin tüm sezgisel olmayan kısımların derinlemesine bir rehberidir.

[JavaScript: The Definitive Guide][6] klasik bir kılavuz ve referans kitabıdır.

Marijn Haverbeke tarafından yazılan [Eloquent Javascript][8] mükemmel bir JS kitabıdır.

[Javascript: The Right Way][10] yeni geliştiricilere Javascript'i tanıtmayı amaçlayan bir rehberdir
ve deneyimli geliştiricilerin JavaScript'in en iyi uygulamaları hakkında daha fazla bilgi edinmesine yardımcı olur.

[Javascript:Info][11] temel bilgileri (ana dil ve tarayıcı ile çalışma) kapsayan modern bir javascript eğitimidir
bunun yanı sıra özlü açıklamalarla ileri düzey konulara sahiptir.


Bu makaleye doğrudan katkıda bulunanlara ek olarak, bazı içerikler şu kaynaklardan uyarlanmıştır
Louie Dinh'in bu sitedeki Python öğreticisi ve [JS Öğreticisi][7]Mozilla Geliştirici Ağı.

### Türkçe Kaynaklar

[tr.javaScript.info][12] JavaScript'in temellerini öğrenmek için harika bir kaynaktır.

[Javascript Dersleri][13] Javascript'i baştan sona öğrenmek için harika bir kaynaktır.



[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: http://www.learneroo.com/modules/64/nodes/350
[5]: http://bonsaiden.github.io/JavaScript-Garden/
[6]: http://www.amazon.com/gp/product/0596805527/
[7]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[8]: http://eloquentjavascript.net/
[10]: http://jstherightway.org/
[11]: https://javascript.info/

[12]: https://tr.javascript.info/
[13]: https://www.yusufsezer.com.tr/javascript-dersleri/




