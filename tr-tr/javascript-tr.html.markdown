---
language: javascript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["Kaan Kaçar", "https://github.com/kaankacar"]
filename: learnjavascript-tr.js
lang: tr-tr
---

JavaScript, Brendan Eich tarafından 1995 yılında oluşturuldu. Başlangıçta, daha karmaşık web uygulamaları için Java'nın kullanıldığı siteler için daha basit bir betik dili olarak tasarlandı, ancak Web sayfalarıyla sıkı entegrasyonu ve tarayıcılardaki yerleşik desteği nedeniyle Java'dan daha yaygın hale geldi.

Ancak, JavaScript yalnızca web tarayıcılarıyla sınırlı değildir: Google Chrome'un V8 JavaScript motoru için bağımsız bir çalışma ortamı sağlayan Node.js giderek daha popüler hale gelmektedir.

JavaScript, C diline benzer bir sözdizimine sahiptir, bu nedenle C veya Java gibi dilleri kullandıysanız, temel sözdizimin büyük bir kısmını zaten biliyorsunuz. Bununla birlikte, isim benzerliğine rağmen, JavaScript'in nesne modeli Java'nınkinden önemli ölçüde farklıdır.

```js
// Tek satırlık yorumlar iki eğik çizgi ile başlar.
/* Çok satırlı yorumlar eğik çizgi-yıldız ile başlar,
   ve yıldız-eğik çizgi ile biter */

// İfadeler ; ile sonlandırılabilir
birSeyYap();

// ... ancak sonlandırma yapmak zorunda değilsiniz, çünkü noktalı virgüller
// otomatik olarak yeni satırlarda yer alır, ancak bazı durumlar hariç.
birSeyYap()

// Bu durumlar beklenmeyen sonuçlara yol açabileceğinden, burada
// noktalı virgüller kullanmaya devam edeceğiz.

///////////////////////////////////
// 1. Sayılar, Dizeler ve Operatörler

// JavaScript'in bir sayı türü vardır (64 bitlik IEEE 754 double).
// Double'lar 52 bitlik bir mantisaya sahiptir, bu da tam sayıları
// yaklaşık olarak 9✕10¹⁵'e kadar kesin olarak depolamak için yeterlidir.
3; // = 3
1.5; // = 1.5

// Temel aritmetik işlemler beklediğiniz gibi çalışır.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Hatta düzensiz bölme işlemi de yapılabilir.
5 / 2; // = 2.5

// Ve modülo bölmesi de yapılabilir.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Bit işlemleri de çalışır; bir bit işlemi yaptığınızda float'ınız
// *en fazla* 32 bitlik bir işaretleme tamsayıya dönüştürülür.
1 << 2; // = 4

// Öncelik parantez ile sağlanır.
(1 + 3) * 2; // = 8

// Üç adet gerçek olmayan sayı değeri vardır:
Infinity; // örneğin 1/0'ın sonucu
-Infinity; // örneğin -1/0'ın sonucu
NaN; // örneğin 0/0'ın sonucu, 'Sayı Değil' anlamına gelir

// Ayrıca boolean türü vardır.
true;
false;

// Stringler ' veya " ile oluşturulur.
'abc';
"Merhaba, dünya";

// Olumsuzluk işareti ! sembolü ile yapılır.
!true; // = false
!false; // = true

// Eşitlik === ile kontrol edilir.
1 === 1; // = true
2 === 1; // = false

// Eşitsizlik !== ile kontrol edilir.
1 !== 1; // = false
2 !== 1; // = true

// Diğer karşılaştırmalar
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Stringler + ile birleştirilir.
"Merhaba " + "dünya!"; // = "Merhaba dünya!"

// ... ve sadece stringlerle sınırlı olmayan bir şekilde çalışır
"1, 2, " + 3; // = "1, 2, 3"
"Merhaba " + ["dünya", "!"]; // = "Merhaba dünya,!"

// ve < ve > ile karşılaştırma yapılabilir.
"a" < "b"; // = true

// Tip dönüşümü(type conversion), çift eşittir işaretiyle yapılan karşılaştırmalarda gerçekleştirilir.
"5" == 5; // = true
null == undefined; // = true

// ...ancak === işaretini kullanırsanız...
"5" === 5; // = false
null === undefined; // = false

// ...bu bazen garip davranışlara yol açabilir...
13 + !0; // 14
"13" + !0; // '13true'

// Bir stringteki karakterlere `charAt` kullanarak erişebilirsiniz.
"Bu bir string(dize)".charAt(0);  // = 'B'

// ...veya daha büyük parçaları almak için `substring` kullanabilirsiniz.
"Merhaba dünya".substring(0, 5); // = "Merhaba"

// `length` bir özelliktir, bu yüzden () kullanmayın.
"Merhaba".length; // = 7

// Ayrıca `null` ve `undefined` değerleri de vardır.
null;      // bilerek atanmamış bir değeri belirtmek için kullanılır.
undefined; // bir değerin şu anda mevcut olmadığını belirtmek için kullanılır (aslında
           // `undefined` kendisi bir değerdir)

// false, null, undefined, NaN, 0 ve "" yanıltıcıdır (falsy); diğer her şey doğrudur (truthy).
// 0 yanıltıcıdır ve "0" doğrudur, 0 == "0" olduğu halde.

///////////////////////////////////
// 2. Değişkenler(Variables), Diziler(Arrays) ve Nesneler(Objects)

// Değişkenler `var` anahtar kelimesiyle tanımlanır. JavaScript dinamik olarak
// yazıldığından türü belirtmeniz gerekmez. Atama işlemi tek `=` karakteriyle yapılır.
var birDegisken = 5;

// Eğer var anahtar kelimesini atlamışsanız, hata almayacaksınız...
ikinciDegisken = 10;

// ...ancak değişkeniniz tanımladığınız kapsamda değil, global kapsamda oluşturulur.

// Atanmadan tanımlanan değişkenler undefined olarak ayarlanır.
var ucuncuDegisken; // = undefined

// Eğer tek satırda birkaç değişken tanımlamak istiyorsanız, virgül ayırıcısını kullanabilirsiniz.
var dorduncuDegisken = 2, besinciDegisken = 4;

// Değişkenler üzerinde matematiksel işlemler için kısa yol bulunmaktadır:
birDegisken += 5; // birDegisken = birDegisken + 5; birDegisken şimdi 10
birDegisken *= 10; // şimdi birDegisken 100

// Ve 1 eklemek veya çıkarmak için daha da kısa bir yol bulunmaktadır
birDegisken++; // şimdi birDegisken 101
birDegisken--; // tekrar 100'e döndü

// Diziler, herhangi bir türde değerlerin sıralı listeleridir.
var benimDizim = ["Merhaba", 45, true];

// Elemanlara, köşeli parantezlerle indeks(index) kullanarak erişilebilir.
// Dizi indeksleri sıfırdan başlar.
benimDizim[1]; // = 45

// Diziler değiştirilebilir ve değişken uzunluğa sahiptir.
benimDizim.push("Dünya");
benimDizim.length; // = 4

// Belirli bir indekse ekleme/düzenleme yapma
benimDizim[3] = "Merhaba";

// Dizinin başından veya sonundan bir eleman ekleyip çıkarma
benimDizim.unshift(3); // İlk eleman olarak ekle
birDegisken = benimDizim.shift(); // İlk elemanı çıkar ve geri döndür
benimDizim.push(3); // Son eleman olarak ekle
birDegisken = benimDizim.pop(); // Son elemanı çıkar ve geri döndür

// Dizinin tüm elemanlarını noktalı virgül ile birleştir
var benimDizim0 = [32,false,"js",12,56,90];
benimDizim0.join(";"); // = "32;false;js;12;56;90"

// İndeks 1'den (dahil) indeks 4'e (hariç) kadar olan elemanları içeren alt diziyi al
benimDizim0.slice(1,4); // = [false,"js",12]

// İndeks 2'den başlayarak 4 elemanı kaldır ve yerine "hi", "wr" ve "ld" dizelerini ekle; kaldırılan alt diziyi döndür
benimDizim0.splice(2,4,"hi","wr","ld"); // = ["js",12,56,90]
// benimDizim0 === [32,false,"hi","wr","ld"]

// JavaScript nesneleri, diğer dillerde "sözlük(dictionary)" veya "haritalar(maps)" ile eşdeğerdir: anahtar-değer çiftlerinin sırasız bir koleksiyonudur.
var benimNesnem = {anahtar1: "Merhaba", anahtar2: "Dünya"};

// Değerler herhangi bir tür olabilir.
var benimNesnem = {benimAnahtarim: "benimDegerim", "digerAnahtarim": 4};

// Nesne özelliklerine köşeli parantezlerle veya nokta sözdizimi kullanarak da erişilebilir,
benimNesnem["digerAnahtarim"]; // = 4
benimNesnem.benimAnahtarim; // = "benimDegerim"

// Nesneler değiştirilebilir; değerler değiştirilebilir ve yeni anahtarlar eklenir.
benimNesnem.benimUcuncuAnahtarim = true;

// Henüz tanımlanmamış bir değeri erişmeye çalışırsanız, undefined alırsınız.
benimNesnem.benimDorduncuAnahtarim; // = undefined

///////////////////////////////////
// 3. Mantık ve Kontrol Yapıları

// `if` yapısı beklediğiniz gibi çalışır.
var sayi = 1;
if (sayi == 3){
    // sayı 3 ise değerlendirilir
} else if (sayi == 4){
    // sayı 4 ise değerlendirilir
} else {
    // ne 3 ne de 4 ise değerlendirilir
}

// `while` da beklediğiniz gibi çalışır.
while (true){
    // Sonsuz döngü!
}

// Do-while döngüleri, while döngülerine benzer, ancak her zaman en az bir kez çalışır.
var input;
do {
    input = getInput();
} while (!isValid(input));

// `for` döngüsü C ve Java ile aynıdır:
// başlatma; devam koşulu; iterasyon.
for (var i = 0; i < 5; i++){
    // 5 kez çalışır
}

// Etiketli döngülerden çıkmak, Java'ya benzer.
dis:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break dis;
            // sadece iç içe olan yerine dış döngüden çıkar
        }
    }
}

// for/in ifadesi bir nesnenin özellikleri üzerinde dolaşmaya olanak sağlar.
var aciklama = "";
var kisi = {ad:"Kaan", soyad:"Kaçar", yas:26};
for (var x in kisi){
    aciklama += kisi[x] + " ";
} // aciklama = 'Kaan Kaçar 26'

// for/of ifadesi, yinelemeli nesneler üzerinde dolaşmayı sağlar
var evdekiHayvanlar = "";
var hayvanlar = ["kedi", "köpek", "hamster", "kirpi"];
for (var hayvan of hayvanlar){
    evdekiHayvanlar += hayvan + " ";
} // evdekiHayvanlar = 'kedi köpek hamster kirpi '

// && mantıksal ve, || mantıksal veya'dır.
if (ev.buyukluk == "büyük" && ev.renk == "mavi"){
    ev.icinde = "ayı";
}
if (renk == "kırmızı" || renk == "mavi"){
    // renk ya kırmızı ya da mavidir
}

// && ve || "kısa devre(short circuit)" yapar, bu varsayılan değerlerin atanmasında kullanışlıdır.
var isim = digerIsim || "varsayılan";

// `switch` ifadesi `===` ile eşitlik kontrolü yapar.
// case kodu çalıştıktan sonra çalışmasını istemiyorsanız "break" kullanın.
not = 'B';
switch (not) {
  case 'A':
    console.log("Harika iş");
    break;
  case 'B':
    console.log("İyi iş");
    break;
  case 'C':
    console.log("Daha iyisini yapabilirsin");
    break;
  default:
    console.log("Eyvah");
    break;
}


///////////////////////////////////
// 4. Fonksiyonlar, Kapsam(Scope) ve Kapanışlar(Closure)

// JavaScript fonksiyonları `function` anahtar kelimesi ile tanımlanır.
function fonksiyonum(sey){
    return sey.toUpperCase();
}
fonksiyonum("foo"); // = "FOO"

// Dikkat edilmesi gereken bir nokta, döndürülecek değerin `return` anahtar kelimesiyle
// aynı satırda başlaması gerektiğidir. Aksi takdirde, otomatik noktalı virgül ekleme
// nedeniyle her zaman `undefined` döndürülür.
function fonksiyonum(){
    return // <- buraya otomatik olarak noktalı virgül eklenir
    {buBir: 'nesne haritası'};
}
fonksiyonum(); // = undefined

// JavaScript fonksiyonları birinci sınıf nesnelerdir, bu nedenle farklı değişken adlarına
// yeniden atanabilir ve diğer fonksiyonlara argüman olarak geçilebilir - örneğin,
// bir olay işleyici sağlarken:
function fonksiyonum(){
    // bu kod 5 saniye sonra çağrılacak
}
setTimeout(fonksiyonum, 5000);
// Not: setTimeout, JS dilinin bir parçası değildir, ancak tarayıcılar ve Node.js tarafından sağlanır.

// Tarayıcılar tarafından sağlanan başka bir fonksiyon da setInterval'dir
function fonksiyonum(){
    // bu kod her 5 saniyede bir çağrılacak
}
setInterval(fonksiyonum, 5000);

// Fonksiyon nesnelerinin bile isimle tanımlanması gerekmez - başka bir argümana
// doğrudan anonim bir fonksiyon tanımı yazabilirsiniz.
setTimeout(function(){
    // bu kod 5 saniye sonra çağrılacak
}, 5000);

// JavaScript, fonksiyon kapsamına sahiptir; fonksiyonlar kendi kapsamlarını alır,
// ancak diğer bloklar almaz.
if (true){
    var i = 5;
}
i; // = 5 - blok kapsamı olan bir dilde beklendiği gibi undefined değil

// Bu, "hemen çalışan anonim fonksiyonlar"ın yaygın bir modeline yol açmıştır.
// Bu model, geçici değişkenlerin global kapsama sızmasını önler.
(function(){
    var gecici = 5;
    // Global kapsama `window` olarak atanarak global kapsama erişebiliriz.
    // Web tarayıcısında global nesne her zaman `window`'dur. Node.js gibi tarayıcı dışı
    // ortamlarda global nesnenin farklı bir adı olabilir.
    window.kalici = 10;
})();
gecici; // Referans Hatası verir
kalici; // = 10

// JavaScript'in en güçlü özelliklerinden biri closure'lardır. Eğer bir fonksiyon başka bir fonksiyonun içinde tanımlanmışsa, iç fonksiyon dış fonksiyonun tüm değişkenlerine erişebilir, hatta dış fonksiyon sonlandıktan sonra bile.
function beşSaniyedeMerhabaDe(isim){
    var mesaj = "Merhaba, " + isim + "!";
    // İç fonksiyonlar varsayılan olarak yerel kapsama yerleştirilir, sanki `var` ile tanımlanmış gibi.
    function içFonksiyon(){
        alert(mesaj);
    }
    setTimeout(içFonksiyon, 5000);
    // setTimeout asenkron bir işlem olduğu için beşSaniyedeMerhabaDe fonksiyonu hemen sonlanacak ve setTimeout daha sonra içFonksiyon'u çağıracak. Ancak içFonksiyon, beşSaniyedeMerhabaDe fonksiyonunun kapsamında olduğu için içFonksiyon sonunda çağrıldığında hala `mesaj` değişkenine erişebilecektir.
}
beşSaniyedeMerhabaDe("Kaan"); // 5 saniye sonra "Merhaba, Kaan!" içeren bir pencere açacak

///////////////////////////////////
// 5. Nesneler Hakkında Daha Fazla: Kurucular(Constructors) ve Prototipler(Prototypes)

// Nesneler fonksiyonlar içerebilir.
var myObj = {
    myFunc: function(){
        return "Merhaba dünya!";
    }
};
myObj.myFunc(); // = "Merhaba dünya!"

// Nesneye bağlı olarak çağrılan fonksiyonlar, `this` anahtar kelimesini kullanarak kendilerine bağlı olan nesneye erişebilir.
myObj = {
    myString: "Merhaba dünya!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Merhaba dünya!"

// `this` hangi bağlamda fonksiyonun çağrıldığıyla ilgilidir, tanımlandığı yerle değil. Bu nedenle, fonksiyonumuz nesnenin bağlamında çağrılmazsa çalışmaz.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Tersine, bir fonksiyon, tanımlandığı sırada bağlı olmasa bile nesneye atanabilir ve `this` aracılığıyla nesneye erişebilir.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "MERHABA DÜNYA!"

// Ayrıca, bir fonksiyonu çağırırken onu hangi bağlamda çalıştıracağımızı `call` veya `apply` kullanarak belirtebiliriz.

var anotherFunc = function(s){
    return this.myString + s;
};
anotherFunc.call(myObj, " Ve Merhaba Ay!"); // = "Merhaba Dünya! Ve Merhaba Ay!"

// `apply` işlevi neredeyse aynıdır, ancak bir argüman listesi için bir dizi alır.

anotherFunc.apply(myObj, [" Ve Merhaba Güneş!"]); // = "Merhaba Dünya! Ve Merhaba Güneş!"

// Bu, bir dizi geçirmek istediğinizde bir argüman dizisini kabul eden bir işlevle çalışırken faydalıdır.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (Haydaaa)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Ancak, `call` ve `apply` geçicidir. Kalıcı olmasını istediğimizde `bind` kullanabiliriz.

var birlestir = anotherFunc.bind(myObj);
birlestir(" Ve Merhaba Satürn!"); // = "Merhaba Dünya! Ve Merhaba Satürn!"

// `bind` ayrıca bir işlevi kısmen uygulamak (curry) için de kullanılabilir.

var product = function(a, b){ return a * b; };
var ikiylecarp = product.bind(this, 2);
ikiylecarp(8); // = 16

// `new` anahtar kelimesiyle bir işlevi çağırdığınızda, yeni bir nesne oluşturulur ve bu nesne `this` anahtar kelimesi aracılığıyla işleve ulaşır. Böyle çağrılmak üzere tasarlanmış işlevlere kurucular denir.

var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// JavaScript, diğer popüler nesne tabanlı dillerin aksine, 'sınıf' taslaklarından oluşturulan 'örnekler' kavramına sahip değildir; bunun yerine, JavaScript, örnekleme ve kalıtımı tek bir kavramda birleştirir: 'prototype' (prototip).

// Her JavaScript nesnesinin bir 'prototype'u vardır. Bir nesnenin üzerinde mevcut olmayan bir özelliğe erişmeye çalıştığınızda, yorumlayıcı prototipine bakar.

// Bazı JS uygulamaları, nesnenin prototipine `__proto__` sihirli özelliği üzerinden erişmenizi sağlar. Bu, prototipleri açıklamak için faydalı olsa da, bunun standart bir parçası değildir; prototipleri kullanmanın standart yollarına daha sonra geleceğiz.
var myObj = {
    myString: "Merhaba dünya!"
};
var prototipim = {
    hayatinAnlami: 42,
    myFunc: function(){
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = prototipim;
myObj.hayatinAnlami; // = 42

// Bu, işlevler için de çalışır.
myObj.myFunc(); // = "merhaba dünya!"

// Elbette, özelliğiniz prototipte değilse, prototipin prototipi aranır ve böyle devam eder.
prototipim.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Burada herhangi bir kopyalama işlemi yoktur; her nesne, prototipine bir referans tutar. Bu, prototipi değiştirebileceğimiz ve değişikliklerimizin her yerde yansıyacağı anlamına gelir.
prototipim.hayatinAnlami = 43;
myObj.hayatinAnlami; // = 43

// for/in ifadesi bir nesnenin özellikleri üzerinde yinelemeye izin verir,
// bir null prototipi görünceye kadar prototip zinciri üzerinde yürür.
for (var x in myObj){
    console.log(myObj[x]);
}
///çıktı:
// Merhaba dünya!
// 43
// [Fonksiyon: myFunc]
// true

// Yalnızca nesneye kendisi tarafından eklenen özellikleri düşünmek için
// ve prototiplerini değil, `hasOwnProperty()` kontrolünü kullanın.
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
///çıktı:
// Merhaba dünya!

// `__proto__`'nun standart olmadığını ve mevcut bir nesnenin prototipini
// değiştirmenin standart bir yolunun olmadığını belirttik. Bununla birlikte, verilen bir prototiple
// yeni bir nesne oluşturmanın iki yolu vardır.

// İlk yöntem:
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// İkinci yol, yapıcılarla(constructors) ilgilidir.
// Yapıcıların prototype adında bir özelliği vardır. Bu, yapıcı işlevinin kendisinin prototipi değildir;
// bunun yerine, yeni nesnelerin, o yapıcı ve new anahtar kelimesiyle oluşturulduklarında verilen
// prototiptir.
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

// Dize ve sayı gibi yerleşik tiplerin ayrıca denk gelen sarma nesnelerini oluşturan yapıcıları vardır.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Ancak, tam olarak eşdeğer değiller.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Bu kod çalışmayacak, çünkü 0 yanlış değerlidir.
}
if (new Number(0)){
   // Bu kod çalışacak, çünkü sarmalanan sayılar nesnelerdir ve nesneler
   // her zaman doğru değerlidir.
}

// Bununla birlikte, sarma nesneleri ve düzenli yerleşik tipler bir prototipi paylaşırlar, bu yüzden
// aslında bir dizeye işlevsellik ekleyebilirsiniz, örneğin.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// Bu genellikle "polyfilling" olarak adlandırılan bir yöntemde kullanılır.

// Örneğin, Object.create bazı uygulamalarda mevcut değil,
// ancak aşağıdaki polyfill ile yine de kullanabiliriz:
if (Object.create === undefined){ // mevcut ise üzerine yazma
    Object.create = function(proto){
        // doğru prototipe sahip geçici bir yapıcı oluştur
        var Constructor = function(){};
        Constructor.prototype = proto;
        // ardından onu kullanarak yeni, uygun prototipli bir nesne oluştur
        return new Constructor();
    };
}

// ES6

// "let" anahtar kelimesi, değişkenleri bir fonksiyon kapsamı yerine
// bir leksikal kapsamda tanımlamanıza olanak tanır, var anahtar kelimesi gibi.
let name = "Billy";

// let ile tanımlanan değişkenlere yeni değerler atanabilir.
name = "William";

// "const" anahtar kelimesi, let ile aynı şekilde bir leksikal kapsamda bir değişken tanımlamanıza olanak tanır,
// ancak bir değer atandıktan sonra tekrar değer atanamaz.

const pi = 3.14;

pi = 4.13; // Bunu yapamazsınız.

// ES6'da "lambda sözdizimi" olarak bilinen bir fonksiyon için farklı bir sözdizimi vardır.
// Bu, sabit ve let ile tanımlanan değişkenler gibi bir leksikal kapsamda fonksiyonların tanımlanmasına olanak tanır.

const isEven = (number) => {
    return number % 2 === 0;
};

isEven(7); // false

// Geleneksel sözdizimde bu fonksiyonun "eşdeğeri" şu şekilde görünür:

function isEven(number) {
    return number % 2 === 0;
};

// "eşdeğer" kelimesini çift tırnak içine koydum çünkü lambda sözdizisi kullanılarak tanımlanan bir fonksiyon, tanımdan önce çağrılamaz.
// Aşağıdaki, geçersiz bir kullanım örneğidir:

add(1, 8);

const add = (firstNumber, secondNumber) => {
    return firstNumber + secondNumber;
};
```

## Daha Fazlası İçin

[Mozilla Developer Network][1] tarayıcılarda kullanılan JavaScript için mükemmel bir dokümantasyon sunar. Ayrıca, bir wiki olduğu için daha fazla bilgi edindikçe kendi bilginizi paylaşarak diğer insanlara yardımcı olabilirsiniz.

MDN'nin [A re-introduction to JavaScript(JavaScript'e Yeniden Giriş)][2] başlıklı kaynağı, burada ele alınan kavramların daha detaylı bir şekilde ele alınmış halini içerir. Bu rehber, bilinçli bir şekilde sadece JavaScript dilini ele almıştır. Eğer JavaScript'i web sayfalarında nasıl kullanacağınız hakkında daha fazla bilgi edinmek isterseniz, [Belge Nesne Modeli (Document Object Model)][3] hakkında öğrenmeye başlayabilirsiniz.

[Patika - Javascript][4]Hakan Yalçınkaya'nın eğitmen olduğu bu kaynak, Javascript'i baştan sonra öğrenmenizi ve öğrendiklerinizi çeşitli test, proje ve coding challenge'lar ile pekiştirmenizi sağlar.

[Learn Javascript by Example and with Challenges][5] Bu da öğrendiklerinizi çeşitli challenge'lar ile pekiştirmenize olanak sağlar.

[JavaScript Garden][6] tüm detaylarının derinlemesine incelendiği bir rehberdir.

[Yusuf Sezer][7] Hem Türkçe hem yazılı olsun diyenler için kapsamlı bir kaynak.

[BTK Akademi][8] BTK Akademi Javascript kursu

[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: https://academy.patika.dev/courses/javascript
[5]: http://www.learneroo.com/modules/64/nodes/350
[6]: https://shamansir.github.io/JavaScript-Garden/
[7]: https://www.yusufsezer.com.tr/javascript-dersleri/
[8]: https://www.btkakademi.gov.tr/portal/course/javascript-8099
