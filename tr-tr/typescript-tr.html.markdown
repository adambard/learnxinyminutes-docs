---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Mustafa Zengin", "http://zengin.github.com"]
filename: learntypescript-tr.ts
lang: tr-tr
---

TypeScript, JavaScript'le yazılmış büyük ölçekli uygulamaların geliştirilmesini kolaylaştırmayı hedefleyen bir dildir.
TypeScript, JavaScript'e sınıflar, modüller, arayüzler, jenerik tipler ve (isteğe bağlı) static tipleme gibi genel konseptler ekler.
JavaScript, TypeScript'in bir alt kümesidir: Bütün JavaScript kodları geçerli birer TypeScript kodudur ve sorunsuz herhangi bir projeye eklenebilirler. TypeScript derleyici JavaScript kodu üretir.

Bu makale sadece TypeScript'e ait ekstra söz dizimini konu alır, JavaScript için bkz: [JavaScript] (../javascript/).

TypeScript derleyiciyi test etmek için [Playground] (http://www.typescriptlang.org/Playground)'a gidin. Orada otomatik tamamlama ile kod yazabilecek ve üretilen JavaScript'i görebileceksiniz.

```js
// TypeScript'te üç ana tip vardır.
var bittiMi: boolean = false;
var satırlar: number = 42;
var isim: string = "Anders";

// Tipin bilinmediği zamanlar için "Any" tipi
var bilinmiyor: any = 4;
bilinmiyor = "belki de bir string'dir";
bilinmiyor = false; // tamam, boolean olsun

// Kolleksiyon olarak, tipli ve jenerik diziler
var liste: number[] = [1, 2, 3];
// Alternatif olarak jenerik Array tipi
var liste: Array<number> = [1, 2, 3];

// 'enum' tipleri:
enum Renk {Kırmızı, Yeşil, Mavi};
var r: Renk = Renk.Yeşil;

// Son olarak, "void" hiç bir şey döndürmeyen fonksiyonlarda kullanılan tiptir.
function çokFeciBirUyarı(): void {
  alert("Ben biraz sinir bozucuyum!");
}

// Fonksiyonlar birinci sınıf vatandaşlardır ve "kalın ok" lambda söz dizimi "=>"
// ve tip çıkarımı kullanırlar.
// Aşağıda listelenenler birbirinin aynısı ve derleyici aynı fonksiyon yapısını
// çıkaracak ve aynı JavaScript kodunu üretecektir
var f1 = function(i: number): number { return i * i; }
// Döndürülen tip tip çıkarımıyla belirlendi
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// Döndürülen tip tip çıkarımıyla belirlendi
var f4 = (i: number) => { return i * i; }
// Döndürülen tip tip çıkarımıyla belirlendi
// Tek satırlık yazımda "return" anahtar kelimesine ihtiyaç yok.
var f5 = (i: number) =>  i * i;

// Arayüzler yapısaldır, listelenen özelliklere sahip her şey arayüzle uyumludur.
interface Kişi {
  isim: string;
  // İsteğe bağlı özellikler "?" ile işaretlenir
  yaş?: number;
  // Ve fonksiyonlar...
  hareketEt(): void;
}

// "Kişi" arayüzünü kullanan bir nesne
// isim ve hareketEt özelliklerine sahip olduğu için Kişi olarak kullanılabilir.
var p: Kişi = { isim: "Anders", hareketEt: () => {} };
// İsteğe bağlı özelliğe sahip bir Kişi
var geçerliKişi: Kişi = { isim: "Anders", yaş: 42, hareketEt: () => {} };
// Geçersiz bir kişi, çünkü yaş bir sayı (number) tipi değil
var geçersizKişi: Kişi = { isim: "Anders", yaş: true };

// Arayüzler bir fonksiyon tipi de ifade edebilirler
interface aramaFonksiyonu {
  (kaynak: string, altString: string): boolean;
}

// Parametrelerin sadece tipleri önemli, isimleri önemli değil
var benimAramam: aramaFonksiyonu;
benimAramam = function(kynk: string, alt: string) {
  return kynk.search(alt) != -1;
}

// Sınıflar - üyeler (members) varsayılan olarak public'tir.
class Nokta {
  // Özellikler
  x: number;

  // Yapıcı (constructor) - bu bağlamdaki public/private anahtar kelimeleri
  // özellikler için gerekli demirbaş kodu oluşturur ve ilk değerlerin
  // atanmasını sağlar.
  // Bu örnekte, "y" de "x" gibi tanımlanacak is, but with less code
  // Default values are also supported

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Fonksiyonlar
  mesafe() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Statik üyeler
  static orijin = new Nokta(0, 0);
}

var p1 = new Nokta(10 ,20);
var p2 = new Nokta(25); //y = 0

// Kalıtım
class Nokta3Boyutlu extends Nokta {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // süper sınıfın yapıcısını çağırmak zorunlu
  }

  // yeniden tanımlama
  mesafe() {
    var d = super.mesafe();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Modüller, "." alt modülleri ayırmak için kullanılabilir
module Geometri {
  export class Kare {
    constructor(public kenarUzunluğu: number = 0) {
    }
    alan() {
      return Math.pow(this.kenarUzunluğu, 2);
    }
  }
}

var s1 = new Geometri.Kare(5);

// Modüle atıfta bulunmak için yerel takma ad
import G = Geometri;

var s2 = new G.Kare(10);

// Jenerik Tipler
// Sınıflar
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// Arayüzler
interface Çift<T> {
  item1: T;
  item2: T;
}

// Ve fonksiyonlar
var çifttenTupleÜret = function<T>(p: Çift<T>) {
  return new Tuple(p.item1, p.item2);
};

var tuple = çifttenTupleÜret({ item1:"merhaba", item2:"dünya"});

// Tanım dosyasına atıfta bulunma:
/// <reference path="jquery.d.ts" />

// Şablon Stringleri (ters apostrof kullanan stringler)
// Şablon Stringlerinin kullanımı
var isim = 'Anders';
var selamlama = `Merhaba ${isim}, nasılsın?`
// Şablon Stringleri ile çok satırlı stringler
var çokSatırlıString = `Bu çok satırlı
bir string örneği`;

```

## Daha fazlası
 * [TypeScript Resmi Sitesi] (http://www.typescriptlang.org/)
 * [TypeScript dil spesifikasyonu (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Channel 9'da TypeScript'e Giriş] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [GitHub'ta Açık Kaynak Kodu] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - tip tanımları için kaynak] (http://definitelytyped.org/)
