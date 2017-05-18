---
language: swift
contributors:
  - ["Özgür Şahin", "https://github.com/ozgurshn/"]
filename: learnswift-tr.swift
lang: tr-tr
---

Swift iOS ve OSX platformlarında geliştirme yapmak için Apple tarafından oluşturulan yeni bir programlama dilidir.  Objective - C ile beraber kullanılabilecek ve de hatalı kodlara karşı daha esnek bir yapı sunacak bir şekilde tasarlanmıştır. Swift 2014 yılında Apple'ın geliştirici konferansı WWDC de tanıtıldı. Xcode 6+'a dahil edilen LLVM derleyici ile geliştirildi.
 

Apple'ın resmi [Swift Programlama Dili](https://itunes.apple.com/us/book/swift-programming-language/id881256329) kitabı iBooks'ta yerini aldı.

Ayrıca Swift ile gelen tüm özellikleri görmek için Apple'ın [başlangıç kılavuzu](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/RoadMapiOS/index.html)na bakmanızda yarar var.



```swift
// modülü import etme
import UIKit

//
// MARK: Temeller
//


//XCode işaretlemelerle kodunuzu bölümlere ayırmanızı ve sağ üstteki metot
//listesinde gruplama yapmanıza olanak sağlıyor
// MARK: Bölüm işareti
// TODO: Daha sonra yapılacak
// FIXME: Bu kodu düzelt

 
//Swift 2 de, println ve print metotları print komutunda birleştirildi.
//Print otomatik olarak yeni satır ekliyor.
print("Merhaba dünya") // println print olarak kullanılıyor.
print("Merhaba dünya", appendNewLine: false) // yeni bir satır eklemeden yazar.

// variables (var) değer atandıktan sonra değiştirilebilir.
// constants (let) değer atndıktan sonra değiştirilemez.

var degiskenim = 42
let øπΩ = "deger" // unicode degişken adları
let π = 3.1415926
let convenience = "keyword" // bağlamsal değişken adı
let isim = "ahmet"; let soyad = "un" // farklı ifadeler noktalı virgül 
kullanılarak ayrılabilir.
let `class` = "keyword" // rezerve edilmiş keywordler tek tırnak içerisine 
alınarak değişken adı olarak kullanılabilir
let doubleOlduguBelli: Double = 70
let intDegisken = 0007 // 7
let largeIntDegisken = 77_000 // 77000
let etiket = "birseyler " + String(degiskenim) // Cast etme
let piYazi = "Pi = \(π), Pi 2 = \(π * 2)" // String içerisine değiken yazdırma


// Builde özel değişkenler
// -D build ayarını kullanır.
#if false
    print("yazılmadı")
    let buildDegiskeni= 3
#else
    let buildDegiskeni = 7
#endif
print("Build degiskeni: \(buildDegiskeni)") // Build degeri: 7

/*  
    Optionals Swift dilinde bazı değerleri veya yokluğu (None) bir değişkende 
    	tutmanıza olanak sağlar.  
    
    Swift'te her bir degişkeninin bir değeri olması gerektiğinden, nil değeri 
    	 bile Optional değer olarak saklanır.

    Optional<T> bir enum'dır.
*/
var baziOptionalString: String? = "optional" // nil olabilir.
// yukarıdakiyle aynı ama ? bir postfix (sona eklenir) operatördür. (kolay 
//okunabilir)
var someOptionalString2: Optional<String> = "optional"  


if baziOptionalString != nil {
    // ben nil değilim
    if baziOptionalString!.hasPrefix("opt") {
        print("ön eki var")
    }
    
    let bos = baziOptionalString?.isEmpty
}
baziOptionalString = nil

// belirgin olarak acilan(unwrap) opsiyonel (optional) değer
var acilanString: String! = "Değer bekleniliyor"
//yukarıdakiyle aynı ama ! bir postfix operatördür (kolay okunabilir)
var acilanString2: ImplicitlyUnwrappedOptional<String> = "Değer bekleniliyor."

if let baziOpsiyonelSabitString = baziOptionalString {
    // eğer bir değeri varsa, nil değilse
    if ! baziOpsiyonelSabitString("tamam") {
        // ön eke sahip değil
    }
}

// Swift değişkenlerde herhangi bir tip saklanabilir.
// AnyObject == id
// Objective-C deki `id` den farklı olarak, AnyObject tüm değişkenlerle
//çalışabilir
(Class, Int, struct, etc)
var herhangiBirObject: AnyObject = 7
herhangiBirObject = "Değer string olarak değişti, iyi bir yöntem değil ama mümkün"

/*
    Yorumlar buraya
    
    /*
        İç içe yorum yazılması da mümkün
    */
*/

//
// MARK: Koleksiyonlar
//

/*
    	Array ve Dictionary tipleri aslında structdırlar. Bu yüzden `let` ve `var` 
    	ayrıca bu tipleri tanımlarken değişebilir(var) veya değişemez(let) 
    	olduğunu 	belirtir.
    	
*/

// Diziler
var liste = ["balik", "su", "limon"]
liste[1] = "şişe su"
let bosDizi = [String]() // let == değiştirilemez
let bosDizi2 = Array<String>() // yukarıdakiyle aynı
var bosDegistirilebilirDizi = [String]() // var == değişebilir


// Dictionary
var meslekler = [
    "Kamil": "Kaptan",
    "Ayse": "Analist"
]
meslekler["Cansu"] = "Halkla İlişkiler"
let bosDictionary = [String: Float]() // let == değiştirilemez
let bosDictionary2 = Dictionary<String, Float>() // yukarıdakiyle aynı
var bosDegistirilebirDictionary = [String: Float]() // var == değiştirilebilir


//
// MARK: Kontroller
//

// for döngüsü (dizi)
let dizi = [1, 1, 2, 3, 5]
for deger in dizi {
    if deger == 1 {
        print("Bir!")
    } else {
        print("Bir degil!")
    }
}

// for döngüsü (dictionary)
var dict = ["one": 1, "two": 2]
for (key, value) in dict {
    print("\(key): \(value)")
}

// for döngüsü (aralık)
for i in -1...liste.count {
    print(i)
}
liste[1...2] = ["et", "yogurt"]
// ..<  kullanarak son elemanı çıkartabilirsiniz

// while döngüsü
var i = 1
while i < 1000 {
    i *= 2
}

// do-while döngüsü
do {
    print("merhaba")
} while 1 == 2

// Switch
// Çok güçlü, `if` ifadesenin daha kolay okunabilir hali olarak düşünün
// String, object örnekleri, ve primitif tipleri (Int, Double, vs) destekler.
let sebze = "kırmızı biber"
switch sebze {
case "sogan":
    let sebzeYorumu = "Biraz da domates ekle"
case "domates", "salata":
    let sebzeYorumu = "İyi bir sandviç olur"
case let lokalScopeDegeri where lokalScopeDegeri.hasSuffix("biber"):
    let sebzeYorumu = "Acı bir \(lokalScopeDegeri)?"
default: // zorunludur (tüm olasılıkları yakalamak icin)
    let sebzeYorumu = "Corbadaki herseyin tadı güzel"
}


//
// MARK: Fonksiyonlar
//

// Fonksiyonlar first-class tiplerdir, yani başka fonksiyon içine konabilir
// ve parametre olarak geçirilebilirler.

// Swift dökümanlarıylaa birlikte Fonksiyonlar (format as reStructedText)

/**
    selamlama işlemi

    :param: isim e isim
    :param: gun  e A gun
    :returns: isim ve gunu iceren bir String
*/
func selam(isim: String, gun: String) -> String {
    return "Merhaba \(isim), bugün \(gun)."
}
selam("Can", "Salı")

// fonksiyon parametre davranışı hariç yukarıdakine benzer
func selam2(#gerekliIsim: String, disParametreIsmi lokalParamtreIsmi: String) -> String {
    return "Merhaba \(gerekliIsim), bugün \(lokalParamtreIsmi)"
}
selam2(gerekliIsim:"Can", disParametreIsmi: "Salı")

// Bir tuple ile birden fazla deger dönen fonksiyon
func fiyatlariGetir() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let fiyatTuple = fiyatlariGetir()
let fiyat = fiyatTuple.2 // 3.79
// _ (alt çizgi) kullanımı Tuple degerlerini veya diğer değerleri görmezden
//gelir
let (_, fiyat1, _) = fiyatTuple // fiyat1 == 3.69
print(fiyat1 == fiyatTuple.1) // true
print("Benzin fiyatı: \(fiyat)")

// Çeşitli Argümanlar
func ayarla(sayilar: Int...) {
    // bu bir dizidir
    let sayi = sayilar[0]
    let argumanSAyisi = sayilar.count
}

// fonksiyonu parametre olarak geçirme veya döndürme
func arttirmaIslemi() -> (Int -> Int) {
    func birEkle(sayi: Int) -> Int {
        return 1 + sayi
    }
    return birEkle
}
var arttir = arttirmaIslemi()
arttir(7)

// referans geçirme
func yerDegistir(inout a: Int, inout b: Int) {
    let tempA = a
    a = b
    b = tempA
}
var someIntA = 7
var someIntB = 3
yerDegistir(&someIntA, &someIntB)
print(someIntB) // 7


//
// MARK: Closurelar
//
var sayilar = [1, 2, 6]

// Fonksiyonlar özelleştirilmiş closurelardır. ({})

// Closure örneği.
// `->` parametrelerle dönüş tipini birbirinden ayırır
// `in` closure başlığını closure bodysinden ayırır.
sayilar.map({
    (sayi: Int) -> Int in
    let sonuc = 3 * sayi
    return sonuc
})

// eger tip biliniyorsa, yukarıdaki gibi, şöyle yapabiliriz
sayilar = sayilar.map({ sayi in 3 * sayi })
// Hatta bunu
//sayilar = sayilar.map({ $0 * 3 })

print(sayilar) // [3, 6, 18]

// Trailing closure
sayilar = sorted(sayilar) { $0 > $1 }

print(sayilar) // [18, 6, 3]

// Super kısa hali ise, < operatörü tipleri çıkartabildiği için

sayilar = sorted(sayilar, < )

print(sayilar) // [3, 6, 18]

//
// MARK: Yapılar
//

// Structurelar ve sınıflar birçok aynı özelliğe sahiptir.
struct IsimTablosu {
    let isimler = [String]()
    
    // Özelleştirilmiş dizi erişimi
    subscript(index: Int) -> String {
        return isimler[index]
    }
}

// Structurelar otomatik oluşturulmuş kurucu metoda sahiptir.
let isimTablosu = IsimTablosu(isimler: ["Ben", "Onlar"])
let isim = isimTablosu[1]
print("İsim \(name)") // İsim Onlar

//
// MARK: Sınıflar
//

// Sınıflar, structurelar ve üyeleri 3 seviye erişime sahiptir.
// Bunlar: internal (default), public, private

public class Sekil {
    public func alaniGetir() -> Int {
        return 0;
    }
}

// Sınıfın tüm değişkenleri ve metotları publictir.
// Eğer sadece veriyi yapılandırılmış bir objede
// saklamak istiyorsanız, `struct` kullanmalısınız.

internal class Rect: Sekil {
    var yanUzunluk: Int = 1
    
    // Özelleştirilmiş getter ve setter propertyleri
    private var cevre: Int {
        get {
            return 4 * yanUzunluk
        }
        set {
            // `newValue ` setterlarda yeni değere erişimi sağlar
            yanUzunluk = newValue / 4
        }
    }
    
    // Bir değişkene geç atama(lazy load) yapmak
    // altSekil getter cağrılana dek nil (oluşturulmamış) olarak kalır
    lazy var altSekil = Rect(yanUzunluk: 4)
    
    // Eğer özelleştirilmiş getter ve setter a ihtiyacınız yoksa,
    // ama bir değişkene get veya set yapıldıktan sonra bir işlem yapmak 
    // istiyorsanız, `willSet` ve `didSet` metotlarını kullanabilirsiniz
    var identifier: String = "defaultID" {
        // `willSet` argümanı yeni değer için değişkenin adı olacaktır.
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }
    
    init(yanUzunluk: Int) {
        self. yanUzunluk = yanUzunluk
        // super.init i her zaman özelleştirilmiş değerleri oluşturduktan sonra
         çağırın
        super.init()
    }
    
    func kisalt() {
        if yanUzunluk > 0 {
            --yanUzunluk
        }
    }
    
    override func alaniGetir() -> Int {
        return yanUzunluk * yanUzunluk
    }
}

// Basit `Kare` sınıfI `Rect` sınıfını extend ediyor.
class Kare: Rect {
    convenience init() {
        self.init(yanUzunluk: 5)
    }
}

var benimKarem = Kare()
print(m benimKarem.alaniGetir()) // 25
benimKarem.kisalt()
print(benimKarem.yanUzunluk) // 4

// sınıf örneğini cast etme
let birSekil = benimKarem as Sekil

// örnekleri karşılaştır, objeleri karşılaştıran == (equal to) ile aynı değil  
if benimKarem === benimKarem {
    print("Evet, bu benimKarem")
}

// Opsiyonel init
class Daire: Sekil {
    var yaricap: Int
    override func alaniGetir() -> Int {
        return 3 * yaricap * yaricap
    }
    
    // Eğer init opsiyonelse (nil dönebilir) `init` den sonra soru işareti
    // son eki ekle.
    init?(yaricap: Int) {
        self.yaricap = yaricap
        super.init()
        
        if yaricap <= 0 {
            return nil
        }
    }
}

var benimDairem = Daire(radius: 1)
print(benimDairem?.alaniGetir())    // Optional(3)
print(benimDairem!. alaniGetir())    // 3
var benimBosDairem = Daire(yaricap: -1)
print(benimBosDairem?. alaniGetir())    // "nil"
if let daire = benimBosDairem {
    // benimBosDairem nil olduğu için çalışmayacak
    print("circle is not nil")
}


//
// MARK: Enumlar
//

// Enumlar opsiyonel olarak özel bir tip veya kendi tiplerinde olabilirler.
// Sınıflar gibi metotlar içerebilirler.

enum Kart {
    case Kupa, Maca, Sinek, Karo
    func getIcon() -> String {
        switch self {
        case .Maca: return "♤"
        case .Kupa: return "♡"
        case .Karo: return "♢"
        case .Sinek: return "♧"
        }
    }
}

// Enum değerleri kısayol syntaxa izin verir. Eğer değişken tipi açık olarak belirtildiyse enum tipini yazmaya gerek kalmaz.
var kartTipi: Kart = .Kupa

// Integer olmayan enumlar direk değer (rawValue) atama gerektirir.
enum KitapAdi: String {
    case John = "John"
    case Luke = "Luke"
}
print("Name: \(KitapAdi.John.rawValue)")

// Değerlerle ilişkilendirilmiş Enum
enum Mobilya {
    // Int ile ilişkilendirilmiş
    case Masa(yukseklik: Int)
    // String ve Int ile ilişkilendirilmiş
    case Sandalye(String, Int)
    
    func aciklama() -> String {
        switch self {
        case .Masa(let yukseklik):
            return "Masa boyu \(yukseklik) cm"
        case .Sandalye(let marka, let yukseklik):
            return "\(brand) marka sandalyenin boyu \(yukseklik) cm"
        }
    }
}

var masa: Mobilya = .Masa(yukseklik: 80)
print(masa.aciklama())     // "Masa boyu 80 cm"
var sandalye = Mobilya.Sandalye("Foo", 40)
print(sandalye.aciklama())    // "Foo marka sandalyenin boyu 40 cm"


//
// MARK: Protokoller
//

// `protocol` onu kullanan tiplerin bazı özel değişkenleri, metotları,
// tip metotlarını,opertörleri ve alt indisleri (subscripts) içermesini
// zorunlu hale getirebilir.

protocol SekilUretici {
    var aktif: Bool { get set }
    func sekilOlustur() -> Sekil
}

// @objc ile tanımlanan protokoller, uygunluğu kontrol edebilmenizi sağlayacak
// şekilde opsiyonel fonksiyonlara izin verir
@objc protocol SekliDondur {
    optional func sekillendirilmis()
    optional func sekillendirilebilir() -> Bool
}

class BenimSeklim: Rect {
    var delegate: SekliDondur?
    
    func buyut() {
        yanUzlunluk += 2

	// Bir çalışma zamanı hatası("optional chaining") fırlatmak yerine nil 
	//değeri görmezden gelerek nil dönmek için opsiyonel değişken, metot veya
	// altindisten sonra soru işareti koyabilirsiniz.
        if let izinVeriyormu = self.delegate?.sekillendirilebilir?() {
            // önce delegate i sonra metodu test edin
            self.delegate?.sekillendirilmis?()
        }
    }
}


//
// MARK: Diğerleri
//

// `extension`lar: Var olan tiplere ekstra özellikler ekleyin

// Kare artık `Printable` protokolüne uyuyor.
extension Kare: Printable {
    var description: String {
        return "Alan: \(alaniGetir()) - ID: \(self.identifier)"
    }
}

print("Kare: \(benimKarem)")

// Dahili tipleri de yeni özellikler ekleyebilirsiniz
extension Int {
    var customProperty: String {
        return "Bu sayı \(self)"
    }
    
    func carp(num: Int) -> Int {
        return num * self
    }
}

print(7.customProperty) // "Bu sayı 7"
print(14.carp(3)) // 42

// Genericler: Java ve C#'a benzer şekilde. `where` anahtar kelimesini
// kullanarak genericlerin özelliklerini belirleyin

func indexiBul<T: Equatable>(dizi: [T], bulunacakDeger: T) -> Int? {
    for (index, deger) in enumerate(dizi) {
        if deger == bulunacakDeger {
            return index
        }
    }
    return nil
}
let bulunanIndex = indexiBul([1, 2, 3, 4], 3)
print(bulunanIndex == 2) // true

// Operatorler:
// Özel operatorler şu karakterlerle başlayabilir:
//      / = - + * % < > ! & | ^ . ~
// veya
// Unicode math, symbol, arrow, dingbat, ve line/box karakterleri.
prefix operator !!! {}

// Yan uzunluğu 3 katına çıkartan prefix operatörü
prefix func !!! (inout sekil: Kare) -> Kare {
    sekil.YanUzunluk *= 3
    return sekil
}

// güncel deger
print(benimKarem.YanUzunluk)  // 4

// yan uzunluğu !!! operatorü kullanarak 3 katına çıkar
!!!benimKarem
print(benimKarem.YanUzunluk) // 12
```
