---
language: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Mustafa Zengin", "http://zengin.github.com/"]
filename: learnfsharp-tr.fs
lang: tr-tr
---

F# fonksiyonel ve nesne yönelimli, genel amaçlı bir programlama dilidir. Bedava ve açık kaynaklıdır ve Linux, Mac, Windows ve dahasında çalışır.

Hataları derleme zamanında yakalayan çok güçlü bir tip sistemine sahiptir, ancak tip çıkarımı yaptığından dinamik bir dil gibi görünür.

F#'ın söz dizimi C-stili dillerden farklıdır:

* Küme parantezi kod bloklarını ayırmak için kullanılmaz. Bunun yerine Python'da olduğu gibi girinti kullanılır.
* Parametreleri birbirinden ayırmak için virgül yerine boşluk karakteri kullanılır.

Aşağıdaki kodu denemek istiyorsanız, [tryfsharp.org](http://www.tryfsharp.org/Create)'a gidin be interaktif REPL'e kodu yapıştırın.

```csharp

// tek satır yorumlar ikili bölme işareti ile başlar
(* çok satırlı yorumlar ( * . . . * ) ikilisini kullanır

-çok satırlı yorumun sonu- *)

// ================================================
// Temel Söz Dizimi
// ================================================

// ------ "Değişkenler" (tam da değil) ------
// "let" anahtar kelimesi (değişmez) değer tanımlar
let tamsayım = 5
let ondalığım = 3.14
let stringim = "merhaba"           // tip bilgisi olmamasına dikkat

// ------ Listeler ------
let ikidenBeşe = [2; 3; 4; 5]      // Köşeli parantezler listeleri oluşturur,
                                   // değerler ise noktalı virgülle ayrılır.
let birdenBeşe = 1 :: ikidenBeşe   // :: yeni birinci elemanı olan bir liste oluşturur.
// Sonuç: [1; 2; 3; 4; 5]
let sıfırdanBeşe = [0; 1] @ ikidenBeşe  // @ iki listeyi birbirine ekler.

// ÖNEMLİ: virgüller hiçbir zaman ayraç olarak kullanılmaz, sadece noktalı virgüller!

// ------ Fonksiyonlar ------
// "let" anahtar kelimesi isimlendirilmiş fonksiyonları da tanımlar.
let kare x = x * x          // Parantez kullanılmadığına dikkat.
kare 3                      // Şimdi fonksiyonu uygulayın. Yine parantez yok.

let topla x y = x + y       // topla (x,y) kullanmayın! Bu tamamen başka bir anlama geliyor.
topla 2 3                   // Şimdi fonksiyonu uygulayın.

// çok satırlı bir fonksiyon tanımlamak için sadece girinti kullanın. Noktalı virgül gerekmez.
let çiftler liste =
   let çiftMi x = x % 2 = 0     // "çiftMi"yi alt fonksiyon olarak tanımlayın
   List.filter çiftMi liste     // List.filter 'boolean bir fonksiyon' ve
                                // 'üzerinde çalışılacak bir liste' parametrelerinden oluşan
                                // bir kütüphane fonksiyonu
                              
çiftler birdenBeşe              // Şimdi fonksiyonu uygula.

// Parantezleri önceliği netleştirmek için kullanabilirsiniz. Bu örnek
// "map"i önce iki argümana, sonra sonuç üzerinde "ekle" uyguluyor.
// Parantezler olmasaydı, "List.map" List.sum'ın ilk argümanı olurdu.
let yüzeKadarKarelerinToplamı =
   List.sum ( List.map kare [1..100] )

// Bir operasyonun sonucunu bir sonrakine "|>" kullanarak besleyebilirsin.
// Veri beslemek F#'ta UNIX'te olduğu gibi yaygındır..

// Burada yüzeKadarKarelerinToplamı fonksiyonunun veri beslemeyle yazılmış hali var:
let veriBeslemeyleYüzeKadarKarelerinToplamı =
   [1..100] |> List.map kare |> List.sum  // "kare" önceden tanımlanmıştı

// Lambda'ları (anonim fonksiyonları) "fun" anahtar kelimesiyle tanımlayabilirsin
let funlaYüzeKadarKarelerinToplamı =
   [1..100] |> List.map (fun x -> x * x) |> List.sum

// F#'ta "return" anahtar kelimesi yoktur. Bir fonksiyon
// her zaman son kullanılan ifadeyi döndürür.

// ------ Kalıp eşleştirme ------
// Match..with.. çok güçlü bir case/switch türevidir.
let basitKalıpEşleştirme =
   let x = "a"
   match x with
    | "a" -> printfn "x a'dır"
    | "b" -> printfn "x b'dir"
    | _ -> printfn "x başka bir şeydir"   // alt çizgi bütün kalıplarla eşleşir

// F# varsayılan olarak null'lara izin vermez -- Option tipini kullanıp
// kalıp eşleştirme yapmalısın.
// Some(..) ve None, Nullable tipler gibidir.
let geçerliDeğer = Some(99)
let geçersizDeğer = None

// Bu örnekte, match..with "Some" ve "None"la eşleştirme yapıyor,
// ve ayrıca "Some" içerisindeki değeri de çıkarıyor.
let optionKalıpEşleştirme input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionKalıpEşleştirme geçerliDeğer
optionKalıpEşleştirme geçersizDeğer

// ------ Yazdırma ------
// printf/printfn fonksiyonları C#'taki 
// Console.Write/WriteLine fonksiyonlarına benzer.
printfn "Bir tamsayı %i, bir ondalık %f, bir boolean %b yazdırma" 1 2.0 true
printfn "Bir string %s, ve jenerik bir tip %A" "merhaba" [1; 2; 3; 4]

// sprintf/sprintfn fonksiyonları ise veriyi string'e
// çevirmek içindir, C#'taki String.Format gibi.

// ================================================
// Fonksiyonlar hakkında dahası
// ================================================

// F# gerçek bir fonksiyonel dildir. Fonksiyonlar birinci
// sınıf varlıklardır ve güçlü yapılar oluşturmak için
// birleştirilebilirler.

// Modüller fonksiyonları gruplamak için kullanılır.
// Her bir modül için girinti gerekir.
module FonksiyonOrnekleri =

    // Temel bir ekleme fonksiyonu tanımla
    let topla x y = x + y

    // Bir fonksiyonun temel kullanımı
    let a = topla 1 2
    printfn "1 + 2 = %i" a

    // Parametreleri kaynaklamak için parçalı uygulama
    let kırkİkiEkle = topla 42
    let b = kırkİkiEkle 1
    printfn "42 + 1 = %i" b

    // Fonksiyonları birleştirmek için kompozisyon
    let birEkle = topla 1
    let ikiEkle = topla 2
    let üçEkle = birEkle >> ikiEkle
    let c = üçEkle 7
    printfn "3 + 7 = %i" c

    // Yüksek dereceli fonksiyonlar
    [1..10] |> List.map üçEkle |> printfn "yeni liste: %A"

    // Fonksiyonlar listesi ve dahası
    let altıEkle = [birEkle; ikiEkle; üçEkle] |> List.reduce (>>)
    let d = altıEkle 7
    printfn "1 + 2 + 3 + 7 = %i" d

// ================================================
// Listeler ve kolleksiyonlar
// ================================================

// Üç çesit sıralı fonksiyon vardır:
// * Listeler en temel değiştirilemez kolleksiyonlardır.
// * Diziler değiştirilebilir ve gerektiğinde daha verimlidirler.
// * Seriler tembel (lazy evaluation) ve sonsuzdurlar (Enumeratörler gibi). 
// 
// Değiştirilmez map'ler ve kümeler ve bütün .NET kolleksiyonları
// diğer kolleksiyon türleridir.

module ListeÖrnekleri =

    // listeler köşeli parantez kullanır
    let liste1 = ["a"; "b"]
    let liste2 = "c" :: liste1    // :: başa eleman ekler
    let liste3 = liste1 @ liste2   // @ listeleri birbirine ekler

    // Liste comprehension'ları (jeneratörler olarak da bilinir)
    let kareler = [for i in 1..10 do yield i * i]

    // asal sayı jeneratörü
    let rec elek = function
        | (p::xler) -> p :: elek [ for x in xler do if x % p > 0 then yield x ]
        | []      -> []
    let asallar = elek [2..50]
    printfn "%A" asallar

    // Listelerle kalıp eşleştirme
    let listeEşleyici liste =
        match liste with
        | [] -> printfn "liste boş"
        | [birinci] -> printfn "listede sadece bir eleman var: %A " birinci
        | [birinci; ikinci] -> printfn "liste: %A ve %A" birinci ikinci
        | _ -> printfn "listede ikiden fazla eleman var"

    listeEşleyici [1; 2; 3; 4]
    listeEşleyici [1; 2]
    listeEşleyici [1]
    listeEşleyici []

    // Listeleri kullanarak recursion
    let rec ekle liste =
        match liste with
        | [] -> 0
        | x::xler -> x + ekle xler
    ekle [1..10]

    // -----------------------------------------
    // Standart kütüphane fonksiyonları
    // -----------------------------------------

    // map
    let üçEkle x = x + 3
    [1..10] |> List.map üçEkle

    // filter
    let çift x = x % 2 = 0
    [1..10] |> List.filter çift

    // ve dahası -- dökümantasyona bakınız

module DiziÖrnekleri =

    // Diziler köşeli parantezle birlikte çubuk karakterini kullanır
    let dizi1 = [| "a"; "b" |]
    let birinci = dizi1.[0]        // nokta kullanarak indeks erişimi

    // Diziler için kalıp eşleştirme listlerle aynıdır
    let diziEşleştirici liste =
        match liste with
        | [| |] -> printfn "dizi boş"
        | [| birinci |] -> printfn "dizide sadece bir eleman var: %A " birinci
        | [| birinci; ikinci |] -> printfn "dizi: %A ve %A" birinci ikinci
        | _ -> printfn "dizide ikiden fazla eleman var"

    diziEşleştirici [| 1; 2; 3; 4 |]

    // Listede olduğu gibi kütüphane fonksiyonları

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "değer: %i. ")


module SeriÖrnekleri =

    // seriler kıvrık parantez kullanır
    let seri1 = seq { yield "a"; yield "b" }

    // seriler yield'ı kullanabilir
    // ve alt seriler barındırabilir
    let garip = seq {
        // "yield" bir eleman ekliyor
        yield 1; yield 2;

        // "yield!" bütün bir alt seriyi ekliyor
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // test
    garip |> Seq.toList


    // Seriler "unfold" kullanılarak oluşturulabilir
    // Fibonacci serisi örneği
    let fib = Seq.unfold (fun (birinci,ikinci) ->
        Some(birinci + ikinci, (ikinci, birinci + ikinci))) (0,1)

    // test
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "ilk 10 fibonacci sayısı: %A" fib10


// ================================================
// Veri Tipleri
// ================================================

module VeriTipiÖrnekleri =

    // Bütün veriler varsayılan olarak değiştirilemezdir.

    // -- Tuple oluşturmak için virgül kullan
    let ikiliTuple = 1, 2
    let üçlüTuple = "a", 2, true
    
    // Tuple'lar çabuk ve kolay anonim tiplerdir.
    // paketi açmak için kalıp eşleştirme kullan
    let x, y = ikiliTuple  // x = 1, y = 2

    // ------------------------------------
    // Record tipi isimlendirilmiş alanlara sahiptir
    // ------------------------------------

    // "type" ile kıvrık parantezleri record tipi oluşturmak için kullan
    type Kişi = {Ad:string; Soyad:string}

    // "let" ile kıvrık parantezi record tipi oluşturmak için kullan
    let kişi1 = {Ad="Falanca"; Soyad="Kişi"}

    // paketi açmak için kalıp eşleştirme kullan
    let {Ad = Ad} = kişi1    // birinci="John"

    // ------------------------------------
    // Union tipleri (değişkenler olarak da bilinir) birden fazla
    // seçeneğe sahiptir. Belli bir zamanda sadece bir tanesi geçerlidir.
    // ------------------------------------

    // "type" ile çubuk karakterini union tipi tanımlamak için kullan
    type Sıcaklık =
        | Santigrat of float
        | Fahrenhayt of float

    // Seçeneklerden birini kullan
    let derece1 = Fahrenhayt 98.6
    let derece2 = Santigrat 37.0

    // Paketi açmak için bütün seçenekler üzerinde kalıp eşleştirme kullan
    let dereceYazdır = function
       | Santigrat t -> printfn "%f C" t
       | Fahrenhayt t -> printfn "%f F" t

    dereceYazdır derece1
    dereceYazdır derece2

    // ------------------------------------
    // Yinelgen (Recursive) tipler
    // ------------------------------------

    // Tipler alt sınıflar oluşturmadan karmaşık şekillerde
    // yinelgen olarak birleştirilebilirler.
    type Çalışan =
      | İşçi of Kişi
      | Yönetici of Çalışan list

    let falancaKişi = {Ad="Falanca"; Soyad="Kişi"}
    let işçi = İşçi falancaKişi

    // ------------------------------------
    // Tipleri Kullanarak Modelleme
    // ------------------------------------

    // Union tipleri bayrak kullanmadan durum modelleme için harikadır.
    type EpostaAdresi =
        | GeçerliEpostaAdresi of string
        | GeçersizEpostaAdresi of string

    let epostaGöndermeyiDene eposta =
        match eposta with                     // kalıp eşleştirme kullan
        | GeçerliEpostaAdresi adres -> ()     // gönder
        | GeçersizEpostaAdresi adres -> ()    // gönderme

    // Union tiplerin record tiplerle birleşimi
    // domain driven design için iyi bir temel oluşturur.
    // Domain'i yansıtan yüzlerce ufak tip oluşturabilirsiniz.

    type Ürün = { ÜrünKodu: string; Miktar: int }
    type Ödeme = Ödeme of float
    type AktifSepetVerisi = { ÖdenmemişÜrünler: Ürün list }
    type ÖdenmişSepetVerisi = { ÖdenmişÜrünler: Ürün list; Ödeme: Ödeme}

    type AlışverişSepeti =
        | BosSepet  // veri yok
        | AktifSepet of AktifSepetVerisi
        | ÖdenmişSepet of ÖdenmişSepetVerisi

    // ------------------------------------
    // Tipler için içgüdüsel davranış
    // ------------------------------------

    // Çekirdek tipler kendinden çok kullanışlı özelliklere sahiptir
    // Ek kodlama gerektirmez
    // * Değişmezlik
    // * Debug ederken yazdırma
    // * Eşitlik ve kıyaslama
    // * Serialization

    // %A kullanarak yazdırma
    printfn "ikiliTuple=%A,\nKişi=%A,\Sıcaklık=%A,\nÇalışan=%A"
             ikiliTuple kişi1 derece1 işçi

    // Eşitlik ve kıyaslama içgüdüseldir.
    // İskambil kartlarıyla bir örnek
    type Simge = Sinek | Karo | Maça | Kupa
    type Sıra = İki | Üç | Dört | Beş | Altı | Yedi | Sekiz
                | Dokuz | On | Bacak | Kız | Papaz | As

    let el = [ Sinek, As; Kupa, Üç; Kupa, As;
                 Maça, Bacak; Karo, İki; Karo, As ]

    // Sıralama
    List.sort el |> printfn "artarak dizilen el: %A"
    List.max el |> printfn "en yüksek kart: %A"
    List.min el |> printfn "en düşük kart: %A"


// ================================================
// Aktif Kalıplar
// ================================================

module AktifKalıpÖrnekleri =

    // F# "aktif kalıplar" denen bir kalıp eşleştirmeye sahiptir.
    // Kalıplar dinamik bir şekilde tespit edilip eşleştirilebilir.

    // Aktif kalıplar için söz dizimi (| ... |) şeklindedir

    // Örneğin, karakter tiplerini eşleyen bir "aktif" kalıp tanımlayın...
    let (|Rakam|Harf|Boşluk|Diğer|) karakter =
       if System.Char.IsDigit(karakter) then Rakam
       else if System.Char.IsLetter(karakter) then Harf
       else if System.Char.IsWhiteSpace(karakter) then Boşluk
       else Diğer

    // ... daha sonra eşleme mantığı çok daha net yapmak için bunu kullanın
    let karakterYazdır karakter =
      match karakter with
      | Rakam -> printfn "%c bir rakamdır" karakter
      | Harf -> printfn "%c bir harftir" karakter
      | Boşluk -> printfn "%c bir boşluktur" karakter
      | _ -> printfn "%c başka bir şeydir" karakter

    // Bir liste yazdırma
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter karakterYazdır

    // -----------------------------------
    // Aktif Kalıpları Kullanarak FizzBuzz
    // -----------------------------------

    // Parçalı eşleşen kalıplar da oluşturabilirsiniz
    // Tanımda alt çizgi karakterini kullanın ve eşleşince Some döndürün.
    let (|ÜçünKatı|_|) i = if i % 3 = 0 then Some ÜçünKatı else None
    let (|BeşinKatı|_|) i = if i % 5 = 0 then Some BeşinKatı else None

    // Ana fonksiyon
    let fizzBuzz i =
      match i with
      | ÜçünKatı & BeşinKatı -> printf "FizzBuzz, "
      | ÜçünKatı -> printf "Fizz, "
      | BeşinKatı -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// Sadelik
// ================================================

module AlgoritmaÖrnekleri =

    // F#'ın sinyal/gürültü oranı yüksektir, dolayısıyla
    // kod algoritmayla hemen hemen aynı görünür.

    // ------ Örnek: karelerToplami fonksiyonunu tanımla ------
    let karelerToplamı n =
       [1..n]                 // 1) 1'den n'e kadar bütün sayıları al
       |> List.map kare       // 2) hepsinin karesini al
       |> List.sum            // 3) sonuçları topla

    // test
    karelerToplamı 100 |> printfn "kareler toplamı = %A"

    // ------ Örnek: bir sıralama fonksiyonu tanımla ------
    let rec sırala liste =
       match liste with
       // Liste boşsa
       | [] ->
            []                              // boş listeyi döndür
       // Liste boş değilse
       | ilkEleman::diğerElemanlar ->       // İlk elemanı al
            let küçükElemanlar =            // Daha küçük elemanları 
                diğerElemanlar              // diğerlerinden ayır
                |> List.filter (fun e -> e < ilkEleman)
                |> sırala                   // ve sırala
            let büyükElemanlar =            // Daha büyük elemanları
                diğerElemanlar              // diğerlerinden ayır
                |> List.filter (fun e -> e >= ilkEleman)
                |> sırala                   // ve sırala
            // 3 parçayı birbirine ekle ve listeyi döndür
            List.concat [küçükElemanlar; [ilkEleman]; büyükElemanlar]

    // test
    sırala [1; 5; 23; 18; 9; 1; 3] |> printfn "Sırala = %A"

// ================================================
// Eşzamansız kod
// ================================================

module EşzamansızÖrneği =

    // F# "pyramid of doom" durumuyla karşılaştırmayacak şekilde
    // içgüdüsel eşzamansız özelliklere sahiptir.
    //
    // Bir sonraki örnek bir web sayfasını paralel bir şekilde indirir.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // İçeriği eşzamansız bir şekilde getir
    let eşzamansızUrlGetir url =
        async {   // "async" anahtar kelimesi ve kıvrık parantez
                  // "async (eşzamansız)" nesneyi oluşturur
            let istek = WebRequest.Create(Uri(url))
            use! cevap = istek.AsyncGetResponse()
                // use! eşzamansız atamadır
            use akış = cevap.GetResponseStream()
                // "use" kullanılan bloğun dışına çıkınca
                // close()'u otomatik olarak tetikler
            use okuyucu = new IO.StreamReader(akış)
            let html = okuyucu.ReadToEnd()
            printfn "İndirme tamamlandı: %s" url
            }

    // İndirmek için bir web sitesi listesi
    let siteler = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // İndir
    siteler
    |> List.map eşzamansızUrlGetir  // eşzamansız görevlerden oluşan bir liste yap
    |> Async.Parallel               // bu görevleri paralel çalışacak şekilde ayarla
    |> Async.RunSynchronously       // başlat

// ================================================
// .NET uyumluluğu
// ================================================

module NetUyumlulukÖrnekleri =

    // F#, C#'ın yapabildiği hemen herşeyi yapabilir,
    // ve .NET ve Mono kütüphaneleriyle tereyağından kıl çeker gibi çalışır.

    // ------- var olan kütüphane fonksiyonları ile çalışma -------

    let (i1başarılı, i1) = System.Int32.TryParse("123");
    if i1başarılı then printfn "%i olarak dönüştürüldü" i1 else printfn "dönüştürme başarısız"

    // ------- Arayüzleri yol üstünde tanımlayın! -------

    // IDisposable'ı sağlayan yeni bir nesne oluştur
    let kaynakOluştur isim =
       { new System.IDisposable
         with member this.Dispose() = printfn "%s atıldı" isim }

    let kaynakKullanVeAt =
        use r1 = kaynakOluştur "birinci kaynak"
        printfn "birinci kaynağı kullanıyor"
        for i in [1..3] do
            let kaynakİsmi = sprintf "\tiç kaynak %d" i
            use geçici = kaynakOluştur kaynakİsmi
            printfn "\t%s ile bir şey yap" kaynakİsmi
        use r2 = kaynakOluştur "ikinci kaynak"
        printfn "ikinci kaynağı kullanıyor"
        printfn "bitti."

    // ------- Nesne yönelimli kod -------

    // F# aynı zamanda tam bir nesne yönelimli dildir.
    // Sınıfları, kalıtımı ve sanal metotları destekler.

    // Genel tipli bir arayüz
    type IEnumerator<'a> =
        abstract member Şimdiki : 'a
        abstract SonrakineGeç : unit -> bool

    // Sanal metotları olan soyut temel sınıflar
    [<AbstractClass>]
    type Şekil() =
        // sadece okunabilir özellikler
        abstract member Genişlik : int with get
        abstract member Yükseklik : int with get
        // sanal olmayan metot
        member this.ÇevreleyenAlan = this.Yükseklik * this.Genişlik
        // temel uygulamasıyla bir sanal metot
        abstract member Yazdır : unit -> unit
        default this.Yazdır () = printfn "Ben bir şekil (önümden çekil!)"

    // Somut bir sınıfın soyut sınıftan kalıtımı
    type Dikdörtgen(x:int, y:int) =
        inherit Şekil()
        override this.Genişlik = x
        override this.Yükseklik = y
        override this.Yazdır ()  = printfn "Ben bir dikdörtgenim"

    // test
    let r = Dikdörtgen(2, 3)
    printfn "Genişlik: %i" r.Genişlik
    printfn "Çevreleyen Alan: %i" r.ÇevreleyenAlan
    r.Yazdır()

    // ------- ekleme metotları -------

    // C#'ta olduğu gibi F# da var olan sınıfları ekleme metotları ile genişletebilir.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // test
    let s = "Ahmet"
    printfn "'%s' 'A' ile başlar = %A" s s.StartsWithA

    // ------- olaylar -------

    type Butonum() =
        let tıklamaOlayı = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = tıklamaOlayı.Publish

        member this.DenemeOlayı(arg) =
            tıklamaOlayı.Trigger(this, arg)

    // test
    let butonum = new Butonum()
    butonum.OnClick.Add(fun (sender, arg) ->
            printfn "arg=%O ile beraber bir tıklama olayı" arg)

    butonum.DenemeOlayı("Merhaba Dünya!")

```

## Daha fazla bilgi

F# hakkında daha fazla demo için [Try F#](http://www.tryfsharp.org/Learn) sitesine gidin, veya benim (yazarın) [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/) serimi okuyun.

F# hakkında daha fazla bilgi için: [fsharp.org](http://fsharp.org/).
