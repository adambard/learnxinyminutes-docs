---
language: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Mustafa Zengin", "http://zengin.github.com/"]
filename: learnfsharp.fs
---

F# fonksiyonel ve nesne yönelimli, genel amaçlı bir programlama dilidir. Bedava ve açık kaynaklıdır ve Linux, Mac, Windows ve dahasında çalışır.

Hataları derleme zamanında yakalayan çok güçlü bir tip sistemine sahiptir, ancak tip çıkarımı yaptığından dinamik bir dil gibi görünür.

F#'ın söz dizimi C-stili dillerden farklıdır:

* Küme parantezi kod bloklarını ayırmak için kullanılmaz. Bunun yerine Python'da olduğu gibi girinti kullanılır.
* Parametreleri birbirinden ayırmak için virgül yerine boşluk karakteri kullanılır.

Aşağıdaki kodu denemek istiyorsanız, [tryfsharp.org](http://www.tryfsharp.org/Create)'a gidin be interaktif REPL'e kodu yapıştırın.

```csharp

// tek satır yorumlar ikili bölme işareti kullanılır
(* çok satırlı yorumlar (* . . . *) ikilisini kullanır

-çok satırlı yorumun sonu- *)

// ================================================
// Temel Söz Dizimi
// ================================================

// ------ "Değişkenler" (tam da değil) ------
// "let" anahtar kelimesi (değişmez) değer tanımlar
let myInt = 5
let myFloat = 3.14
let myString = "merhaba"           	// tip bilgisi olmamasına dikkat

// ------ Listeler ------
let ikidenBese = [2; 3; 4; 5]      	// Köşeli parantezler listeleri oluşturur,
									// değerler ise noktalı virgülle ayrılır.
let birdenBese = 1 :: ikidenBese   	// :: yeni birinci elemanı olan bir liste oluşturur.
// Sonuç: [1; 2; 3; 4; 5]
let sifirdanBese = [0; 1] @ ikidenBese	// @ iki listeyi birbirine ekler.

// ÖNEMLİ: virgüller hiçbir zaman ayraç olarak kullanılmaz, sadece noktalı virgüller!

// ------ Fonksiyonlar ------
// "let" anahtar kelimesi isimlendirilmiş fonksiyonları da tanımlar.
let kare x = x * x          // Parantez kullanılmadığına dikkat.
kare 3                      // Şimdi fonksiyonu uygula. Yine parantez yok.

let topla x y = x + y       // topla (x,y) kullanmayın! Bu tamamen başka bir anlama geliyor.
topla 2 3                   // Şimdi fonksiyonu uygula.

// çok satırlı bir fonksiyon tanımlamak için sadece girinti kullan. Noktalıı virgül gerekmez.
let ciftler liste =
   let ciftMi x = x % 2 = 0 	// "ciftMi"yi alt fonksiyon olarak tanımla
   List.filter ciftMi liste    	// List.filter boolean bir fonksiyon ve
								// üzerinde çalışılacak bir liste parametrelerinden oluşan
								// bir kütüphane fonksiyonu
                              
evens birdenBese               	// Şimdi fonksiyonu uygula.

// Parantezleri önceliği netleştirmek için kullanabilirsin. Bu örnekte
// "map"i önce iki argümanla uygula, sonra sonuç üzerinde "ekle" uygula.
// Parantezler olmasaydı, "List.map" List.ekle'ın ilk argümanı olurdu.
let yuzeKadarKarelerinToplami =
   List.ekle ( List.map kare [1..100] )

// Bir operasyonun sonucunu bir sonrakine "|>" kullanarak besleyebilirsin.
// Veri beslemek F#'ta UNIX'te olduğu gibi yaygındır..

// Burada karelerToplami fonksiyonunun veri beslemeyle yazılmış hali var:
let veriBeslemeyleYuzeKadarKarelerinToplami =
   [1..100] |> List.map kare |> List.ekle  // "kare" önceden tanımlanmıştı

// Lambda'ları (anonim fonksiyonları) "fun" anahtar kelimesiyle tanımlayabilirsin
let funlaYuzeKadarKarelerinToplami =
   [1..100] |> List.map (fun x -> x * x) |> List.ekle

// F#'ta "return" anahtar kelimesi yoktur. Bir fonksiyon
// her zaman son kullanılan ifadeyi döndürür.

// ------ Kalıp eşleştirme ------
// Match..with.. çok güçlü bir case/switch türevidir.
let basitKalipEslestirme =
   let x = "a"
   match x with
    | "a" -> printfn "x a'dır"
    | "b" -> printfn "x b'dir"
    | _ -> printfn "x başka bir şeydir"   // alt çizgi bütün kalıplarla eşleşir

// F# varsayılan olarak null'lara izin vermez -- Option tipini kullanıp
// kalıp eşleştirme yapmalısın.
// Some(..) ve None, Nullable tipler gibidir.
let gecerliDeger = Some(99)
let gecersizDeger = None

// Bu örnekte, match..with "Some" ve "None"la eşleştirme yapıyor,
// ve ayrıca "Some" içerisindeki değeri de çıkarıyor.
let optionKalipEslestirme input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionKalipEslestirme gecerliDeger
optionKalipEslestirme gecersizDeger

// ------ Yazdırma ------
// printf/printfn fonksiyonları C#'taki 
// Console.Write/WriteLine fonksiyonlarına benzer.
printfn "Bir int %i, bir ondalık %f, bir boolean %b yazdırma" 1 2.0 true
printfn "Bir string %s, ve jenerik bir tip %A" "hello" [1; 2; 3; 4]

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
    let kirkIkiEkle = topla 42
    let b = kirkIkiEkle 1
    printfn "42 + 1 = %i" b

    // Fonksiyonları birleştirmek için kompozisyon
    let birEkle = topla 1
    let ikiEkle = topla 2
    let ucEkle = birEkle >> ikiEkle
    let c = ucEkle 7
    printfn "3 + 7 = %i" c

    // Yüksek dereceli fonksiyonlar
    [1..10] |> List.map ucEkle |> printfn "yeni liste: %A"

    // Fonksiyonlar listesi ve dahası
    let altiEkle = [birEkle; ikiEkle; ucEkle] |> List.reduce (>>)
    let d = altiEkle 7
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

module ListExamples =

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
    let listMatcher liste =
        match liste with
        | [] -> printfn "liste boş"
        | [birinci] -> printfn "listede sadece bir eleman var: %A " birinci
        | [birinci; ikinci] -> printfn "liste: %A ve %A" birinci ikinci
        | _ -> printfn "listede ikiden fazla eleman var"

    listMatcher [1; 2; 3; 4]
    listMatcher [1; 2]
    listMatcher [1]
    listMatcher []

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
    let ucEkle x = x + 3
    [1..10] |> List.map ucEkle

    // filter
    let cift x = x % 2 = 0
    [1..10] |> List.filter cift

    // ve dahası -- dökümantasyona bakınız

module DiziOrnekleri =

    // Diziler köşeli parantezle birlikte çubuk karakterini kullanır
    let dizi1 = [| "a"; "b" |]
    let birinci = dizi1.[0]        // nokta kullanarak indeks erişimi

    // Diziler için kalıp eşleştirme listlerle aynıdır
    let diziEslestirici liste =
        match liste with
        | [| |] -> printfn "dizi boş"
        | [| birinci |] -> printfn "dizide sadece bir eleman var: %A " birinci
        | [| birinci; ikinci |] -> printfn "dizi: %A ve %A" birinci ikinci
        | _ -> printfn "dizide ikiden fazla eleman var"

    diziEslestirici [| 1; 2; 3; 4 |]

    // Listede olduğu gibi kütüphane fonksiyonları

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "değer: %i. ")


module SeriOrnekleri =

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

module VeriTipiOrnekleri =

    // Bütün veriler varsayılan olarak değiştirilemezdir.

    // Tuple'lar çabuk ve kolay anonim tiplerdir.
    // paketi açmak için kalıp eşleştirme kullan
    let x, y = ikiliTuple  // x = 1, y = 2

    // -- Tuple oluşturmak için virgül kullan
    let ikiliTuple = 1, 2
    let ucluTuple = "a", 2, true

    // ------------------------------------
    // Record tipi isimlendirilmiş alanlara sahiptir
    // ------------------------------------

    // "type" ile kıvrık parantezleri record tipi oluşturmak için kullan
    type Kisi = {birinci:string; Last:string}

    // "let" ile kıvrık parantezi record tipi oluşturmak için kullan
    let kisi1 = {birinci="John"; Last="Doe"}

    // paketi açmak için kalıp eşleştirme kullan
    let {birinci = birinci} = kisi1    // birinci="John"

    // ------------------------------------
    // Union tipleri (değişkenler olarak da bilinir) birden fazla
	// seçeneğe sahiptir. Belli bir zamanda sadece bir tanesi geçerlidir.
    // ------------------------------------

    // "type" ile çubuk karakterini union tipi tanımlamak için kullan
    type Derece =
        | Santigrat of float
        | Fahrenhayt of float

    // Seçeneklerden birini kullan
    let derece1 = Fahrenhayt 98.6
    let derece2 = Santigrat 37.0

    // Paketi açmak için bütün seçenekler üzerinde kalıp eşleştirme kullan
    let dereceYazdir = function
       | Santigrat t -> printfn "%f C" t
       | Fahrenhayt t -> printfn "%f F" t

    dereceYazdir derece1
    dereceYazdir derece2

    // ------------------------------------
    // Yinelgen (Recursive) tipler
    // ------------------------------------

	// Tipler alt sınıflar oluşturmadan karmaşık şekillerde
	// yinelgen olarak birleştirilebilirler.
    type Calisan =
      | Isci of Kisi
      | Yonetici of Calisan liste

    let falancaKisi = {birinci="John"; Last="Doe"}
    let isci = Isci falancaKisi

    // ------------------------------------
    // Tipleri Kullanarak Modelleme
    // ------------------------------------

    // Union tipleri bayrak kullanmadan durum modelleme için harikadır.
    type EpostaAdresi =
        | GecerliEpostaAdresi of string
        | GecersizEpostaAdresi of string

    let epostaGondermeyiDene eposta =
        match eposta with // kalıp eşleştirme kullan
        | GecerliEpostaAdresi adres -> ()   	// gönder
        | GecersizEpostaAdresi adres -> () 		// gönderme

	// Union tiplerin record tiplerle birleşimi
	// domain driven design için iyi bir temel oluşturur.
    // Domain'i yansıtan yüzlerce ufak tip oluşturabilirsiniz.

    type Urun = { UrunKodu: string; Miktar: int }
    type Odeme = Odeme of float
    type AktifSepetVerisi = { OdenmemisUrunler: Urun liste }
    type OndenmisSepetVerisi = { OdenmisUrunler: Urun liste; Odeme: Odeme}

    type AlisverisSepeti =
        | BosSepet  // veri yok
        | AktifSepet of AktifSepetVerisi
        | OdenmisSepet of OndenmisSepetVerisi

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
    printfn "ikiliTuple=%A,\nKisi=%A,\nDerece=%A,\nCalisan=%A"
             ikiliTuple kisi1 derece1 isci

    // Eşitlik ve kıyaslama içgüdüseldir.
    // İskambil kartlarıyla bir örnek
    type Simge = Sinek | Karo | Maca | Kupa
    type Sira = Iki | Uc | Dort | Bes | Alti | Yedi | Sekiz
                | Dokuz | On | Bacak | Kiz | Papaz | As

    let el = [ Sinek, As; Kupa, Uc; Kupa, As;
                 Maca, Bacak; Karo, Iki; Karo, As ]

    // Sıralama
    List.sirala el |> printfn "artarak dizilen el: %A"
    List.max el |> printfn "en yüksek kart: %A"
    List.min el |> printfn "en düşük kart: %A"


// ================================================
// Aktif Kalıplar
// ================================================

module AktifKalipOrnekleri =

    // F# "aktif kalıplar" denen bir kalıp eşleştirmeye sahiptir.
    // Kalıplar dinamik bir şekilde tespit edilip eşleştirilebilir.

    // Aktif kalıplar için söz dizimi (| ... |) şeklindedir

    // Örneğin, karakter tiplerini eşleyen bir "aktif" kalıp tanımlayın...
    let (|Rakam|Harf|Bosluk|Diger|) karakter =
       if System.Char.IsDigit(karakter) then Rakam
       else if System.Char.IsLetter(karakter) then Harf
       else if System.Char.IsWhiteSpace(karakter) then Bosluk
       else Diger

    // ... daha sonra eşleme mantığı çok daha net yapmak için bunu kullanın
    let karakterYazdir karakter =
      match karakter with
      | Rakam -> printfn "%c bir rakamdır" karakter
      | Harf -> printfn "%c bir harftir" karakter
      | Bosluk -> printfn "%c bir boşluktur" karakter
      | _ -> printfn "%c başka bir şeydir" karakter

    // Bir liste yazdırma
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter karakterYazdir

    // -----------------------------------
    // Aktif Kalıpları Kullanarak FizzBuzz
    // -----------------------------------

	// Parçalı eşleşen kalıplar da oluşturabilirsiniz
    // Tanımda alt çizgi karakterini kullanın ve eşleşince Some döndürün.
    let (|UcunKati|_|) i = if i % 3 = 0 then Some UcunKati else None
    let (|BesinKati|_|) i = if i % 5 = 0 then Some BesinKati else None

    // Ana fonksiyon
    let fizzBuzz i =
      match i with
      | UcunKati & BesinKati -> printf "FizzBuzz, "
      | UcunKati -> printf "Fizz, "
      | BesinKati -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// Sadelik
// ================================================

module AlgoritmaOrnekleri =

    // F#'ın sinyal/gürültü oranı yüksektir, dolayısıyla
    // kod algoritmayla hemen hemen aynı görünür.

    // ------ Örnek: karelerToplami fonksiyonunu tanımla ------
    let karelerToplami n =
       [1..n]              	// 1) 1'den n'e kadar bütün sayıları al
       |> List.map kare  	// 2) hepsinin karesini al
       |> List.ekle         // 3) sonuçları topla

    // test
    karelerToplami 100 |> printfn "kareler toplamı = %A"

    // ------ Örnek: bir sıralama fonksiyonu tanımla ------
    let rec sirala liste =
       match liste with
       // Liste boşsa
       | [] ->
            []                            	// boş listeyi döndür
       // Liste boş değilse
       | ilkEleman::digerElemanlar ->		// İlk elemanı al
            let kucukElemanlar =         	// Daha küçük elemanları 
                digerElemanlar             	// diğerlerinden ayır
                |> List.filter (fun e -> e < ilkEleman)
                |> sirala                   // ve sırala
            let buyukElemanlar =          	// Daha büyük elemanları
                digerElemanlar             	// diğerlerinden ayır
                |> List.filter (fun e -> e >= ilkEleman)
                |> sirala                   // ve sırala
            // 3 parçayı birbirine ekle ve listeyi döndür
            List.concat [kucukElemanlar; [ilkEleman]; buyukElemanlar]

    // test
    sirala [1; 5; 23; 18; 9; 1; 3] |> printfn "Sırala = %A"

// ================================================
// Eşzamansız kod
// ================================================

module EszamansizOrnegi =

    // F# "pyramid of doom" durumuyla karşılaştırmayacak şekilde
	// içgüdüsel eşzamansız özelliklere sahiptir.
    //
    // Bir sonraki örnek bir web sayfasını paralel bir şekilde indirir.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // İçeriği eşzamansız bir şekilde getir
    let eszamansizUrlGetir url =
        async {   // "async" anahtar kelimesi ve kıvrık parantez
                  // "async (eşzamansız)" nesneyi oluşturur
            let istek = WebRequest.Create(Uri(url))
            use! cevap = istek.AsyncGetResponse()
                // use! eşzamansız atamadır
            use akis = cevap.GetResponseStream()
                // "use" kullanılan bloğun dışına çıkınca
                // close()'u otomatik olarak tetikler
            use okuyucu = new IO.StreamReader(akis)
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
    |> List.map eszamansizUrlGetir  // eşzamansız görevlerden oluşan bir liste yap
    |> Async.Parallel          		// bu görevleri paralel çalışacak şekilde ayarla
    |> Async.RunSynchronously  		// başlat

// ================================================
// .NET uyumluluğu
// ================================================

module NetUyumlulukOrnekleri =

    // F#, C#'ın yapabildiği hemen herşeyi yapabilir,
    // ve .NET ve Mono kütüphaneleriyle tereyağından kıl çeker gibi çalışır.

    // ------- var olan kütüphane fonksiyonları ile çalışma -------

    let (i1success, i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- Arayüzleri yol üstünde tanımla! -------

    // IDisposable'ı sağlayan yeni bir nesne oluştur
    let makeResource name =
       { new System.IDisposable
         with member this.Dispose() = printfn "%s disposed" name }

    let useAndDisposeResources =
        use r1 = makeResource "birinci resource"
        printfn "using birinci resource"
        for i in [1..3] do
            let resourceName = sprintf "\tinner resource %d" i
            use temp = makeResource resourceName
            printfn "\tdo something with %s" resourceName
        use r2 = makeResource "ikinci resource"
        printfn "using ikinci resource"
        printfn "done."

    // ------- Nesne yönelimli kod -------

    // F# aynı zamanda tam bir nesne yönelimli dildir.
    // Sınıfları, kalıtımı ve sanal metotları destekler.

    // Genel ipli bir arayüz
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // Sanal metotları olan soyut temel sınıflar
    [<AbstractClass>]
    type Shape() =
        // sadece okunabilir özellikler
        abstract member Width : int with get
        abstract member Height : int with get
        // sanal olmayan metot
        member this.BoundingArea = this.Height * this.Width
        // temel uygulamasıyla bir sanal metot
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // Somut bir sınıfın soyut sınıftan kalıtımı
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    // test
    let r = Rectangle(2, 3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()

    // ------- ekleme metotları -------

    // C#'ta olduğu gibi F# da var olan sınıfları ekleme metotları ile genişletebilir.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // test
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA

    // ------- olaylar -------

    type MyButton() =
        let clickEvent = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = clickEvent.Publish

        member this.TestEvent(arg) =
            clickEvent.Trigger(this, arg)

    // test
    let myButton = new MyButton()
    myButton.OnClick.topla(fun (sender, arg) ->
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")

```

## Daha fazla bilgi

F# hakkında daha fazla demo için [Try F#](http://www.tryfsharp.org/Learn) sitesine gidin, veya benim (yazarın) [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/) serimi okuyun.

F# hakkında daha fazla bilgi için: [fsharp.org](http://fsharp.org/).
