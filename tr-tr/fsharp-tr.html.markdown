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
let myString = "hello"           // tip bilgisi olmamasına dikkat

// ------ Listeler ------
let twoToFive = [2; 3; 4; 5]        // Köşeli parantezler listeleri oluşturur,
                                 // değerler ise noktalı virgülle ayrılır.
let oneToFive = 1 :: twoToFive   // :: yeni birinci elemanı olan bir liste oluşturur.
// Sonuç: [1; 2; 3; 4; 5]
let zeroToFive = [0; 1] @ twoToFive   // @ iki listeyi birbirine ekler.

// ÖNEMLİ: virgüller hiçbir zaman ayraç olarak kullanılmaz, sadece noktalı virgüller!

// ------ Fonksiyonlar ------
// "let" anahtar kelimesi isimlendirilmiş fonksiyonları da tanımlar.
let square x = x * x          // Parantez kullanılmadığına dikkat.
square 3                      // Şimdi fonksiyonu uygula. Yine parantez yok.

let add x y = x + y           // add (x,y) kullanmayın! Bu tamamen başka bir anlama geliyor.
add 2 3                       // Şimdi fonksiyonu uygula.

// çok satırlı bir fonksiyon tanımlamak için sadece girinti kullan. Noktalıı virgül gerekmez.
let evens list =
   let isEven x = x % 2 = 0     // "isEven"ı alt fonksiyon olarak tanımla
   List.filter isEven list    // List.filter boolean bir fonksiyon ve
                              // üzerinde çalışılacak bir liste parametrelerinden oluşan
                              // bir kütüphane fonksiyonu
                              
evens oneToFive               // Şimdi fonksiyonu uygula.

// Parantezleri önceliği netleştirmek için kullanabilirsin. Bu örnekte
// "map"i önce iki argümanla uygula, sonra sonuç üzerinde "sum" uygula.
// Parantezler olmasaydı, "List.map" List.sum'ın ilk argümanı olurdu.
let sumOfSquaresTo100 =
   List.sum ( List.map square [1..100] )

// Bir operasyonun sonucunu bir sonrakine "|>" kullanarak besleyebilirsin.
// Veri beslemek F#'ta UNIX'te olduğu gibi yaygındır..

// Burada sumOfSquares fonksiyonunun veri beslemeyle yazılmış hali var:
let sumOfSquaresTo100piped =
   [1..100] |> List.map square |> List.sum  // "square" önceden tanımlanmıştı

// Lambda'ları (anonim fonksiyonları) "fun" anahtar kelimesiyle tanımlayabilirsin
let sumOfSquaresTo100withFun =
   [1..100] |> List.map (fun x -> x * x) |> List.sum

// F#'ta "return" anahtar kelimesi yoktur. Bir fonksiyon
// her zaman son kullanılan ifadeyi döndürür.

// ------ Kalıp eşleştirme ------
// Match..with.. çok güçlü bir case/switch türevidir.
let simplePatternMatch =
   let x = "a"
   match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"   // alt çizgi bütün kalıplarla eşleşir

// F# varsayılan olarak null'lara izin vermez -- Option tipini kullanıp
// kalıp eşleştirme yapmalısın.
// Some(..) ve None, Nullable tipler gibidir.
let validValue = Some(99)
let invalidValue = None

// Bu örnekte, match..with "Some" ve "None"la eşleştirme yapıyor,
// ve ayrıca "Some" içerisindeki değeri de çıkarıyor.
let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// ------ Yazdırma ------
// printf/printfn fonksiyonları C#'taki 
// Console.Write/WriteLine fonksiyonlarına benzer.
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1; 2; 3; 4]

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
module FunctionExamples =

    // Temel bir ekleme fonksiyonu tanımla
    let add x y = x + y

    // Bir fonksiyonun temel kullanımı
    let a = add 1 2
    printfn "1 + 2 = %i" a

    // Parametreleri kaynaklamak için parçalı uygulama
    let add42 = add 42
    let b = add42 1
    printfn "42 + 1 = %i" b

    // Fonksiyonları birleştirmek için kompozisyon
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3 + 7 = %i" c

    // Yüksek dereceli fonksiyonlar
    [1..10] |> List.map add3 |> printfn "new list is %A"

    // Fonksiyonlar listesi ve dahası
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
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
    let list1 = ["a"; "b"]
    let list2 = "c" :: list1    // :: başa eleman ekler
    let list3 = list1 @ list2   // @ listeleri birbirine ekler

    // Liste comprehension'ları (jeneratörler olarak da bilinir)
    let squares = [for i in 1..10 do yield i * i]

    // asal sayı jeneratörü
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primes = sieve [2..50]
    printfn "%A" primes

    // Listelerle kalıp eşleştirme
    let listMatcher aList =
        match aList with
        | [] -> printfn "the list is empty"
        | [first] -> printfn "the list has one element %A " first
        | [first; second] -> printfn "list is %A and %A" first second
        | _ -> printfn "the list has more than two elements"

    listMatcher [1; 2; 3; 4]
    listMatcher [1; 2]
    listMatcher [1]
    listMatcher []

    // Listeleri kullanarak recursion
    let rec sum aList =
        match aList with
        | [] -> 0
        | x::xs -> x + sum xs
    sum [1..10]

    // -----------------------------------------
    // Standart kütüphane fonksiyonları
    // -----------------------------------------

    // map
    let add3 x = x + 3
    [1..10] |> List.map add3

    // filter
    let even x = x % 2 = 0
    [1..10] |> List.filter even

    // ve dahası -- dökümantasyonu bakınız

module ArrayExamples =

    // Diziler köşeli parantezle birlikte çubuk karakterini kullanır
    let array1 = [| "a"; "b" |]
    let first = array1.[0]        // nokta kullanarak indeks erişimi

    // Diziler için kalıp eşleştirme listlerle aynıdır
    let arrayMatcher aList =
        match aList with
        | [| |] -> printfn "the array is empty"
        | [| first |] -> printfn "the array has one element %A " first
        | [| first; second |] -> printfn "array is %A and %A" first second
        | _ -> printfn "the array has more than two elements"

    arrayMatcher [| 1; 2; 3; 4 |]

    // Listede olduğu gibi kütüphane fonksiyonları

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "value is %i. ")


module SequenceExamples =

    // seriler kıvrık parantez kullanır
    let seq1 = seq { yield "a"; yield "b" }

    // seriler yield'ı kullanabilir
    // ve alt seriler barındırabilir
    let strange = seq {
        // "yield" bir eleman ekliyor
        yield 1; yield 2;

        // "yield!" bütün bir alt seriyi ekliyor
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // test
    strange |> Seq.toList


    // Seriler "unfold" kullanılarak oluşturulabilir
    // Fibonacci serisi örneği
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // test
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// Veri Tipleri
// ================================================

module DataTypeExamples =

    // Bütün veriler varsayılan olarak değiştirilemezdir.

    // Tuple'lar çabuk ve kolay anonim tiplerdir.
    // -- Tuple oluşturmak için virgül kullan
    let twoTuple = 1, 2
    let threeTuple = "a", 2, true

    // paketi açmak için kalıp eşleştirme kullan
    let x, y = twoTuple  // x = 1, y = 2

    // ------------------------------------
    // Record tipi isimlendirilmiş alanlara sahiptir
    // ------------------------------------

    // "type" ile kıvrık parantezleri record tipi oluşturmak için kullan
    type Person = {First:string; Last:string}

    // "let" ile kıvrık parantezi record tipi oluşturmak için kullan
    let person1 = {First="John"; Last="Doe"}

    // paketi açmak için kalıp eşleştirme kullan
    let {First = first} = person1    // first="John"

    // ------------------------------------
    // Union tipleri (değişkenler olarak da bilinir) birden fazla
	// seçeneğe sahiptir. Belli bir zamanda sadece bir tanesi geçerlidir.
    // ------------------------------------

    // "type" ile çubuk karakterini union tipi tanımlamak için kullan
    type Temp =
        | DegreesC of float
        | DegreesF of float

    // Seçeneklerden birini kullan
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // Paketi açmak için bütün seçenekler üzerinde kalıp eşleştirme kullan
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t

    printTemp temp1
    printTemp temp2

    // ------------------------------------
    // Yinelgen (Recursive) tipler
    // ------------------------------------

	// Tipler alt sınıflar oluşturmadan karmaşık şekillerde
	// yinelgen olarak birleştirilebilirler.
    type Employee =
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John"; Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // Tipleri Kullanarak Modelleme
    // ------------------------------------

    // Union tipleri bayrak kullanmadan durum modelleme için harikadır.
    type EmailAddress =
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // kalıp eşleştirme kullan
        | ValidEmailAddress address -> ()   // gönder
        | InvalidEmailAddress address -> () // gönderme

	// Union tiplerin record tiplerle birleşimi
	// domain driven design için iyi bir temel oluşturur.
    // Domain'i yansıtan yüzlerce ufak tip oluşturabilirsiniz.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // veri yok
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

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
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // Eşitlik ve kıyaslama içgüdüseldir.
    // İskambil kartlarıyla bir örnek
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club, Ace; Heart, Three; Heart, Ace;
                 Spade, Jack; Diamond, Two; Diamond, Ace ]

    // Sıralama
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"


// ================================================
// Aktif Kalıplar
// ================================================

module ActivePatternExamples =

    // F# "aktif kalıplar" denen bir kalıp eşleştirmeye sahiptir.
    // Kalıplar dinamik bir şekilde tespit edilip eşleştirilebilir.

    // Aktif kalıplar için söz dizimi (| ... |) şeklindedir

    // Örneğin, karakter tiplerini eşleyen bir "aktif" kalıp tanımlayın...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       else if System.Char.IsLetter(ch) then Letter
       else if System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ... daha sonra eşleme mantığı çok daha net yapmak için bunu kullanın
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // Bir liste yazdırma
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter printChar

    // -----------------------------------
    // Aktif Kalıpları Kullanarak FizzBuzz
    // -----------------------------------

	// Parçalı eşleşen kalıplar da oluşturabilirsiniz
    // Tanımda alt çizgi karakterini kullanın ve eşleşince Some döndürün.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // Ana fonksiyon
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// Sadelik
// ================================================

module AlgorithmExamples =

    // F#'ın sinyal/gürültü oranı yüksektir, dolayısıyla
    // kod algoritmayla hemen hemen aynı görünür.

    // ------ Örnek: sumOfSquares fonksiyonunu tanımla ------
    let sumOfSquares n =
       [1..n]              // 1) 1'den n'e kadar bütün sayıları al
       |> List.map square  // 2) hepsinin karesini al
       |> List.sum         // 3) sonuçları topla

    // test
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ Örnek: bir sıralama fonksiyonu tanımla ------
    let rec sort list =
       match list with
       // Liste boşsa
       | [] ->
            []                            // boş listeyi döndür
       // Liste boş değilse
       | firstElem::otherElements ->      // İlk elemanı al
            let smallerElements =         // Daha küçük elemanları 
                otherElements             // diğerlerinden ayır
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // ve sırala
            let largerElements =          // Daha büyük elemanları
                otherElements             // diğerlerinden ayır
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // ve sırala
            // 3 parçayı birbirine ekle ve listeyi döndür
            List.concat [smallerElements; [firstElem]; largerElements]

    // test
    sort [1; 5; 23; 18; 9; 1; 3] |> printfn "Sorted = %A"

// ================================================
// Eşzamanlı olmayan kod
// ================================================

module AsyncExample =

    // F# "pyramid of doom" durumuyla karşılaştırmayacak şekilde
	// içgüdüsel eşzamanlı olmayan özelliklere sahiptir.
    //
    // Bir sonraki örnek bir web sayfasını paralel bir şekilde indirir.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // İçeriği eşzamanlı olmayan bir şekilde getir
    let fetchUrlAsync url =
        async {   // "async" anahtar kelimesi ve kıvrık parantez
                  // "async (eşzamanlı olmayan)" nesneyi oluşturur
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use! eşzamanlı olmayan atamadır
            use stream = resp.GetResponseStream()
                // "use" kullanılan bloğun dışına çıkınca
                // close()'u otomatik olarak tetikler
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
            }

    // İndirmek için bir web sitesi listesi
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // İndir
    sites
    |> List.map fetchUrlAsync  // async görevlerden oluşan bir liste yap
    |> Async.Parallel          // bu görevleri paralel çalışacak şekilde ayarla
    |> Async.RunSynchronously  // başlat

// ================================================
// .NET uyumluluğu
// ================================================

module NetCompatibilityExamples =

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
        use r1 = makeResource "first resource"
        printfn "using first resource"
        for i in [1..3] do
            let resourceName = sprintf "\tinner resource %d" i
            use temp = makeResource resourceName
            printfn "\tdo something with %s" resourceName
        use r2 = makeResource "second resource"
        printfn "using second resource"
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
    myButton.OnClick.Add(fun (sender, arg) ->
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")

```

## Daha fazla bilgi

F# hakkında daha fazla demo için [Try F#](http://www.tryfsharp.org/Learn) sitesine gidin, veya benim (yazarın) [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/) serimi okuyun.

F# hakkında daha fazla bilgi için: [fsharp.org](http://fsharp.org/).
