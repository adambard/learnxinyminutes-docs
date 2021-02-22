---
language: kotlin
filename: kotlin-tr.kt
contributors:
        - ["Baha Can Aydın", "https://github.com/bahacan19"]
lang: tr-tr
---
Kotlin, JVM, Android ve tarayıcı için statik olarak yazılmış bir programlama dilidir.
Java %100 birlikte çalışabilir.
[Daha:](https://kotlinlang.org/)

```kotlin

// Tek satır yoruma almak için : //
/*
    Birkaç satırı yoruma almak için
*/

//  "package" anahtar kelimesi tıpkı Java'da olduğu gibidir.
package com.learnxinyminutes.kotlin

/*
Bir Kotlin programının başlama noktası (Java'da olduğu gibi) "com.learnxinyminutes.kotlin.main" metodudur.
Bu metoda komut satırından bir 'Array' gönderilebilir.
*/
fun main(args: Array<String>) {
    /*
    Bir değer tanımlamak için "var" ya da "val" anahtar kelimeleri kullanılıyor.
    "val" tanımlananlar tekrar atanamazken "var" tanımlananlar atanabilir.
    */
    val fooVal = 10 // fooVal değerini daha sonra tekrar atayamıyoruz
    var fooVar = 10
    fooVar = 20 // fooVar tekrar atanabilir.

    /*
    Çoğu zaman, Kotlin bir değişkenin tipini anlayabilir,
    bu yüzden her zaman belirtmeye gerek yoktur.
    Bir değişkenin tipini şöyle belirtebiliriz:
    */
    val foo: Int = 7

    /*
    String değerler Java'da olduğu gibi tanımlanır.
    */
    val fooString = "İşte String bu!"
    val barString = "Yeni satıra geçiyorum...?\nGeçtim!"
    val bazString = "Tab mı istedin?\tAl bakalım!"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Raw string, üçlü çift-tırnak sınırlandırılan String bloklarıdır.
    Tıpkı bir text editör gibi String tanımlamaya izin verir.
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Merhaba, dünya!")
}
"""
    println(fooRawString)

    /*
    String değerler, ($) işareti ile birtakım deyimler ve değerler içererbilir
    */
    val fooTemplateString = "$fooString değerinin ${fooString.length} adet karakteri vardır."
    println(fooTemplateString)

    /*
    Null atanabilen bir değişken nullable olarak tanımlanmalıdır.
    Bu, deişken tipinin sonuna ? eklenerek yapılabilir.
    Erişim ise '?.' operatörü ile yapılır.
    Bir değişken null ise, yerine kullaılacak alternatif bir değer belirtmek için
    '?:' operatörünü kullanırız.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    Metodlar "fun" anahtar kelimesi ile tanımlanır.
    Metod argümanları, Metod adından sonra  parantez içinde belirtilir.
    Metod argümanlarının opsiyonel olarak default (varsayılan) değerleri olabilir.
    Metodun dönüş tipi, gerekirse, metod parentezinden sonra ':' operatörü ile belirtilir.
    */
    fun hello(name: String = "dünya"): String {
        return "Merhaba, $name!"
    }
    println(hello("foo")) // => Merhaba, foo!
    println(hello(name = "bar")) // => Merhaba, bar!
    println(hello()) // => Merhaba, dünya!

    /*
    Bir metoda çokca argüman göndermek için 'vararg' anahtar kelimesi
    kullanılır.
    */
    fun varargExample(vararg names: Int) {
        println("${names.size}  adet arguman paslanmıştır")
    }
    varargExample() // => 0 adet arguman paslanmıştır
    varargExample(1) // => 1 adet arguman paslanmıştır
    varargExample(1, 2, 3) // => 3 adet arguman paslanmıştır

    /*
    Bir metod tek bir ifadeden oluşuyorsa
    süslü parantezler yerine '=' kullanılabilir.
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Eğer dönüş tipi anlaşılabiliyorsa ayrıca belirtmemize gerek yoktur.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Metodlar, metodları arguman ve dönüş tipi olarak alabilir
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)} // bu satırdaki !f.invoke(n) metodu !f(n) şeklinde sadeleştirilebilir.
    }


    // Bir metodu sadece '::' ön eki ile de arguman olarak çağırabiliriz
    println(not(::odd)(4)) // ==> true

    // Metodlar değişken gibi atanabilir.
    val notOdd = not(::odd)
    val notEven = not(::even)

    // Lambda ifadeleri arguman olarak paslanabilir.
    val notZero = not {n -> n == 0}
    /*
    Eğer bir lambda fonksiyonu sadece bir arguman alıyorsa,
    '->' ifadesi atlanabilir, 'it' ifadesi ile belirtilebilir.
    */
    val notPositive = not { it > 0} // not(n -> n > 0) ifadesi ile aynı

    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    /*
    * Diğer for döngüleri
    * */
    val myInt = 3
    for (i in 1..100) {  }  // kapalı aralık. 100 dahil.
    for (i in 1 until 100) {  } // 100 dahil değil
    for (x in 2..10 step 2) {  } // ikişer adımlı
    for (x in 10 downTo 1) {  } // Ondan geriye doğru. 1 dahil.
    if (myInt in 1..10) {  }



    /*
    Bir sınıf tanımlamak için 'class' anahtar kelimesi kullanılır.
    Kotlin'de bütün sınıflar varsayılan olarak 'final' tanımlanırlar.
    * */
    class ExampleClass(val x: Int) {

        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun yTimes(y: Int): Int {
            return x * y
        }
    }
    /*
    * Bir sınıfı türetilebilir yapmak için 'open' anahtar kelimesi kullanılır.
    * */
    open class A

    class B : A()


    /*
    Yeni bir instance oluşturmak için doğrudan constructor çağırılır.
    Kotlinde 'new' anahtar kelimesi yoktur.
    */
    val fooExampleClass = ExampleClass(7)
    // Bir sınıfa üye metodları . (nokta) ile çağırabiliriz.
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    'infix' ön eki ile tanımlanan metodlar
    alışılan metod çağrısını daha kolay bir söz dizimine dönüştürür.
    */
    println(fooExampleClass yTimes 4) // => 28

    /*
    Data class lar sadece veri tutan sınıflar için uygun bir çözümdür.
    Bu şekilde tanımlanan sınıfların "hashCode"/"equals" ve "toString" metodları
    otomatik olarak oluşur.
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // Data class ların copy metodları olur.
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Destructuring Declarations, bir objeyi çoklu değişkenler ile ifade etme yöntemidir.
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // bir 'for' döngüsü içinde 'Destructuring' :
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }

    val mapData = mapOf("a" to 1, "b" to 2)
    // Map.Entry de destructurable gösterilebilir.
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // 'with' metodu ile bir objeye bir lamda metodu uygulayabiliriz.
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableData = MutableDataClassExample(7, 4, 9)
    with (fooMutableData) {
        x -= 2
        y += 2
        z--
    }

    println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    'listOf' metodu ile bir liste oluşturulabilir.
    Oluşan liste immutable olacaktır, yani elaman eklenemez ve çıkarılamaz.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // Elemanlara indexleri ile erişilebilir.
    println(fooList[1]) // => b

    // Mutable bir liste ise 'mutableListOf' metodu ile oluşturabilir.
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Bir 'set' oluşturmak için 'setOf' metodunu kullanabiliriz.
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // 'mapOf' metodu ile 'map' oluşturabiliriz.
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Map değerlerine ulaşmak için :
    println(fooMap["a"]) // => 8

    /*
    Sequence, Kotlin dilinde lazy-hesaplanan collection ları temsil eder.
    Bunun için 'generateSequence' metodunu kullanabiliriz. Bu metod bir önceki değerden
    bir sonraki değeri hesaplamak için gerekli bir lamda metodunu arguman olarak alır.
    */
    val fooSequence = generateSequence(1, { it + 1 })

    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Örneğin fibonacci serisi oluşturabilen bir 'Sequence' oluşturmak için:
    fun fibonacciSequence(): Sequence<Long> {
        var a = 0L
        var b = 1L

        fun next(): Long {
            val result = a + b
            a = b
            b = result
            return a
        }

        return generateSequence(::next)
    }
    val y = fibonacciSequence().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]


    // Kotlin Collection lar ile çalışmak için higher-order metodlar sağlar.
    val z = (1..9)
            .map {it * 3} // her bir elamanı 3 ile çarp
            .filter {it < 20} // 20 den küçük değerleri ele
            .groupBy {it % 2 == 0} // ikiye tam bölünen ve bölünmeyen şeklinde grupla (Map)
            .mapKeys {if (it.key) "even" else "odd"} // oluşan map in boolen 'key' lerini String bir değere dönüştür.
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Bir 'for' döngüsü 'itearator' sağlayan her objeye uygulanabilir.
    for (c in "merhaba") {
        println(c)
    }

    // 'while' döngüsü diğer dillere benzer şekilde çalışır.
    var ctr = 0
    while (ctr < 5) {
        println(ctr)
        ctr++
    }
    do {
        println(ctr)
        ctr++
    } while (ctr < 10)

    /*
    'if' bir dönüş değeri olan deyim gibi de kullanılabilir.
    Bu sebepten Kotlin, Java'da bulunan '?:' ifadesi içermez.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // 'if-else if' yapıları için 'when' kullanılabilir.
    val i = 10
    when {
        i < 7                         ->    println("first block")
        fooString.startsWith("hello") ->    println("second block")
        else                          ->    println("else block")
    }

    // 'when' bir parametre ile de kullanılabilir.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // 'when' dönüş değeri olan bir metod gibi de davranabilir.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)


    /*
    Bir objenin tipini 'is' operatörü ile tayin edebiliriz.
    Eğer obje tip kontrolünü geçerse, cast etmeden doğrudan
    o tipteymiş gibi kullanılabilir.
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x otomatik olarak Boolean'a cast edilir.
            return x
        } else if (x is Int) {
            // x otomatik olarak Int tipine cast edilir.
            return x > 0
        } else if (x is String) {
            // x otomatik olarak String tipine cast edilir.
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(smartCastExample("Merhaba, dünya!")) // => true
    println(smartCastExample("")) // => false
    println(smartCastExample(5)) // => true
    println(smartCastExample(0)) // => false
    println(smartCastExample(true)) // => true

    // Smartcast 'when' bloğu ile de çalışır.
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    Extension lar, bir sınıfa fonksinolalite eklemenin bir yoludur.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Merhaba, dünya!".remove('a')) // => Merhb, düny!



    //Biraz detaylı Kotlin


    /*
     * Delegated Properties, bir değişken tanımlarken kullanılan birkaç standart yöntemler içerir.
     * https://kotlinlang.org/docs/reference/delegated-properties.html
     * En bilinen delegate property metodları: lazy(), observable()
     * */

    /*
     * Lazy, bir değişkeni ilk erişimde çalıştırılacak olan bir lambda ile tanımlama metodudur.
     * Sonraki erişimlerde değişkene atanan değer hatırlanır.
     * Lazy, synchronized bir delegation yöntemidir; değer sadece bir thread içinde hesaplanır,
     * tüm thread ler aynı değere erişir. Eğer senkronizasyon gerekli değilse, lazy metodu içine
     * LazyThreadSafetyMode.PUBLICATION paslanabilir.
     * */

    val lazyValue: String by lazy( {
        println("bi sn... hesaplıyorum....")
        "Selam!"
    })

    println(lazyValue)// bi sn... hesaplıyorum.... Selam!
    println(lazyValue) // Selam!
    /*
     * Observable, bir değişkende olabilecek yeniden atama değişikliklerini dinleme yöntemidir.
     * İki arguman alır; değişkenin ilk değeri, değiştiğinde çağrılan bir handler metodu. Handler
     * metodu değişken her değiştiğinde çağırılır.
     * */
    var myObservableName: String by Delegates.observable("<isim yok>") {
        prop, old, new ->
        println("$old -> $new")
    }
    myObservableName = "Baha" //<isim yok> -> Baha
    myObservableName = "Can"  //Baha -> Can


    /*
     * Eğer değişkenin yeniden atanmasını denetlemek isterek vetoable()
     * metodunu kullanabiliriz.
     * */

    var myVetoableName : String by Delegates.vetoable("<isim yok>"){
        property, oldValue, newValue ->
        if (newValue.length < 2) {
            println("Tek harfli isim kabul etmiyoruz!")
            false
        } else {
            println("$oldValue -> $newValue")
            true
        }
    }

    myVetoableName = "Baha" //<isim yok> -> Baha
    myVetoableName = "C"    //Tek harfli isim kabul etmiyoruz!
    println(myVetoableName) //Baha


    //singleton değişkene ulaşmak:
    println(ObjectExample.hello()) // => Merhaba
}

// Enum class lar Java'daki enum lara benzerdir.
enum class EnumExample {
    A, B, C
}

/*
'object' anahtar kelimesi ile singleton nesneler oluşturulabilir.
Bu şekilde tanımlanan sınıflardan yeni nesneler oluşturulamaz, sadece adı ile refere edilebilir.
*/
object ObjectExample {
    fun hello(): String {
        return "Merhaba"
    }
}

fun useObject() {
    ObjectExample.hello()
    val someRef: Any = ObjectExample
}

```

### İlerisi için:

* [Kotlin tutorials](https://kotlinlang.org/docs/tutorials/)
* [Try Kotlin in your browser](http://try.kotlinlang.org/)
* [A list of Kotlin resources](http://kotlin.link/)
* [Kotlin Koans in your IDE](https://kotlinlang.org/docs/tutorials/koans.html/)
