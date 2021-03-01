---
language: Rust
filename: learnrust-tr.fs
contributors:
    - ["Murat Koptur", "https://muratkoptur.com"]
    - ["P1start", "http://p1start.github.io/"]
lang: tr-tr
---

Rust, Mozilla Research tarafından geliştirilen bir programlama dilidir.
Rust, performans üzerindeki düşük seviyeli dillerin kontrolünü ve yüksek seviyeli dillerdeki
rahatlık ve güvenlik garantileriyle birleştirir.

Bu hedeflere herhangi bir garbage collector veya runtime olmadan ulaşır, bu nedenle
Rust dilinin C dili yerine kullanımını mümkün kılar.

Ayrıca Rust programlama dili fonksiyonel programlama konseptleri de içerdiğinden,
etkili kodlama yapmayı kolay kılmaktadır.

```rust
// Bu bir satır açıklamasıdır
// ve çok satıra genişletilebilir.

/// Dokümantasyon açıklamaları bu şekilde yazılır ve Markdown notasyonunu destekler.
/// # Örnekler
///
/// ```
/// let five = 5
/// ```

///////////////
// 1. Temeller //
///////////////

#[allow(dead_code)]
// Fonksiyonlar
// `i32` 32-bit işaretli tamsayı tipidir
fn add2(x: i32, y: i32) -> i32 {
    // Örtük return (noktalı virgül yok)
    x + y
}

#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
// Main fonksiyonu
fn main() {
    // Sayılar //

    // Immutable bindings - Sabit atama
    let x: i32 = 1;

    // Tam/kesirli sayı sonekleri
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // Tip çıkarımı
    // Çoğu zaman Rust derleyicisi bir değişkenin tipine çıkarım yapabildiğinden
    // açık olarak tip belirtmeye gerek yoktur.
    let implicit_x = 1;
    let implicit_f = 1.3;

    // Aritmetik
    let sum = x + y + 13;

    // Değiştirilebilir (mutable) atama
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // Karakter dizileri //
    let x: &str = "hello world!";

    // Çıktı
    println!("{} {}", f, x); // 1.3 hello world

    // heap-allocated karakter dizisi
    let s: String = "hello world".to_string();

    // string slice – Başka bir karakter dizisinin değiştirilemez görünümü
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // Vektörler/Diziler //

    // Sabit-büyüklü dizi
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // Dinamik dizi (vektör)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // Dilim – Başka bir dizi/vektörün değiştirilemez görünümü
    let slice: &[i32] = &vector;

    // Bir şeyi debug-stili yazdırmak için `{:?}` kullanılır
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Demetler (tuples) //

    // Demet, farklı tiplerdeki değerlerin sabit büyüklükteki bir kümesidir
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // Destructuring `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // İndeksleme
    println!("{}", x.1); // hello

    //////////////
    // 2. Tipler //
    //////////////

    // Struct
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // İsimsiz field içeren structlar ‘tuple struct’ olarak adlandırılır
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // C-benzeri enum
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // Enum with fields
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // Generics //

    struct Foo<T> { bar: T }

    // Bu yapı standart kütüphanede `Option` şeklinde tanımlıdır.
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // Methodlar //

    impl<T> Foo<T> {
        // Methodlar açıkça `self` parametresini alır
        fn bar(&self) -> &T { // self ödünç alındı
            &self.bar
        }
        fn bar_mut(&mut self) -> &mut T { // self değiştirilebilir olarak ödünç alındı
            &mut self.bar
        }
        fn into_bar(self) -> T { // self kullanıldı
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.bar()); // 1

    // Özellikler (traits) (diğer dillerde typeclasses ya da interface olarak da bilinir) //

    trait Frobnicate<T> {
        fn frobnicate(self) -> Option<T>;
    }

    impl<T> Frobnicate<T> for Foo<T> {
        fn frobnicate(self) -> Option<T> {
            Some(self.bar)
        }
    }

    let another_foo = Foo { bar: 1 };
    println!("{:?}", another_foo.frobnicate()); // Some(1)

    /////////////////////////
    // 3. Desen eşleme //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // Gelişmiş Desen eşleme 
    struct FooBar { x: i32, y: OptionalI32 }
    let bar = FooBar { x: 15, y: OptionalI32::AnI32(32) };

    match bar {
        FooBar { x: 0, y: OptionalI32::AnI32(0) } =>
            println!("The numbers are zero!"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } if n == m =>
            println!("The numbers are the same"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } =>
            println!("Different numbers: {} {}", n, m),
        FooBar { x: _, y: OptionalI32::Nothing } =>
            println!("The second number is Nothing!"),
    }

    /////////////////////
    // 4. Kontrol akışı //
    /////////////////////

    // `for` döngüsü
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // Aralıklar (ranges)
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // prints `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // İfade olarak `if`
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` döngüsü
    while 1 == 1 {
        println!("The universe is operating normally.");
        // break döngüden çıkmaya yarar
        // gerekli olmayan iterasyonları önler
        break
    }

    // Sonsuz döngü
    loop {
        println!("Hello!");
        // break döngüden çıkmaya yarar
        break
    }

    /////////////////////////////////
    // 5. Hafıza güvenliği ve işaretleyiciler //
    /////////////////////////////////

    // Sahipli işaretleyici – Bir anda sadece birisi bu işaretleyiciye sahip olabilir
    // `Box` scope'dan çıktığında, otomatik olarak güvenle kaldırılabilir.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // dereference
    // Şimdi, `now_its_mine`, `mine`'nın sahiplini alır. Diğer bir anlatımla, `mine` taşınır.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // Bu kod derlenmez, çünkü `now_its_mine` işaretleyiciye sahiptir.

    // Referans – Bir veriye işaret eden değiştirilemez bir işaretleyici
    // Bir değere referans alındığında, değerin 'ödünç alındığını' söyleriz.
    // Bir değer değiştirilemez bir şekilde ödünç alınırken, değiştirilemez veya taşınamaz.
    // Borçlanma değişkeninin son kullanımına kadar borçlanma aktiftir.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // `var` halen kullanılabilir
    println!("{}", *ref_var);
    // var = 5; // `var` ödünç alındığı için, bu kod derlenemez
    // *ref_var = 6; // `ref_var` değiştirilemez olduğu için, bu kod derlenemez
    ref_var; // İşlem yok, ama kullanım olarak sayılır ve ödünç aktif tutulur
    var = 2; // ref_var yukarıdaki satırdan sonra aktif değildir, ödünç sona erer

    // Değiştirilebilir Referans
    // Bir değer değiştirilebilir olarak ödünç alınırken, hiçbir şekilde erişilemez.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*', var2 değişkenini işaret etmek için kullanılır

    println!("{}", *ref_var2); // 6 , // var2 derlenemez.
    // ref_var2, &mut i32 tipindedir; değeri değil, bir i32 değerine referans saklar
    // var2 = 2; // `var2` ödünç alındığı için derlenemez.
    ref_var2; // İşlem yok, ama kullanım olarak sayılır ve ödünç aktif tutulur
}
```

## Daha Fazla

Rust hakkında daha fazla öğrenmek için [The Rust Programming
Language](http://doc.rust-lang.org/book/index.html) ve
[/r/rust](http://reddit.com/r/rust) adreslerini ziyaret edebilirsiniz.
irc.mozilla.org sunucusundaki #rust kanalında ise yeni başlayanlara her zaman
destek verilmektedir.

Rust online olarak [Rust playpen](http://play.rust-lang.org) veya
[Rust website](http://rust-lang.org) adreslerinden denenebilir.
