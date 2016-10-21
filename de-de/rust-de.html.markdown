---
language: rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
translators:
    - ["Christian Albrecht", "https://github.com/coastalchief"]
lang: de-de
filename: lernerust-de.rs
---

Rust ist eine Programmiersprache von Mozilla Research.  
Rust vereint Sicherheit, Nebenläufigkeit und eine hohe Praxistauglichkeit.  
  
Sicherheit bedeuted, dass Programmierfehler ausgeschlossen werden, die zu  
Speicherzugriffsfehlern führen könnten. Das funktioniert u.a. dadurch, dass  
es keinen Garbage Collector gibt, sondern ein besonderes Typsystem.  
  
Das erste Release von Rust, 0.1, wurde im Januar 2012 veröffentlicht.  
In den nächsten drei Jahren wurde die Sprache so schnell und aktiv weiter-  
entwickelt, dass es einfach keine stabile gab und geraten wurde den  
nightly build zu nutzen.  
  
Am 15. Mai 2015 wurde Rust 1.0 freigegeben, und zwar mit der Garantie einer  
Abwärtskompatabilität. Verbesserungen der Kompilierzeit und andere Compiler  
verbesserungen finden im Moment im nightly build statt. Von Rust gibt es im  
Moment ungefähr alle sechs Wochen ein Release. Rust 1.1 beta wurde zusammen  
mit dem 1.0 Release zur Verfügung gestellt.  
  
Obwohl Rust eine ziemlich low-level Sprache ist, vereint sie Ansätze aus  
der Welt der funktionalen, der objektorientierten und der nebenläufigen  
Programmierung. Dadurch kann in Rust nicht nur schnell, sondern auch sehr  
effizient entwickelt werden.  
  
  
```rust
// Dies ist ein Kommentar. Ein einzeiliger...
/* ...und multi-zeilen Kommentare sehe so aus */

/////////////////////
// 0. Installation //
/////////////////////
// Stabile binaries gibt es unter https://www.rust-lang.org/downloads.html

// Programme werden in .rs Dateien geschrieben also zum Beispiel  
// "main.rs" und dann kompiliert "rustc main.rs"  
// Herauskommt eine ausführbare Datei "main"  
// Für dieses Tutorial reicht das vollkommen aus. Für größere Projekte  
// sollte das unten beschriebene Cargo angeschaut werden.  

// Cargo  
// Ein gängiges Tool um Rust Projekte zu verwalten ist Cargo. Es macht im  
// wesentlichen drei Dinge: Code bauen, Dependencies laden und  
// Dependencies bauen.  
// Um ein vorhandenes Projekt zu cargo-ifyen müssen drei Dinge gemacht werden  
// * Erstelle eine Cargo.toml Konfigurationsdatei  
// * Verschiebe Source Code in ein src Verzeichnis  
// * Lösche das alte Executable  
//
// 'cargo build' baut den Code  
// 'cargo run' baut und führt das Programm aus  

///////////////
// 1. Basics //
///////////////

// Funktionen
// `i32` ist der Typ für einen 32-bit signed Integer
fn add2(x: i32, y: i32) -> i32 {
    // Impliziter return (kein Semikolon)
    x + y
}

// Main Funktion
fn main() {
    // Zahlen //

    // Unveränderliche Variable
    let x: i32 = 1;

    // Integer/float Suffixe
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // Type inference
    Meistens kann der Rust Compiler selbst schlussfolgern, von welchem
    Typ eine Variable ist, so dass man den Typ nicht explizit angeben muss.
    In diesem Tutorial werden Typen explizit angegeben, um zu demonstrieren,
    welche Möglichkeiten es gibt. Wenn man damit vertraut ist, kann man die
    Typen auch weglassen und die Type Inference hilft dann im Hintergrund.

    let implicit_x = 1;
    let implicit_f = 1.3;

    // Arithmetik
    let sum = x + y + 13;

    // Veränderliche Variable
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // Strings //
    // Strings gibt es in zwei Typen: &str und String

    // Zunächst &str
    let x: &str = "hello world!";

    // Ausgabe
    println!("{} {}", f, x); // 1.3 hello world

    // Ein `String` – heap-allokierter String
    let s: String = "hello world".to_string();

    // Ein string slice – ist eigentlich ein unveränderlicher Pointer
    // auf einen String – er enthält nicht den Inhalt den String, sondern
    // eben nur den Pointer auf etwas, dass den Inhalt kennt:
    // (In diesem Fall, `s`)
    let s_slice: &str = &s;

    // Ausgabe
    println!("{} {}", s, s_slice); // hello world hello world

    // Vektoren/Arrays //

    // Ein Array mit fester Größe
    let vier_ints: [i32; 4] = [1, 2, 3, 4];

    // Ein dynamisches Array (Vektorentor)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // Ein slice – eine unveränderliche Ansicht, oder Pointer auf einen
    // Vektor oder ein Array. Wie bei Strings, nur eben bei Vektoren
    let slice: &[i32] = &vector;

    // Benutze `{:?}` um eine debug Ausgabe zu erzeugen
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Tuples //

    // Ein Tuple ist eine Liste mit fester Größe und kann Werte
    // von unterschiedlichen Typen enthalten
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // Werte aus Vektor mit `let` destrukturieren
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // Vektor Indizes
    println!("{}", x.1); // hello

    //////////////
    // 2. Typen //
    //////////////

    // Struct
    struct Punkt {
        x: i32,
        y: i32,
    }

    let anfang: Punkt = Punkt { x: 0, y: 0 };

    // Ein struct mit unbenannten Felder heisst ‘tuple struct’
    struct Punkt2(i32, i32);

    let anfang2 = Punkt2(0, 0);

    // Einfache enum, so ähnlich wie in C
    enum Richtung {
        Links,
        Rechts,
        Hoch,
        Runter,
    }

    let hoch = Richtung::Hoch;

    // Enum mit Feldern
    enum OptionalI32 {
        EinI32(i32),
        Nix,
    }

    let zwei: OptionalI32 = OptionalI32::EinI32(2);
    let nix = OptionalI32::Nix;

    // Generics //

    struct Foo<T> { bar: T }

    // In der Standard Bibliothek heisst das hier `Option`
    enum Optional<T> {
        EinWert(T),
        KeinWert,
    }

    // Methoden //

    impl<T> Foo<T> {
        // Methoden erwarten einen `self` Parameter
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.get_bar()); // 1

    // Traits (vergleichbar mit Interfaces oder Typklassen in anderen Sprachen)
    // In Traits werden nur Method Signaturen erstellt.
    // Die Implementierung findet im impl statt.

    trait MacheIrgendwas<T> {
        fn macheIrgendwas(self) -> Option<T>;
    }

    impl<T> MacheIrgendwas<T> for Foo<T> {
        fn macheIrgendwas(self) -> Option<T> {
            mache(self.bar)
        }
    }

    let anderes_foo = Foo { bar: 1 };
    println!("{:?}", anderes_foo.macheIrgendwas()); // mache(1)

    /////////////////////////
    // 3. Pattern matching //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::EinI32(n) => println!("hier ist ein i32: {}", n),
        OptionalI32::Nix  => println!("hier ist nix!"),
    }

    // Advanced pattern matching
    struct FooBar { x: i32, y: OptionalI32 }
    let bar = FooBar { x: 15, y: OptionalI32::EinI32(32) };

    match bar {
        FooBar { x: 0, y: OptionalI32::EinI32(0) } =>
            println!("Beide Zahlen sind 0!"),
        FooBar { x: n, y: OptionalI32::EinI32(m) } if n == m =>
            println!("Beide Zahlen sind gleich"),
        FooBar { x: n, y: OptionalI32::EinI32(m) } =>
            println!("Zahlen sind unterschiedlich: {} {}", n, m),
        FooBar { x: _, y: OptionalI32::Nix } =>
            println!("Die zweite Zahl ist leer!"),
    }

    /////////////////////
    // 4. Control      //
    /////////////////////

    // `for` Schleife/Iterationen
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // Ranges
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // gibt aus: `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Mathe ist klappt!");
    } else {
        println!("Oh nein...");
    }

    // `if` als Ausdruck
    let wert = if true {
        "gut"
    } else {
        "schlecht"
    };

    // `while` Schleife
    while 1 == 1 {
        println!("Läuft...");
    }

    // Unendliche Schleifen
    loop {
        println!("Hello!");
    }

    /////////////////////////////////////
    // 5. Speichersicherheit & Pointer //
    /////////////////////////////////////

    // Owned pointer – nur eine Sache kann einen Pointer 'besitzen'.
    // Das heisst, wenn das Objekt `Box` seinen scope verlässt oder verliert,
    // wird es automatisch im Speicher de-allokiert.
    let mut mine: Box<i32> = Box::new(3);
    // Jetzt wird die Box dereferenziert
    *mine = 5; 
    // Jetzt geht `mine` in den Besitz von `now_its_mine` über. 
    // `mine` wird verschoben.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // ergibt 7

    // Das würde nicht kompilieren, da `now_its_mine` jetzt den Pointer besitzt
    // println!("{}", mine); 

    // Reference – ein unveränderlicher Pointer der fremde Daten referenziert
    // Wenn eine Referenz auf einen Wert gesetzt wird, heisst das, dass man den
    // Wert ausleiht (‘borrowed’).
    // Ein ausgeliehener Wert ist unveränderlich und lebt solange wie der
    // Scope existiert, in dem er erstellt wurde.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // Anders als `mine`, `var` kann hier weiter verwendet werden
    println!("{}", *ref_var);
    // var = 5; // das kompiliert nicht, da `var` ausgeliehen ist
    // *ref_var = 6; // das kompiliert auch nicht, da `ref_var` eine unveränderliche Referenz ist

    // Veränderliche Referenzen
    // Solange ein Wert veränderlich geliehen wurde, kann man nicht darauf zugreifen
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*' wird benutzt um auf den veränderlich geliehenen Wert var2 zu zeigen

    println!("{}", *ref_var2); // 6 , //var2 würde nicht kompilieren. //ref_var2 ist vom Typ &mut i32, also                                                      //stores a reference to an i32 not the value.
    // var2 = 2; // würde das nicht kompilieren, da `var2` geliehen wurde.
}
```

## Weitere Informationen

Es gibt eine ganze Reihe mehr über Rust zu sagen. Dieser Text gibt nur einen  
Einblick in die wichtigsten Sprachmerkmale.  
Um mehr über Rust zu erfahren, sollte man mit den folgenden Stellen starten:

1. Englisch:  
  * [Die offizielle Rust Webseite](http://rust-lang.org)
  * [The Rust Programming Language](https://doc.rust-lang.org/stable/book/README.html)
  * [/r/rust](http://reddit.com/r/rust)
  * the #rust channel on irc.mozilla.org 
  
2. Deutsch
  * [Rust Wikipedia](https://de.wikipedia.org/wiki/Rust_(Programmiersprache))
  * [Artikel im LinuxMagazin](http://www.linux-magazin.de/Ausgaben/2015/08/Rust)
