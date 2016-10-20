---
language: Go
filename: learngo-de.go
contributors:
    - ["Joseph Adams", "https://github.com/jcla1"]
    - ["Dennis Keller", "https://github.com/denniskeller"]
translators:
    - ["Jerome Meinke", "https://github.com/jmeinke"]
lang: de-de
---
Die Sprache Go (auch golang) wurde von Google entwickelt und wird seit 2007
benutzt. Go ähnelt in der Syntax der Sprache C, bietet darüber hinaus aber viele
Vorteile. Einerseits verzichtet Go auf Speicherarithmetik und
benutzt einen Garbage Collector. Andererseits enthält Go native Sprachelemente
für die Unterstützung von Nebenläufigkeit. Durch den Fokus auf einen schnellen
Kompilierprozess wird außerdem die Softwareentwicklung in Großprojekten
erleichtert.

Außerdem beinhaltet Go eine gut ausgestattete Standardbibliothek und hat eine
aktive Community.

```go
// Einzeiliger Kommentar
/* Mehr-
   zeiliger Kommentar */

// Wie bei Java gehört jede Quelldatei einem Paket an (Modularisierung).
// "main" ist ein besonderer Paketname, da er ein ausführbares Programm
// einleitet, im Gegensatz zu jedem anderen Namen, der eine Bibliothek
// deklariert.
package main

// Ein "import" wird verwendet, um Pakete zu deklarieren, die in dieser
// Quelldatei Anwendung finden.
import (
    "fmt"      // Ein Paket in der Go Standardbibliothek
    "net/http" // Ja, ein Webserver.
    "strconv"  // Zeichenkettenmanipulation
)

// Es folgt die Definition einer Funktion, in diesem Fall von "main". Auch hier
// ist der Name wieder besonders. "main" markiert den Eintrittspunkt des
// Programms.
func main() {
    // Println gibt eine Zeile zu stdout aus.
    // Der Prefix "fmt" bestimmt das Paket aus welchem die Funktion stammt.
    fmt.Println("Hello world!")

    // Aufruf einer weiteren Funktion definiert innerhalb dieses Pakets.
    beyondHello()
}

// Funktionen können Parameter akzeptieren. Diese werden in Klammern deklariert,
// die aber auch ohne Parameter erforderlich sind.
func beyondHello() {
    var x int // Deklaration einer Variable, muss vor Gebrauch geschehen.
    x = 3     // Zuweisung eines Werts.
    // Kurze Deklaration: Benutzen Sie ":=", um die Typisierung automatisch zu
    // folgern, die Variable zu deklarieren und ihr einen Wert zuzuweisen.
    y := 4

    // Eine Funktion mit mehreren Rückgabewerten.
    sum, prod := learnMultiple(x, y)

    fmt.Println("sum:", sum, "prod:", prod) // Simple Ausgabe
    learnTypes()                            // In < y Minuten lernen Sie mehr!
}

// Funktionen können mehrere Parameter und (mehrere!) Rückgabewerte haben.
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // Wiedergabe zweier Werte
}

// Überblick über einige eingebaute Typen und Literale.
func learnTypes() {
    // Kurze Deklarationen sind die Norm.
    s := "Lernen Sie Go!" // Zeichenketten-Typ

    s2 := `Eine "raw" Zeichenkette kann
Zeilenumbrüche beinhalten.` // Selber Zeichenketten-Typ

    // nicht-ASCII Literal.  Go Quelltext ist UTF-8 kompatibel.
    g := 'Σ' // Ein Runen-Typ, alias int32, gebraucht für unicode code points.

    f := 3.14195 // float64, eine IEEE-754 64-bit Dezimalzahl
    c := 3 + 4i  // complex128, besteht intern aus zwei float64-er

    // "var"-Syntax mit Initalwert
    var u uint = 7 // Vorzeichenlos, aber die Größe ist implementationsabhängig
    var pi float32 = 22. / 7

    // Umwandlungs-Syntax mit kurzer Deklaration
    n := byte('\n') // byte ist ein Alias für uint8

    // Arrays haben bei Kompile-Zeit festgelegte Größen
    var a4 [4]int           // Ein Array mit 4 ints, alle mit Initialwert 0
    a3 := [...]int{3, 1, 5} // Ein Array mit 4 ints, Initialwerte wie angezeigt

    // "slices" haben eine dynamische Größe. Arrays und Slices haben beide ihre
    // Vorzüge, aber slices werden viel häufiger verwendet
    s3 := []int{4, 5, 9}       // Vergleichen Sie mit a3, hier: keine Ellipse
    s4 := make([]int, 4)       // Weist Speicher für 4 ints zu, alle mit Wert 0
    var d2 [][]float64         // Nur eine Deklaration, keine Speicherzuweisung
    bs := []byte("eine slice") // Umwandlungs-Syntax

    p, q := learnMemory() // Deklariert p & q als Zeiger zu einer int.
    fmt.Println(*p, *q)   // Die gibt die zwei Werte aus. "*" für den Zugriff

    // "Maps" sind dynamische Datenstrukturen mit variabler Größe. Sie sind wie
    // "hashs" oder "dictionaries" aus anderen Sprachen.
    m := map[string]int{"drei": 3, "vier": 4}
    m["eins"] = 1

    // Ungebrauchte Variablen sind Fehler in Go
    // Der Unterstrich wird verwendet, um einen Wert zu verwerfen.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Die Ausgabe zählt natürlich auch als Gebrauch
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // Auf zum Kontrollfluss!
}

// Go ist komplett "garbage collected". Sie unterstützt Zeiger (pointers) aber
// keine Zeiger-Rechnungen. Fehler können sich durch "nil" einschleichen, jedoch
// nicht durch erhöhen eines Zeigers.
func learnMemory() (p, q *int) {
    // Die bennanten Rückgabewerte p & q sind vom Typ *int
    p = new(int) // Eingebaute Funktion "new" weist neuen Speicherplatz zu
    // Der zugewiesene Speicher ist mit 0 initialisiert, p ist nicht länger nil
    s := make([]int, 20) // So weist man 20 ints nebeneinander (im Speicher) zu
    s[3] = 7             // Einer von ihnen wird ein Wert zugewiesen
    r := -2              // Deklaration einer weiteren lokalen Variable
    return &s[3], &r     // & gibt die Addresse einer Variable
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // Bedingte Anweisungen verlangen nach geschweiften Klammern, normale
    // Klammern um die Bedingung werden aber nicht gebraucht.
    if true {
        fmt.Println("hab's dir ja gesagt!")
    }
    // Die Formatierung ist durch den Befehl "go fmt" standardisiert
    if false {
        // nicht hier
    } else {
        // sondern hier! spielt die Musik
    }

    // Benutzen Sie ein "switch" Statement anstatt eine Anreihung von if-s
    x := 1
    switch x {
    case 0:
    case 1:
        // Einzelne Fälle fallen nicht zum nächsten durch!
    case 2:
        // wird nicht ausgeführt
    }
    // Wie bei "if", braucht "for" auch keine Klammern um die Bedingung
    for x := 0; x < 3; x++ { // ++ ist ein Statement
        fmt.Println(x, "-te Iteration")
    }
    // Ab hier gilt wieder: x == 1

    // For ist die einzige Schleifenform in Go, sie hat aber mehrere Formen:
    for { // Endlosschleife
        break    // nur ein Spaß
        continue // wird nie ausgeführt
    }

    // Wie bei for, bedeutet := in einer bedingten Anweisung zunächst die
    // Zuweisung und erst dann die Überprüfung der Bedingung.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // Funktionsliterale sind "closures"
    xBig := func() bool {
        return x > 100 // Verweist auf x, deklariert vor dem switch
    }
    fmt.Println("xBig:", xBig()) // true (im moment gilt: x == 1e6)
    x /= 1e5                     // dies macht x == 10
    fmt.Println("xBig:", xBig()) // jetzt: false

    // Wenn Sie's brauchen, werden Sie's lieben!
    goto love
love:

    learnInterfaces() // Jetzt zum interessanten Teil!
}

// Definiere "Stringer" als ein Interface mit einer Methode: String
type Stringer interface {
    String() string
}

// Definiere ein Paar als struct mit zwei Feldern, Integers mit Namen x & y.
type pair struct {
    x, y int
}

// Definiere eine Methode von "pair".
// Dieser Typ erfüllt jetzt das Stringer interface.
func (p pair) String() string { // p ist der Empfänger
    // Sprintf ist eine weitere öffentliche Funktion von fmt.
    // Der Syntax mit Punkt greift auf die Felder zu.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // Der Klammer-Syntax ist ein "struct literal". Es ist ein vollkommen
    // initialisiertes struct. Der := Syntax deklariert und initialisiert p.
    p := pair{3, 4}
    fmt.Println(p.String()) // Aufruf der String() Methode von p.
    var i Stringer          // Deklariere i vom Typ: Stringer
    i = p                   // Ok, weil p auch vom Typ Stringer ist.
    // Aufruf der String Methode von i, gleiche Ausgabe wie zuvor.
    fmt.Println(i.String())

    // Funktionen des fmt-Pakets rufen die String() Methode auf um eine
    // druckbare Variante des Empfängers zu erhalten.
    fmt.Println(p) // gleiche Ausgabe wie zuvor
    fmt.Println(i) // und wieder die gleiche Ausgabe wie zuvor

    learnErrorHandling()
}

func learnErrorHandling() {
    // Das ", ok" Idiom wird häufig verwendet um zu überprüfen ob etwas schief
    // gegangen ist.
    m := map[int]string{3: "drei", 4: "vier"}
    if x, ok := m[1]; !ok { // ok wird false sein, da 1 nicht in der map ist.
        fmt.Println("keine eins gefunden")
    } else {
        fmt.Print(x) // x wäre der Wert, wenn er in der map wäre.
    }
    // Ein Fehler-Wert (error value) gibt mehr Informationen über den Grund für
    // das Problem an.
    if _, err := strconv.Atoi("nicht-int"); err != nil { // _ verwirft den Wert
        // Gibt: "strconv.ParseInt: parsing "nicht-int": invalid syntax" aus
        fmt.Println(err)
    }
    // Wir kommen bald nochmal auf Interfaces zurück. Aber inzwischen:
    learnConcurrency()
}

// c ist ein Kanal, ein sicheres Kommunikationsmedium.
func inc(i int, c chan int) {
    c <- i + 1 // <- ist der "send" Operator, wenn ein Kanal auf der Linken ist
}

// Wir verwenden "inc" um Zahlen parallel zu erhöhen.
func learnConcurrency() {
    // Die selbe "make"-Funktion wie vorhin. Sie initialisiert Speicher für
    // maps, slices und Kanäle.
    c := make(chan int)
    // Starte drei parallele "Goroutines".
    // Die Zahlen werden parallel (concurrently) erhöht.
    // Alle drei senden ihr Ergebnis in den gleichen Kanal.
    go inc(0, c) // "go" ist das Statement zum Start einer neuen Goroutine
    go inc(10, c)
    go inc(-805, c)
    // Auslesen und dann Ausgeben der drei berechneten Werte.
    // Man kann nicht im voraus feststellen in welcher Reihenfolge die Werte
    // ankommen.
    fmt.Println(<-c, <-c, <-c) // mit dem Kanal rechts ist <- der Empfangs-Operator

    cs := make(chan string)       // ein weiterer Kanal, diesmal für strings
    cc := make(chan chan string)  // ein Kanal für string Kanäle

    // Start einer neuen Goroutine, nur um einen Wert zu senden
    go func() { c <- 84 }()
    go func() { cs <- "wortreich" }() // schon wieder, diesmal für
    // "select" hat eine Syntax wie ein switch Statement, aber jeder Fall ist
    // eine Kanaloperation. Es wählt einen Fall zufällig aus allen, die
    // kommunikationsbereit sind, aus.
    select {
    case i := <-c: // der empfangene Wert kann einer Variable zugewiesen werden
        fmt.Printf("es ist ein: %T", i)
    case <-cs: // oder der Wert kann verworfen werden
        fmt.Println("es ist eine Zeichenkette!")
    case <-cc: // leerer Kanal, nicht bereit für den Empfang
        fmt.Println("wird nicht passieren.")
    }
    // Hier wird eine der beiden Goroutines fertig sein, die andere nicht.
    // Sie wird warten bis der Wert den sie sendet von dem Kanal gelesen wird.

    learnWebProgramming() // Go kann es und Sie hoffentlich auch bald.
}

// Eine einzige Funktion aus dem http-Paket kann einen Webserver starten.
func learnWebProgramming() {
    // Der erste Parameter von "ListenAndServe" ist eine TCP Addresse, an die
    // sich angeschlossen werden soll.
    // Der zweite Parameter ist ein Interface, speziell: ein http.Handler
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // Fehler sollte man nicht ignorieren!
}

// Wir lassen "pair" das http.Handler Interface erfüllen, indem wir seine einzige
// Methode implementieren: ServeHTTP
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Senden von Daten mit einer Methode des http.ResponseWriter
    w.Write([]byte("Sie haben Go in Y Minuten gelernt!"))
}
```

## Weitere Resourcen
Informationen zu Go findet man auf der [offiziellen Go Webseite](http://golang.org/).
Dort gibt es unter anderem ein Tutorial und interaktive Quelltext-Beispiele, vor
allem aber Dokumentation zur Sprache und den Paketen.

Auch zu empfehlen ist die Spezifikation von Go, die nach heutigen Standards sehr
kurz und gut verständlich formuliert ist. Auf der Leseliste von Go-Neulingen
ist außerdem der Quelltext der [Go standard Bibliothek](http://golang.org/src/pkg/)
einzusehen. Dieser kann als Referenz für leicht zu verstehendes und im idiomatischen Stil
verfasstes Go dienen. Erreichbar ist der Quelltext auch durch das Klicken der Funktionsnamen
in der [offiziellen Dokumentation von Go](http://golang.org/pkg/).
