---
language: Go
filename: learngo-de.go
contributors:
    - ["Joseph Adams", "https://github.com/jcla1"]
lang: de-de
---
Go wurde entwickelt um probleme zu lösen. Sie ist zwar nicht der neuste Trend in
der Informatik, aber sie ist eine der neusten und schnellsten Wege um Aufgabe in
der realen Welt zu lösen.

Sie hat vertraute Elemente von imperativen Sprachen mit statisher Typisierung
und kann schnell kompiliert und ausgeführt werden. Verbunden mit leicht zu
verstehenden Parallelitäts-Konstrukten, um die heute üblichen mehrkern
Prozessoren optimal nutzen zu können, eignet sich Go äußerst gut für große
Programmierprojekte.

Außerdem beinhaltet Go eine gut ausgestattete standard bibliothek und hat eine
aktive community.

```go
// Einzeiliger Kommentar
/* Mehr-
   zeiliger Kommentar */

// Eine jede Quelldatei beginnt mit einer Packet-Klausel.
// "main" ist ein besonderer Packetname, da er ein ausführbares Programm
// einleitet, im Gegensatz zu jedem anderen Namen, der eine Bibliothek
// deklariert.
package main

// Ein "import" wird verwendet um Packte zu deklarieren, die in dieser
// Quelldatei Anwendung finden.
import (
    "fmt"      // Ein Packet in der Go standard Bibliothek
    "net/http" // Ja, ein Webserver.
    "strconv"  // Zeichenkettenmanipulation
)

// Es folgt die Definition einer Funktions, in diesem Fall von "main". Auch hier
// ist der Name wieder besonders. "main" markiert den Eintrittspunkt des
// Programms. Vergessen Sie nicht die geschweiften Klammern!
func main() {
    // Println gibt eine Zeile zu stdout aus.
    // Der Prefix "fmt" bestimmt das Packet aus welchem die Funktion stammt.
    fmt.Println("Hello world!")

    // Aufruf einer weiteren Funktion definiert innerhalb dieses Packets.
    beyondHello()
}

// Funktionen können Parameter akzeptieren, diese werden in Klammern deklariert,
// die aber auch bei keinen Parametern erforderlich sind.
func beyondHello() {
    var x int // Deklaration einer Variable, muss vor Gebrauch geschehen.
    x = 3     // Zuweisung eines Werts.
    // Kurze Deklaration: Benutzen Sie ":=" um die Typisierung automatisch zu
    // folgern, die Variable zu deklarieren und ihr einen Wert zu zuweisen.
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

// Überblick ueber einige eingebaute Typen und Literale.
func learnTypes() {
    // Kurze Deklarationen sind die Norm.
    s := "Lernen Sie Go!" // Zeichenketten-Typ

    s2 := `Eine "raw" Zeichenkette kann
Zeilenumbrüche beinhalten.` // Selber Zeichenketten-Typ

    // nicht-ASCII Literal.  Go Quelltext ist UTF-8 kompatibel.
    g := 'Σ' // Ein Runen-Typ, alias uint32, gebraucht für unicode code points.

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
    s4 := make([]int, 4)       // Weist Speicher für 4 ints zu, alle mit Initialwert 0
    var d2 [][]float64         // Nur eine Deklaration, keine Speicherzuweisung
    bs := []byte("eine slice") // Umwandlungs-Syntax

    p, q := learnMemory() // Deklariert p & q als Zeiger zu einer int.
    fmt.Println(*p, *q)   // Die gibt die zwei Werte aus. "*" für den Zugriff

    // "Maps" sind dynamische Datenstrukturen mit variabler Größe. Sie sind wie
    // "hashs" oder "dictionaries" aus anderen Sprachen.
    m := map[string]int{"drei": 3, "vier": 4}
    m["eins"] = 1

    // Ungebrauchte Variablen sind Fehler in Go
    // Der Unterstrich wird verwendet um einen Wert zu verwerfen.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Die Ausgabe zählt natürlich auch als Gebrauch
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // Auf zum Kontrollfluss!
}

// Go is fully garbage collected.  It has pointers but no pointer arithmetic.
// You can make a mistake with a nil pointer, but not by incrementing a pointer.
func learnMemory() (p, q *int) {
    // Named return values p and q have type pointer to int.
    p = new(int) // built-in function new allocates memory.
    // The allocated int is initialized to 0, p is no longer nil.
    s := make([]int, 20) // allocate 20 ints as a single block of memory
    s[3] = 7             // assign one of them
    r := -2              // declare another local variable
    return &s[3], &r     // & takes the address of an object.
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // If statements require brace brackets, and do not require parens.
    if true {
        fmt.Println("told ya")
    }
    // Formatting is standardized by the command line command "go fmt."
    if false {
        // pout
    } else {
        // gloat
    }
    // Use switch in preference to chained if statements.
    x := 1
    switch x {
    case 0:
    case 1:
        // cases don't "fall through"
    case 2:
        // unreached
    }
    // Like if, for doesn't use parens either.
    for x := 0; x < 3; x++ { // ++ is a statement
        fmt.Println("iteration", x)
    }
    // x == 1 here.

    // For is the only loop statement in Go, but it has alternate forms.
    for { // infinite loop
        break    // just kidding
        continue // unreached
    }
    // As with for, := in an if statement means to declare and assign y first,
    // then test y > x.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // Function literals are closures.
    xBig := func() bool {
        return x > 100 // references x declared above switch statement.
    }
    fmt.Println("xBig:", xBig()) // true (we last assigned 1e6 to x)
    x /= 1e5                     // this makes it == 10
    fmt.Println("xBig:", xBig()) // false now

    // When you need it, you'll love it.
    goto love
love:

    learnInterfaces() // Good stuff coming up!
}

// Define Stringer as an interface type with one method, String.
type Stringer interface {
    String() string
}

// Define pair as a struct with two fields, ints named x and y.
type pair struct {
    x, y int
}

// Define a method on type pair.  Pair now implements Stringer.
func (p pair) String() string { // p is called the "receiver"
    // Sprintf is another public function in package fmt.
    // Dot syntax references fields of p.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // Brace syntax is a "struct literal."  It evaluates to an initialized
    // struct.  The := syntax declares and initializes p to this struct.
    p := pair{3, 4}
    fmt.Println(p.String()) // call String method of p, of type pair.
    var i Stringer          // declare i of interface type Stringer.
    i = p                   // valid because pair implements Stringer
    // Call String method of i, of type Stringer.  Output same as above.
    fmt.Println(i.String())

    // Functions in the fmt package call the String method to ask an object
    // for a printable representation of itself.
    fmt.Println(p) // output same as above. Println calls String method.
    fmt.Println(i) // output same as above

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" idiom used to tell if something worked or not.
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // ok will be false because 1 is not in the map.
        fmt.Println("no one there")
    } else {
        fmt.Print(x) // x would be the value, if it were in the map.
    }
    // An error value communicates not just "ok" but more about the problem.
    if _, err := strconv.Atoi("non-int"); err != nil { // _ discards value
        // prints "strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // We'll revisit interfaces a little later.  Meanwhile,
    learnConcurrency()
}

// c is a channel, a concurrency-safe communication object.
func inc(i int, c chan int) {
    c <- i + 1 // <- is the "send" operator when a channel appears on the left.
}

// We'll use inc to increment some numbers concurrently.
func learnConcurrency() {
    // Same make function used earlier to make a slice.  Make allocates and
    // initializes slices, maps, and channels.
    c := make(chan int)
    // Start three concurrent goroutines.  Numbers will be incremented
    // concurrently, perhaps in parallel if the machine is capable and
    // properly configured.  All three send to the same channel.
    go inc(0, c) // go is a statement that starts a new goroutine.
    go inc(10, c)
    go inc(-805, c)
    // Read three results from the channel and print them out.
    // There is no telling in what order the results will arrive!
    fmt.Println(<-c, <-c, <-c) // channel on right, <- is "receive" operator.

    cs := make(chan string)       // another channel, this one handles strings.
    cc := make(chan chan string)  // a channel of string channels.
    go func() { c <- 84 }()       // start a new goroutine just to send a value
    go func() { cs <- "wordy" }() // again, for cs this time
    // Select has syntax like a switch statement but each case involves
    // a channel operation.  It selects a case at random out of the cases
    // that are ready to communicate.
    select {
    case i := <-c: // the value received can be assigned to a variable
        fmt.Printf("it's a %T", i)
    case <-cs: // or the value received can be discarded
        fmt.Println("it's a string")
    case <-cc: // empty channel, not ready for communication.
        fmt.Println("didn't happen.")
    }
    // At this point a value was taken from either c or cs.  One of the two
    // goroutines started above has completed, the other will remain blocked.

    learnWebProgramming() // Go does it.  You want to do it too.
}

// A single function from package http starts a web server.
func learnWebProgramming() {
    // ListenAndServe first parameter is TCP address to listen at.
    // Second parameter is an interface, specifically http.Handler.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // don't ignore errors
}

// Make pair an http.Handler by implementing its only method, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Serve data with a method of http.ResponseWriter
    w.Write([]byte("You learned Go in Y minutes!"))
}
```

## Further Reading

The root of all things Go is the [official Go web site](http://golang.org/).
There you can follow the tutorial, play interactively, and read lots.

The language definition itself is highly recommended.  It's easy to read
and amazingly short (as language definitions go these days.)

On the reading list for students of Go is the [source code to the standard
library](http://golang.org/src/pkg/).  Comprehensively documented, it
demonstrates the best of readable and understandable Go, Go style, and Go
idioms.  Or you can click on a function name in [the
documentation](http://golang.org/pkg/) and the source code comes up!

