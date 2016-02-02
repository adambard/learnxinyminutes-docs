---
language: Go
lang: hu-hu
filename: learngo-hu.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
translators:
    - ["Szabó Krisztián", "https://github.com/thenonameguy/"]
    - ["Árpád Goretity", "https://github.com/H2CO3"]
---

A Go programozási nyelv az életszerű feladatok könnyebb elvégzése miatt született.
A mai legújabb programozási trendeket elkerülve,
praktikus megoldást nyújt a valós, üzleti problémákra.

C-szerű szintaktikával és statikus típuskezeléssel rendelkezik.
A fordító szempillantás alatt végez és egy gyorsan futó,statikus futtatható állományt hoz létre.
A nyelv könnyen érthető, folyamatok közötti csatornákon áthaladó üzenetekkel kommunikáló konkurens programozást tesz lehetővé, így könnyen ki lehet használni
a mai számítógépek több magos processzorait, ez nagy rendszerek építéséhez ideális.

A Go alap könyvtára mindenre területre kiterjed, ennek köszönhetően a nyelvnek egyre növekvő tábora van.

```go
// Egy soros komment
/* Több
   soros komment */

// Minden forrás fájl egy csomag-definícióval kezdődik, ez hasonlít a Python
// csomagkezelésére
// A main egy különleges csomagnév, ennek a fordítása futtatható állományt hoz
// létre egy könyvtár helyett.
package main

// Az import rész meghatározza melyik csomagokat kívánjuk használni ebben a
// forrásfájlban
import (
    "fmt"      // A Go alap könyvtárának része
    "net/http" // Beépített webszerver!
    "strconv"  // Stringek átalakítására szolgáló csomag
)

// Függvénydeklarálás, a main nevű függvény a program kezdőpontja.
func main() {
    // Println kiírja a beadott paramétereket a standard kimenetre.
    // Ha más csomagot függvényeit akarjuk használni, akkor azt jelezni kell a
    // csomag nevével
    fmt.Println("Hello world!")

    // Meghívunk egy másik függvényt ebből a csomagból
    beyondHello()
}

// A függvények paraméterei zárójelek között vannak.
// Ha nincsenek paraméterek, akkor is kötelező a zárójel-pár.
func beyondHello() {
    var x int // Változó deklaráció, használat előtt muszáj ezt megtenni.
    x = 3     // Változó értékadás
    // "Rövid" deklaráció is létezik, ez az érték alapján deklarálja,
    // definiálja és értéket is ad a változónak
    y := 4
    sum, prod := learnMultiple(x, y)        // a függvényeknek több
                                            // visszatérési értéke is lehet
    fmt.Println("sum:", sum, "prod:", prod) // egyszerű kiíratás
    learnTypes()
}

// A funkcióknak elnevezett visszatérési értékük is lehet
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // visszatérünk két értékkel
    /*
    sum = x + y
    prod = x * y
    return
    Ez ugyanezzel az eredménnyel járt volna, mint a fenti sor.
    Üres return esetén, az elnevezett visszatérési változók 
    aktuális értékeikkel térnek vissza. */
}

// Beépített típusok
func learnTypes() {
    // Rövid deklarálás az esetek többségében elég lesz a változókhoz
    s := "Tanulj Go-t!" // string típus

    s2 := `A "nyers" stringekben lehetnek
    újsorok is!` // de ettől még ez is ugyanolyan string mint az s, nincs külön
                 // típusa

    // nem ASCII karakterek.  Minden Go forrás UTF-8 és a stringek is azok.
    g := 'Σ' // rúna(rune) típus, megegyezik az uint32-vel, egy UTF-8 karaktert
             // tárol

    f := 3.14195 // float64, az IEEE-754 szabványnak megfelelő 64-bites
                 // lebegőpontos szám
    c := 3 + 4i  // complex128, belsőleg két float64-gyel tárolva

    // Var szintaxis változótípus-definiálással
    var u uint = 7 // unsigned, az implementáció dönti el mekkora, akárcsak az
                   // int-nél
    var pi float32 = 22. / 7

    // Rövid deklarásnál átalakítás is lehetséges
    n := byte('\n') // byte típus, ami megegyezik az uint8-al

    // A tömböknek fordítás-időben fixált méretük van
    var a4 [4]int           // egy tömb 4 int-tel, mind 0-ra inicializálva
    a3 := [...]int{3, 1, 5} // egy tömb 3 int-tel, láthatóan inicalizálva egyedi
                            // értékekre

    // A "szeleteknek" (slices) dinamikus a méretük. A szeleteknek és a tömböknek is
    // megvannak az előnyeik de a szeleteket sokkal gyakrabban használjuk.
    s3 := []int{4, 5, 9}    // vesd össze a3-mal, nincsenek pontok.
    s4 := make([]int, 4)    // allokál 4 int-et, mind 0-ra inicializálva
    var d2 [][]float64      // ez csak deklaráció, semmi sincs még allokálva
    bs := []byte("a slice") // típus konverzió szintaxisa

    p, q := learnMemory() // deklarál két mutatót (p,q), két int-re
    fmt.Println(*p, *q)   // * követi a mutatót. Ez a sor kiírja a két int értékét.

    // A map a dinamikusan növelhető asszociatív tömb része a nyelvnek, hasonlít
    // a hash és dictionary típusokra más nyelvekben.
    m := map[string]int{"three": 3, "four": 4}
    m["one"] = 1

    // A felhasználatlan változók fordítás-idejű hibát okoznak a Go-ban.
    // Az aláhúzással "használod" a változókat, de eldobod az értéküket.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Kiíratás is természetesen használatnak minősül
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl()
}

// A Go nyelvben szemétgyűjtés (garbage collection) működik. Megtalálhatók benne
// mutatók, de nincs pointeraritmetika. Ez azt jelenti, hogy üres (null) mutatóval még
// mindig hibázhatsz, de hozzáadni/műveleteket végezni már nem lehet.
func learnMemory() (p, q *int) {
    // Elnevezett visszatérési változóknak int-re mutató a típusa
    p = new(int) // a beépített "new" funkció, egy típusnak elegendő memóriát
                 // allokál, és visszaad rá egy mutatót.
    // Az allokált int nullázva van, p többé nem üres mutató.
    s := make([]int, 20) // allokáljunk 20 int változót egy memóriaterületen.
    s[3] = 7             // adjunk értéket az egyiknek
    r := -2              // hozzánk létre egy lokális változót
    return &s[3], &r     // A & megadja a memóriacímét a változónak
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // Az elágazásoknak kötelező a kapcsos zárójel, a zárójel nem szükséges.
    if true {
        fmt.Println("megmondtam")
    }
    // A kód formátumát a nyelvvel járó "go" parancssori program "go fmt"
    // parancsa szabványosítja
    if false {
        // így lehet
    } else {
        // if/else-t csinálni
    }
    // Használjunk switchet a hosszabb elágazások alkalmazása helyett.
    x := 1
    switch x {
    case 0:
    case 1:
        // Az "esetek" nem "esnek át", tehát
    case 2:
        // ez nem fog lefutni, nincs szükség break-ekre.
    }
    // A for ciklus sem használ zárójeleket
    for x := 0; x < 3; x++ { 
        fmt.Println("iteráció", x)
    }
    // itt az x == 1.

    // A for az egyetlen ciklus fajta a Go-ban, de több formája van.
    for { // végtelen ciklus
        break    // csak vicceltem
        continue // soha nem fut le
    }

    //Akárcsak a for-nál, az if-nél is lehet rövid deklarálással egy lokális változót létrehozni, 
    //ami a blokk összes if/else szerkezetén keresztül érvényes marad. 
    if y := expensiveComputation(); y > x {
        x = y
    }
    // Függvényeket használhatjuk closure-ként is.
    xBig := func() bool {
        return x > 100 // a switch felett deklarált x-et használjuk itt
    }
    fmt.Println("xBig:", xBig()) // igaz (utoljára 1e6 lett az értéke az x-nek)
    x /= 1e5                     // így most már x == 10
    fmt.Println("xBig:", xBig()) // 10 pedig kisebb mint 100, tehát hamis

    // Ha nagyon-nagyon szükséges, akkor használhatjuk a jó öreg goto-t.
    goto love
love:

    learnInterfaces() // Itt kezdődnek az érdekes dolgok!
}

// Definiáljuk a Stringert egy olyan interfésznek, amelynek egy metódusa van, a
// String, ami visszatér egy stringgel.
type Stringer interface {
    String() string
}

// Definiáljuk a pair-t egy olyan struktúrának amelynek két int változója van,
// x és y.
type pair struct {
    x, y int
}

// Definiáljunk egy metódust a pair struktúrának, ezzel teljesítve a Stringer interfészt.
func (p pair) String() string { // p lesz a "fogadó" (receiver)
    // Sprintf az fmt csomag egy publikus függvénye, műkődése megegyezik a C-s
    // megfelelőjével. A pontokkal érjük el a mindenkori p struktúra elemeit
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // A kapcsos zárójellel jelezzük, hogy egyből inicializálni
    // szeretnénk a struktúra változóit a sorrendnek megfelelően.
    p := pair{3, 4}
    fmt.Println(p.String()) // meghívjuk a p String metódusát.
    var i Stringer          // deklaráljuk i-t Stringer típusú interfésznek
    i = p                   // lehetséges, mert a pair struktúra eleget tesz a
                            // Stringer interfésznek
    // Meghívjuk i String metódusát, az eredmény ugyanaz, mint az előbb.
    fmt.Println(i.String())

    // Az fmt csomag függvényei automatikusan meghívják a String függvényt
    // hogy megtudják egy objektum szöveges reprezentációját.
    fmt.Println(p) // ugyan az az eredmény mint az előbb, a Println meghívja
                   // a String metódust.
    fmt.Println(i) // dettó

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" szokásos megoldás arra, hogy jól működött-e a függvény.
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // ok hamis lesz, mert az 1 nincs benne a map-ban.
        fmt.Println("nincs meg")
    } else {
        fmt.Print(x) // x lenne az érték, ha benne lenne a map-ban.
    }
    // A hiba érték többet is elmond a függvény kimeneteléről, mint hogy minden
    // "ok" volt-e
    if _, err := strconv.Atoi("non-int"); err != nil { // _ eldobja az értéket,
                                                       // úgy se lesz jó jelen
                                                       // esetben
        // kiírja, hogy "strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // Az interfészekre még visszatérünk, addig is jöjjön a konkurens programozás!
    learnConcurrency()
}

// c egy csatorna, egy konkurens-biztos kommunikációs objektum.
func inc(i int, c chan int) {
    c <- i + 1 // <- a "küldés" operátor, ha a bal oldalán csatorna van, így
               // i+1-et küld be a csatornába
}

// Az inc-et fogjuk arra használni, hogy konkurensen megnöveljünk számokat
func learnConcurrency() {
    // Ugyanaz a make függvény, amivel korábban szeleteket hoztunk létre.
    // A make allokál map-eket, szeleteket és csatornákat.
    c := make(chan int)
    // Indítsunk három konkurens goroutine-t.  A számok konkurensen lesznek 
    // megnövelve, ha a számítógép képes rá és jól be van állítva, akkor pedig
    // paralellizálva/egymás mellett. Mind a 3 ugyanabba a csatornába küldi az
    // eredményeket.
    go inc(0, c) // A go utasítás indít el goroutine-okat.
    go inc(10, c)
    go inc(-805, c)
    // Beolvassuk 3x a csatornából az eredményeket és kiírjuk őket a kimenetre.
    // Nem lehet tudni milyen sorrendben fognak érkezni az eredmények!
    fmt.Println(<-c, <-c, <-c) // hogyha a jobb oldalon csatorna van, akkor a 
                               // "<-" a beolvasó/kapó operátor

    cs := make(chan string)       // még egy csatorna, ez stringekkel kommunikál
    cc := make(chan chan string)  // egy csatorna csatornával
    go func() { c <- 84 }()       // indítsunk egy új goroutine-t, csak azért
                                  // hogy küldjünk egy számot
    go func() { cs <- "wordy" }() // ugyanez, csak a cs csatornába stringet
                                  // küldünk
    // A select olyan mint a switch, csak feltételek helyett csatorna műveletek
    // vannak. Véletlenszerűen kiválasztja az első olyan esetet, ahol létrejöhet
    // kommunikáció.
    select {
    case i := <-c: // a megkapott értéket el lehet tárolni egy változóban
        fmt.Println("ez egy", i)
    case <-cs: // vagy el lehet dobni az értékét
        fmt.Println("ez egy string volt")
    case <-cc: // üres csatorna, soha nem fog rajta semmi se érkezni
        fmt.Println("sose futok le :'( ")
    }
    // Ezen a ponton vagy c vagy a cs goroutine-ja lefutott.
    // Amelyik hamarabb végzett, annak a megfelelő case-e lefutott, a másik
    // blokkolva vár.

    learnWebProgramming() // a Go képes rá. Te is képes akarsz rá lenni.
}

// Egy függvény a http csomagból elindít egy webszervert.
func learnWebProgramming() {
    // A ListenAndServe első paramétre egy TCP port, amin kiszolgálunk majd.
    // Második paramétere egy interfész, pontosabban a http.Handler interfész.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // nem felejtjük el kiírni az esetleges hibákat!
}

// Csináljunk a pair-ból egy http.Handler-t úgy, hogy implementáljuk az
// egyetlen metódusát, a ServeHTTP-t.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Minden kapcsolatra elküldjük ezt a http.ResponseWriter-rel
    w.Write([]byte("Megtanultad a Go-t Y perc alatt!"))
}
```

## További olvasmányok

Minden Go-val kapcsolatos megtaláható a [hivatalos Go weboldalon](http://golang.org/).
Ott követhetsz egy tutorialt, játszhatsz a nyelvvel az interneten, és sok érdekességet olvashatsz.

A nyelv specifikációját kifejezetten érdemes olvasni, viszonylag rövid és sokat tanul belőle az ember.

Ha pedig jobban bele akarod vetni magad a Go-ba, akkor a legjobb praktikákat kilesheted a standard könyvtárból.
TIPP: a dokumentációban kattints egy függvény nevére és rögtön megmutatja a hozzá tartozó kódot!

Ha pedig a nyelvnek egy bizonyos részéről szeretnél hasonló leírást találni, akkor a
[gobyexample.com](https://gobyexample.com/)-on megtalálod, amit keresel.
