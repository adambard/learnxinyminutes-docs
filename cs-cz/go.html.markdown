---
name: Go
category: language
language: Go
filename: learngo.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Christopher Bess", "https://github.com/cbess"]
    - ["Jesse Johnson", "https://github.com/holocronweaver"]
    - ["Quint Guvernator", "https://github.com/qguv"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
    - ["Alexej Friesen", "https://github.com/heyalexej"]
    - ["Clayton Walker", "https://github.com/cwalk"]
translators:
    - ["Ondra Linek", "https://github.com/defectus/"]

---

Jazyk Go byl vytvořen, jelikož bylo potřeba dokončit práci. Není to poslední 
trend ve světě počítačové vědy, ale je to nejrychlejší a nejnovější způsob,
jak řešit realné problémy.

Go používá známé koncepty imperativních jazyků se statickým typováním.
Rychle se kompiluje a také rychle běží. Přidává snadno pochopitelnou
podporu konkurenčnosti, což umožňuje využít výhody multi-core procesorů a
jazyk také obsahuje utility, které pomáhají se škálovatelným programováním.

Go má již v základu vynikající knihovnu a je s ním spojená nadšená komunita.

```go
// Jednořádkový komentář
/* Několika
 řádkový komentář */

// Každý zdroják začíná deklarací balíčku (package)
// Main je vyhrazené jméno, které označuje spustitelný soubor,
// narozdíl od knihovny
package main

// Importní deklarace říkají, které knihovny budou použity v tomto souboru.
import (
	"fmt"       // Obsahuje formátovací funkce a tisk na konzolu
	"io/ioutil" // Vstupně/výstupní funkce
	m "math"    // Odkaz na knihovnu math (matematické funkce) pod zkratkou m
	"net/http"  // Podpora http protokolu, klient i server.
	"strconv"   // Konverze řetězců, např. na čísla a zpět.
)

// Definice funkce. Funkce main je zvláštní, je to vstupní bod do programu.
// Ať se vám to líbí, nebo ne, Go používá složené závorky
func main() {
	// Println vypisuje na stdout.
	// Musí být kvalifikováno jménem svého balíčko, ftm.
	fmt.Println("Hello world!")

	// Zavoláme další funkci
	svetPoHello()
}

// Funkce mají své parametry v závorkách
// Pokud funkce nemá parametry, tak musíme stejně závorky uvést.
func svetPoHello() {
	var x int // Deklarace proměnné. Proměnné musí být před použitím deklarované
	x = 3     // Přiřazení hodnoty do proměnné
	// Existuje "krátká" deklarace := kde se typ proměnné odvodí, 
	// proměnná vytvoří a přiřadí se jí hodnota
	y := 4
	sum, prod := naucSeNasobit(x, y)        // Funkce mohou vracet více hodnot
	fmt.Println("sum:", sum, "prod:", prod) // Jednoduchý výstup
	naucSeTypy()                            // < y minut je za námi, je čas učit se víc!
}

/* <- začátek mnohořádkového komentáře
Funkce mohou mít parametry a (několik) návratových hodnot.
V tomto případě jsou `x`, `y` parametry a `sum`, `prod` jsou návratové hodnoty.
Všiměte si, že `x` a `sum` jsou typu `int`.
*/
func naucSeNasobit(x, y int) (sum, prod int) {
	return x + y, x * y // Vracíme dvě hodnoty
}

// zabudované typy a literáty.
func naucSeTypy() {
	// Krátká deklarace většinou funguje
	str := "Learn Go!" // typ řetězec.

	s2 := `"surový" literát řetězce
může obsahovat nové řádky` // Opět typ řetězec.

	// Můžeme použít ne ASCII znaky, Go používá UTF-8.
	g := 'Σ' // type runa, což je alias na int32 a ukládá se do něj znak UTF-8

	f := 3.14195 // float64, je IEEE-754 64-bit číslem s plovoucí čárkou.
	c := 3 + 4i  // complex128, interně uložené jako dva float64.

	// takhle vypadá var s inicializací
	var u uint = 7 // Číslo bez znaménka, jehož velikost záleží na implementaci,
	               // stejně jako int
	var pi float32 = 22. / 7

	// takto se převádí typy za pomoci krátké syntaxe
	n := byte('\n') // byte je jiné jméno pro uint8.

	// Pole mají fixní délku, které se určuje v době kompilace.
	var a4 [4]int           // Pole 4 intů, všechny nastaveny na 0.
	a3 := [...]int{3, 1, 5} // Pole nastaveno na tři hodnoty
	// elementy mají hodntu 3, 1 a 5

	// Slicy mají dynamickou velikost. Pole i slacy mají své výhody,
	// ale většinou se používají slicy.
	s3 := []int{4, 5, 9}    // Podobně jako a3, ale není tu výpustka.
	s4 := make([]int, 4)    // Alokuj slice 4 intů, všechny nastaveny na 0.
	var d2 [][]float64      // Deklarace slicu, nic se nealokuje.
	bs := []byte("a slice") // Přetypování na slice

	// Protože jsou dynamické, můžeme ke slicům přidávat za běhu
	// Přidat ke slicu můžeme pomocí zabudované funkce append().
	// Prvním parametrem je slice, návratová hodnota je aktualizovaný slice.
	s := []int{1, 2, 3}		// Výsledkem je slice se 3 elementy.
	s = append(s, 4, 5, 6)	// Přidány další 3 elementy. Slice má teď velikost 6.
	fmt.Println(s) // Slice má hodnoty [1 2 3 4 5 6]

	// Pokud chceme k poli přičíst jiné pole, můžeme předat referenci na slice,
	// nebo jeho literát a přidat výpustku, čímž se slicu "rozbalí" a přidá se k
	// původnímu slicu.
	s = append(s, []int{7, 8, 9}...) // druhým parametrem je literát slicu.
	fmt.Println(s)	// slice má teď hodnoty [1 2 3 4 5 6 7 8 9]

	p, q := naucSePraciSPameti() // Deklarujeme p a q jako typ pointer na int.
	fmt.Println(*p, *q)   // * dereferencuje pointer. Tím se vypíší dva inty.

	// Mapy jsou dynamické rostoucí asociativní pole, jako hashmapa, nebo slovník
	// (dictionary) v jiných jazycích
	m := map[string]int{"tri": 3, "ctyri": 4}
	m["jedna"] = 1

	// Napoužité proměnné jsou v Go chybou.
	// Použijte podtržítko, abychom proměnno "použili".
	_, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs
	// Výpis promenné se počítá jako použití.
	fmt.Println(s, c, a4, s3, d2, m)

	naucSeVetveníProgramu() // Zpátky do běhu.
}

// narozdíl od jiných jazyků, v Go je možné mít pojmenované návratové hodnoty.
// Tak můžeme vracet hodnoty z mnoha míst funkce, aniž bychom uváděli hodnoty v
// return.
func naucSePojmenovaneNavraty(x, y int) (z int) {
	z = x * y
	return // z je zde implicitní, jelikož bylo pojmenováno.
}

// Go má garbage collector. Používá pointery, ale neumožňuje jejich aritmetiku.
// Můžete tedy udělat chybu použitím nil odkazu, ale ne jeho posunutím.
func naucSePraciSPameti() (p, q *int) {
	// Pojmenované parametry p a q mají typ odkaz na int.
	p = new(int) // Zabudované funkce new alokuje paměť.
	// Alokované místo pro int má hodnotu 0 a p už není nil.
	s := make([]int, 20) // Alokujeme paměť pro 20 intů.
	s[3] = 7             // Jednu z nich nastavíme.
	r := -2              // Deklarujeme další lokální proměnnou.
	return &s[3], &r     // a vezmeme si jejich odkaz pomocí &.
}

func narocnyVypocet() float64 {
	return m.Exp(10)
}

func naucSeVetveníProgramu() {
	// Výraz if vyžaduje složené závorky, ale podmínka nemusí být v závorkách.
	if true {
		fmt.Println("říkal jsme ti to")
	}
	// Formátování je standardizované pomocí utility "go fmt".
	if false {
		// posměšek.
	} else {
		// úšklebek.
	}
	// Použij switch, když chceš zřetězit if.
	x := 42.0
	switch x {
	case 0:
	case 1:
	case 42:
		// jednotlivé case nepropadávají. není potřeba "break"
	case 43:
		// nedosažitelné, jelikož už bylo ošetřeno.
	default:
		// implicitní větev je nepovinná.
	}
	// Stejně jako if, for (smyčka) nepoužívá závorky.
	// Proměnné definované ve for jsou lokální vůči smyčce.
	for x := 0; x < 3; x++ { // ++ je výrazem.
		fmt.Println("iterace", x)
	}
	// zde je x == 42.

	// For je jediná smyčka v Go, ale má několik tvarů.
	for { // Nekonečná smyčka
		break    // Dělám si legraci
		continue // Sem se nedostaneme
	}

	// Můžete použít klíčové slovo range pro iteraci nad mapami, poli, slicy,
	// řetězci a kanály.
	// range vrací jednu (kanál) nebo dvě hodnoty (pole, slice, řetězec a mapa).
	for key, value := range map[string]int{"jedna": 1, "dva": 2, "tri": 3} {
		// pro každý pár (klíč a hodnota) je vypiš
		fmt.Printf("klíč=%s, hodnota=%d\n", key, value)
	}

	// stejně jako for, := v podmínce if přiřazuje hodnotu
	// nejříve nastavíme y a pak otestujeme, jestli je y větší než x.
	if y := narocnyVypocet(); y > x {
		x = y
	}
	// Funkční literáty jsou tzv. uzávěry (closure)
	xBig := func() bool {
		return x > 10000 // odkazuje na x deklarované ve příkladu použití switch
	}
	x = 99999
	fmt.Println("xBig:", xBig()) // true
	x = 1.3e3                    // To udělá z x == 1300
	fmt.Println("xBig:", xBig()) // teď už false.

	// Dále je možné funkční literáty definovat a volat na místě jako parametr
	// funkce, dokavaď:
	// a) funkční literát je okamžitě volán pomocí (),
	// b) výsledek se shoduje s očekávaným typem.
	fmt.Println("Sečte + vynásobí dvě čísla: ",
		func(a, b int) int {
			return (a + b) * 2
		}(10, 2)) // Voláno s parametry 10 a 2
	// => Sečti a vynásob dvě čísla. 24

	// Když to potřebujete, tak to milujete
	goto miluji
miluji:

	naučteSeFunkčníFactory() // funkce vracející funkce je zábava(3)(3)
	naučteSeDefer()      // malá zajížďka k důležitému klíčovému slovu.
	naučteSeInterfacy() // Přichází dobré věci!
}

func naučteSeFunkčníFactory() {
	// Následující dvě varianty jsou stejné, ale ta druhá je praktičtější
	fmt.Println(větaFactory("létní")("Hezký", "den!"))

	d := větaFactory("letní")
	fmt.Println(d("Hezký", "den!"))
	fmt.Println(d("Líný", "odpoledne!"))
}

// Dekorátory jsou běžné v jiných jazycích. To samé můžete udělat v Go
// pomocí parameterizovatelných funkčních literátů.
func větaFactory(můjŘetězec string) func(před, po string) string {
	return func(před, po string) string {
		return fmt.Sprintf("%s %s %s", před, můjŘetězec, po) // nový řetězec
	}
}

func naučteSeDefer() (ok bool) {
	// Odloží (defer) příkazy na okamžik těsně před opuštěním funkce.
	// tedy poslední se provede první
	defer fmt.Println("odložené příkazy jsou zpravovaná v LIFO pořadí.")
	defer fmt.Println("\nProto je tato řádka vytištěna první")
	// Defer se běžně používá k zavírání souborů a tím se zajistí, že soubor
	// bude po ukončení funkce zavřen.
	return true
}

// definuje typ interfacu s jednou metodou String()
type Stringer interface {
	String() string
}

// Definuje pár jako strukturu se dvěma poli typu int x a y.
type pár struct {
	x, y int
}

// Definuje method pár. Pár tedy implementuje interface Stringer.
func (p pár) String() string { // p je tu nazýváno "Receiver" - přijímač
	// Sprintf je další veřejná funkce z balíčku fmt.
	// Pomocí tečky přistupujeme k polím proměnné p
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func naučteSeInterfacy() {
	// Složené závorky jsou "strukturální literáty. Vyhodnotí a inicializuje
	// strukturu. Syntaxe := deklaruje a inicializuje strukturu.
	p := pár{3, 4}
	fmt.Println(p.String()) // Volá metodu String na p typu pár.
	var i Stringer          // Deklaruje i jako proměnné typu Stringer.
	i = p                   // Toto je možné, jelikož oba implementují Stringer
	// zavolá metodu String(( typu Stringer a vytiskne to samé jako předchozí.
	fmt.Println(i.String())

	// Funkce ve balíčku fmt volají metodu String, když zjišťují, jak se má typ
	// vytisknout.
	fmt.Println(p) // Vytiskne to samé, jelikož Println volá String().
	fmt.Println(i) // Ten samý výstup.

	naučSeVariabilníParametry("super", "učit se", "tady!")
}

// Funcke mohou mít proměnlivé množství parametrů.
func naučSeVariabilníParametry(mojeŘetězce ...interface{}) {
	// Iterujeme přes všechny parametry
	// Potržítku tu slouží k ignorování indexu v poli.
	for _, param := range mojeŘetězce {
		fmt.Println("parameter:", param)
	}

	// Použít variadický parametr jako variadický parametr, nikoliv pole.
	fmt.Println("parametery:", fmt.Sprintln(mojeŘetězce...))

	naučSeOšetřovatChyby()
}

func naučSeOšetřovatChyby() {
	// ", ok" je metodou na zjištění, jestli něco fungovalo, nebo ne.
	m := map[int]string{3: "tri", 4: "ctyri"}
	if x, ok := m[1]; !ok { // ok bude false, jelikož 1 není v mapě.
		fmt.Println("není tu jedna")
	} else {
		fmt.Print(x) // x by bylo tou hodnotou, pokud by bylo v mapě.
	}
	// hodnota error není jen znamením OK, ale může říct více o chybě.
	if _, err := strconv.Atoi("ne-int"); err != nil { // _ hodnotu zahodíme
		// vytiskne 'strconv.ParseInt: parsing "non-int": invalid syntax'
		fmt.Println(err)
	}
	// Znovu si povíme o interfacech, zatím se podíváme na
	naučSeKonkurenčnost()
}

// c je kanál, způsob, jak bezpečně komunikovat v konkurenčním prostředí.
func zvyš(i int, c chan int) {
	c <- i + 1 // <- znamená "pošli" a posílá data do kanálu na levé straně.
}

// Použijeme funkci zvyš a konkurečně budeme zvyšovat čísla.
func naučSeKonkurenčnost() {
	// funkci make jsme již použili na slicy. make alokuje a inicializuje slidy,
	// mapy a kanály.
	c := make(chan int)
	// nastartuj tři konkurenční go-rutiny. Čísla se budou zvyšovat
	// pravděpodobně paralelně pokud je počítač takto nakonfigurován.
	// Všechny tři zapisují do toho samého kanálu.
	go zvyš(0, c) // go je výraz pro start nové go-rutiny.
	go zvyš(10, c)
	go zvyš(-805, c)
	// Přečteme si tři výsledky a vytiskeneme je..
	// Nemůžeme říct, v jakém pořadí výsledky přijdou!
	fmt.Println(<-c, <-c, <-c) // pokud je kanál na pravo, jedná se o "přijmi".

	cs := make(chan string)       // Další kanál, tentokrát pro řetězce.
	ccs := make(chan chan string) // Kanál kanálu řetězců.
	go func() { c <- 84 }()       // Start nové go-rutiny na posílání hodnot.
	go func() { cs <- "wordy" }() // To samé s cs.
	// Select má syntaxi jako switch, ale vztahuje se k operacím nad kanály.
	// Náhodně vybere jeden case, který je připraven na komunikaci.
	select {
        case i := <-c: // Přijatá hodnota může být přiřazena proměnné.
            fmt.Printf("je to typ %T", i)
        case <-cs: // nebo může být zahozena
            fmt.Println("je to řetězec")
        case <-ccs: // prázdný kanál, nepřipraven ke komunikaci.
      		fmt.Println("to se nestane.")
	}
	// V tomto okamžiku máme hodnotu buď z kanálu c nabo cs. Jedna nebo druhá
	// nastartovaná go-rutina skončila a další zůstane blokovaná.

	naučSeProgramovatWeb() // Go to umí. A vy to chcete taky.
}

// jen jedna funkce z balíčku http spustí web server.
func naučSeProgramovatWeb() {

	// První parametr ListenAndServe je TCP adresa, kde poslouchat.
	// Druhý parametr je handler, implementující interace http.Handler.
	go func() {
		err := http.ListenAndServe(":8080", pár{})
		fmt.Println(err) // neignoruj chyby
	}()

	requestServer()
}

// Umožní typ pár stát se http tím, že implementuje její jedinou metodu
// ServeHTTP.
func (p pár) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	// Servíruj data metodou http.ResponseWriter
	w.Write([]byte("Naučil ses Go za y minut!"))
}

func requestServer() {
	resp, err := http.Get("http://localhost:8080")
	fmt.Println(err)
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Printf("\nWebserver řekl: `%s`", string(body))
}
```

## Kam dále

Vše hlavní o Go se nachází na [oficiálních stránkách go](http://golang.org/).
Tam najdete tutoriály, interaktivní konzolu a mnoho materiálu ke čtení.
Kromě úvodu, [dokumenty](https://golang.org/doc/) tam obsahují jak psát čistý kód v Go
popis balíčků (package), dokumentaci příkazové řádky a historii releasů.

Také doporučujeme přečíst si definici jazyka. Je čtivá a překvapivě krátká. Tedy alespoň proti
jiným současným jazyků.

Pokud si chcete pohrát s Go, tak navštivte [hřiště Go](https://play.golang.org/p/r46YvCu-XX).
Můžete tam spouštět programy s prohlížeče. Také můžete [https://play.golang.org](https://play.golang.org) použít jako
[REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop), kde si v rychlosti vyzkoušíte věci, bez instalace Go.

Na vašem knižním seznamu, by neměly chybět [zdrojáky stadardní knihovny](http://golang.org/src/pkg/).
Důkladně popisuje a dokumentuje Go, styl zápisu Go a Go idiomy. Pokud kliknete na [dokumentaci](http://golang.org/pkg/)
tak se podíváte na dokumentaci.

Dalším dobrým zdrojem informací je [Go v ukázkách](https://gobyexample.com/).

Go mobile přidává podporu pro Android a iOS. Můžete s ním psát nativní mobilní aplikace nebo knihovny, které půjdou
spustit přes Javu (pro Android), nebo Objective-C (pro iOS). Navštivte [web Go Mobile](https://github.com/golang/go/wiki/Mobile)
pro více informací.
