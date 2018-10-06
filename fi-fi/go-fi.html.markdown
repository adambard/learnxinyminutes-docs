---
name: Go
category: language
language: Go
filename: learngo-fi.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Christopher Bess", "https://github.com/cbess"]
    - ["Jesse Johnson", "https://github.com/holocronweaver"]
    - ["Quint Guvernator", "https://github.com/qguv"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
    - ["Alexej Friesen", "https://github.com/heyalexej"]
    - ["Clayton Walker", "https://github.com/cwalk"]
translators:
    - ["Timo Virkkunen", "https://github.com/ComSecNinja"]
lang: fi-fi
---

Go luotiin työn tekemistä varten. Se ei ole tietojenkäsittelyn uusin trendi,
mutta se on uusin nopein tapa ratkaista oikean maailman ongelmia.

Sillä on staattisesti tyypitetyistä imperatiivisista kielistä tuttuja
konsepteja. Se kääntyy ja suorittuu nopeasti, lisää helposti käsitettävän
samanaikaisten komentojen suorittamisen nykyaikaisten moniytimisten
prosessoreiden hyödyntämiseksi ja antaa käyttäjälle ominaisuuksia suurten
projektien käsittelemiseksi.

Go tuo mukanaan loistavan oletuskirjaston sekä innokkaan yhteisön.  

```go
// Yhden rivin kommentti
/* Useamman
 rivin kommentti */

// Package -lausekkeella aloitetaan jokainen lähdekooditiedosto.
// Main on erityinen nimi joka ilmoittaa
// suoritettavan tiedoston kirjaston sijasta.
package main

// Import -lauseke ilmoittaa tässä tiedostossa käytetyt kirjastot.
import (
	"fmt"       // Paketti Go:n oletuskirjastosta.
	"io/ioutil" // Implementoi hyödyllisiä I/O -funktioita.
	m "math"    // Matematiikkakirjasto jolla on paikallinen nimi m.
	"net/http"  // Kyllä, web-palvelin!
	"strconv"   // Kirjainjonojen muuntajia.
)

// Funktion määrittelijä. Main on erityinen: se on ohjelman suorittamisen
// aloittamisen alkupiste. Rakasta tai vihaa sitä, Go käyttää aaltosulkeita.
func main() {
    // Println tulostaa rivin stdoutiin.
	// Se tulee paketin fmt mukana, joten paketin nimi on mainittava.
	fmt.Println("Hei maailma!")

    // Kutsu toista funktiota tämän paketin sisällä.
	beyondHello()
}

// Funktioilla voi olla parametrejä sulkeissa.
// Vaikkei parametrejä olisikaan, sulkeet ovat silti pakolliset.
func beyondHello() {
	var x int // Muuttujan ilmoittaminen: ne täytyy ilmoittaa ennen käyttöä.
	x = 3     // Arvon antaminen muuttujalle.
    // "Lyhyet" ilmoitukset käyttävät := joka päättelee tyypin, ilmoittaa
    // sekä antaa arvon muuttujalle.
	y := 4
	sum, prod := learnMultiple(x, y)          // Funktio palauttaa kaksi arvoa.
	fmt.Println("summa:", sum, "tulo:", prod) // Yksinkertainen tuloste.
	learnTypes()                              // < y minuuttia, opi lisää!
}

/* <- usean rivin kommentti
Funktioilla voi olla parametrejä ja (useita!) palautusarvoja.
Tässä `x`, `y` ovat argumenttejä ja `sum`, `prod` ovat ne, mitä palautetaan.
Huomaa että `x` ja `sum` saavat tyyin `int`.
*/
func learnMultiple(x, y int) (sum, prod int) {
	return x + y, x * y // Palauta kaksi arvoa.
}

// Sisäänrakennettuja tyyppejä ja todellisarvoja.
func learnTypes() {
    // Lyhyt ilmoitus antaa yleensä haluamasi.
	str := "Opi Go!" // merkkijonotyyppi.

	s2 := `"raaka" todellisarvoinen merkkijono
voi sisältää rivinvaihtoja.` // Sama merkkijonotyyppi.

    // Ei-ASCII todellisarvo. Go-lähdekoodi on UTF-8.
	g := 'Σ' // riimutyyppi, lempinimi int32:lle, sisältää unicode-koodipisteen.

	f := 3.14195 //float64, IEEE-754 64-bittinen liukuluku.
	c := 3 + 4i  // complex128, sisäisesti ilmaistu kahdella float64:lla.

	// var -syntaksi alkuarvoilla.
	var u uint = 7 // Etumerkitön, toteutus riippuvainen koosta kuten int.
	var pi float32 = 22. / 7

    // Muuntosyntaksi lyhyellä ilmoituksella.
	n := byte('\n') // byte on leminimi uint8:lle.

    // Listoilla on kiinteä koko kääntöhetkellä.
	var a4 [4]int           // 4 int:in lista, alkiot ovat alustettu nolliksi.
	a3 := [...]int{3, 1, 5} // Listan alustaja jonka kiinteäksi kooksi tulee 3
	// alkiota, jotka saavat arvot 3, 1, ja 5.

    // Siivuilla on muuttuva koko. Sekä listoilla että siivuilla on puolensa,
    // mutta siivut ovat yleisempiä käyttötapojensa vuoksi.
	s3 := []int{4, 5, 9}    // Vertaa a3: ei sananheittoa (...).
	s4 := make([]int, 4)    // Varaa 4 int:n siivun, alkiot alustettu nolliksi.
	var d2 [][]float64      // Vain ilmoitus, muistia ei varata.
	bs := []byte("a slice") // Tyypinmuuntosyntaksi.

    // Koska siivut ovat dynaamisia, niitä voidaan yhdistellä sellaisinaan.
    // Lisätäksesi alkioita siivuun, käytä sisäänrakennettua append()-funktiota.
	// Ensimmäinen argumentti on siivu, johon alkoita lisätään.
	s := []int{1, 2, 3}		// Tuloksena on kolmen alkion pituinen lista.
	s = append(s, 4, 5, 6)	// Lisätty kolme alkiota. Siivun pituudeksi tulee 6.
	fmt.Println(s) // Päivitetty siivu on nyt [1 2 3 4 5 6]

    // Lisätäksesi siivun toiseen voit antaa append-funktiolle referenssin
    // siivuun tai todellisarvoiseen siivuun lisäämällä sanaheiton argumentin
    // perään. Tämä tapa purkaa siivun alkiot ja lisää ne siivuun s.
	s = append(s, []int{7, 8, 9}...) // 2. argumentti on todellisarvoinen siivu.
	fmt.Println(s)	// Päivitetty siivu on nyt [1 2 3 4 5 6 7 8 9]

	p, q := learnMemory() // Ilmoittaa p ja q olevan tyyppiä osoittaja int:iin.
	fmt.Println(*p, *q)   // * seuraa osoittajaa. Tämä tulostaa kaksi int:ä.

    // Kartat ovat dynaamisesti kasvavia assosiatiivisia listoja, kuten hash tai
    // dictionary toisissa kielissä.
	m := map[string]int{"three": 3, "four": 4}
	m["one"] = 1

    // Käyttämättömät muuttujat ovat virheitä Go:ssa.
    // Alaviiva antaa sinun "käyttää" muuttujan mutta hylätä sen arvon.
	_, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs
	// Tulostaminen tietysti lasketaan muuttujan käyttämiseksi.
	fmt.Println(s, c, a4, s3, d2, m)

	learnFlowControl() // Takaisin flowiin.
}

// Go:ssa on useista muista kielistä poiketen mahdollista käyttää nimettyjä
// palautusarvoja.
// Nimen antaminen palautettavan arvon tyypille funktion ilmoitusrivillä
// mahdollistaa helpon palaamisen useasta eri funktion suorituskohdasta sekä
// pelkän return-lausekkeen käytön ilman muita mainintoja.
func learnNamedReturns(x, y int) (z int) {
	z = x * y
	return // z on epäsuorasti tässä, koska nimesimme sen aiemmin.
}

// Go kerää kaikki roskansa. Siinä on osoittajia mutta ei niiden laskentoa.
// Voit tehdä virheen mitättömällä osoittajalla, mutta et
// kasvattamalla osoittajaa.
func learnMemory() (p, q *int) {
    // Nimetyillä palautusarvoilla p ja q on tyyppi osoittaja int:iin.
	p = new(int) // Sisäänrakennettu funktio new varaa muistia.
    // Varattu int on alustettu nollaksi, p ei ole enää mitätön.
	s := make([]int, 20) // Varaa 20 int:ä yhteen kohtaan muistissa.
	s[3] = 7             // Anna yhdelle niistä arvo.
	r := -2              // Ilmoita toinen paikallinen muuttuja.
	return &s[3], &r     // & ottaa asian osoitteen muistissa.
}

func expensiveComputation() float64 {
	return m.Exp(10)
}

func learnFlowControl() {
    // If -lausekkeet vaativat aaltosulkeet mutta ei tavallisia sulkeita.
	if true {
		fmt.Println("mitä mä sanoin")
	}
	// Muotoilu on standardoitu käyttämällä komentorivin komentoa "go fmt".
	if false {
		// Nyrpistys.
	} else {
		// Nautinto.
	}
	// Käytä switch -lauseketta ketjutettujen if -lausekkeiden sijasta.
	x := 42.0
	switch x {
	case 0:
	case 1:
	case 42:
		// Tapaukset eivät "tipu läpi".
		/*
		Kuitenkin meillä on erikseen `fallthrough` -avainsana. Katso:
		  https://github.com/golang/go/wiki/Switch#fall-through
		*/
	case 43:
		// Saavuttamaton.
	default:
		// Oletustapaus (default) on valinnainen.
	}
    // Kuten if, for -lauseke ei myöskään käytä tavallisia sulkeita.
	// for- ja if- lausekkeissa ilmoitetut muuttujat ovat paikallisia niiden
    // piireissä.
	for x := 0; x < 3; x++ { // ++ on lauseke. Sama kuin "x = x + 1".
		fmt.Println("iteraatio", x)
	}
	// x == 42 tässä.

    // For on kielen ainoa silmukkalauseke mutta sillä on vaihtoehtosia muotoja.
	for { // Päättymätön silmukka.
		break    // Kunhan vitsailin.
		continue // Saavuttamaton.
	}

    // Voit käyttää range -lauseketta iteroidaksesi listojen, siivujen, merkki-
    // jonojen, karttojen tai kanavien läpi. range palauttaa yhden (kanava) tai
    // kaksi arvoa (lista, siivu, merkkijono ja kartta).
	for key, value := range map[string]int{"yksi": 1, "kaksi": 2, "kolme": 3} {
        // jokaista kartan paria kohden, tulosta avain ja arvo
		fmt.Printf("avain=%s, arvo=%d\n", key, value)
	}

    // Kuten for -lausekkeessa := if -lausekkeessa tarkoittaa ilmoittamista ja
    // arvon asettamista.
    // Aseta ensin y, sitten testaa onko y > x.
	if y := expensiveComputation(); y > x {
		x = y
	}
    // Todellisarvoiset funktiot ovat sulkeumia.
	xBig := func() bool {
		return x > 10000 // Viittaa ylempänä ilmoitettuun x:ään.
	}
	fmt.Println("xBig:", xBig()) // tosi (viimeisin arvo on e^10).
	x = 1.3e3                    // Tämä tekee x == 1300
	fmt.Println("xBig:", xBig()) // epätosi nyt.

    // Lisäksi todellisarvoiset funktiot voidaan samalla sekä ilmoittaa että
    // kutsua, jolloin niitä voidaan käyttää funtioiden argumentteina kunhan:
    // a) todellisarvoinen funktio kutsutaan välittömästi (),
    // b) palautettu tyyppi vastaa odotettua argumentin tyyppiä.
	fmt.Println("Lisää ja tuplaa kaksi numeroa: ",
		func(a, b int) int {
			return (a + b) * 2
		}(10, 2)) // Kutsuttu argumenteilla 10 ja 2
	// => Lisää ja tuplaa kaksi numeroa: 24

	// Kun tarvitset sitä, rakastat sitä.
	goto love
love:

	learnFunctionFactory() // Funktioita palauttavat funktiot
	learnDefer()      // Nopea kiertoreitti tärkeään avainsanaan.
	learnInterfaces() // Hyvää kamaa tulossa!
}

func learnFunctionFactory() {
    // Seuraavat kaksi ovat vastaavia, mutta toinen on käytännöllisempi
	fmt.Println(sentenceFactory("kesä")("Kaunis", "päivä!"))

	d := sentenceFactory("kesä")
	fmt.Println(d("Kaunis", "päivä!"))
	fmt.Println(d("Laiska", "iltapäivä!"))
}

// Somisteet ovat yleisiä toisissa kielissä. Sama saavutetaan Go:ssa käyttämällä
// todellisarvoisia funktioita jotka ottavat vastaan argumentteja.
func sentenceFactory(mystring string) func(before, after string) string {
	return func(before, after string) string {
		return fmt.Sprintf("%s %s %s", before, mystring, after) // uusi jono
	}
}

func learnDefer() (ok bool) {
    // Lykätyt lausekkeet suoritetaan juuri ennen funktiosta palaamista.
	defer fmt.Println("lykätyt lausekkeet suorittuvat")
    defer fmt.Println("käänteisessä järjestyksessä (LIFO).")
	defer fmt.Println("\nTämä rivi tulostuu ensin, koska")
    // Defer -lauseketta käytetään yleisesti tiedoston sulkemiseksi, jotta
    // tiedoston sulkeva funktio pysyy lähellä sen avannutta funktiota.
	return true
}

// Määrittele Stringer rajapintatyypiksi jolla on
// yksi jäsenfunktio eli metodi, String.
type Stringer interface {
	String() string
}

// Määrittele pair rakenteeksi jossa on kaksi kenttää, x ja y tyyppiä int.
type pair struct {
	x, y int
}

// Määrittele jäsenfunktio pair:lle. Pair tyydyttää nyt Stringer -rajapinnan.
func (p pair) String() string { // p:tä kutsutaan nimellä "receiver"
    // Sprintf on toinen julkinen funktio paketissa fmt.
	// Pistesyntaksilla viitataan P:n kenttiin.
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // Aaltosuljesyntaksi on "todellisarvoinen rakenne". Se todentuu alustetuksi
    // rakenteeksi. := -syntaksi ilmoittaa ja alustaa p:n täksi rakenteeksi.
	p := pair{3, 4}
	fmt.Println(p.String()) // Kutsu p:n (tyyppiä pair) jäsenfunktiota String.
	var i Stringer          // Ilmoita i Stringer-rajapintatyypiksi.
	i = p                   // Pätevä koska pair tyydyttää rajapinnan Stringer.
    // Kutsu i:n (Stringer) jäsenfunktiota String. Tuloste on sama kuin yllä.
	fmt.Println(i.String())

    // Funktiot fmt-paketissa kutsuvat argumenttien String-jäsenfunktiota
    // selvittääkseen onko niistä saatavilla tulostettavaa vastinetta.
	fmt.Println(p) // Tuloste on sama kuin yllä. Println kutsuu String-metodia.
	fmt.Println(i) // Tuloste on sama kuin yllä.

	learnVariadicParams("loistavaa", "oppimista", "täällä!")
}

// Funktioilla voi olla muuttuva eli variteettinen
// määrä argumentteja eli parametrejä.
func learnVariadicParams(myStrings ...interface{}) {
    // Iteroi jokaisen argumentin läpi.
    // Tässä alaviivalla sivuutetaan argumenttilistan kunkin kohdan indeksi.
	for _, param := range myStrings {
		fmt.Println("param:", param)
	}

    // Luovuta variteettinen arvo variteettisena parametrinä.
	fmt.Println("params:", fmt.Sprintln(myStrings...))

	learnErrorHandling()
}

func learnErrorHandling() {
	// "; ok" -muotoa käytetään selvittääksemme toimiko jokin vai ei.
	m := map[int]string{3: "kolme", 4: "neljä"}
	if x, ok := m[1]; !ok { // ok on epätosi koska 1 ei ole kartassa.
		fmt.Println("ei ketään täällä")
	} else {
		fmt.Print(x) // x olisi arvo jos se olisi kartassa.
	}
    // Virhearvo voi kertoa muutakin ongelmasta.
	if _, err := strconv.Atoi("ei-luku"); err != nil { // _ sivuuttaa arvon
        // tulostaa strconv.ParseInt: parsing "ei-luku": invalid syntax
		fmt.Println(err)
	}
    // Palaamme rajapintoihin hieman myöhemmin. Sillä välin,
	learnConcurrency()
}

// c on kanava, samanaikaisturvallinen viestintäolio.
func inc(i int, c chan int) {
	c <- i + 1 // <- on "lähetysoperaattori" kun kanava on siitä vasemmalla.
}

// Käytämme inc -funktiota samanaikaiseen lukujen lisäämiseen.
func learnConcurrency() {
    // Sama make -funktio jota käytimme aikaisemmin siivun luomiseksi. Make
    // varaa muistin ja alustaa siivut, kartat ja kanavat.
	c := make(chan int)
    // Aloita kolme samanaikaista gorutiinia (goroutine). Luvut kasvavat
    // samanaikaisesti ja ehkäpä rinnakkain jos laite on kykenevä ja oikein
    // määritelty. Kaikki kolme lähettävät samalle kanavalle.
	go inc(0, c) // go -lauseke aloittaa uuden gorutiinin.
	go inc(10, c)
	go inc(-805, c)
    // Lue kolme palautusarvoa kanavalta ja tulosta ne.
    // Niiden saapumisjärjestystä ei voida taata!
    // <- on "vastaanotto-operaattori" jos kanava on oikealla
	fmt.Println(<-c, <-c, <-c)

	cs := make(chan string)       // Toinen kanava joka käsittelee merkkijonoja.
	ccs := make(chan chan string) // Kanava joka käsittelee merkkijonokanavia.
	go func() { c <- 84 }()       // Aloita uusi gorutiini arvon lähettämiseksi.
	go func() { cs <- "sanaa" }() // Uudestaan, mutta cs -kanava tällä kertaa.
    // Select -lausekkeella on syntaksi kuten switch -lausekkeella mutta
    // jokainen tapaus sisältää kanavaoperaation. Se valitsee satunnaisen
    // tapauksen niistä kanavista, jotka ovat kommunikaatiovalmiita
	select {
	case i := <-c: // Vastaanotettu arvo voidaan antaa muuttujalle
		fmt.Printf("se on %T", i)
	case <-cs: // tai vastaanotettu arvo voidaan sivuuttaa.
		fmt.Println("se on merkkijono")
	case <-ccs: // Tyhjä kanava; ei valmis kommunikaatioon.
		fmt.Println("ei tapahtunut.")
	}
    // Tässä vaiheessa arvo oli otettu joko c:ltä tai cs:ltä. Yksi kahdesta
    // ylempänä aloitetusta gorutiinista on valmistunut, toinen pysyy tukossa.

	learnWebProgramming() // Go tekee sitä. Sinäkin haluat tehdä sitä.
}

// Yksittäinen funktio http -paketista aloittaa web-palvelimen.
func learnWebProgramming() {

    // ListenAndServe:n ensimmäinen parametri on TCP-osoite, jota kuunnellaan.
    // Toinen parametri on rajapinta, http.Handler.
	go func() {
		err := http.ListenAndServe(":8080", pair{})
		fmt.Println(err) // älä sivuuta virheitä.
	}()

	requestServer()
}

// Tee pair:sta http.Handler implementoimalla sen ainoa metodi, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Tarjoa dataa metodilla http.ResponseWriter.
	w.Write([]byte("Opit Go:n Y minuutissa!"))
}

func requestServer() {
	resp, err := http.Get("http://localhost:8080")
	fmt.Println(err)
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Printf("\nWeb-palvelin sanoo: `%s`", string(body))
}
```

## Lisää luettavaa

Go-tietämyksen alku ja juuri on sen [virallinen verkkosivu]()(http://golang.org/).
Siellä voit seurata oppitunteja, askarrella vuorovaikutteisesti sekä lukea paljon.
Kierroksen lisäksi [dokumentaatio](https://golang.org/doc/) pitää sisällään tietoa
siistin Go-koodin kirjoittamisesta, pakettien ja komentojen käytöstä sekä julkaisuhistoriasta.

Kielen määritelmä itsessään on suuresti suositeltavissa. Se on helppolukuinen ja
yllättävän lyhyt (niissä määrin kuin kielimääritelmät nykypäivänä ovat.)

Voit askarrella parissa kanssa [Go playgroundissa](https://play.golang.org/p/tnWMjr16Mm).
Muuttele sitä ja aja se selaimestasi! Huomaa, että voit käyttää [https://play.golang.org](https://play.golang.org)
[REPL:na](https://en.wikipedia.org/wiki/Read-eval-print_loop) testataksesi ja koodataksesi selaimessasi, ilman Go:n asentamista.

Go:n opiskelijoiden lukulistalla on [oletuskirjaston lähdekoodi](http://golang.org/src/pkg/).
Kattavasti dokumentoituna se antaa parhaan kuvan helppolukuisesta ja ymmärrettävästä Go-koodista,
-tyylistä ja -tavoista. Voit klikata funktion nimeä [doukumentaatiossa](http://golang.org/pkg/) ja
lähdekoodi tulee esille!

Toinen loistava paikka oppia on [Go by example](https://gobyexample.com/).

Go Mobile lisää tuen mobiilialustoille (Android ja iOS). Voit kirjoittaa pelkällä Go:lla natiiveja applikaatioita tai tehdä kirjaston joka sisältää sidoksia
Go-paketista, jotka puolestaan voidaan kutsua Javasta (Android) ja Objective-C:stä (iOS). Katso [lisätietoja](https://github.com/golang/go/wiki/Mobile).
