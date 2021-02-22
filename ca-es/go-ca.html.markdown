---
name: Go
category: language
language: Go
lang: ca-es
filename: learngo-ca.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Christopher Bess", "https://github.com/cbess"]
    - ["Jesse Johnson", "https://github.com/holocronweaver"]
    - ["Quint Guvernator", "https://github.com/qguv"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
    - ["Alexej Friesen", "https://github.com/heyalexej"]
    - ["Clayton Walker", "https://github.com/cwalk"]
    - ["Leonid Shevtsov", "https://github.com/leonid-shevtsov"]
translators:
    - ["Xavier Sala", "http://github.com/utrescu"]
---

Go es va crear degut a la necessitat de fer la feina ràpidament. No segueix
la darrera tendència en informàtica, però és la nova forma i la més ràpida de
resoldre problemes reals.

Té conceptes familiars de llenguatges imperatius amb tipat estàtic. És ràpid
compilant i ràpid al executar, afegeix una forma fàcil d'entedre de
concurrència per CPUs de diferents núclis i té característiques que ajuden en
la programació a gran escala.

Go té una gran llibreria de funcions estàndard i una comunitat d'usuaris
entusiasta.

```go
// Comentari d'una sola línia
/* Comentari
multilínia */

// La clausula `package` apareix sempre a sobre de cada fitxer de codi font.
// Quan es desenvolupa un executable en comptes d'una llibreria el nom que
// s'ha de fer servir de `package` ha de ser 'main'.
package main

// `import` es fa servir per indicar quins paquets de llibreries fa servir
// aquest fitxer.
import (
	"fmt"       // Un paquet de la llibreria estàndard de Go.
	"io/ioutil" // Les funcions ioutil de io
	m "math"    // La llibreria de matemàtiques que es referenciarà com a m.
	"net/http"  // Si, un servidor web!
	"os"        // funcions per treballar amb el sistema de fitxers
	"strconv"   // Conversions de cadenes
)

// La definició d'una funció. `main` és especial. És el punt d'entrada per
// l'executable. Tant si t'agrada com si no, Go fa servir corxets.
func main() {
	// Println imprimeix una línia al canal de sortida.
	// Es qualifica amb el nom del paquet, fmt.
	fmt.Println("Hola món!")

	// Crida a una altra funció dins d'aquest paquet.
	mesEnllaDeHola()
}

// Els paràmetres de les funcions es posen dins de parèntesis.
// Els parèntesis fan falta encara que no hi hagi cap paràmetre.
func mesEnllaDeHola() {
	var x int // Declaració d'una variable.
	          // S'han de declarar abans de fer-les servir.
	x = 3     // Assignació d'una variable
	// Hi ha una forma "Curta" amb :=
	// Descobreix el tipus, declara la variable i li assigna valor.
	y := 4
	sum, prod := learnMultiple(x, y)        // La funció retorna dos valors.
	fmt.Println("sum:", sum, "prod:", prod) // Sortida simple.
	aprenTipus()                            // < y minuts, aprèn més!
}

/* <- comentari multilínia
Les funcions poden tenir paràmetres i (multiples!) valors de retorn.
Aquí `x`, `y` són els argumens i `sum` i `prod` són els valors retornats.
Fixa't que `x` i `sum` reben el tipus `int`.
*/
func learnMultiple(x, y int) (sum, prod int) {
	return x + y, x * y // Retorna dos valors.
}

// Alguns tipus incorporats i literals.
func aprenTipus() {
	// Normalment la declaració curta et dóna el que vols.
	str := "Learn Go!" // tipus string

	s2 := `Un tipus cadena "normal" pot tenir
salts de línia.` // El mateix tipus

	// literals Non-ASCII literal. El tipus de Go és UTF-8.
	g := 'Σ' // El tipus rune, és un àlies de int32 conté un caràcter unicode.

	f := 3.14195 // float64, un número de 64 bits amb coma flotant IEEE-754.
	c := 3 + 4i  // complex128, representat internament amb dos float64.

	// Sintaxi amb var i inicialitzadors.
	var u uint = 7 // Sense signe, però depèn de la mida com els int.
	var pi float32 = 22. / 7

	// Conversió de tipus amb declaració curta.
	n := byte('\n') // byte és un àlies de uint8.

	// Les taules tenen mida fixa en temps de compilació.
	var a4 [4]int           // Taula de 4 enters inicialitzats a zero.
	a3 := [...]int{3, 1, 5} // Taula inicialitzada amb tres elements
	// amb els valors 3, 1, i 5.

	// Els "Slices" tenen mida dinàmica. Tant les taules com els "slices"
	// tenen avantatges però és més habitual que es facin servir slices.
	s3 := []int{4, 5, 9}    // Compara amb a3. Aquí no hi ha els tres punts
	s4 := make([]int, 4)    // Crea un slice de 4 enters inicialitzats a zero.
	var d2 [][]float64      // Només es declara però no hi ha valors.
	bs := []byte("a slice") // Sintaxi de conversió de tipus.

	// Com que són dinàmics es poden afegir valors nous als slices.
	// Per afegir-hi elements es fa servir el mètode append().
	// El primer argument és l'slice en el que s'afegeix.
	// Sovint ell mateix com aquí sota.
	s := []int{1, 2, 3}		// Un slice amb tres elements.
	s = append(s, 4, 5, 6)	// Ara s tindrà tres elements més
	fmt.Println(s) // El resultat serà [1 2 3 4 5 6]

	// Per afegir un slice dins d'un altre en comptes de valors atòmics
	// S'hi pot passar una referència a l'altre slice o un literal acabat
	// amb tres punts, que vol dir que s'ha de desempaquetar els elements
	// i afegir-los a "s"
	s = append(s, []int{7, 8, 9}...) // El segon argument és un slice
	fmt.Println(s)	// El resultat ara és [1 2 3 4 5 6 7 8 9]

	p, q := aprenMemoria() // Declara p i q com a punters de int.
	fmt.Println(*p, *q)   // * segueix el punter fins a trobar els valors

	// Els "Mapes" són taules dinàmiques associatives com els hash o els
	// diccionaris d'altres llenguatges.
	m := map[string]int{"tres": 3, "quatre": 4}
	m["un"] = 1

	// En Go les variables que no es fan servir generen un error.
	// El subratllat permet fer servir una variable i descartar-ne el valor.
	_, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs
	// És útil per descartar algun dels valors retornats per una funció
	// Per exemple, es pot ignorar l'error retornat per os.Create amb la idea
	// de que sempre es crearà.
	file, _ := os.Create("output.txt")
	fmt.Fprint(file, "Així es pot escriure en un fitxer")
	file.Close()

	// La sortida compta com a ús d'una variable.
	fmt.Println(s, c, a4, s3, d2, m)

	aprenControlDeFluxe() // Tornem.
}

// A diferència d'altres llenguatges les funcions poden retornar valors amb
// nom. Assignant un nom al valor retornat en la declaració de la funció
// permet retornar valors des de diferents llocs del programa a més de posar
// el return sense valors
func aprenRetornAmbNoms(x, y int) (z int) {
	z = x * y
	return // el retorn de z és implícit perquè ja té valor
}

// Go té un recollidor de basura.
// Té punters però no té aritmetica de punters
// Es poden cometre errors amb un punter a nil però no incrementant-lo.
func aprenMemoria() (p, q *int) {
	// Els valors retornats p i q són punters a un enter.
	p = new(int) // Funció per reservar memòria
	// A la memòria ja hi ha un 0 per defecte, no és nil.
	s := make([]int, 20) // Reserva un bloc de memòria de 20 enters.
	s[3] = 7             // Assigna un valor a un d'ells.
	r := -2              // Declare una altra variable local.
	return &s[3], &r     // & agafa l'adreça d'un objecte.
}

func expensiveComputation() float64 {
	return m.Exp(10)
}

func aprenControlDeFluxe() {
	// Els "If" necessiten corxets però no parèntesis.
	if true {
		fmt.Println("ja ho hem vist")
	}
	// El format del codi està estandaritzat amb la comanda "go fmt."
	if false {
		// Pout.
	} else {
		// Gloat.
	}
	// Si cal encadenar ifs és millor fer servir switch.
	x := 42.0
	switch x {
	case 0:
	case 1:
	case 42:
		// Els case no "passen a través" no cal "break" per separar-los.
		/*
		Per fer-ho hi ha una comanda `fallthrough`, mireu:
		  https://github.com/golang/go/wiki/Switch#fall-through
		*/
	case 43:
		// No hi arriba.
	default:
		// La opció "default" és opcional
	}
	// El 'for' tampoc necessita parèntesis, com el 'if'.
	// Les variables dins d'un bloc if o for són local del bloc.
	for x := 0; x < 3; x++ { // ++ is a statement.
		fmt.Println("iteració", x)
	}
	// x == 42.

	// L'única forma de fer bucles en Go és el 'for' però té moltes variants.
	for { // bucle infinit.
		break    // És una broma!.
		continue // No hi arriba mai.
	}

	// Es fa servir "range" per iterar a una taula, un slice, un mapa
	// o un canal.
	// range torna un valor (channel) o dos (array, slice, string o map).
	for key, value := range map[string]int{"un": 1, "dos": 2, "tres": 3} {
		// Per cada parell del mapa imprimeix la clau i el valor.
		fmt.Printf("clau=%s, valor=%d\n", key, value)
	}
	// Si només cal el valor es pot fer servir _
	for _, name := range []string{"Robert", "Bill", "Josep"} {
		fmt.Printf("Hola, %s\n", name)
	}

	// Es pot usar := per declarar i assignar valors i després
	// comprovar-lo y > x.
	if y := expensiveComputation(); y > x {
		x = y
	}
	// Les funcions literals són closures
	xBig := func() bool {
		return x > 10000 // Referencia a x declarada sobre el switch.
	}
	x = 99999
	fmt.Println("xBig:", xBig()) // cert
	x = 1.3e3                    //  x val 1300
	fmt.Println("xBig:", xBig()) // fals.

	// A més les funcions poden ser definides i cridades com a arguments per
	// una funció sempre que:
	// a) La funció es cridi inmediatament (),
	// b) El tipus del resultat sigui del tipus esperat de l'argument.
	fmt.Println("Suma i duplica dos números: ",
		func(a, b int) int {
			return (a + b) * 2
		}(10, 2)) // Es crida amb els arguments 10 i 2
	// => Suma i duplica dos números: 24

	// Quan el necessitis t'agradarà que hi sigui
	goto love
love:

	aprenFabricaDeFuncions() // func que retorna func és divertit(3)(3)
	aprenDefer()      // Revisió ràpida d'una paraula clau.
	aprendreInterficies() // Bon material properament!
}

func aprenFabricaDeFuncions() {
	// Les dues seguents són equivalents, però la segona és més pràctica
	fmt.Println(sentenceFactory("dia")("Un bonic", "d'estiu!"))

	d := sentenceFactory("dia")
	fmt.Println(d("Un bonic", "d'estiu!"))
	fmt.Println(d("Un tranquil", "de primavera!"))
}

// Els decoradors són habituals en altres llenguatges.
// Es pot fer el mateix en Go amb funcions literals que accepten arguments.
func sentenceFactory(mystring string) func(before, after string) string {
	return func(before, after string) string {
		return fmt.Sprintf("%s %s %s", before, mystring, after) // nou string
	}
}

func aprenDefer() (ok bool) {
	// Les comandes marcades amb defer s'executen després de que la funció
	// hagi acabat.
	defer fmt.Println("Les comandes defer s'executen en ordre invers (LIFO).")
	defer fmt.Println("\nAquesta és la primera línia que s'imprimeix")
	// Defer es fa servir gairebé sempre per tancar un fitxer, en el moment
	// en que acaba el mètode.
	return true
}

// Defineix Stringer com un tipus interfície amb el mètode String().
type Stringer interface {
	String() string
}

// Defineix una estrutura que conté un parell d'enters, x i y.
type parell struct {
	x, y int
}

// Defineix un mètode de l'estructura parell. Ara parell implementa Stringer.
func (p parell) String() string { // p és anomenat el "receptor"
	// Sprintf és una funció del paquet fmt.
	// Fa referència als camps de p.
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func aprendreInterficies() {
	// La sintaxi de claus es pot fer servir per inicialitzar un "struct".
	// Gràcies a := defineix i inicialitza p com un struct 'parell'.
	p := parell{3, 4}
	fmt.Println(p.String()) // Es crida al mètode de p.
	var i Stringer          // Declara i de tipus Stringer.
	i = p                   // parell implementa Stringer per tant és vàlid.
	// Es pot cridar el mètode String() igual que abans.
	fmt.Println(i.String())

	// Les funcions de fmt criden a String() per aconseguir una representació
	// imprimible d'un objecte.
	fmt.Println(p) // Treu el mateix d'abans. Println crida el mètode String.
	fmt.Println(i) // Idèntic resultat

	aprendreParamentesVariables("Aquí", "estem", "aprenent!")
}

// Les funcions poden tenir paràmetres variables.
func aprendreParamentesVariables(myStrings ...interface{}) {
	// Itera per cada un dels valors dels paràmetres
	// Ignorant l'índex de la seva posició
	for _, param := range myStrings {
		fmt.Println("paràmetre:", param)
	}

	// Passa el valor de múltipes variables com a paràmetre.
	fmt.Println("parametres:", fmt.Sprintln(myStrings...))

	aprenControlErrors()
}

func aprenControlErrors() {
	// ", ok" Es fa servir per saber si hi és o no.
	m := map[int]string{3: "tres", 4: "quatre"}
	if x, ok := m[1]; !ok { // ok serà fals perquè 1 no està en el mapa.
		fmt.Println("no hi és")
	} else {
		fmt.Print(x) // x seria el valor, si no estés en el mapa.
	}
	// Un valor d'error donarà més informació sobre l'error.
	if _, err := strconv.Atoi("no-int"); err != nil { // _ descarta el valor
		// imprimeix 'strconv.ParseInt: intenta convertir "non-int":
		// syntaxi invalida'
		fmt.Println(err)
	}
	// Es tornarà a les interfícies més tard. Mentrestant,
	aprenConcurrencia()
}

// c és un canal (channel), una forma segura de comunicar objectes.
func inc(i int, c chan int) {
	c <- i + 1 // <- és l'operador "envia" quan un canal està a l'esquerra.
}

// Es pot fer servir inc per incrementar un número de forma concurrent.
func aprenConcurrencia() {
	// La funció make es pot fer servir per crear slices, mapes i canals.
	c := make(chan int)
	// S'inicien tres goroutines.
	// Els números s'incrementaran de forma concurrent, En paral·lel
	// si la màquina on s'executa pot fer-ho i està correctament configurada.
	// Tots tres envien al mateix canal.
	go inc(0, c) // go és la comanda que inicia una nova goroutine.
	go inc(10, c)
	go inc(-805, c)
	// Llegeix tres resultats del canal i els imprimeix.
	// No es pot saber en quin ordre arribaran els resultats!
	fmt.Println(<-c, <-c, <-c) // Canal a la dreta <- és l'operador "rebre"

	cs := make(chan string)       // Un altre canal que processa strings.
	ccs := make(chan chan string) // Un canal de canals string.
	go func() { c <- 84 }()       // Inicia una goroutine i li envia un valor.
	go func() { cs <- "paraula" }() // El mateix però amb cs.
	// Select té una sintaxi semblant a switch però amb canals.
	// Selecciona un cas aleatòriament dels que poden comunicar-se.
	select {
	case i := <-c: // El valor rebit pot ser assignat a una variable,
		fmt.Printf("és un %T", i)
	case <-cs: // O es pot descartar
		fmt.Println("és un string")
	case <-ccs: // Canal buit, no preparat per la comunicació.
		fmt.Println("no ha passat.")
	}
	// Quan arribi aquí s'haurà agafat un valor de c o bé de cs. Una de les
	// goroutines iniciades haurà acabat i l'altra romandrà bloquejada.

	aprenProgramacioWeb() // Go ho fa. Tu vols fer-ho.
}

// Una funció del paquet http inicia un servidor web.
func aprenProgramacioWeb() {

	// El primer paràmetre de ListenAndServe és l'adreça on escoltar
	// i el segon és una interfície http.Handler.
	go func() {
		err := http.ListenAndServe(":8080", pair{})
		fmt.Println(err) // no s'han d'ignorar els errors
	}()

	requestServer()
}

// Es converteix "parell" en un http.Handler només implementant el
// mètode ServeHTTP.
func (p parell) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	// Serveix dades en el http.ResponseWriter.
	w.Write([]byte("Has après Go en Y minuts!"))
}

func requestServer() {
	resp, err := http.Get("http://localhost:8080")
	fmt.Println(err)
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Printf("\nEl servidor diu: `%s`", string(body))
}
```

## Més informació

L'arrel de tot en Go és la web oficial [official Go web site]
(http://golang.org/). Allà es pot seguir el tutorial, jugar interactivament
i llegir molt més del que hem vist aquí.En el "tour",
[the docs](https://golang.org/doc/) conté informació sobre com escriure codi
net i efectiu en Go, comandes per empaquetar i generar documentació, i
història de les versions.

És altament recomanable llegir La definició del llenguatge. És fàcil de llegir
i sorprenentment curta (com la definició del llenguatge en aquests dies).

Es pot jugar amb codi a [Go playground](https://play.golang.org/p/tnWMjr16Mm).
Prova de fer canvis en el codi i executar-lo des del navegador! Es pot fer
servir [https://play.golang.org](https://play.golang.org) com a [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) per provar coses i codi
en el navegador sense haver d'instal·lar Go.

En la llista de lectures pels estudiants de Go hi ha
[el codi font de la llibreria estàndard](http://golang.org/src/pkg/).
Ampliament comentada, que demostra el fàcil que és de llegir i entendre els
programes en Go, l'estil de programació, i les formes de treballar-hi. O es
pot clicar en un nom de funció en [la documentació](http://golang.org/pkg/)
i veure'n el codi!

Un altre gran recurs per aprendre Go és
[Go by example](https://gobyexample.com/).

Go Mobile afegeix suport per plataformes mòbils (Android i iOS). Es poden
escriure aplicacions mòbils o escriure llibreries de paquets de Go, que es
poden cridar des de Java (android) i Objective-C (iOS).
Comproveu la [Go Mobile page](https://github.com/golang/go/wiki/Mobile) per
més informació.
