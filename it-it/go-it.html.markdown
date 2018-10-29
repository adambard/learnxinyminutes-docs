---
name: Go
language: Go
filename: learngo-it.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Christopher Bess", "https://github.com/cbess"]
    - ["Jesse Johnson", "https://github.com/holocronweaver"]
    - ["Quint Guvernator", "https://github.com/qguv"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
    - ["Alexej Friesen", "https://github.com/heyalexej"]
    - ["Clayton Walker", "https://github.com/cwalk"]
translators:
    - ["Tommaso Pifferi","http://github.com/neslinesli93"]
lang: it-it
---

Go è stato creato per avere tra le mani uno strumento in grado di arrivare
al punto, nel modo più veloce ed efficiente possibile. Non è all'ultima
moda tra i linguaggi di programmazione, ma è una delle migliori soluzioni
per risolvere in maniera efficace i problemi di tutti i giorni.

Go presenta alcuni concetti già presenti nei linguaggi imperativi con
tipizzazione statica. Compila velocemente ed esegue altrettanto veloce.
Aggiunge la concorrenza in maniera diretta e semplice da capire, per far
forza sulle CPU multi-core di oggigiorno. Presenta caratteristiche utili
per la programmazione in larga scala.

Go include un'ottima libreria standard e ha una community entusiasta.

```go
// Commento su riga singola
/* Commento
 su riga multipla */

// In cima ad ogni file è necessario specificare il package.
// Main è un package speciale che identifica un eseguibile anziché una libreria.
package main

// Con import sono dichiarate tutte le librerie a cui si fa riferimento 
// all'interno del file.
import (
	"fmt"       // Un package nella libreria standard di Go.
	"io/ioutil" // Implementa alcune funzioni di utility per l'I/O.
	m "math"    // Libreria matematica, con alias locale m
	"net/http"  // Sì, un web server!
	"strconv"   // Package per la conversione di stringhe.
)

// Una definizione di funzione. Il main è speciale: è il punto di ingresso
// per il programma. Amalo o odialo, ma Go usa le parentesi graffe.
func main() {
    // Println stampa una riga a schermo.
    // Questa funzione è all'interno del package fmt.
	fmt.Println("Ciao mondo!")

    // Chiama un'altra funzione all'interno di questo package.
	oltreIlCiaoMondo()
}

// Le funzioni ricevono i parametri all'interno di parentesi tonde.
// Se la funzione non riceve parametri, vanno comunque messe le parentesi (vuote).
func oltreIlCiaoMondo() {
	var x int // Dichiarazione di una variabile. Ricordati di dichiarare sempre le variabili prima di usarle!
	x = 3     // Assegnazione di una variabile.
    // E' possibile la dichiarazione "rapida" := per inferire il tipo, dichiarare e assegnare contemporaneamente.
	y := 4
    // Una funzione che restituisce due valori.
	somma, prod := imparaMoltepliciValoriRestituiti(x, y)
	fmt.Println("somma:", somma, "prodotto:", prod)    // Semplice output.
	imparaTipi()                                       // < y minuti, devi imparare ancora!
}

/* <- commento su righe multiple
Le funzioni possono avere parametri e restituire (molteplici!) valori.
In questo esempio, x e y sono gli argomenti, mentre somma e prod sono i valori restituiti.
Da notare il fatto che x e somma vengono dichiarati come interi.
*/
func imparaMoltepliciValoriRestituiti(x, y int) (somma, prod int) {
	return x + y, x * y // Restituisce due valori.
}

// Ecco alcuni tipi presenti in Go
func imparaTipi() {
    // La dichiarazione rapida di solito fa il suo lavoro.
	str := "Impara il Go!" // Tipo stringa.

	s2 := `Una stringa letterale
può includere andata a capo.` // Sempre di tipo stringa.

    // Stringa letterale non ASCII. I sorgenti Go sono in UTF-8.
	g := 'Σ' // Il tipo runa, alias per int32, è costituito da un code point unicode.

	f := 3.14195 // float64, un numero in virgola mobile a 64-bit (IEEE-754)

	c := 3 + 4i  // complex128, rappresentato internamente con due float64.

    // Inizializzare le variabili con var.
	var u uint = 7 // Senza segno, ma la dimensione dipende dall'implementazione (come l'int)
	var pi float32 = 22. / 7 

    // Sintassi per la conversione.
	n := byte('\n') // Il tipo byte è un alias per uint8.

    // I vettori hanno dimensione fissa, stabilita durante la compilazione.
	var a4 [4]int           // Un vettore di 4 interi, tutti inizializzati a 0.
	a3 := [...]int{3, 1, 5} // Un vettore inizializzato con una dimensione fissa pari a 3, i cui elementi sono 3, 1 e 5.

    // Gli slice hanno dimensione variabile. Vettori e slice hanno pro e contro,
    // ma generalmente si tende a usare più spesso gli slice.
	s3 := []int{4, 5, 9}    // La differenza con a3 è che qua non ci sono i 3 punti all'interno delle parentesi quadre.
	s4 := make([]int, 4)    // Alloca uno slice di 4 interi, tutti inizializzati a 0.
	var d2 [][]float64      // Semplice dichiarazione, non vengono fatte allocazioni.
	bs := []byte("uno slice") // Sintassi per la conversione.

    // Poiché gli slice sono dinamici, è possibile aggiungere elementi
    // quando è necessario. Per farlo, si usa la funzione append(). Il primo
    // argomento è lo slice a cui stiamo aggiungendo elementi. Di solito
    // lo slice viene aggiornato, senza fare una copia, come nell'esempio:
	s := []int{1, 2, 3}		// Il risultato è uno slice di dimensione 3.
	s = append(s, 4, 5, 6)	// Aggiunge 3 elementi: lo slice ha dimensione 6.
	fmt.Println(s) // Lo slice aggiornato è [1 2 3 4 5 6]
    // Per aggiungere un altro slice, invece che elencare gli elementi uno ad
    // uno, è possibile passare alla funzione append un riferimento ad uno
    // slice, oppure uno slice letterale: in questo caso si usano i tre punti,
    // dopo lo slice, a significare "prendi ciascun elemento dello slice":
	s = append(s, []int{7, 8, 9}...) // Il secondo argomento è uno slice letterale.
	fmt.Println(s)	// Lo slice aggiornato è [1 2 3 4 5 6 7 8 9]

	p, q := imparaLaMemoria() // Dichiara due puntatori a intero: p e q.
	fmt.Println(*p, *q)   // * dereferenzia un puntatore. Questo stampa due interi.

    // Una variabile di tipo map è un vettore associativo di dimensione variabile,
    // e funzionano come le tabelle di hash o i dizionari in altri linguaggi.
	m := map[string]int{"tre": 3, "quattro": 4}
	m["uno"] = 1

    // Le variabili dichiarate e non usate sono un errore in Go.
    // L'underscore permette di "usare" una variabile, scartandone il valore.
	_, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs
	// Stampare a schermo ovviamente significa usare una variabile.
	fmt.Println(s, c, a4, s3, d2, m)

	imparaControlloDiFlusso() // Torniamo in carreggiata.
}

// In Go è possibile associare dei nomi ai valori restituiti da una funzione.
// Assegnare un nome al tipo di dato restituito permette di fare return in vari
// punti all'interno del corpo della funzione, ma anche di usare return senza
// specificare in modo esplicito che cosa restituire.
func imparaValoriRestituitiConNome(x, y int) (z int) {
	z = x * y
	return // z è implicito, perchè compare nella definizione di funzione.
}

// Go è dotato di garbage collection. Ha i puntatori, ma non l'aritmetica dei
// puntatori. Puoi commettere errori a causa di puntatori nulli, ma non puoi
// incrementare un puntatore direttamente.
func imparaLaMemoria() (p, q *int) {
    // I valori restituiti (con nome) p e q sono puntatori a int.
	p = new(int) // La funzione new si occupa di allocare memoria.
    // L'int allocato viene inizializzato a 0, dunque p non è più nil.
	s := make([]int, 20) // Alloca 20 int come un singolo blocco di memoria.
	s[3] = 7             // Ne assegna uno.
	r := -2              // Dichiara un'altra variabile locale
	return &s[3], &r     // & "prende" l'indirizzo di un oggetto.
}

func calcoloCostoso() float64 {
	return m.Exp(10)
}

func imparaControlloDiFlusso() {
    // L'istruzione if richiede parentesi graffe per il corpo, mentre non ha
    // bisogno di parentesi tonde per la condizione.
	if true {
		fmt.Println("te l'ho detto")
	}
    // Eseguendo "go fmt" da riga di comando, il codice viene formattato
    // in maniera standard.
	if false {
		// :(
	} else {
		// :D
	}
    // L'istruzione switch serve ad evitare tanti if messi in cascata.
	x := 42.0
	switch x {
	case 0:
	case 1:
	case 42:
		// Quando è soddisfatta la condizione all'interno di un case, il
        // programma esce dal switch senza che siano specificate istruzioni
        // di tipo "break". In Go infatti di default non è presente il
        // cosiddetto "fall through" all'interno dell'istruzione switch.
        // Tuttavia, il linguaggio mette a disposizione la parola chiave
        // fallthrough per permettere, in casi particolari, questo comportamento.
	case 43:
		// Non si arriva qua.
	default:
		// Il caso di default è opzionale.
	}
    // Come l'if, anche il for non usa parentesi tonde per la condizione.
    // Le variabili dichiarate all'interno di if/for sono locali al loro scope.
	for x := 0; x < 3; x++ { // ++ è un'istruzione!
		fmt.Println("ciclo numero", x)
	}
	// x == 42 qua.

    // Il for è l'unica istruzione per i loop in Go, ma ha varie forme.
	for { // Ciclo infinito.
		break    // Scherzavo.
		continue // Non si arriva qua.
	}

    // Puoi usare range per iterare lungo un vettore, slice, stringa, mappa o canale.
    // range restituisce uno (per i canali) o due valori (vettore, slice, stringa, mappa).
	for chiave, valore := range map[string]int{"uno": 1, "due": 2, "tre": 3} {
        // per ogni coppia dentro la mappa, stampa chiave e valore
		fmt.Printf("chiave=%s, valore=%d\n", chiave, valore)
	}

    // Come nel for, := dentro la condizione dell'if è usato per dichiarare
    // e assegnare y, poi testare se y > x.
	if y := calcoloCostoso(); y > x {
		x = y
	}
	// Le funzioni letterali sono closure.
	xGrande := func() bool {
		return x > 10000 // Si riferisce a x dichiarata sopra al switch (vedi sopra).
	}
	fmt.Println("xGrande:", xGrande()) // true (abbiamo assegnato e^10 a x).
	x = 1.3e3                          // Adesso x == 1300
	fmt.Println("xGrande:", xGrande()) // false ora.

    // Inoltre le funzioni letterali possono essere definite e chiamate
    // inline, col ruolo di parametri di funzione, a patto che:
    // a) la funzione letterale venga chiamata subito (),
    // b) il valore restituito è in accordo con il tipo dell'argomento.
	fmt.Println("Somma e raddoppia due numeri: ",
		func(a, b int) int {
			return (a + b) * 2
		}(10, 2)) // Chiamata con argomenti 10 e 2
	// => Somma e raddoppia due numeri: 24

	// Quando ti servirà, lo amerai.
	goto amore
amore:

	imparaFabbricaDiFunzioni() // Una funzione che restituisce un'altra funzione è divertente!
	imparaDefer()              // Un tour veloce di una parola chiave importante.
	imparaInterfacce()         // Arriva la roba buona!
}

func imparaFabbricaDiFunzioni() {
    // Questi due blocchi di istruzioni sono equivalenti, ma il secondo è più semplice da capire.
	fmt.Println(fabbricaDiFrasi("estate")("Una bella giornata", "giornata!"))

	d := fabbricaDiFrasi("estate")
	fmt.Println(d("Una bella", "giornata!"))
	fmt.Println(d("Un pigro", "pomeriggio!"))
}

// I decoratori sono comuni in alcuni linguaggi. Si può fare lo stesso in Go
// con le funzioni letterali che accettano argomenti.
func fabbricaDiFrasi(miaStringa string) func(prima, dopo string) string {
	return func(prima, dopo string) string {
		return fmt.Sprintf("%s %s %s", prima, miaStringa, dopo) // Nuova stringa
	}
}

func imparaDefer() (ok bool) {
	// La parola chiave "defer" inserisce una funzione in una lista.
	// La lista contenente tutte le chiamate a funzione viene eseguita DOPO
	// il return finale della funzione che le circonda.
	defer fmt.Println("le istruzioni 'deferred' sono eseguite in ordine inverso (LIFO).")
	defer fmt.Println("\nQuesta riga viene stampata per prima perché")
    // defer viene usato di solito per chiudere un file, così la funzione che
    // chiude il file, preceduta da "defer", viene messa vicino a quella che lo apre.
	return true
}

// Definisce Stringer come un'interfaccia con un metodo, String.
type Stringer interface {
	String() string
}

// Definisce coppia come una struct con due campi interi, chiamati x e y.
type coppia struct {
	x, y int
}

// Definisce un metodo sul tipo coppia, che adesso implementa Stringer.
func (p coppia) String() string { // p viene definito "ricevente"
    // Sprintf è un'altra funzione del package ftm.
    // La notazione con il punto serve per richiamare i campi di p.
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func imparaInterfacce() {
	// Brace syntax is a "struct literal". It evaluates to an initialized
	// struct. The := syntax declares and initializes p to this struct.
    // Le parentesi graffe sono usate per le cosiddette "struct letterali".
    // Con :=, p viene dichiarata e inizializzata a questa struct.
	p := coppia{3, 4}
	fmt.Println(p.String()) // Chiama il metodo String di p, che è di tipo coppia.
	var i Stringer          // Dichiara i come interfaccia Stringer.
	i = p                   // Valido perchè coppia implementa Stringer.
    // Chiama il metodo String di i, che è di tipo Stringer. Output uguale a sopra.
	fmt.Println(i.String())

	// Functions in the fmt package call the String method to ask an object
	// for a printable representation of itself.
    // Le funzioni dentro al package fmt chiamano il metodo String per
    // chiedere ad un oggetto una rappresentazione in stringhe di sé stesso.
	fmt.Println(p) // Output uguale a sopra. Println chiama il metodo String.
	fmt.Println(i) // Output uguale a sopra.

	imparaParametriVariadici("grande", "imparando", "qua!")
}

// Le funzioni possono avere parametri variadici (ovvero di lunghezza variabile).
func imparaParametriVariadici(mieStringhe ...interface{}) {
    // Cicla su ogni valore variadico.
    // L'underscore serve a ignorare l'indice del vettore.
	for _, param := range mieStringhe {
		fmt.Println("parametro:", param)
	}

    // Passa un valore variadico come parametro variadico.
	fmt.Println("parametri:", fmt.Sprintln(mieStringhe...))

	imparaGestioneErrori()
}

func imparaGestioneErrori() {
    // La sintassi ", ok" è usata per indicare se qualcosa ha funzionato o no.
	m := map[int]string{3: "tre", 4: "quattro"}
	if x, ok := m[1]; !ok { // ok sarà false perchè 1 non è dentro la mappa.
		fmt.Println("qua non c'è nessuno!")
	} else {
		fmt.Print(x) // x sarebbe il valore che corrisponde alla chiave 1, se fosse nella mappa.
	}
    // Un errore non riporta soltanto "ok" ma è più specifico riguardo al problema.
	if _, err := strconv.Atoi("non_intero"); err != nil { // _ scarta il valore
		// stampa 'strconv.ParseInt: parsing "non_intero": invalid syntax'
		fmt.Println(err)
	}
    // Approfondiremo le interfacce un'altra volta. Nel frattempo,
	imparaConcorrenza()
}

// c è un canale, un oggetto per comunicare in modo concorrente e sicuro.
func inc(i int, c chan int) {
	c <- i + 1 // <- è l'operatore di "invio" quando un canale sta a sinistra.
}

// Useremo inc per incrementare alcuni numeri in modo concorrente.
func imparaConcorrenza() {
    // Stessa funzione usata prima per creare uno slice. Make alloca e
    // inizializza slice, mappe e canali.
	c := make(chan int)
    // Lancia tre goroutine. I numeri saranno incrementati in modo concorrente,
    // forse in parallelo se la macchina lo supporta. Tutti e tre inviano dati
    // sullo stesso canale.
	go inc(0, c) // go è un'istruzione che avvia una goroutine.
	go inc(10, c)
	go inc(-805, c)
    // Legge tre risultati dal canale e li stampa a schermo.
    // Non si conosce a priori l'ordine in cui i risultati arriveranno!
	fmt.Println(<-c, <-c, <-c) // <- è l'operatore di "ricevuta" quando
    // un canale sta a destra.

	cs := make(chan string)       // Un altro canale, gestisce le stringhe.
	ccs := make(chan chan string) // Un canale che gestisce canali di stringhe.
	go func() { c <- 84 }()       // Lancia una goroutine, solo per inviare un valore.
	go func() { cs <- "parolina" }() // Stessa cosa ma per cs.
    // select è simile a switch, ma ogni case riguarda un'operazione su un
    // canale. Seleziona, in modo random, uno tra i canali che sono pronti
    // a comunicare.
	select {
	case i := <-c: // Il valore ricevuto può essere assegnato a una variabile,
		fmt.Printf("E' un %T", i)
	case <-cs: // oppure il valore ricevuto può essere scartato.
		fmt.Println("E' una stringa.")
	case <-ccs: // Canale vuoto, non pronto per comunicare.
		fmt.Println("Non succede niente.")
	}
    // A questo punto un valore è stato preso da c o cs. Una delle tue goroutine
    // cominciate sopra ha completato l'esecuzione, l'altra rimarrà bloccata.

	imparaProgrammazioneWeb() // Se lo fa Go, lo puoi fare anche tu.
}

// Una funzione all'interno del package http avvia un webserver.
func imparaProgrammazioneWeb() {

    // Il primo parametro di ListenAndServe è l'indirizzo TCP su cui ascoltare.
    // Il secondo parametro è un'interfaccia, precisamente http.Handler.
	go func() {
		err := http.ListenAndServe(":8080", coppia{})
		fmt.Println(err) // Non ignorare gli errori.
	}()

	richiediServer()
}

// Per rendere coppia un http.Handler basta implementare il metodo ServeHTTP.
func (p coppia) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Il server fornisce dati con un metodo di http.ResponseWriter.
	w.Write([]byte("Hai imparato Go in Y minuti!"))
}

func richiediServer() {
	risposta, err := http.Get("http://localhost:8080")
	fmt.Println(err)
	defer risposta.Body.Close()
	corpo, err := ioutil.ReadAll(risposta.Body)
	fmt.Printf("\nIl webserver dice: `%s`", string(corpo))
}
```

## Letture consigliate

La risorsa più importante per imparare il Go è il [sito ufficiale di Go](http://golang.org/).
Qui puoi seguire i tutorial, scrivere codice in modo interattivo, e leggere tutti i dettagli.
Oltre al tour, [la documentazione](https://golang.org/doc/) contiene informazioni su
come scrivere ottimo codice in Go, documentazione sui package e sui comandi, e
la cronologia delle release.

Anche il documento che definisce il linguaggio è un'ottima lettura. E' semplice
da leggere e incredibilmente corto (rispetto ad altri documenti riguardanti
la creazione di linguaggi).

Puoi giocare con il codice visto finora nel [Go playground](https://play.golang.org/p/Am120Xe7qf).
Prova a cambiarlo e ad eseguirlo dal browser!
Osserva che puoi usare [https://play.golang.org](https://play.golang.org) come
una [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) per scrivere
codice all'interno del browser, senza neanche installare Go!

Una lettura importante per capire Go in modo più profondo è il [codice 
sorgente della libreria standard](http://golang.org/src/pkg/). Infatti è
molto ben documentato e costituisce quanto più chiaro e conciso ci sia riguardo
gli idiomi e le buone pratiche del Go. Inoltre, clickando sul nome di una
funzione [nella documentazione](http://golang.org/pkg/) compare il relativo
codice sorgente!

Un'altra ottima risorsa per imparare è [Go by example](https://gobyexample.com/).

Go Mobile aggiunge il supporto per lo sviluppo mobile (Android e iOS).
In questo modo è possibile scrivere un'app mobile nativa in Go, oppure
una libreria che contiene binding da un package scritto in Go, e che può
essere richiamata da Java(Android) e Objective-C(iOS). Visita la pagina di
[Go Mobile](https://github.com/golang/go/wiki/Mobile) per maggiori informazioni.
