---
nome: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
---

Gleam è un nuovo linguaggio per la macchina virtuale BEAM di Erlang che si basa su
potenza di un sistema di tipi robusti, espressività della programmazione funzionale,
e il runtime Erlang con tolleranza agli errori altamente simultaneo che utilizza il familiare e
sintassi moderna ispirata a linguaggi come OCaml, Rust ed Elixir.

Essendo uno sviluppo piuttosto moderno, Gleam viene fornito con un compilatore, uno strumento di compilazione,
un formattatore di codice, diverse integrazioni di editor e un gestore di pacchetti.

Essendo parte dell'ecosistema BEAM più ampio, i programmi creati con Gleam
possono anche usare migliaia di pacchetti pubblicati scritti in Erlang o Elixir.

Il design del linguaggio è molto conciso, quindi non presenta valori nulli,
nessuna eccezione, messaggi di errore chiari e un sistema di tipi pratico.

JavaScript è inoltre supportato come destinazione di compilazione, quindi puoi eseguire Gleam
codice nel browser o qualsiasi altro runtime abilitato per JS. Quando si utilizza questa funzione,
Vengono create le definizioni TypeScript, così puoi interagire con il tuo codice Gleam
con fiducia, anche dall'esterno.

Per eseguire questo codice, crea prima un progetto Gleam:

```sh
gleam new my_project
cd my_project
```

Sostituisci il codice generato con questo esempio ed esegui:

```sh
gleam run
```


```gleam
//// Questo commento con quattro barre è a livello di modulo.
//// Questo tipo di commenti vengono utilizzati per descrivere l'intero modulo.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/dict

// Il nome di un tipo inizia sempre con una lettera maiuscola, a differenza delle variabili
// e funzioni, che iniziano con una lettera minuscola.

//Quando viene utilizzata la parola chiave pub, l'alias del tipo è pubblico ed è possibile fare riferimento
// da altri moduli.

pub type IdUtente =
  Int

pub fn main() {
  io.println("Ciao da learnxinyminutes.com!")
  // io.println("Questa affermazione è stata commentata da un commento con due barre.!")

  // I moduli sono le unità in cui viene organizzato tutto il codice Gleam.
  // In un modulo troverai un sacco di definizioni di tipi, funzioni, ecc.
  // che sembrano appartenere insieme.
  // Ad esempio, il modulo gleam/io contiene una varietà di funzioni per
  // stampa, come println.

  // Tutto il codice gleam è in qualche modulo, il cui nome deriva dal nome
  // del file in cui si trova.
  // Ad esempio, gleam/io si trova in un file chiamato io.gleam in una directory
  // chiamata gleam.

  // Gleam ha un robusto sistema di tipi statici che ti aiuta mentre scrivi e modifichi
  // codice, rilevando gli errori e mostrandoti dove apportare modifiche.
  // io.println(10)
  // Se rimuovi il commento dalla riga precedente verrà segnalato un errore in fase di compilazione
  // poiché la funzione io.println funziona solo con stringhe, non con interi.

  // La compilazione genererà un errore simile al seguente:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │ io.println(10)
  //  │              ^^
  //
  // Tipo atteso:
  //
  //     String
  //
  // Tipo trovato:
  //
  //     Int

  // Lavorare con i numeri

  //Quando vengono eseguiti sulla macchina virtuale Erlang, gli int non hanno né massimo né minimo
  // dimensione.
  // Quando si esegue su runtime JavaScript, gli int vengono rappresentati utilizzando JavaScript
  // Numeri in virgola mobile a 64 bit.

  // Possiamo usare "echo" per eseguire il debug della stampa di un valore.

  // Aritmetica degli interi
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Int comparisons using variables to prevent compiler static analysis warnings
  let due = 2
  let one = 1
  let same_one = 1
  let same_two = 2
  let _ = echo two > one
  let _ = echo two < one
  let _ = echo two >= one
  let _ = echo two <= one

  // L'uguaglianza funziona per qualsiasi tipo e viene verificata strutturalmente, il che significa che due
  // i valori sono uguali se hanno la stessa struttura anziché se sono uguali
  // la stessa posizione di memoria.
  let _ = echo one == same_one
  // True
  let _ = echo two != same_two
  // False

  // Funzioni int della libreria standard
  echo int.min(142, 137)
  // 137
  echo int.clamp(-80, min: 0, max: 100) // clamp(valore, min, max) specifica il valore che rientra nell'intervallo specificato
  // 0
  let _ = echo int.base_parse("10", 2) // analizza la stringa binaria come intero in base 2. 
  // Ok(2) : 10 in binary equals to 2 in integer

  // Letterali Int binari, ottali ed esadecimali
  echo 0b00001111
  echo 0o17
  echo 0xF

  // Utilizzare i caratteri di sottolineatura per migliorare la leggibilità dei numeri interi
  echo 1_000_000

  // Gli operatori numerici di Gleam non sono sovraccarichi, quindi sono dedicati
  // operatori per lavorare con i float.

  // Aritmetica dei float
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Float comparisons using variables to prevent compiler static analysis warnings
  let due_punto_due = 2.2
  let uno_punto_tre = 1.3
  let _ = echo due_punto_due >. uno_punto_tre
  let _ = echo due_punto_due <. uno_punto_tre
  let _ = echo due_punto_due >=. uno_punto_tre
  let _ = echo due_punto_due <=. uno_punto_tre

  // I float sono rappresentati come numeri in virgola mobile a 64 bit su entrambi gli Erlang
  // e runtime JavaScript.
  // Il comportamento in virgola mobile è nativo dei rispettivi runtime, quindi
  // il loro comportamento esatto sarà leggermente diverso nei due tempi di esecuzione.

  // Nel runtime JavaScript, superamento del massimo (o minimo)
  //valore rappresentabile per un valore in virgola mobile risulterà in Infinity
  // (o -Infinito). Se provi a dividere due infiniti otterrai NaN
  // di conseguenza.

  // Durante l'esecuzione su BEAM, qualsiasi overflow genererà un errore. Quindi non c'è
  // Valore float NaN o Infinity nel runtime Erlang.

  // La divisione per zero non è un errore ed è definita come zero
  echo 3.14 /. 0.0
  // 0.0

  // Funzioni float della libreria standard
  echo float.max(2.0, 9.5)
  // 9.5
  echo float.ceiling(5.4)
  // 6.0

  // Sono supportati anche i caratteri di sottolineatura per i float
  echo 10_000.01

  // Lavorare con le stringhe
  echo "⭐ Gleam ⭐ - 별"
  echo "this
    is
    a
    multi
    line
    string"

  echo "\u{1F600}"
  // Emette una faccina 😀

  // È possibile evitare le doppie virgolette
  io.println("\"X\" segna il punto")

  // Concatenazione di stringhe
  echo "One " <> "Two"

  // Funzioni sulle stringhe
  echo string.reverse("1 2 3 4 5")
  echo "abc" <> "def"

  io.println(string.reverse("!desrever tog gnirts sihT"))
  // Restituisce "Questa stringa è stata invertita!"

  // Sono supportate diverse sequenze di escape:

  // \" - virgoletta doppia
  // \\ - barra rovesciata
  // \f - avanzamento modulo
  // \n - nuova riga
  // \r - ritorno a capo
  // \t - tab

  // Operatori booleani
  // Il || e gli operatori && funzionano cortocircuitando

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Funzioni booleane
  echo bool.to_string(True)
  // "True"

  //Incarichi
  let x = "Original valore"
  echo x

  // Assegna "y" al valore di "x".
  let y = x
  echo y

  // Assegna "x" a un nuovo valore
  let x = "New valore"
  echo x

  // La "y" si riferisce ancora al valore originale
  echo y

  // In Gleam i nomi delle variabili e delle funzioni sono scritti in snake_case.
    let respuesta_dell_universo = 42
  echo respuesta_dell_universo

    let e_tutto_il_resto = respuesta_dell_universo
  // Ora l'utilizzo di una variabile produce un avviso

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │     let e_tutto_il_resto = respuesta_dell_universo
  //     │       ^^^^^^^^^^^^^^ Questa variabile non viene mai usata
  // Suggerimento: puoi ignorarla con un trattino basso: `_e_tutto_il_resto`.

  // Digita annotazioni

    let _nome: String = "Gleam"

    let _e_bello: Bool = True

  let _version: Int = 1
  // Utili a scopo di documentazione, ma non cambiano il modo in cui il compilatore
  // controlla il codice oltre ad assicurarsi che l'annotazione corrisponda al tipo;
  // altrimenti ottieni un errore.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │ let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Tipo atteso:
  //
  //      Int
  //
  //  Tipo trovato:
  //
  //      Bool

  // Digitare gli alias
    let uno: IdUtente = 1
  // Fare riferimento all'inizio del file per la definizione del tipo UserId

    let due: Int = 2

  // Gli alias servono solo per creare codice più leggibile e più preciso
  // documentazione.
  // Sotto il cofano ci sono ancora valori dello stesso tipo quindi operazioni
  // funzionano ancora
  echo uno + due
  // 3

  // Blocchi: ambito e valore
  let raggio = {
    let valore = 100.0
    valore
  }
  //echo valore // <- Questo non verrà compilato perché "valore" non rientra nell'ambito

  let area = 3.14159 *. raggio *. raggio
  echo area

  // Utilizza i blocchi per raggruppare le operazioni anziché le parentesi
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Elenchi

  // Nipoti di Paperone de' Paperoni
    let nipoti = ["Qui", "Quo", "Qua"]
  echo nipoti
  // ["Qui", "Quo", "Qua"]

  // Anteporre in modo immutabile in modo che l'elenco originale non venga modificato
  echo ["Paperino", ..nipoti]
  // ["Paperino", "Qui", "Quo", "Qua"]

  // Alcune funzioni della libreria standard per gli elenchi

  list.each(nipoti, io.println)
  // Qui
  // Quo
  // Qua

  echo list.drop(nipoti, 2)
  // ["Qua"]

  altri_esempi()
  altri_esempi_funzioni()
  esempi_tipi_generici()
  demo_pipeline()
  etichette_nelle_chiamate()
  mostra_controllo_di_flusso()
  altro_su_ricorsione()
  altro_su_pattern_matching()
  mostra_tipi()
  altro_sui_tipi()
  altro_su_callback()
  mostra_esterni()
  mostra_costanti()
  mostra_dict()
  mostra_array_di_bit()
  mostra_tipi_opachi()
  mostra_panic()
}

// La parola chiave fn viene utilizzata per definire nuove funzioni.
fn moltiplica(a: Int, b: Int) -> Int {
  // Nessun ritorno esplicito
  // Viene restituita l'ultima espressione
  a * b
}

// Le funzioni raddoppia e moltiplica sono definite senza la parola chiave pub.
// Questo le rende funzioni private, possono essere utilizzate solo all'interno di questo modulo.
// Se un altro modulo tentasse di utilizzarli, si verificherebbe un errore del compilatore.
fn raddoppia(a: Int) -> Int {
  moltiplica(a, 2)
}

// Solo le funzioni pubbliche vengono esportate e possono essere chiamate dall'esterno del modulo.

// Le annotazioni di tipo sono facoltative per gli argomenti della funzione e i valori restituiti
// ma sono considerate buone pratiche per chiarezza e per incoraggiare
// design intenzionale e ponderato.

pub fn e_bisestile(anno: Int) -> Bool {
  { anno % 4 == 0 } && { { anno % 100 != 0 } || { anno % 400 == 0 } }
}

fn altri_esempi() {
  //Debug restituisce anche un valore, quindi il suo output è il valore restituito di
  // questa funzione
  echo raddoppia(10)
  // 20
  echo e_bisestile(2000)
  // True
}

// Gleam supporta funzioni di ordine superiore:
// Possono essere assegnati a variabili, passati come argomenti ad altre funzioni
// o anche essere restituiti come valori da blocchi o altre funzioni
fn chiama_funzione_su_intero(func: fn(Int) -> Int, valore: Int) -> Int {
  func(valore)
}

fn altri_esempi_funzioni() -> Int {
  echo chiama_funzione_su_intero(raddoppia, 2)
  // 4

  let quadrato = fn(x: Int) -> Int { x * x }
  echo quadrato(3)
  // 9

  // Chiamare una funzione anonima immediatamente dopo averla definita
  echo fn(x: Int) { x + 1 }(1)

  // Esempio di chiusura
  let crea_sommatore = fn(n: Int) -> fn(Int) -> Int {
    fn(argomento: Int) -> Int { argomento + n }
  }

  let sumadora_de_cincos = crea_sommatore(5)
  echo sumadora_de_cincos(10)
  // 15

  // Le funzioni anonime possono essere utilizzate in modo intercambiabile con le funzioni denominate.
  echo chiama_funzione_su_intero(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Creiamo un decoratore di funzioni
  let due_volte = fn(funzione_avvolta: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argomento: Int) -> Int { funzione_avvolta(funzione_avvolta(argomento)) }
  }
    let quadruplo = due_volte(raddoppia)
    echo quadruplo(1)

    let quadruplo_2 = fn(a: Int) -> Int { moltiplica(4, a) }
  echo quadruplo_2(2)
  // 8

  // Una cattura di funzione è una sintassi abbreviata per creare funzioni anonime
  // che accetta un argomento e con quello chiama immediatamente un'altra funzione
  // argomento
    let quadruplo_3 = moltiplica(4, _)
  echo quadruplo_3(4)
  // 16
}

// Le funzioni generiche sono supportate utilizzando variabili di tipo.
fn doppio_generico(func: fn(valore) -> valore, argomento: valore) -> valore {
  func(func(argomento))
}

// In doppio_generico il valore era la variabile di tipo.
// In decoratore_doppio_generico the_type è la variabile di tipo.
// Come in qualsiasi altra variabile puoi scegliere il nome.
fn decoratore_doppio_generico(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argomento: the_type) -> the_type { func(func(argomento)) }
}

fn esempi_tipi_generici() {
  let raddoppia_interi = fn(a: Int) -> Int { a * 2 }
  let raddoppia_float = fn(a: Float) -> Float { a *. 2.0 }
  echo doppio_generico(raddoppia_interi, 3)
  echo doppio_generico(raddoppia_float, 3.0)

  let quadruplo_interi = decoratore_doppio_generico(raddoppia_interi)
  let quadruplo_float = decoratore_doppio_generico(raddoppia_float)
  echo quadruplo_interi(1)
  // 4
  echo quadruplo_float(1.0)
  // 4.0
}

// L'operatore pipe di Gleam |> prende il risultato dell'espressione alla sua sinistra
// e lo passa come argomento alla funzione alla sua destra.
fn demo_pipeline() {
  // Siamo onesti: vuoi usare Gleam solo per questo fantastico operatore, giusto?
  ["ciao", "mondo"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Molto più pulito di così, vero?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["ciao", "mondo"], " "), ["!"]),
      ),
    )

  //Soluzione al primo problema del Progetto Eulero:
  // URL: https://projecteuler.net/problem=1
  // Descrizione: trova la somma di tutti i multipli di 3 e 5 inferiori a 1000.
  // Utilizzando int.range da gleam/int per iterare in modo ricorsivo.
  int.range(from: 1, to: 999, with: 0, run: fn(accum, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> accum + n
      False -> accum
    }
  })
  |> int.to_string
  |> fn(somma_come_testo: String) {
    "Soluzione al problema #1 di Project Euler: " <> somma_come_testo
  }
  |> echo
  // Soluzione al problema #1 di Project Euler: 233168
}

// È possibile aggiungere etichette prima di ogni argomento
fn chiama_funzione_su_intero_con_etichette(
  func funzione_passata: fn(Int) -> Int,
  valore n: Int,
) -> Int {
  funzione_passata(n)
}

// L'etichetta e l'argomento possono avere lo stesso nome
fn aggiungi_uno(numero numero: Int) -> Int {
  numero + 1
}

fn somma_due_interi(primo n: Int, secondo m: Int) -> Int {
  n + m
}

fn etichette_nelle_chiamate() -> Int {
  // Poiché stiamo etichettando gli argomenti, possiamo invertire l'ordine
  // se vogliamo
  echo chiama_funzione_su_intero_con_etichette(valore: 8, func: raddoppia)
  echo aggiungi_uno(numero: 1)
  // 2
  echo string.contains(does: "tema", contain: "te")
  // True
  // Gli argomenti senza etichetta devono essere preceduti
  echo somma_due_interi(2, secondo: 2)
  // 4
}

fn mostra_controllo_di_flusso() {
  // Caso d'uso se si desidera utilizzare la corrispondenza dei modelli per farlo
  // selezionare quale codice eseguire.
  // Gleam si assicurerà che tutti i valori possibili siano coperti
  // effettuando controlli di esaustività.
  // Altrimenti ottieni errori di compilazione.
  let cuccioli = ["Bear", "Frisco", "Ranger"]
  let conteo = list.length(of: cuccioli)
  {
    "Abbiamo "
    <> int.to_string(conteo)
    <> " "
    <> // Il carattere di sottolineatura corrisponde a qualsiasi altro valore
    case conteo {
      1 -> "cucciolo"
      _ -> "cuccioli"
    }
  }
  |> echo

  // Gleam consente ai modelli nelle espressioni maiuscole e minuscole di assegnare anche variabili.
  {
    "Numero di cuccioli: "
    <> case list.length(cuccioli) {
      0 -> "Nessuno."
      1 -> "Solo uno."
      altro_conteo -> "Ben " <> int.to_string(altro_conteo) <> " cuccioli."
    }
  }
  |> echo

  // Considera che i linguaggi BEAM sono funzionali nel design e Gleam non fa eccezione
  // quindi non sono disponibili costrutti if, for o while.

  // Utilizza la corrispondenza dei modelli per i condizionali
  let respuesta = 42
  case respuesta == 42 {
    True -> {
      echo "Questa e la respuesta dell'universo."
    }
    False -> {
      echo "Questa e la respuesta a qualcos'altro."
    }
  }

  // Usa la ricorsione invece del looping
  da_uno_a_dieci(1)
}

// Funzione ricorsiva
fn da_uno_a_dieci(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> da_uno_a_dieci(n + 1)
  }
}

// Per evitare l'esaurimento della memoria dovuto alla creazione eccessiva
//impilare i frame quando si chiamano funzioni in modo ricorsivo, supporta Gleam
// "ottimizzazione delle chiamate di coda", il che significa che il compilatore può riutilizzare
// lo stack frame per la funzione corrente se lo è una chiamata di funzione
// l'ultima cosa che fa la funzione.

pub fn fibonacci(x: Int) -> Int {
  // La funzione pubblica chiama la funzione ricorsiva della coda privata
  ciclo_fibonacci(x, 1)
}

fn ciclo_fibonacci(x: Int, accumulatore: Int) -> Int {
  case x {
    1 -> accumulatore

    // L'ultima cosa che fa questa funzione è chiamare se stessa
    // Nella lezione precedente l'ultima cosa che ha fatto è stata moltiplicare due interi
    _ -> ciclo_fibonacci(x - 1, accumulatore + x)
  }
}

// Gleam supporta la corrispondenza dei modelli del primo elemento e del resto
// di una lista con il pattern [x, ..y] all'interno di un'espressione case.
fn inverti_lista(la_lista: List(valore)) -> List(valore) {
  case la_lista {
    [cabecera, ..cola] -> list.flatten([inverti_lista(cola), [cabecera]])
    [] -> []
  }
}

fn altro_su_ricorsione() {
  echo fibonacci(10)
  // 55
  echo inverti_lista([1, 2, 3])
}

fn altro_su_pattern_matching() {
  // Quando si verifica la corrispondenza dei modelli sulle stringhe, l'operatore <> corrisponde alle stringhe
  // con un prefisso specifico e assegna il promemoria a una variabile
    let lucia = "Ciao, Lucia"
  let _ = echo case lucia {
    "Ciao, " <> nome -> "Saluti per " <> nome
    _ -> "Forse nessun saluto"
  }

  // Sono supportati modelli alternativi, quindi viene utilizzata la stessa clausola
  // per più valori
  let mese = 2
  let anno = 2024
  let numero_di_giorni = case mese {
    2 ->
      case e_bisestile(anno) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Numero di giorni: " <> int.to_string(numero_di_giorni)
  // 29

  // Guardie in abbinamento di modelli:
  // Quando si utilizza la parola chiave if, un'espressione deve restituire True
  // affinché il modello corrisponda.
  let lista_empieza_con = fn(la_lista: List(valore), il_valore: valore) -> Bool {
    case la_lista {
      [cabecera, ..] if cabecera == il_valore -> True
      _ -> False
    }
  }
  echo lista_empieza_con([10, 20, 30], 10)
  // True
}

pub type Genere {
  Maschio
  Femmina
  Altro
}

// Registrazioni:
// - Supporta varianti
// - Ogni variante è simile a una struttura con campi
pub type Forma {
  Rettangolo(base: Float, altezza: Float)
  Triangolo(base: Float, altezza: Float)
}

// I record con una variante assomigliano alle strutture
pub type Punto {
  Punto(x: Float, y: Float)
}

fn mostra_tipi() {
  //Tuple:
  // - Può mescolare insieme elementi di diverso tipo
  // - Il loro tipo è implicito, ad es. #{1, "Hello"} è di tipo #{Int, String}
  // - È possibile accedere ai loro elementi tramite indici numerici
  let tupla_01 = #(1, "Ferris", "crostaceo", True)
  let tupla_02 = #(1, "Lucia", "stella_marina", True)
  echo tupla_01
  echo tupla_01.0
  // 1
  echo tupla_02.1
  // Lucia
  let #(_, nome, specie, _) = tupla_01
  echo nome <> " il " <> specie

  // Corrispondenza di modelli con tuple inclusa l'assegnazione delle variabili
  let _ = stampa_stato_mascotte(tupla_02)

  // Utilizzo di un tipo personalizzato con corrispondenza di modelli
    let genere = Altro
  let _ = echo genere_in_stringa(genere)

  // Utilizzo dei record
  let rettangolo_1 = Rettangolo(base: 10.0, altezza: 20.0)
  echo rettangolo_1.altezza
  // 10.3

  let punto_1 = Punto(x: 3.2, y: 4.3)
  echo punto_1

  // Aggiornamento di un record
  let punto_2 = Punto(..punto_1, y: 5.7)
  echo punto_2

  // In Gleam, i valori non sono annullabili.
  // Nil è l'unico valore del suo tipo.
  let qualche_variabile = Nil
    let risultato = io.println("Ciao!")
  echo qualche_variabile == risultato
  // True
}

fn stampa_stato_mascotte(mascotte: #(Int, String, String, Bool)) {
  case mascotte {
    #(_, nome, _, True) -> echo nome <> " e una mascotte."
    #(_, nome, _, False) -> echo nome <> " non e una mascotte."
  }
}

fn genere_in_stringa(genere: Genere) -> String {
  case genere {
    Maschio -> "Maschio"
    Femmina -> "Femmina"
    Altro -> "Indeterminato"
  }
}

pub type Minerale {
  Oro
  Argento
  Rame
}

// Tipi personalizzati generici con tipi contenuti come parametri
pub type Purezza(tipo_interno) {
  Puro(tipo_interno)
  Impuro(tipo_interno)
}

pub type Bevanda {
  Acqua
  Succo
}

// Tipi personalizzati esistenti dai moduli gleam/option e gleam/result
// facilitare l'utilizzo di valori nullable e la gestione di potenziali errori
pub type Persona {
  Persona(nome: String, soprannome: Option(String))
}

pub type ErroreDado {
  ValoreDadoFuoriIntervallo
}

fn valore_dado_controllato(valore: Int) -> Result(Int, ErroreDado) {
  case valore {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(valore)
    _ -> Error(ValoreDadoFuoriIntervallo)
  }
}

fn raddoppia_valore_dado(valore: Int) -> Result(Int, ErroreDado) {
  case valore {
    1 | 2 | 3 -> Ok(valore * 2)
    _ -> Error(ValoreDadoFuoriIntervallo)
  }
}

fn altro_sui_tipi() {
    let campione_minerale_01: Purezza(Minerale) = Puro(Oro)
    let campione_minerale_02 = Impuro(Argento)
  echo campione_minerale_01
  echo campione_minerale_02

  // Un bicchiere può essere vuoto oppure no
    let bicchiere_01: Option(Bevanda) = Some(Acqua)
    let bicchiere_02 = None
  echo bicchiere_01
  echo bicchiere_02

  // Una persona può avere un soprannome oppure no
    let persona_01 = Persona(nome: "Giovanni", soprannome: Some("Lo Squartatore"))
    let persona_02 = Persona(nome: "Martino", soprannome: None)
  echo persona_01
  echo persona_02

  // Utilizzo di funzioni che restituiscono valori di tipo Risultato
    let dado_01 = 5
  case valore_dado_controllato(dado_01) {
    Ok(valore_controllato) ->
      echo "Il valore di " <> int.to_string(valore_controllato) <> " e valido."
    Error(ValoreDadoFuoriIntervallo) ->
      echo "Il valore del dado e fuori intervallo"
  }

  // Proviamo a raddoppiare il valore se il valore risultante è fermo
  // un numero su uno qualsiasi dei lati del dado.
  // Altrimenti, mettiamo il valore massimo.
  2
  |> valore_dado_controllato
  |> result.try(raddoppia_valore_dado)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn lancia_dado_come_risultato() {
  Ok(int.random(6) + 1)
}

pub fn somma_valori_dadi(a: Int, b: Int) {
  Ok(a + b)
}

//Scommesse su funzioni di prima classe e pattern-matching
// può facilmente portare a tonnellate di rientranze
fn lancia_due_dadi_senza_use() {
    result.try(lancia_dado_come_risultato(), fn(primo_dado) {
        result.try(lancia_dado_come_risultato(), fn(secondo_dado) {
            somma_valori_dadi(primo_dado, secondo_dado)
    })
  })
}

// L'espressione use ci consente comunque di scrivere codice che utilizza i callback
// ma ripulisce il rientro eccessivo:
// - Una chiamata a una funzione di ordine superiore va a destra dell'operatore <-
// - I nomi degli argomenti per la funzione di callback vanno sul lato sinistro di
//   l'operatore <-
// - Tutto il codice rimanente nel blocco {} che lo racchiude diventa il corpo del file
//   funzione di richiamata.
fn lancia_due_dadi_con_use() {
    use primo_dado <- result.try(lancia_dado_come_risultato())
    use secondo_dado <- result.try(lancia_dado_come_risultato())
        somma_valori_dadi(primo_dado, secondo_dado)
}

fn altro_su_callback() {
  let _ = echo lancia_due_dadi_senza_use()
  let _ = echo lancia_due_dadi_con_use()
  Nil
}

// Dalla versione 1.14.0, l'annotazione @external può essere utilizzata anche su tipi esterni
// indirizzarli ai tipi Erlang o TypeScript.
pub type DataOra

// Le funzioni esterne devono annotare un tipo restituito.
// Questa funzione è definita solo sul target Erlang; compilazione in JavaScript
// richiederebbe un corpo di fallback o un'annotazione @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn adesso() -> DataOra {
  todo
}

fn mostra_esterni() {
  let _ = echo adesso()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Costanti a livello di modulo (supportate dalla versione 1.0.0, con elenco anteposto dalla versione 1.16.0)
pub const numeri_primi = [2, 3, 5]
pub const altri_primi = [7, 11, ..numeri_primi]

fn mostra_costanti() {
  let _ = echo numeri_primi
  let _ = echo altri_primi

  // Dalla versione 1.17.0, puoi anche utilizzare `todo` all'interno delle dichiarazioni di costanti:
  // pub const costante_imminente = da fare
  Nil
}

fn mostra_dict() {
  // I dizionari sono raccolte di valori-chiave (stdlib v0.33.0)
  let puntajes = dict.new()
    |> dict.insert("Alicia", 10)
    |> dict.insert("Bruno", 15)

      let _ = echo dict.get(puntajes, "Alicia")
  // Ok(10)
    let _ = echo dict.get(puntajes, "Carlo")
  // Error(Nil)
  Nil
}

fn mostra_array_di_bit() {
  // Gli array di bit rappresentano sequenze di dati binari (stdlib v0.32.0)
  let dati_in_binario = <<0x41, 0x42, 0x43>> //"ABC" in ASCII
  let _ = echo dati_in_binario

  // In Gleam, il pattern match con `let` deve essere esaustivo (deve coprire tutti
  // valori possibili). Se un modello potrebbe non riuscire a corrispondere in fase di esecuzione, dobbiamo utilizzare
  // `let assert` invece di `let`. Se il match fallisce, il programma va nel panico:
  let assert <<primo_byte, resto:bytes>> = dati_in_binario
  let _ = echo primo_byte // 65
  let _ = echo resto // <<66, 67>>
  Nil
}

// I tipi opachi sono tipi i cui costruttori sono privati del modulo di definizione,
// abilitare l'incapsulamento e nascondere i dettagli di implementazione.
pub opaque type DatoCifrato {
  DatoCifrato(valore: String)
}

pub fn cifra(dato: String) -> DatoCifrato {
  DatoCifrato(dato <> "_cifrato")
}

fn mostra_tipi_opachi() {
    let segreto = cifra("mia_password")
  // Possiamo passare il segreto, ma non possiamo creare corrispondenze su DatoCifrato
  // oppure accedi al suo campo .valore dall'esterno di questo modulo.
  let _ = echo segreto
  Nil
}

fn mostra_panic() {
  // Possiamo interrompere deliberatamente l'esecuzione utilizzando la parola chiave panico
  // per far sì che il nostro programma si blocchi immediatamente
    let tre = 3
  let due = 2
    case tre == due {
    True -> panic as "L'operatore di uguaglianza e rotto!"
    False -> "L'operatore di uguaglianza funziona per gli interi"
  }
  // Anche la chiamata di una funzione che utilizza la parola chiave todo si blocca
  // compiti()
}

pub fn compito() {
  todo as "Questa funzione non e ancora implementata"
}
```

## Ulteriori letture

* [Sito ufficiale di Gleam](https://gleam.run/)
* [Tour linguistico](https://tour.gleam.run/) - Include l'editor di codice live
* [Documentazione ufficiale](https://gleam.run/documentation/)
* [L'elenco fantastico di Gleam](https://github.com/gleam-lang/awesome-gleam)
* [Traccia degli esercizi per Gleam](https://exercism.org/tracks/gleam)

I documenti ufficiali contengono suggerimenti per le persone che hanno familiarità con:

* [Elisir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Elm](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Rust](https://gleam.run/cheatsheets/gleam-for-rust-users)
