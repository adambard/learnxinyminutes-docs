---
name: Gleam
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

pub type UserId =
  Int

pub fn main() {
  io.println("Hello from learnxinmyminutes.com!")
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
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // Lavorare con i numeri

  //Quando vengono eseguiti sulla macchina virtuale Erlang, gli int non hanno né massimo né minimo
  // dimensione.
  // Quando si esegue su runtime JavaScript, gli int vengono rappresentati utilizzando JavaScript
  // Numeri in virgola mobile a 64 bit.

  // Possiamo usare "echo" per eseguire il debug della stampa di un valore.

  // Int arithmetic
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Int comparisons using variables to prevent compiler static analysis warnings
  let two = 2
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
  echo int.clamp(-80, min: 0, max: 100) // clamp(value, min, max) specifica il valore che rientra nell'intervallo specificato
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

  // Float arithmetic
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Float comparisons using variables to prevent compiler static analysis warnings
  let two_point_two = 2.2
  let one_point_three = 1.3
  let _ = echo two_point_two >. one_point_three
  let _ = echo two_point_two <. one_point_three
  let _ = echo two_point_two >=. one_point_three
  let _ = echo two_point_two <=. one_point_three

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
  io.println("\"X\" marks the spot")

  // String concatenation
  echo "One " <> "Two"

  // String functions
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

  // Bool operators
  // Il || e gli operatori && funzionano cortocircuitando

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Bool functions
  echo bool.to_string(True)
  // "True"

  //Incarichi
  let x = "Original value"
  echo x

  // Assegna "y" al valore di "x".
  let y = x
  echo y

  // Assegna "x" a un nuovo valore
  let x = "New value"
  echo x

  // La "y" si riferisce ancora al valore originale
  echo y

  // In Gleam i nomi delle variabili e delle funzioni sono scritti in snake_case.
  let answer_to_the_universe = 42
  echo answer_to_the_universe

  let and_everything = answer_to_the_universe
  // Ora l'utilizzo di una variabile produce un avviso

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │   let and_everything = answer_to_the_universe
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_and_everything`.

  // Digita annotazioni

  let _name: String = "Gleam"

  let _is_cool: Bool = True

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
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // Digitare gli alias
  let one: UserId = 1
  // Fare riferimento all'inizio del file per la definizione del tipo UserId

  let two: Int = 2

  // Gli alias servono solo per creare codice più leggibile e più preciso
  // documentazione.
  // Sotto il cofano ci sono ancora valori dello stesso tipo quindi operazioni
  // funzionano ancora
  echo one + two
  // 3

  // Blocchi: ambito e valore
  let radius = {
    let value = 100.0
    value
  }
  //echo value // <- Questo non verrà compilato perché "value" non rientra nell'ambito

  let area = 3.14159 *. radius *. radius
  echo area

  // Utilizza i blocchi per raggruppare le operazioni anziché le parentesi
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Elenchi

  // Nipoti di Paperone de' Paperoni
  let nephews = ["Huey", "Dewey", "Louie"]
  echo nephews
  // ["Huey", "Dewey", "Louie"]

  // Anteporre in modo immutabile in modo che l'elenco originale non venga modificato
  echo ["Donald", ..nephews]
  // ["Paperino", "Huey", "Dewey", "Louie"]

  // Alcune funzioni della libreria standard per gli elenchi

  list.each(nephews, io.println)
  // Ehi
  // Dewey
  // Louie

  echo list.drop(nephews, 2)
  // ["Louie"]

  more_examples()
  more_function_examples()
  generic_typing_examples()
  beloved_pipelines_demo()
  labels_in_function_calls()
  showcase_flow_control()
  more_on_recursion()
  more_on_pattern_matching()
  showcase_types()
  more_on_types()
  more_on_callbacks()
  showcase_externals()
  showcase_constants()
  showcase_dicts()
  showcase_bit_arrays()
  showcase_opaque_types()
  showcase_panic()
}

// La parola chiave fn viene utilizzata per definire nuove funzioni.
fn multiply(a: Int, b: Int) -> Int {
  // Nessun ritorno esplicito
  // Viene restituita l'ultima espressione
  a * b
}

// Le funzioni double e multiply sono definite senza la parola chiave pub.
// Questo le rende funzioni private, possono essere utilizzate solo all'interno di questo modulo.
// Se un altro modulo tentasse di utilizzarli, si verificherebbe un errore del compilatore.
fn double(a: Int) -> Int {
  multiply(a, 2)
}

// Solo le funzioni pubbliche vengono esportate e possono essere chiamate dall'esterno del modulo.

// Le annotazioni di tipo sono facoltative per gli argomenti della funzione e i valori restituiti
// ma sono considerate buone pratiche per chiarezza e per incoraggiare
// design intenzionale e ponderato.

pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 } && { { year % 100 != 0 } || { year % 400 == 0 } }
}

fn more_examples() {
  //Debug restituisce anche un valore, quindi il suo output è il valore restituito di
  // questa funzione
  echo double(10)
  // 20
  echo is_leap_year(2000)
  // True
}

// Gleam supporta funzioni di ordine superiore:
// Possono essere assegnati a variabili, passati come argomenti ad altre funzioni
// o anche essere restituiti come valori da blocchi o altre funzioni
fn call_func_on_int(func: fn(Int) -> Int, value: Int) -> Int {
  func(value)
}

fn more_function_examples() -> Int {
  echo call_func_on_int(double, 2)
  // 4

  let square = fn(x: Int) -> Int { x * x }
  echo square(3)
  // 9

  // Chiamare una funzione anonima immediatamente dopo averla definita
  echo fn(x: Int) { x + 1 }(1)

  // Esempio di chiusura
  let make_adder = fn(n: Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { argument + n }
  }

  let adder_of_fives = make_adder(5)
  echo adder_of_fives(10)
  // 15

  // Le funzioni anonime possono essere utilizzate in modo intercambiabile con le funzioni denominate.
  echo call_func_on_int(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Creiamo un decoratore di funzioni
  let twice = fn(wrapped_func: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { wrapped_func(wrapped_func(argument)) }
  }
  let quadruple = twice(double)
  echo quadruple(1)

  let quadruple_2 = fn(a: Int) -> Int { multiply(4, a) }
  echo quadruple_2(2)
  // 8

  // Una cattura di funzione è una sintassi abbreviata per creare funzioni anonime
  // che accetta un argomento e con quello chiama immediatamente un'altra funzione
  // argomento
  let quadruple_3 = multiply(4, _)
  echo quadruple_3(4)
  // 16
}

// Le funzioni generiche sono supportate utilizzando variabili di tipo.
fn generic_twice(func: fn(value) -> value, argument: value) -> value {
  func(func(argument))
}

// In generic_twice il valore era la variabile di tipo.
// In generic_twice_decorator the_type è la variabile di tipo.
// Come in qualsiasi altra variabile puoi scegliere il nome.
fn generic_twice_decorator(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argument: the_type) -> the_type { func(func(argument)) }
}

fn generic_typing_examples() {
  let double_integers = fn(a: Int) -> Int { a * 2 }
  let double_floats = fn(a: Float) -> Float { a *. 2.0 }
  echo generic_twice(double_integers, 3)
  echo generic_twice(double_floats, 3.0)

  let quadruple_integers = generic_twice_decorator(double_integers)
  let quadruple_floats = generic_twice_decorator(double_floats)
  echo quadruple_integers(1)
  // 4
  echo quadruple_floats(1.0)
  // 4.0
}

// L'operatore pipe di Gleam |> prende il risultato dell'espressione alla sua sinistra
// e lo passa come argomento alla funzione alla sua destra.
fn beloved_pipelines_demo() {
  // Siamo onesti: vuoi usare Gleam solo per questo fantastico operatore, giusto?
  ["hello", "world"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Molto più pulito di così, vero?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hello", "world"], " "), ["!"]),
      ),
    )

  //Soluzione al primo problema del Progetto Eulero:
  // URL: https://projecteuler.net/problem=1
  // Description: Find the sum of all the multiples of 3 and 5 below 1000.
  // Utilizzando int.range da gleam/int per iterare in modo ricorsivo.
  int.range(from: 1, to: 999, with: 0, run: fn(acc, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> acc + n
      False -> acc
    }
  })
  |> int.to_string
  |> fn(sum_as_text: String) {
    "Solution to Project Euler's problem #1: " <> sum_as_text
  }
  |> echo
  // Solution to Project Euler's problem #1: 233168
}

// È possibile aggiungere etichette prima di ogni argomento
fn call_func_on_int_with_labels(
  func passed_func: fn(Int) -> Int,
  value n: Int,
) -> Int {
  passed_func(n)
}

// L'etichetta e l'argomento possono avere lo stesso nome
fn add_one(number number: Int) -> Int {
  number + 1
}

fn add_two_integers(first n: Int, second m: Int) -> Int {
  n + m
}

fn labels_in_function_calls() -> Int {
  // Poiché stiamo etichettando gli argomenti, possiamo invertire l'ordine
  // se vogliamo
  echo call_func_on_int_with_labels(value: 8, func: double)
  echo add_one(number: 1)
  // 2
  echo string.contains(does: "theme", contain: "the")
  // True
  // Gli argomenti senza etichetta devono essere preceduti
  echo add_two_integers(2, second: 2)
  // 4
}

fn showcase_flow_control() {
  // Caso d'uso se si desidera utilizzare la corrispondenza dei modelli per farlo
  // selezionare quale codice eseguire.
  // Gleam si assicurerà che tutti i valori possibili siano coperti
  // effettuando controlli di esaustività.
  // Altrimenti ottieni errori di compilazione.
  let puppies = ["Bear", "Frisco", "Ranger"]
  let count = list.length(of: puppies)
  {
    "We have "
    <> int.to_string(count)
    <> " "
    <> // Il carattere di sottolineatura corrisponde a qualsiasi altro valore
    case count {
      1 -> "puppy"
      _ -> "puppies"
    }
  }
  |> echo

  // Gleam consente ai modelli nelle espressioni maiuscole e minuscole di assegnare anche variabili.
  {
    "Puppy count: "
    <> case list.length(puppies) {
      0 -> "None."
      1 -> "Just one."
      other -> "As many as " <> int.to_string(other) <> " puppies."
    }
  }
  |> echo

  // Considera che i linguaggi BEAM sono funzionali nel design e Gleam non fa eccezione
  // quindi non sono disponibili costrutti if, for o while.

  // Utilizza la corrispondenza dei modelli per i condizionali
  let answer = 42
  case answer == 42 {
    True -> {
      echo "This is the answer to the universe."
    }
    False -> {
      echo "This is the answer to something else."
    }
  }

  // Usa la ricorsione invece del looping
  from_one_to_ten(1)
}

// Funzione ricorsiva
fn from_one_to_ten(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> from_one_to_ten(n + 1)
  }
}

// Per evitare l'esaurimento della memoria dovuto alla creazione eccessiva
//impilare i frame quando si chiamano funzioni in modo ricorsivo, supporta Gleam
// "ottimizzazione delle chiamate di coda", il che significa che il compilatore può riutilizzare
// lo stack frame per la funzione corrente se lo è una chiamata di funzione
// l'ultima cosa che fa la funzione.

pub fn fib(x: Int) -> Int {
  // La funzione pubblica chiama la funzione ricorsiva della coda privata
  fib_loop(x, 1)
}

fn fib_loop(x: Int, accumulator: Int) -> Int {
  case x {
    1 -> accumulator

    // L'ultima cosa che fa questa funzione è chiamare se stessa
    // Nella lezione precedente l'ultima cosa che ha fatto è stata moltiplicare due interi
    _ -> fib_loop(x - 1, accumulator + x)
  }
}

// Gleam supporta la corrispondenza dei modelli del primo elemento e del resto
// di una lista con il pattern [x, ..y] all'interno di un'espressione case.
fn reverse_list(the_list: List(value)) -> List(value) {
  case the_list {
    [head, ..tail] -> list.flatten([reverse_list(tail), [head]])
    [] -> []
  }
}

fn more_on_recursion() {
  echo fib(10)
  // 55
  echo reverse_list([1, 2, 3])
}

fn more_on_pattern_matching() {
  // Quando si verifica la corrispondenza dei modelli sulle stringhe, l'operatore <> corrisponde alle stringhe
  // con un prefisso specifico e assegna il promemoria a una variabile
  let lucy = "Hello, Lucy"
  let _ = echo case lucy {
    "Hello, " <> name -> "Greetings for " <> name
    _ -> "Potentially no greetings"
  }

  // Sono supportati modelli alternativi, quindi viene utilizzata la stessa clausola
  // per più valori
  let month = 2
  let year = 2024
  let number_of_days = case month {
    2 ->
      case is_leap_year(year) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Number of days: " <> int.to_string(number_of_days)
  // 29

  // Guardie in abbinamento di modelli:
  // Quando si utilizza la parola chiave if, un'espressione deve restituire True
  // affinché il modello corrisponda.
  let list_starts_with = fn(the_list: List(value), the_value: value) -> Bool {
    case the_list {
      [head, ..] if head == the_value -> True
      _ -> False
    }
  }
  echo list_starts_with([10, 20, 30], 10)
  // True
}

pub type Gender {
  Male
  Female
  Other
}

// Registrazioni:
// - Supporta varianti
// - Ogni variante è simile a una struttura con campi
pub type Shape {
  Rectangle(base: Float, height: Float)
  Triangle(base: Float, height: Float)
}

// I record con una variante assomigliano alle strutture
pub type Point {
  Point(x: Float, y: Float)
}

fn showcase_types() {
  //Tuple:
  // - Può mescolare insieme elementi di diverso tipo
  // - Il loro tipo è implicito, ad es. #{1, "Hello"} è di tipo #{Int, String}
  // - È possibile accedere ai loro elementi tramite indici numerici
  let tuple_01 = #(1, "Ferris", "rustacean", True)
  let tuple_02 = #(1, "Lucy", "starfish", True)
  echo tuple_01
  echo tuple_01.0
  // 1
  echo tuple_02.1
  // Lucia
  let #(_, name, species, _) = tuple_01
  echo name <> " the " <> species

  // Corrispondenza di modelli con tuple inclusa l'assegnazione delle variabili
  let _ = print_mascot_status(tuple_02)

  // Utilizzo di un tipo personalizzato con corrispondenza di modelli
  let gender = Other
  let _ = echo gender_to_string(gender)

  // Utilizzo dei record
  let rectangle_1 = Rectangle(base: 10.0, height: 20.0)
  echo rectangle_1.height
  // 10.3

  let point_1 = Point(x: 3.2, y: 4.3)
  echo point_1

  // Aggiornamento di un record
  let point_2 = Point(..point_1, y: 5.7)
  echo point_2

  // In Gleam, i valori non sono annullabili.
  // Nil è l'unico valore del suo tipo.
  let some_var = Nil
  let result = io.println("Hello!")
  echo some_var == result
  // True
}

fn print_mascot_status(mascot: #(Int, String, String, Bool)) {
  case mascot {
    #(_, name, _, True) -> echo name <> " is a mascot."
    #(_, name, _, False) -> echo name <> " is not a mascot."
  }
}

fn gender_to_string(gender: Gender) -> String {
  case gender {
    Male -> "Boy"
    Female -> "Girl"
    Other -> "Undetermined"
  }
}

pub type Mineral {
  Gold
  Silver
  Copper
}

// Tipi personalizzati generici con tipi contenuti come parametri
pub type Purity(inner_type) {
  Pure(inner_type)
  Impure(inner_type)
}

pub type Beverage {
  Water
  Juice
}

// Tipi personalizzati esistenti dai moduli gleam/option e gleam/result
// facilitare l'utilizzo di valori nullable e la gestione di potenziali errori
pub type Person {
  Person(name: String, nickname: Option(String))
}

pub type DiceError {
  DiceValueOutOfRange
}

fn checked_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(value)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn double_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 -> Ok(value * 2)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn more_on_types() {
  let mineral_sample_01: Purity(Mineral) = Pure(Gold)
  let mineral_sample_02 = Impure(Silver)
  echo mineral_sample_01
  echo mineral_sample_02

  // Un bicchiere può essere vuoto oppure no
  let glass_01: Option(Beverage) = Some(Water)
  let glass_02 = None
  echo glass_01
  echo glass_02

  // Una persona può avere un soprannome oppure no
  let person_01 = Person(name: "John", nickname: Some("The Ripper"))
  let person_02 = Person(name: "Martin", nickname: None)
  echo person_01
  echo person_02

  // Utilizzo di funzioni che restituiscono valori di tipo Risultato
  let dice_01 = 5
  case checked_dice_value(dice_01) {
    Ok(checked_value) ->
      echo "The value of " <> int.to_string(checked_value) <> " is OK."
    Error(DiceValueOutOfRange) ->
      echo "The value of the dice is out of range"
  }

  // Proviamo a raddoppiare il valore se il valore risultante è fermo
  // un numero su uno qualsiasi dei lati del dado.
  // Altrimenti, mettiamo il valore massimo.
  2
  |> checked_dice_value
  |> result.try(double_dice_value)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn throw_dice_as_result() {
  Ok(int.random(6) + 1)
}

pub fn sum_dice_values(a: Int, b: Int) {
  Ok(a + b)
}

//Scommesse su funzioni di prima classe e pattern-matching
// può facilmente portare a tonnellate di rientranze
fn roll_two_dices_without_use() {
  result.try(throw_dice_as_result(), fn(first_dice) {
    result.try(throw_dice_as_result(), fn(second_dice) {
      sum_dice_values(first_dice, second_dice)
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
fn roll_two_dices_with_use() {
  use first_dice <- result.try(throw_dice_as_result())
  use second_dice <- result.try(throw_dice_as_result())
  sum_dice_values(first_dice, second_dice)
}

fn more_on_callbacks() {
  let _ = echo roll_two_dices_without_use()
  let _ = echo roll_two_dices_with_use()
  Nil
}

// Dalla versione 1.14.0, l'annotazione @external può essere utilizzata anche su tipi esterni
// indirizzarli ai tipi Erlang o TypeScript.
pub type DateTime

// Le funzioni esterne devono annotare un tipo restituito.
// Questa funzione è definita solo sul target Erlang; compilazione in JavaScript
// richiederebbe un corpo di fallback o un'annotazione @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn now() -> DateTime {
  todo
}

fn showcase_externals() {
  let _ = echo now()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Costanti a livello di modulo (supportate dalla versione 1.0.0, con elenco anteposto dalla versione 1.16.0)
pub const prime_numbers = [2, 3, 5]
pub const more_primes = [7, 11, ..prime_numbers]

fn showcase_constants() {
  let _ = echo prime_numbers
  let _ = echo more_primes

  // Dalla versione 1.17.0, puoi anche utilizzare `todo` all'interno delle dichiarazioni di costanti:
  // pub const costante_imminente = da fare
  Nil
}

fn showcase_dicts() {
  // I dizionari sono raccolte di valori-chiave (stdlib v0.33.0)
  let scores = dict.new()
    |> dict.insert("Alice", 10)
    |> dict.insert("Bob", 15)

  let _ = echo dict.get(scores, "Alice")
  // Ok(10)
  let _ = echo dict.get(scores, "Charlie")
  // Error(Nil)
  Nil
}

fn showcase_bit_arrays() {
  // Gli array di bit rappresentano sequenze di dati binari (stdlib v0.32.0)
  let binary_data = <<0x41, 0x42, 0x43>> //"ABC" in ASCII
  let _ = echo binary_data

  // In Gleam, il pattern match con `let` deve essere esaustivo (deve coprire tutti
  // valori possibili). Se un modello potrebbe non riuscire a corrispondere in fase di esecuzione, dobbiamo utilizzare
  // `let assert` invece di `let`. Se il match fallisce, il programma va nel panico:
  let assert <<first_byte, rest:bytes>> = binary_data
  let _ = echo first_byte // 65
  let _ = echo rest // <<66, 67>>
  Nil
}

// I tipi opachi sono tipi i cui costruttori sono privati del modulo di definizione,
// abilitare l'incapsulamento e nascondere i dettagli di implementazione.
pub opaque type EncryptedData {
  EncryptedData(value: String)
}

pub fn encrypt(data: String) -> EncryptedData {
  EncryptedData(data <> "_encrypted")
}

fn showcase_opaque_types() {
  let secret = encrypt("my_password")
  // Possiamo passare il segreto, ma non possiamo creare corrispondenze su EncryptedData
  // oppure accedi al suo campo .value dall'esterno di questo modulo.
  let _ = echo secret
  Nil
}

fn showcase_panic() {
  // Possiamo interrompere deliberatamente l'esecuzione utilizzando la parola chiave panico
  // per far sì che il nostro programma si blocchi immediatamente
  let three = 3
  let two = 2
  case three == two {
    True -> panic as "The equality operator is broken!"
    False -> "Equality operator works for integers"
  }
  // Anche la chiamata di una funzione che utilizza la parola chiave todo si blocca
  // compiti()
}

pub fn homework() {
  todo as "This function is not implemented yet"
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
