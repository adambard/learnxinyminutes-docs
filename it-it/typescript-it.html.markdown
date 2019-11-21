---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Christian Grasso", "https://grasso.io"]
filename: learntypescript-it.ts
lang: it-it
---

TypeScript è un linguaggio basato su JavaScript che punta a rendere il codice
più scalabile introducendo concetti quali le classi, i moduli, le interface,
e i generics.
Poichè TypeScript è un superset di JavaScript, è possibile sfruttare le sue
funzionalità anche in progetti esistenti: il codice JavaScript valido è anche
valido in TypeScript. Il compilatore di TypeScript genera codice JavaScript.

Questo articolo si concentrerà solo sulle funzionalità aggiuntive di TypeScript.

Per testare il compilatore, puoi utilizzare il
[Playground](http://www.typescriptlang.org/Playground), dove potrai scrivere
codice TypeScript e visualizzare l'output in JavaScript.

```ts
// TypeScript ha tre tipi di base
let completato: boolean = false;
let righe: number = 42;
let nome: string = "Andrea";

// Il tipo può essere omesso se è presente un assegnamento a scalari/literal
let completato = false;
let righe = 42;
let nome = "Andrea";

// Il tipo "any" indica che la variabile può essere di qualsiasi tipo
let qualsiasi: any = 4;
qualsiasi = "oppure una stringa";
qualsiasi = false; // o magari un boolean

// Usa la keyword "const" per le costanti
const numeroViteGatti = 9;
numeroViteGatti = 1; // Errore

// Per gli array, puoi usare l'apposito tipo o la versione con i generics
let lista: number[] = [1, 2, 3];
let lista: Array<number> = [1, 2, 3];

// Per le enumerazioni:
enum Colore { Rosso, Verde, Blu };
let c: Colore = Colore.Verde;

// Infine, "void" viene utilizzato per le funzioni che non restituiscono valori
function avviso(): void {
  alert("Sono un piccolo avviso fastidioso!");
}

// Le funzioni supportano la sintassi "a freccia" (lambda) e supportano la type
// inference, cioè per scalari/literal non c'è bisogno di specificare il tipo

// Tutte le seguenti funzioni sono equivalenti, e il compilatore genererà
// lo stesso codice JavaScript per ognuna di esse
let f1 = function (i: number): number { return i * i; }
// Type inference
let f2 = function (i: number) { return i * i; }
// Sintassi lambda
let f3 = (i: number): number => { return i * i; }
// Sintassi lambda + type inference
let f4 = (i: number) => { return i * i; }
// Sintassi lambda + type inference + sintassi abbreviata (senza return)
let f5 = (i: number) => i * i;

// Le interfacce sono strutturali, e qualunque oggetto con le stesse proprietà
// di un'interfaccia è compatibile con essa
interface Persona {
  nome: string;
  // Proprietà opzionale, indicata con "?"
  anni?: number;
  // Funzioni
  saluta(): void;
}

// Oggetto che implementa l'interfaccia Persona
// È una Persona valida poichè implementa tutta le proprietà non opzionali
let p: Persona = { nome: "Bobby", saluta: () => { } };
// Naturalmente può avere anche le proprietà opzionali:
let pValida: Persona = { nome: "Bobby", anni: 42, saluta: () => { } };
// Questa invece NON è una Persona, poichè il tipo di "anni" è sbagliato
let pNonValida: Persona = { nome: "Bobby", anni: true };

// Le interfacce possono anche descrivere una funzione
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// I nomi dei parametri non sono rilevanti: vengono controllati solo i tipi
let ricerca: SearchFunc;
ricerca = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// Classi - i membri sono pubblici di default
class Punto {
  // Proprietà
  x: number;

  // Costruttore - in questo caso la keyword "public" può generare in automatico
  // il codice per l'inizializzazione di una variabile.
  // In questo esempio, verrà creata la variabile y in modo identico alla x, ma
  // con meno codice. Sono supportati anche i valori di default.
  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Funzioni
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Membri statici
  static origine = new Point(0, 0);
}

// Le classi possono anche implementare esplicitamente delle interfacce.
// Il compilatore restituirà un errore nel caso in cui manchino delle proprietà. 
class PersonaDiRiferimento implements Persona {
    nome: string
    saluta() {}
}

let p1 = new Punto(10, 20);
let p2 = new Punto(25); // y = 0

// Inheritance
class Punto3D extends Punto {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // La chiamata esplicita a super è obbligatoria
  }

  // Sovrascrittura
  dist() {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// Moduli - "." può essere usato come separatore per i sottomoduli
module Geometria {
  export class Quadrato {
    constructor(public lato: number = 0) { }

    area() {
      return Math.pow(this.lato, 2);
    }
  }
}

let s1 = new Geometria.Quadrato(5);

// Alias locale per un modulo
import G = Geometria;

let s2 = new G.Quadrato(10);

// Generics
// Classi
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// Interfacce
interface Pair<T> {
  item1: T;
  item2: T;
}

// E funzioni
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// Interpolazione con le template string (definite con i backtick)
let nome = 'Tyrone';
let saluto = `Ciao ${name}, come stai?`
// Possono anche estendersi su più righe
let multiriga = `Questo è un esempio
di stringa multiriga.`;

// La keyword "readonly" rende un membro di sola lettura
interface Persona {
  readonly nome: string;
  readonly anni: number;
}

var p1: Persona = { nome: "Tyrone", anni: 42 };
p1.anni = 25; // Errore, p1.anni è readonly

var p2 = { nome: "John", anni: 60 };
var p3: Person = p2; // Ok, abbiamo creato una versione readonly di p2
p3.anni = 35; // Errore, p3.anni è readonly
p2.anni = 45; // Compila, ma cambia anche p3.anni per via dell'aliasing!

class Macchina {
  readonly marca: string;
  readonly modello: string;
  readonly anno = 2018;

  constructor() {
    // Possiamo anche assegnare nel constructor
    this.marca = "Marca sconosciuta";
    this.modello = "Modello sconosciuto";
  }
}

let numeri: Array<number> = [0, 1, 2, 3, 4];
let altriNumeri: ReadonlyArray<number> = numbers;
altriNumeri[5] = 5; // Errore, gli elementi sono readonly
altriNumeri.push(5); // Errore, il metodo push non esiste (modifica l'array)
altriNumeri.length = 3; // Errore, length è readonly
numeri = altriNumeri; // Errore, i metodi di modifica non esistono
```

## Altre risorse
 * [Sito ufficiale di TypeScript](http://www.typescriptlang.org/)
 * [Specifica di TypeScript](https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md)
 * [Anders Hejlsberg - Introducing TypeScript su Channel 9](http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [TypeScript su GitHub](https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - definizioni per le librerie](http://definitelytyped.org/)
