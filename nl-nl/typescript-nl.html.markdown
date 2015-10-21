---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
filename: learntypescript-nl.ts
translators:
  - ["Niels van Velzen", "https://nielsvanvelzen.me"]
lang: nl-nl
---

TypeScript is een taal gericht op het versoepelen van de ontwikkeling van
grote applicaties gemaakt in JavaScript.
TypeScript voegt veelgebruikte technieken zoals klassen, modules, interfaces,
generieken en statische typen toe aan JavaScript.
TypeScript is een superset van JavaScript: alle JavaScript code is geldige
TypeScript code waardoor de overgang van JavaScript naar TypeScript wordt versoepeld.

Dit artikel focust zich alleen op de extra's van TypeScript tegenover [JavaScript] (../javascript-nl/).

Om de compiler van TypeScript te kunnen proberen kun je naar de [Playground] (http://www.typescriptlang.org/Playground) gaan.
Hier kun je automatisch aangevulde code typen in TypeScript en de JavaScript variant bekijken.

```js
// Er zijn 3 basis typen in TypeScript
var isKlaar: boolean = false;
var lijnen: number = 42;
var naam: string = "Peter";

// Wanneer het type onbekend is gebruik je "Any"
var nietZeker: any = 4;
nietZeker = "misschien een string";
nietZeker = false; // Toch een boolean

// Voor collecties zijn er "typed arrays"
var lijst: number[] = [1, 2, 3];
// of generieke arrays
var lijst: Array<number> = [1, 2, 3];

// Voor enumeraties:
enum Kleur {Rood, Groen, Blauw};
var c: Kleur = Kleur.Groen;

// Als laatst, "void" wordt gebruikt voor als een functie geen resultaat geeft
function groteVerschrikkelijkeMelding(): void {
  alert("Ik ben een vervelende melding!");
}

// Functies zijn eersteklas ?, supporten de lambda "fat arrow" syntax en
// gebruiken gebruiken "type inference"

// Het volgende is allemaal hetzelfde
var f1 = function(i: number): number { return i * i; }
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
var f4 = (i: number) => { return i * i; }
// Omdat we maar 1 lijn gebruiken hoeft het return keyword niet gebruikt te worden
var f5 = (i: number) =>  i * i;

// Interfaces zijn structureel, elk object wat de eigenschappen heeft
// is een gebruiker van de interface
interface Persoon {
  naam: string;
  // Optionele eigenschappen worden gemarkeerd met "?"
  leeftijd?: number;
  // En natuurlijk functies
  verplaats(): void;
}

// Object die gebruikt maakt van de "Persoon" interface
// Kan gezien worden als persoon sinds het de naam en verplaats eigenschappen bevat
var p: Persoon = { naam: "Bobby", verplaats: () => {} };
// Object met de optionele leeftijd eigenschap
var geldigPersoon: Persoon = { naam: "Bobby", leeftijd: 42, verplaats: () => {} };
// Ongeldig persoon vanwege de leeftijds type
var ongeldigPersoon: Persoon = { naam: "Bobby", leeftijd: true };

// Interfaces kunnen ook een functie ype beschrijven
interface ZoekFunc {
  (bron: string, subString: string): boolean;
}
// Alleen de parameters types zijn belangrijk, namen maken niet uit.
var mySearch: ZoekFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// Classes - leden zijn standaard publiek
class Punt {
    // Eigenschappen
    x: number;

    // Constructor - de publieke / prive trefwoorden in deze context zullen
    // eigenschappen in de klasse kunnen aanmaken zonder ze te defineren.
    // In dit voorbeeld zal "y" net als "x" gedefineerd worden met minder code.
    // Standaard waardes zijn ook gesupport

    constructor(x: number, public y: number = 0) {
        this.x = x;
    }

    // Functies
    dist(): number { return Math.sqrt(this.x * this.x + this.y * this.y); }

    // Statische leden
    static origin = new Punt(0, 0);
}

var p1 = new Punt(10 ,20);
var p2 = new Punt(25); // y zal de waarde 0 krijgen

// Overnemen
class Punt3D extends Punt {
    constructor(x: number, y: number, public z: number = 0) {
        super(x, y); // Constructor van ouder aanroepen (Punt)
    }

    // Overschrijven
    dist(): number {
        var d = super.dist();
        return Math.sqrt(d * d + this.z * this.z);
    }
}

// Modules werken ongeveer hetzelfde als namespaces
// met "." kan je submodules defineren
module Geometrie {
  export class Vierkant {
    constructor(public zijLengte: number = 0) {
    }

    oppervlakte() {
      return Math.pow(this.zijLengte, 2);
    }
  }
}

var s1 = new Geometrie.Vierkant(5);

// Local alias for referencing a module
import G = Geometrie;

var s2 = new G.Vierkant(10);

// Generieken
// Classes
class Tupel<T1, T2> {
    constructor(public item1: T1, public item2: T2) {
    }
}

// Interfaces
interface Paar<T> {
    item1: T;
    item2: T;
}

// En functies
var paarNaarTupel = function<T>(p: Paar<T>) {
    return new Tupel(p.item1, p.item2);
};

var tupel = paarNaarTupel({ item1: "hallo", item2: "wereld" });

// Refferentie naar een definitie bestand:
/// <reference path="jquery.d.ts" />

```

## Verder lezen (engels)
 * [TypeScript Official website] (http://www.typescriptlang.org/)
 * [TypeScript language specifications (pdf)] (http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [Source Code on GitHub] (https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
