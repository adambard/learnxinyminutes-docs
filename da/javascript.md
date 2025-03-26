---
contributors:
  - ["Leigh Brenecki", "https://leigh.net.au"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
  - ["Daniel Niemann Hjermitslev", "https://github.com/dnh33"]
---

JavaScript-vejledning på dansk.

JavaScript blev skabt af Brendan Eich i 1995, mens han arbejdede hos Netscape.
Hans oprindelige intention var at skabe et simpelt sprog til websteder, som skulle
supplere Java til mere komplekse applikationer. På grund af den tætte integration med
websteder og standardunderstøttelse i moderne browsere er det blevet meget mere
almindeligt til frontend-udvikling end Java.

JavaScript er dog ikke kun begrænset til webbrowsere: Node.js, et projekt der giver et
uafhængigt kørselsmiljø til Googles Chrome V8-motor, bliver mere og mere populært.

```javascript
// Enkeltlinje kommentarer starter med to skråstreger.
/* Flerlinje kommentarer starter med skråstreg-stjerne,
   og slutter med stjerne-skråstreg */

// Udsagn kan afsluttes med ; (semikolon)
gørNoget();

// ... men semikoloner behøver ikke at være der, da semikoloner automatisk indsættes
// hvor der er en ny linje, undtagen i visse tilfælde.
gørNoget();

// Da disse tilfælde kan forårsage uventede resultater, vil vi fortsætte med at bruge
// semikoloner i denne guide.

///////////////////////////////////
// 1. Tal, Strenge og Operatorer

// JavaScript har én taltype (som er en 64-bit IEEE 754 double).
// Doubles har en 52-bit mantissa, hvilket er nok til at gemme heltal
// op til omkring 9✕10¹⁵ præcist.
3; // = 3
1.5; // = 1.5

// Nogle grundlæggende aritmetiske operationer virker som du ville forvente.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Inklusiv ujævn division.
5 / 2; // = 2.5

// Og modulo division.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Bitvise operationer virker også; når du udfører en bitvis operation, konverteres din float
// til et signeret heltal *op til* 32 bits.
1 << 2; // = 4

// Præcedens håndhæves med parenteser.
(1 + 3) * 2; // = 8

// Der er tre specielle ikke-et-rigtigt-tal værdier:
Infinity; // resultat af f.eks. 1/0
-Infinity; // resultat af f.eks. -1/0
NaN; // resultat af f.eks. 0/0, står for 'Not a Number'

// Der er også en boolesk type.
true;
false;

// Strenge oprettes med ' eller ".
("abc");
("Hej, verden!");

// Negation bruger ! symbolet
!true; // = false
!false; // = true

// Lighed er ===
1 === 1; // = true
2 === 1; // = false

// Ulighed er !==
1 !== 1; // = false
2 !== 1; // = true

// Flere sammenligninger
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Strenge sammenkædes med +
"Hej " + "verden!"; // = "Hej verden!"

// ... hvilket virker med mere end bare strenge
"1, 2, " + 3; // = "1, 2, 3"
"Hej " + ["verden", "!"]; // = "Hej verden,!"

// ... hvilket kan resultere i nogle underlige adfærd...
13 + !0; // 14
"13" + !0; // '13true'

// og sammenlignes med < og >
"a" < "b"; // = true

// Type tvang udføres for sammenligninger med dobbelt lighedstegn...
"5" == 5; // = true
null == undefined; // = true

// ... medmindre du bruger ===
"5" === 5; // = false
null === undefined; // = false

// Du kan tilgå tegn i en streng med `charAt`
"Denne streng er god".charAt(0); // = 'D'

// ... eller bruge `substring` for at få større dele.
"Hej verden".substring(0, 3); // = "Hej"

// `length` er en egenskab, så brug ikke ().
"Hej".length; // = 3

// Der er også `null` og `undefined`.
null; // bruges til at angive en bevidst ikke-værdi
undefined; // bruges til at angive at en værdi ikke er til stede i øjeblikket (selvom
// `undefined` faktisk er en værdi i sig selv)

// false, null, undefined, NaN, 0 og "" er falsy; alt andet er truthy.
// Bemærk at 0 er falsy og "0" er truthy, selvom 0 == "0".

///////////////////////////////////
// 2. Variable, Arrays og Objekter

// Variable deklareres med `var` nøgleordet. JavaScript er dynamisk
// typet, så du behøver ikke at angive typen. Tildeling bruger et enkelt `=`
// tegn.
var nogleVar = 5;

// Hvis du udelader var nøgleordet, får du ikke en fejl...
andenVar = 10;

// ... men din variabel vil blive oprettet i det globale scope, ikke i det scope
// du definerede den i.

// Variable deklareret uden at blive tildelt er sat til undefined.
var tredjeVar; // = undefined

// Hvis du vil deklarere et par variable, kan du bruge et komma
// separator
var fjerdeVar = 2,
  femteVar = 4;

// Der er en kortform for at udføre matematiske operationer på variable:
nogleVar += 5; // tilsvarende nogleVar = nogleVar + 5; nogleVar er nu 10
nogleVar *= 10; // nu er nogleVar 100

// og en endnu kortere form for at tilføje eller trække 1
nogleVar++; // nu er nogleVar 101
nogleVar--; // tilbage til 100

// Arrays er ordnede lister af værdier, af enhver type.
var mitArray = ["Hej", 45, true];

// Deres medlemmer kan tilgås ved hjælp af firkantede parenteser subscript syntaks.
// Array indekser starter ved nul.
mitArray[1]; // = 45

// Arrays er mutable og af variabel længde.
mitArray.push("Verden");
mitArray.length; // = 4

// Tilføj/Ændr ved specifik indeks
mitArray[3] = "Hej";

// Tilføj og fjern element fra fronten eller bagsiden af et array
mitArray.unshift(3); // Tilføj som det første element
nogleVar = mitArray.shift(); // Fjern første element og returner det
mitArray.push(3); // Tilføj som det sidste element
nogleVar = mitArray.pop(); // Fjern sidste element og returner det

// Sammenføj alle elementer i et array med semikolon
var mitArray0 = [32, false, "js", 12, 56, 90];
mitArray0.join(";"); // = "32;false;js;12;56;90"

// Få subarray af elementer fra indeks 1 (inkluderet) til 4 (ekskluderet)
mitArray0.slice(1, 4); // = [false, "js", 12]

// Fjern 4 elementer startende fra indeks 2, og indsæt der strenge
// "hej", "ver" og "den"; returner fjernet subarray
mitArray0.splice(2, 4, "hej", "ver", "den"); // = ["js", 12, 56, 90]
// mitArray0 === [32, false, "hej", "ver", "den"]

// JavaScripts objekter svarer til "ordbøger" eller "maps" i andre
// sprog: en uordnet samling af nøgle-værdi par.
var mitObj = { nøgle1: "Hej", nøgle2: "Verden" };

// Nøgler er strenge, men citationstegn er ikke påkrævet, hvis de er et gyldigt
// JavaScript identifikator. Værdier kan være af enhver type.
var mitObj = { minNøgle: "minVærdi", "min anden nøgle": 4 };

// Objektattributter kan også tilgås ved hjælp af subscript syntaks,
mitObj["min anden nøgle"]; // = 4

// ... eller ved hjælp af punkt syntaks, forudsat at nøglen er et gyldigt identifikator.
mitObj.minNøgle; // = "minVærdi"

// Objekter er mutable; værdier kan ændres og nye nøgler tilføjes.
mitObj.minTredjeNøgle = true;

// Hvis du prøver at tilgå en værdi, der endnu ikke er sat, får du undefined.
mitObj.minFjerdeNøgle; // = undefined

///////////////////////////////////
// 3. Logik og Kontrolstrukturer

// `if` strukturen virker som du ville forvente.
var tæller = 1;
if (tæller == 3) {
  // evalueres hvis tæller er 3
} else if (tæller == 4) {
  // evalueres hvis tæller er 4
} else {
  // evalueres hvis det ikke er enten 3 eller 4
}

// Det samme gør `while`.
while (true) {
  // En uendelig løkke!
}

// Do-while løkker er som while løkker, undtagen at de altid kører mindst én gang.
var inddata;
do {
  inddata = hentInddata();
} while (!erGyldig(inddata));

// `for` løkken er den samme som i C og Java:
// initialisering; fortsættelsesbetingelse; iteration.
for (var i = 0; i < 5; i++) {
  // vil køre 5 gange
}

// Breaking out of labeled loops is similar to Java
ydre: for (var i = 0; i < 10; i++) {
  for (var j = 0; j < 10; j++) {
    if (i == 5 && j == 5) {
      break ydre;
      // bryder ud af den ydre løkke i stedet for kun den indre
    }
  }
}

// for/in udsagnet tillader iteration over egenskaber i et objekt.
var beskrivelse = "";
var person = { fornavn: "Paul", efternavn: "Ken", alder: 18 };
for (var x in person) {
  beskrivelse += person[x] + " ";
} // beskrivelse = 'Paul Ken 18 '

// for/of udsagnet tillader iteration over iterable objekter (inklusive de indbyggede String,
// Array, f.eks. de Array-lignende arguments eller NodeList objekter, TypedArray, Map og Set,
// og brugerdefinerede iterables).
var mineKæledyr = "";
var kæledyr = ["kat", "hund", "hamster", "pindsvin"];
for (var kæledyrItem of kæledyr) {
  mineKæledyr += kæledyrItem + " ";
} // mineKæledyr = 'kat hund hamster pindsvin '

// && er logisk og, || er logisk eller
if (hus.størrelse == "stor" && hus.farve == "blå") {
  hus.indeholder = "bjørn";
}
if (farve == "rød" || farve == "blå") {
  // farve er enten rød eller blå
}

// && og || "kortslutter", hvilket er nyttigt til at sætte standardværdier.
var navn = andetNavn || "standard";

// `switch` udsagnet checker for lighed med `===`.
// Brug 'break' efter hver case
// ellers vil cases efter den korrekte også blive udført.
karakter = "B";
switch (karakter) {
  case "A":
    console.log("Godt arbejde");
    break;
  case "B":
    console.log("OK arbejde");
    break;
  case "C":
    console.log("Du kan gøre det bedre");
    break;
  default:
    console.log("Åh nej");
    break;
}

///////////////////////////////////
// 4. Funktioner, Scope og Closures

// JavaScript funktioner deklareres med `function` nøgleordet.
function minFunktion(ting) {
  return ting.toUpperCase();
}
minFunktion("hej"); // = "HEJ"

// Bemærk at værdien, der skal returneres, skal starte på samme linje som
// `return` nøgleordet, ellers vil du altid returnere `undefined` på grund af
// automatisk semikolonindsættelse. Vær opmærksom på dette ved brug af Allman stil.
function andenFunktion() {
  return; // <- semikolon indsættes automatisk her
  {
    detteErEn: "objekt literal";
  }
}
andenFunktion(); // = undefined

// JavaScript funktioner er første klasses objekter, så de kan tildeles til
// forskellige variabelnavne og sendes til andre funktioner som argumenter - for
// eksempel, når man leverer en event handler:
function tidFunktion() {
  // denne kode vil blive kaldt om 5 sekunder
}
setTimeout(tidFunktion, 5000);
// Bemærk: setTimeout er ikke en del af JS sproget, men leveres af browsere
// og Node.js.

// En anden funktion leveret af browsere er setInterval
function intervalFunktion() {
  // denne kode vil blive kaldt hvert 5. sekund
}
setInterval(intervalFunktion, 5000);

// Funktionsobjekter behøver ikke engang at blive deklareret med et navn - du kan skrive
// en anonym funktionsdefinition direkte i argumenterne til en anden.
setTimeout(function () {
  // denne kode vil blive kaldt om 5 sekunder
}, 5000);

// JavaScript har funktionsscope; funktioner får deres eget scope, men andre blokke
// gør ikke.
if (true) {
  var i = 5;
}
i; // = 5 - ikke undefined som du ville forvente i et blok-scopet sprog

// Dette har ført til et almindeligt mønster af "umiddelbart-udførende anonyme
// funktioner", som forhindrer midlertidige variable i at lække ind i det globale
// scope.
(function () {
  var midlertidig = 5;
  // Vi kan tilgå det globale scope ved at tildele til "det globale objekt", som
  // i en webbrowser altid er `window`. Det globale objekt kan have et
  // andet navn i ikke-browser miljøer som Node.js.
  window.permanent = 10;
})();
midlertidig; // rejser ReferenceError
permanent; // = 10

// En af JavaScripts mest kraftfulde funktioner er closures. Hvis en funktion er
// defineret inde i en anden funktion, har den indre funktion adgang til alle
// den ydre funktions variable, selv efter den ydre funktion er afsluttet.
function sigHejOmFemSekunder(navn) {
  var besked = "Hej, " + navn + "!";
  // Indre funktioner placeres i det lokale scope som standard, som om de var
  // deklareret med `var`.
  function indre() {
    alert(besked);
  }
  setTimeout(indre, 5000);
  // setTimeout er asynkron, så sigHejOmFemSekunder funktionen vil
  // afslutte øjeblikkeligt, og setTimeout vil kalde indre bagefter. Men
  // fordi indre er "closed over" sigHejOmFemSekunder, har indre stadig
  // adgang til `besked` variablen, når den endelig kaldes.
}
sigHejOmFemSekunder("Adam"); // vil åbne en popup med "Hej, Adam!" om 5 sekunder

///////////////////////////////////
// 5. Mere om Objekter; Konstruktører og Prototyper

// Objekter kan indeholde funktioner.
var mitObj = {
  minFunc: function () {
    return "Hej verden!";
  },
};
mitObj.minFunc(); // = "Hej verden!"

// Når funktioner tilknyttet et objekt kaldes, kan de tilgå objektet
// de er tilknyttet ved hjælp af `this` nøgleordet.
mitObj = {
  minStreng: "Hej verden!",
  minFunc: function () {
    return this.minStreng;
  },
};
mitObj.minFunc(); // = "Hej verden!"

// Hvad `this` er sat til har at gøre med, hvordan funktionen kaldes, ikke hvor
// den er defineret. Så vores funktion virker ikke, hvis den ikke kaldes i
// konteksten af objektet.
var minFunc = mitObj.minFunc;
minFunc(); // = undefined

// Omvendt kan en funktion tildeles til objektet og få adgang til det
// gennem `this`, selvom den ikke var tilknyttet, da den blev defineret.
var minAndenFunc = function () {
  return this.minStreng.toUpperCase();
};
mitObj.minAndenFunc = minAndenFunc;
mitObj.minAndenFunc(); // = "HEJ VERDEN!"

// Vi kan også specificere en kontekst for en funktion at udføre i, når vi kalder den
// ved hjælp af `call` eller `apply`.
var endnuEnFunc = function (s) {
  return this.minStreng + s;
};
endnuEnFunc.call(mitObj, " Og hej måne!"); // = "Hej verden! Og hej måne!"

// `apply` funktionen er næsten identisk, men tager et array som argumentliste.
endnuEnFunc.apply(mitObj, [" Og hej sol!"]); // = "Hej verden! Og hej sol!"

// Dette er nyttigt, når man arbejder med en funktion, der accepterer en sekvens af
// argumenter, og du vil sende et array.
Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Men `call` og `apply` er kun midlertidige. Når vi vil have det til at blive ved, kan vi
// bruge `bind`.
var bundetFunc = endnuEnFunc.bind(mitObj);
bundetFunc(" Og hej Saturn!"); // = "Hej verden! Og hej Saturn!"

// Når du kalder en funktion med `new` nøgleordet, oprettes et nyt objekt, og
// gøres tilgængeligt for funktionen via `this` nøgleordet. Funktioner designet til at blive
// kaldt sådan kaldes konstruktører.
var MinKonstruktør = function () {
  this.mitTal = 5;
};
mitNyeObj = new MinKonstruktør(); // = {mitTal: 5}
mitNyeObj.mitTal; // = 5

// I modsætning til de fleste andre populære objektorienterede sprog har JavaScript ingen
// begreb om 'instanser' oprettet fra 'klasse' blueprints; i stedet kombinerer JavaScript
// instantiation og arv i et enkelt begreb: en 'prototype'.

// Hvert JavaScript objekt har en 'prototype'. Når du prøver at tilgå en egenskab
// på et objekt, der ikke findes på selve objektet, vil interpreteren
// kigge på dens prototype.

// Nogle JS implementeringer lader dig tilgå et objekts prototype på den magiske
// egenskab `__proto__`. Selvom dette er nyttigt til at forklare prototyper, er det ikke
// en del af standarden; vi vil komme til standard måder at bruge prototyper på senere.
var mitObj = {
  minStreng: "Hej verden!",
};
var minPrototype = {
  livetsBetydning: 42,
  minFunc: function () {
    return this.minStreng.toLowerCase();
  },
};
mitObj.__proto__ = minPrototype;
mitObj.livetsBetydning; // = 42

// Dette virker også for funktioner.
mitObj.minFunc(); // = "hej verden!"

// Selvfølgelig, hvis din egenskab ikke er på din prototype, søges prototypens
// prototype, og så videre.
minPrototype.__proto__ = {
  minBool: true,
};
mitObj.minBool; // = true

// Der er ingen kopiering involveret her; hvert objekt gemmer en reference til sin
// prototype. Dette betyder, at vi kan ændre prototypen, og vores ændringer vil blive
// afspejlet overalt.
minPrototype.livetsBetydning = 43;
mitObj.livetsBetydning; // = 43

// for/in udsagnet tillader iteration over egenskaber i et objekt,
// går op ad prototypekæden, indtil det ser en null prototype.
for (var x in mitObj) {
  console.log(mitObj[x]);
}
// udskriver:
// Hej verden!
// 43
// [Function: minFunc]
// true

// For kun at overveje egenskaber tilknyttet selve objektet
// og ikke dets prototyper, brug `hasOwnProperty()` check.
for (var x in mitObj) {
  if (mitObj.hasOwnProperty(x)) {
    console.log(mitObj[x]);
  }
}
// udskriver:
// Hej verden!

// Vi nævnte at `__proto__` var ikke-standard, og der er ingen standard måde at
// ændre prototypen på et eksisterende objekt. Der er dog to måder at
// oprette et nyt objekt med en given prototype.

// Den første er Object.create, som er en nylig tilføjelse til JS, og derfor
// ikke tilgængelig i alle implementeringer endnu.
var mitObj = Object.create(minPrototype);
mitObj.livetsBetydning; // = 43

// Den anden måde, som virker overalt, har at gøre med konstruktører.
// Konstruktører har en egenskab kaldet prototype. Dette er *ikke* prototypen af
// selve konstruktørfunktionen; i stedet er det prototypen, som nye objekter
// får, når de oprettes med den konstruktør og new nøgleordet.
MinKonstruktør.prototype = {
  mitTal: 5,
  hentMitTal: function () {
    return this.mitTal;
  },
};
var mitNyeObj2 = new MinKonstruktør();
mitNyeObj2.hentMitTal(); // = 5
mitNyeObj2.mitTal = 6;
mitNyeObj2.hentMitTal(); // = 6

// Indbyggede typer som strenge og tal har også konstruktører, der opretter
// tilsvarende wrapper objekter.
var mitTal = 12;
var mitTalObj = new Number(12);
mitTal == mitTalObj; // = true

// Bortset fra at de ikke er præcist tilsvarende.
typeof mitTal; // = 'number'
typeof mitTalObj; // = 'object'
mitTal === mitTalObj; // = false
if (0) {
  // Denne kode vil ikke blive udført, fordi 0 er falsy.
}
if (new Number(0)) {
  // Denne kode vil blive udført, fordi wrapped numbers er objekter, og objekter
  // er altid truthy.
}

// Dog deler wrapper objekterne og de almindelige indbyggede en prototype, så
// du kan faktisk tilføje funktionalitet til en streng, for eksempel.
String.prototype.førsteTegn = function () {
  return this.charAt(0);
};
"abc".førsteTegn(); // = "a"

// Dette faktum bruges ofte i "polyfilling", som er implementering af nyere
// funktioner i JavaScript i en ældre undergruppe af JavaScript, så de kan
// bruges i ældre miljøer som forældede browsere.

// For eksempel nævnte vi, at Object.create ikke er tilgængelig i alle
// implementeringer endnu, men vi kan stadig bruge det med denne polyfill:
if (Object.create === undefined) {
  // overskriv det ikke, hvis det eksisterer
  Object.create = function (proto) {
    // lav en midlertidig konstruktør med den rigtige prototype
    var Konstruktør = function () {};
    Konstruktør.prototype = proto;
    // brug den derefter til at oprette et nyt, passende prototyperet objekt
    return new Konstruktør();
  };
}

// ES6 Tilføjelser

// "let" nøgleordet tillader dig at definere variable i et leksikalsk scope,
// i modsætning til et funktionsscope som var nøgleordet gør.
let navn = "Billy";

// Variable defineret med let kan tildeles nye værdier.
navn = "William";

// "const" nøgleordet tillader dig at definere en variabel i et leksikalsk scope
// som med let, men du kan ikke tildele værdien igen, når den først er tildelt.
const pi = 3.14;
pi = 4.13; // Du kan ikke gøre dette.

// Der er en ny syntaks for funktioner i ES6 kendt som "lambda syntaks".
// Dette tillader funktioner at blive defineret i et leksikalsk scope som med variable
// defineret af const og let.
const erLige = (tal) => {
  return tal % 2 === 0;
};
erLige(7); // false

// Den "tilsvarende" funktion i den traditionelle syntaks ville se sådan ud:
function erLigeTraditionel(tal) {
  return tal % 2 === 0;
}

// Jeg satte ordet "tilsvarende" i citationstegn, fordi en funktion defineret
// ved hjælp af lambda syntaksen ikke kan kaldes før definitionen.
// Følgende er et eksempel på ugyldig brug:
tilføj(1, 8);
const tilføj = (førsteTal, andetTal) => {
  return førsteTal + andetTal;
};
```
