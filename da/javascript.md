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
/_ Flerlinje kommentarer starter med skråstreg-stjerne,
og slutter med stjerne-skråstreg _/

// Udsagn kan afsluttes med ;
doStuff();

// ... men de behøver ikke at være der, da semikoloner automatisk indsættes
// hvor der er en ny linje, undtagen i visse tilfælde.
doStuff()

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
10 \* 2; // = 20
35 / 5; // = 7

// Inklusiv ujævn division.
5 / 2; // = 2.5

// Og modulo division.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Bitvise operationer virker også; når du udfører en bitvis operation, konverteres din float
// til et signeret heltal _op til_ 32 bits.
1 << 2; // = 4

// Præcedens håndhæves med parenteser.
(1 + 3) \* 2; // = 8

// Der er tre specielle ikke-et-rigtigt-tal værdier:
Infinity; // resultat af f.eks. 1/0
-Infinity; // resultat af f.eks. -1/0
NaN; // resultat af f.eks. 0/0, står for 'Not a Number'

// Der er også en boolesk type.
true;
false;

// Strenge oprettes med ' eller ".
'abc';
"Hello, world";

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
"Hello " + "world!"; // = "Hello world!"

// ... hvilket virker med mere end bare strenge
"1, 2, " + 3; // = "1, 2, 3"
"Hello " + ["world", "!"]; // = "Hello world,!"

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
"This is a string".charAt(0); // = 'T'

// ... eller bruge `substring` til for at få større dele.
"Hello world".substring(0, 5); // = "Hello"

// `length` er en egenskab, så brug ikke ().
"Hello".length; // = 5

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
var someVar = 5;

// Hvis du udelader var nøgleordet, får du ikke en fejl...
someOtherVar = 10;

// ... men din variabel vil blive oprettet i det globale scope, ikke i det scope
// du definerede den i.

// Variable deklareret uden at blive tildelt er sat til undefined.
var someThirdVar; // = undefined

// Hvis du vil deklarere et par variable, kan du bruge et komma
// separator
var someFourthVar = 2, someFifthVar = 4;

// Der er en kortform for at udføre matematiske operationer på variable:
someVar += 5; // tilsvarende someVar = someVar + 5; someVar er nu 10
someVar \*= 10; // nu er someVar 100

// og en endnu kortere form for at tilføje eller trække 1
someVar++; // nu er someVar 101
someVar--; // tilbage til 100

// Arrays er ordnede lister af værdier, af enhver type.
var myArray = ["Hello", 45, true];

// Deres medlemmer kan tilgås ved hjælp af firkantede parenteser subscript syntaks.
// Array indekser starter ved nul.
myArray[1]; // = 45

// Arrays er mutable og af variabel længde.
myArray.push("World");
myArray.length; // = 4

// Tilføj/Ændr ved specifik indeks
myArray[3] = "Hello";

// Tilføj og fjern element fra fronten eller bagsiden af et array
myArray.unshift(3); // Tilføj som det første element
someVar = myArray.shift(); // Fjern første element og returner det
myArray.push(3); // Tilføj som det sidste element
someVar = myArray.pop(); // Fjern sidste element og returner det

// Sammenføj alle elementer i et array med semikolon
var myArray0 = [32,false,"js",12,56,90];
myArray0.join(";"); // = "32;false;js;12;56;90"

// Få subarray af elementer fra indeks 1 (inkluderet) til 4 (ekskluderet)
myArray0.slice(1,4); // = [false,"js",12]

// Fjern 4 elementer startende fra indeks 2, og indsæt der strenge
// "hi","wr" og "ld"; returner fjernet subarray
myArray0.splice(2,4,"hi","wr","ld"); // = ["js",12,56,90]
// myArray0 === [32,false,"hi","wr","ld"]

// JavaScripts objekter svarer til "ordbøger" eller "maps" i andre
// sprog: en uordnet samling af nøgle-værdi par.
var myObj = {key1: "Hello", key2: "World"};

// Nøgler er strenge, men citationstegn er ikke påkrævet, hvis de er et gyldigt
// JavaScript identifikator. Værdier kan være af enhver type.
var myObj = {myKey: "myValue", "my other key": 4};

// Objektattributter kan også tilgås ved hjælp af subscript syntaks,
myObj["my other key"]; // = 4

// ... eller ved hjælp af punkt syntaks, forudsat at nøglen er et gyldigt identifikator.
myObj.myKey; // = "myValue"

// Objekter er mutable; værdier kan ændres og nye nøgler tilføjes.
myObj.myThirdKey = true;

// Hvis du prøver at tilgå en værdi, der endnu ikke er sat, får du undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Logik og Kontrolstrukturer

// `if` strukturen virker som du ville forvente.
var count = 1;
if (count == 3){
// evalueres hvis count er 3
} else if (count == 4){
// evalueres hvis count er 4
} else {
// evalueres hvis det ikke er enten 3 eller 4
}

// Det samme gør `while`.
while (true){
// En uendelig løkke!
}

// Do-while løkker er som while løkker, undtagen at de altid kører mindst én gang.
var input;
do {
input = getInput();
} while (!isValid(input));

// `for` løkken er den samme som i C og Java:
// initialisering; fortsættelsesbetingelse; iteration.
for (var i = 0; i < 5; i++){
// vil køre 5 gange
}

// At bryde ud af navngivne løkker ligner Java
outer:
for (var i = 0; i < 10; i++) {
for (var j = 0; j < 10; j++) {
if (i == 5 && j ==5) {
break outer;
// bryder ud af den ydre løkke i stedet for kun den indre
}
}
}

// for/in udsagnet tillader iteration over egenskaber i et objekt.
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
description += person[x] + " ";
} // description = 'Paul Ken 18 '

// for/of udsagnet tillader iteration over iterable objekter (inklusive de indbyggede String,
// Array, f.eks. de Array-lignende arguments eller NodeList objekter, TypedArray, Map og Set,
// og brugerdefinerede iterables).
var myPets = "";
var pets = ["cat", "dog", "hamster", "hedgehog"];
for (var pet of pets){
myPets += pet + " ";
} // myPets = 'cat dog hamster hedgehog '

// && er logisk og, || er logisk eller
if (house.size == "big" && house.colour == "blue"){
house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
// colour er enten rød eller blå
}

// && og || "kortslutter", hvilket er nyttigt til at sætte standardværdier.
var name = otherName || "default";

// `switch` udsagnet checker for lighed med `===`.
// Brug 'break' efter hver case
// ellers vil cases efter den korrekte også blive udført.
grade = 'B';
switch (grade) {
case 'A':
console.log("Great job");
break;
case 'B':
console.log("OK job");
break;
case 'C':
console.log("You can do better");
break;
default:
console.log("Oy vey");
break;
}

///////////////////////////////////
// 4. Funktioner, Scope og Closures

// JavaScript funktioner deklareres med `function` nøgleordet.
function myFunction(thing){
return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Bemærk at værdien, der skal returneres, skal starte på samme linje som
// `return` nøgleordet, ellers vil du altid returnere `undefined` på grund af
// automatisk semikolonindsættelse. Vær opmærksom på dette ved brug af Allman stil.
function myFunction(){
return // <- semikolon indsættes automatisk her
{thisIsAn: 'object literal'};
}
myFunction(); // = undefined

// JavaScript funktioner er første klasses objekter, så de kan tildeles til
// forskellige variabelnavne og sendes til andre funktioner som argumenter - for
// eksempel, når man leverer en event handler:
function myFunction(){
// denne kode vil blive kaldt om 5 sekunder
}
setTimeout(myFunction, 5000);
// Bemærk: setTimeout er ikke en del af JS sproget, men leveres af browsere
// og Node.js.

// En anden funktion leveret af browsere er setInterval
function myFunction(){
// denne kode vil blive kaldt hvert 5. sekund
}
setInterval(myFunction, 5000);

// Funktionsobjekter behøver ikke engang at blive deklareret med et navn - du kan skrive
// en anonym funktionsdefinition direkte i argumenterne til en anden.
setTimeout(function(){
// denne kode vil blive kaldt om 5 sekunder
}, 5000);

// JavaScript har funktionsscope; funktioner får deres eget scope, men andre blokke
// gør ikke.
if (true){
var i = 5;
}
i; // = 5 - ikke undefined som du ville forvente i et blok-scopet sprog

// Dette har ført til et almindeligt mønster af "umiddelbart-udførende anonyme
// funktioner", som forhindrer midlertidige variable i at lække ind i det globale
// scope.
(function(){
var temporary = 5;
// Vi kan tilgå det globale scope ved at tildele til "det globale objekt", som
// i en webbrowser altid er `window`. Det globale objekt kan have et
// andet navn i ikke-browser miljøer som Node.js.
window.permanent = 10;
})();
temporary; // rejser ReferenceError
permanent; // = 10

// En af JavaScripts mest kraftfulde funktioner er closures. Hvis en funktion er
// defineret inde i en anden funktion, har den indre funktion adgang til alle
// den ydre funktions variable, selv efter den ydre funktion er afsluttet.
function sayHelloInFiveSeconds(name){
var prompt = "Hello, " + name + "!";
// Indre funktioner placeres i det lokale scope som standard, som om de var
// deklareret med `var`.
function inner(){
alert(prompt);
}
setTimeout(inner, 5000);
// setTimeout er asynkron, så sayHelloInFiveSeconds funktionen vil
// afslutte øjeblikkeligt, og setTimeout vil kalde inner bagefter. Men
// fordi inner er "closed over" sayHelloInFiveSeconds, har inner stadig
// adgang til `prompt` variablen, når den endelig kaldes.
}
sayHelloInFiveSeconds("Adam"); // vil åbne en popup med "Hello, Adam!" om 5 sekunder

///////////////////////////////////
// 5. Mere om Objekter; Konstruktører og Prototyper

// Objekter kan indeholde funktioner.
var myObj = {
myFunc: function(){
return "Hello world!";
}
};
myObj.myFunc(); // = "Hello world!"

// Når funktioner tilknyttet et objekt kaldes, kan de tilgå objektet
// de er tilknyttet ved hjælp af `this` nøgleordet.
myObj = {
myString: "Hello world!",
myFunc: function(){
return this.myString;
}
};
myObj.myFunc(); // = "Hello world!"

// Hvad `this` er sat til har at gøre med, hvordan funktionen kaldes, ikke hvor
// den er defineret. Så vores funktion virker ikke, hvis den ikke kaldes i
// konteksten af objektet.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Omvendt kan en funktion tildeles til objektet og få adgang til det
// gennem `this`, selvom den ikke var tilknyttet, da den blev defineret.
var myOtherFunc = function(){
return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// Vi kan også specificere en kontekst for en funktion at udføre i, når vi kalder den
// ved hjælp af `call` eller `apply`.
var anotherFunc = function(s){
return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// `apply` funktionen er næsten identisk, men tager et array som argumentliste.
anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Dette er nyttigt, når man arbejder med en funktion, der accepterer en sekvens af
// argumenter, og du vil sende et array.
Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Men `call` og `apply` er kun midlertidige. Når vi vil have det til at blive ved, kan vi
// bruge `bind`.
var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` kan også bruges til delvist at anvende (curry) en funktion.
var product = function(a, b){ return a \* b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Når du kalder en funktion med `new` nøgleordet, oprettes et nyt objekt, og
// gøres tilgængeligt for funktionen via `this` nøgleordet. Funktioner designet til at blive
// kaldt sådan kaldes konstruktører.
var MyConstructor = function(){
this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// I modsætning til de fleste andre populære objektorienterede sprog har JavaScript ingen
// begreb om 'instanser' oprettet fra 'klasse' blueprints; i stedet kombinerer JavaScript
// instantiation og arv i et enkelt begreb: en 'prototype'.

// Hvert JavaScript objekt har en 'prototype'. Når du prøver at tilgå en egenskab
// på et objekt, der ikke findes på selve objektet, vil interpreteren
// kigge på dens prototype.

// Nogle JS implementeringer lader dig tilgå et objekts prototype på den magiske
// egenskab `__proto__`. Selvom dette er nyttigt til at forklare prototyper, er det ikke
// en del af standarden; vi vil komme til standard måder at bruge prototyper på senere.
var myObj = {
myString: "Hello world!"
};
var myPrototype = {
meaningOfLife: 42,
myFunc: function(){
return this.myString.toLowerCase();
}
};
myObj.**proto** = myPrototype;
myObj.meaningOfLife; // = 42

// Dette virker også for funktioner.
myObj.myFunc(); // = "hello world!"

// Selvfølgelig, hvis din egenskab ikke er på din prototype, søges prototypens
// prototype, og så videre.
myPrototype.**proto** = {
myBoolean: true
};
myObj.myBoolean; // = true

// Der er ingen kopiering involveret her; hvert objekt gemmer en reference til sin
// prototype. Dette betyder, at vi kan ændre prototypen, og vores ændringer vil blive
// afspejlet overalt.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// for/in udsagnet tillader iteration over egenskaber i et objekt,
// går op ad prototypekæden, indtil det ser en null prototype.
for (var x in myObj){
console.log(myObj[x]);
}
/// udskriver:
// Hello world!
// 43
// [Function: myFunc]
// true

// For kun at overveje egenskaber tilknyttet selve objektet
// og ikke dets prototyper, brug `hasOwnProperty()` check.
for (var x in myObj){
if (myObj.hasOwnProperty(x)){
console.log(myObj[x]);
}
}
/// udskriver:
// Hello world!

// Vi nævnte at `__proto__` var ikke-standard, og der er ingen standard måde at
// ændre prototypen på et eksisterende objekt. Der er dog to måder at
// oprette et nyt objekt med en given prototype.

// Den første er Object.create, som er en nylig tilføjelse til JS, og derfor
// ikke tilgængelig i alle implementeringer endnu.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// Den anden måde, som virker overalt, har at gøre med konstruktører.
// Konstruktører har en egenskab kaldet prototype. Dette er _ikke_ prototypen af
// selve konstruktørfunktionen; i stedet er det prototypen, som nye objekter
// får, når de oprettes med den konstruktør og new nøgleordet.
MyConstructor.prototype = {
myNumber: 5,
getMyNumber: function(){
return this.myNumber;
}
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6;
myNewObj2.getMyNumber(); // = 6

// Indbyggede typer som strenge og tal har også konstruktører, der opretter
// tilsvarende wrapper objekter.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Bortset fra at de ikke er præcist tilsvarende.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
// Denne kode vil ikke blive udført, fordi 0 er falsy.
}
if (new Number(0)){
// Denne kode vil blive udført, fordi wrapped numbers er objekter, og objekter
// er altid truthy.
}

// Dog deler wrapper objekterne og de almindelige indbyggede en prototype, så
// du kan faktisk tilføje funktionalitet til en streng, for eksempel.
String.prototype.firstCharacter = function(){
return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// Dette faktum bruges ofte i "polyfilling", som er implementering af nyere
// funktioner i JavaScript i en ældre undergruppe af JavaScript, så de kan
// bruges i ældre miljøer som forældede browsere.

// For eksempel nævnte vi, at Object.create ikke er tilgængelig i alle
// implementeringer endnu, men vi kan stadig bruge det med denne polyfill:
if (Object.create === undefined){ // overskriv det ikke, hvis det eksisterer
Object.create = function(proto){
// lav en midlertidig konstruktør med den rigtige prototype
var Constructor = function(){};
Constructor.prototype = proto;
// brug den derefter til at oprette et nyt, passende prototyperet objekt
return new Constructor();
};
}

// ES6 Tilføjelser

// "let" nøgleordet tillader dig at definere variable i et leksikalsk scope,
// i modsætning til et funktionsscope som var nøgleordet gør.
let name = "Billy";

// Variable defineret med let kan tildeles nye værdier.
name = "William";

// "const" nøgleordet tillader dig at definere en variabel i et leksikalsk scope
// som med let, men du kan ikke tildele værdien igen, når den først er tildelt.
const pi = 3.14;
pi = 4.13; // Du kan ikke gøre dette.

// Der er en ny syntaks for funktioner i ES6 kendt som "lambda syntaks".
// Dette tillader funktioner at blive defineret i et leksikalsk scope som med variable
// defineret af const og let.
const isEven = (number) => {
return number % 2 === 0;
};
isEven(7); // false

// Den "tilsvarende" funktion i den traditionelle syntaks ville se sådan ud:
function isEven(number) {
return number % 2 === 0;
};

// Jeg satte ordet "tilsvarende" i citationstegn, fordi en funktion defineret
// ved hjælp af lambda syntaksen ikke kan kaldes før definitionen.
// Følgende er et eksempel på ugyldig brug:
add(1, 8);
const add = (firstNumber, secondNumber) => {
return firstNumber + secondNumber;
};
```
