---
language: javascript
contributors:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
translators:
    - ["ggb", "http://www.ideen-und-soehne.de"]
filename: learnjavascript-de.js
lang: de-de
---

(Anmerkungen des Original-Autors:)
JavaScript wurde im Jahr 1995 von Brendan Eich bei Netscape entwickelt. Ursprünglich war es als einfachere Skriptsprache für Websites gedacht, ergänzend zu Java, das für komplexere Webanwendungen verwendet wird. Die enge Integration in Websites und der in Browser eingebaute Support der Sprache haben dafür gesorgt, dass JavaScript weit häufiger für Web-Frontends verwendet wird als Java.

Dabei ist JavaScript inzwischen nicht mehr auf Browser beschränkt: Node.js, ein Projekt, das eine eigene Laufzeitumgebung auf Grundlage von Google Chromes V8 mitbringt, wird derzeit immer populärer.

Feedback ist herzlich Willkommen! Der ursprüngliche Autor ist unter [@adambrenecki](https://twitter.com/adambrenecki) oder [adam@brenecki.id.au](mailto:adam@brenecki.id.au) zu erreichen. Der Übersetzer unter [gregorbg@web.de](mailto:gregorbg@web.de).

```js
// Kommentare werden wie in C gesetzt: Einzeilige Kommentare starten mit zwei 
// Slashes
/* während mehrzeilige Kommentare mit einem 
Slash und einem Stern anfangen und enden */

// Statements können mit einem Semikolon beendet werden
machWas();

// ...müssen sie aber nicht, weil Semikola automatisch eingefügt werden, wenn 
// eine neue Zeile beginnt, abgesehen von einigen Ausnahmen.
machWas()

// Obwohl wir uns für den Anfang nicht um diese Ausnahmen kümmern müssen ist 
// es besser die Semikola immer zu setzen.

///////////////////////////////////
// 1. Nummern, Strings und Operationen

// JavaScript hat einen Nummern-Typ (64-bit IEEE 754 double).
3; // = 3
1.5; // = 1.5

// Beinahe alle grundlegenden arithmetischen Operationen arbeiten wie erwartet.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
10 * 2; // = 20
35 / 5; // = 7

// Division funktioniert auch mit einem Ergebnis nach dem Komma.
5 / 2; // = 2.5

// Bit-weise Operationen sind auch möglich; wenn eine Bit-weise Operation 
// ausgeführt wird, wird die Fließkomma-Zahl in einen 32-bit Integer (mit 
// Vorzeichen) umgewandelt.
1 << 2; // = 4

// Die Rangfolge der Operationen kann mit Klammern erzwungen werden.
(1 + 3) * 2; // = 8

// Es gibt drei spezielle, nicht-reale Nummern-Werte:
Infinity; // Ergebnis von z. B. 1 / 0
-Infinity; // Ergebnis von z. B. -1 / 0
NaN; // Ergebnis von z. B. 0 / 0

// Es gibt auch einen Boolean-Typ (für Wahrheitswerte).
true;
false;

// Strings werden mit ' oder " erzeugt.
'abc';
"Hello, world";

// Für die Negation wird das ! benutzt.
!true; // = false
!false; // = true

// Gleichheit wird mit === geprüft.
1 === 1; // = true
2 === 1; // = false

// Ungleichheit wird mit !== überprüft.
1 !== 1; // = false
2 !== 1; // = true

// Andere Vergleichsoperatoren sind
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Strings können mit + verbunden 
"Hello " + "world!"; // = "Hello world!"

// und mit < und > verglichen werden.
"a" < "b"; // = true

// Für den Vergleich von Werten mit "==" wird eine Typumwandlung erzwungen...
"5" == 5; // = true

// ...solange man nicht === verwendet.
"5" === 5; // = false

// Auf einzelne Buchstaben innerhalb eines Strings kann mit der Methode 
// 'charAt' zugegriffen werden
"This is a string".charAt(0);  // = "T"

// Die Methode 'substring' gibt Teilbereiche eines Strings zurück
"Hello world".substring(0, 5); // = "Hello"

// 'length' ist eine Eigenschaft und wird folglich ohne '()' benutzt
"Hello".length; // = 5

// Es gibt außerdem die Werte 'null' und 'undefined'
null; // wird verwendet um einen vorsätzlich gewählten 'Nicht'-Wert anzuzeigen
undefined; // wird verwendet um anzuzeigen, dass der Wert (aktuell) nicht 
           // verfügbar ist (obwohl genau genommen undefined selbst einen Wert 
           // darstellt)

// false, null, undefined, NaN, 0 und "" sind 'falsy', d. h. alles andere ist 
// wahr. Man beachte, dass 0 falsch und "0" wahr ist, obwohl 0 == "0".

///////////////////////////////////
// 2. Variablen, Arrays und Objekte

// Variablen werden mit dem Schlüsselwort 'var' und einem frei wählbaren 
// Bezeichner deklariert. JavaScript ist dynamisch typisiert, so dass man einer
// Variable keinen Typ zuweisen muss. Die Zuweisung verwendet ein einfaches =.
var einWert = 5;

 // Wenn man das Schlüsselwort 'var' weglässt, bekommt man keinen Fehler
einAndererWert = 10;

// ...aber die Variable wird im globalen Kontext erzeugt, nicht in dem Kontext,
// in dem sie erzeugt wurde.

// Variablen die erzeugt wurden ohne ihnen einen Wert zuzuweisen, erhalten den
// Wert 'undefined'.
var einDritterWert; // = undefined

// Es existiert eine Kurzform, um mathematische Operationen mit Variablen 
// auszuführen:
einWert += 5; // äquivalent zu einWert = einWert + 5; einWert ist nun also 10
einWert *= 10; // einWert ist nach dieser Operation 100

// Und es existiert eine weitere, sogar noch kürzere Form, um 1 zu addieren 
// oder zu subtrahieren
einWert++; // nun ist einWert 101
einWert--; // wieder 100

// Arrays sind geordnete Listen von Werten irgendeines Typs
var myArray = ["Hello", 45, true];

// Auf einzelne Elemente eines Arrays kann zugegriffen werden, in dem der Index
// in eckigen Klammern hinter das Array geschrieben werden. Die Indexierung 
// beginnt bei 0.
myArray[1]; // = 45

// Arrays haben keine feste Länge
myArray.push("World");
myArray.length; // = 4

// und sind veränderlich
myArray[3] = "Hello";

// Die Objekte in JavaScript entsprechen 'dictionaries' oder 'maps' in anderen 
// Sprachen: es handelt sich um ungeordnete Schlüssel-Wert-Paare.
var myObj = { key1: "Hello", key2: "World" };

// Schlüssel sind Strings, aber es werden keine Anführungszeichen benötigt, 
// sofern es sich um reguläre JavaScript-Bezeichner handelt. Werte können von
// jedem Typ sein.
var myObj = { myKey: "myValue", "my other key": 4 };

// Auf Attribute von Objekten kann ebenfalls mit eckigen Klammern zugegriffen
// werden,
myObj["my other key"]; // = 4

// ... oder in dem man die Punkt-Notation verwendet, vorausgesetzt es handelt 
// sich bei dem Schlüssel um einen validen Bezeichner.
myObj.myKey; // = "myValue"

// Objekte sind veränderlich, Werte können verändert und neue Schlüssel 
// hinzugefügt werden.
myObj.myThirdKey = true;

// Der Zugriff auf einen noch nicht definierten Schlüssel, liefert ein 
// undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Logik und Kontrollstrukturen

// Die if-Struktur arbeitet, wie man es erwartet.
var count = 1;
if (count == 3){
    // wird evaluiert, wenn count gleich 3 ist
} else if (count == 4) {
    // wird evaluiert, wenn count gleich 4 ist
} else {
    // wird evaluiert, wenn es weder 3 noch 4 ist
}

// Genauso 'while'.
while (true) {
    // Eine unendliche Schleife!
}

// Do-while-Scheifen arbeiten wie while-Schleifen, abgesehen davon, dass sie 
// immer mindestens einmal ausgeführt werden.
var input;
do {
    input = getInput();
} while ( !isValid( input ) )

// Die for-Schleife arbeitet genau wie in C und Java:
// Initialisierung; Bedingung, unter der die Ausführung fortgesetzt wird; 
// Iteration.
for ( var i = 0; i < 5; i++ ) {
    // wird 5-mal ausgeführt
}

// '&&' ist das logische und, '||' ist das logische oder
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
    // Die Größe des Hauses ist groß und die Farbe blau.
}
if (colour == "red" || colour == "blue"){
    // Die Farbe ist entweder rot oder blau.
}

// Die Auswertung von '&&' und '||' erfolgt so, dass abgebrochen wird, wenn die
// Bedingung erfüllt ist (bei oder) oder nicht-erfüllt ist (bei und). Das ist 
// nützlich, um einen Default-Wert zu setzen.
var name = otherName || "default";

// Ein 'switch' Statement prüft Gleichheit mit ===
// ohne ein 'break' nach jedem Fall
// werden auch die Fälle nach dem korrekten aufgerufen
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
// 4. Funktionen, Geltungsbereich und Closures

// In JavaScript werden Funktionen mit dem Schlüsselwort 'function' deklariert.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Vorsicht: der Ausdruck der den Rückgabewert einer Funktion bildet muss
// auf der selben Zeile beginnen auf der auch das 'return' Keyword steht
// Sonst wird hier ein automatisches Semikolon eingefügt und die Funktion
// gibt 'undefined' zurück
function myFunction()
{
    return // <- Hier wird automatisch ein Semikolon eingefügt
    {
        thisIsAn: 'object literal'
    }
}
myFunction(); // = undefined

// In JavaScript sind Funktionen 'Bürger erster Klasse', also können sie wie 
// Variablen verwendet und als Parameter anderen Funktionen übergeben werden 
// - zum Beispiel, um einen 'event handler' zu 'beliefern'.
function myFunction() {
    // wird ausgeführt, nachdem 5 Sekunden vergangen sind
}
setTimeout(myFunction, 5000);

// Funktionen können auch deklariert werden, ohne ihnen einen Namen zuzuweisen.
// Es ist möglich diese anonymen Funktionen direkt als (oder im) Argument 
// einer anderen Funktion zu definieren.
setTimeout(function(){
    // wird ausgeführt, nachdem 5 Sekunden vergangen sind
}, 5000);

// JavaScript hat einen Geltungsbereich, der sich auf Funktionen erstreckt: 
// Funktionen haben ihren eigenen Geltungsbereich, andere Blöcke nicht.
if(true) {
    var i = 5;
}
i; // = 5 - nicht undefined, wie man es von einer Sprache erwarten würde, die 
   // ihren Geltungsbereich nach Blöcken richtet

// Daraus ergibt sich ein bestimmtes Muster für sofort-ausführbare, anonyme 
// Funktionen, die es vermeiden, dass der globale Geltungsbereich von Variablen
// 'verschmutzt' wird.
(function(){
    var temporary = 5;
    // Auf eine Variable im globalen Geltungsbereich kann zugegriffen werden, 
    // sofern sie im globalen Objekt definiert ist (in einem Webbrowser ist 
    // dies immer das 'window'-Objekt, in anderen Umgebungen, bspw. Node.js, 
    // kann das anders aussehen). 
    window.permanent = 10;
})();
temporary; // wirft einen ReferenceError
permanent; // = 10

// Eines der mächtigsten Charakteristika von JavaScript sind Closures. Wird 
// eine Funktion innerhalb einer anderen Funktion definiert, dann hat die 
// innere Funktion Zugriff auf alle Variablen der äußeren Funktion, sogar dann,
// wenn die äußere Funktion beendet wurde.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout wird asynchron ausgeführt. Also wird sayHelloInFiveSeconds 
    // sofort verlassen und setTimeout wird die innere Funktion 'im nachhinein' 
    // aufrufen. Dennoch: Weil sayHelloInFiveSeconds eine Hülle um die innere 
    // Funktion bildet, hat die innere Funktion immer noch Zugriff auf die 
    // Variable prompt.
}
sayHelloInFiveSeconds("Adam");  // wird nach 5 Sekunden ein Popup mit der 
                                // Nachricht "Hello, Adam!" öffnen.

///////////////////////////////////
// 5. Mehr über Objekte, Konstruktoren und Prototypen

// Objekte können Funktionen enthalten.
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// Wenn Funktionen aufgerufen werden, die zu einem Objekt gehören, können sie 
// auf das eigene Objekt mit dem Schlüsselwort 'this' zugreifen.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// Worauf 'this' gesetzt wird, ist davon abhängig, wie die Funktion aufgerufen 
// wird, nicht wo sie definiert wurde. Unsere Funktion wird daher nicht 
// funktionieren, sofern sie außerhalb des Kontextes des Objekts aufgerufen 
// wird.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Umgekehrt ist es möglich eine Funktion einem Objekt zuzuweisen und dadurch 
// Zugriff auf den this-Kontext zu erhalten, sogar dann, wenn die Funktion dem
// Objekt nach dessen Definition zugewiesen wird.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// Mit den Methoden 'call' und 'apply' kann der Kontext eines Funktionsaufrufs
// verändert werden

var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// 'apply' funktioniert beiahe identisch, erwartet die übergebenen Argumente
// aber in einem Array

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Das ist hilfreich wenn man einer Funktion eine beliebige Zahl Argumente
// übergeben kann

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// 'call' und 'apply' beeinflussen aber nur den spezifischen Aufruf.
// Um den Kontext einer Funktion dauerhaft zu ändern wird 'bind' benutzt.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// Mit 'bind' lassen sich Funktionen auch teilweise anwenden / "curryen".
var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Wenn eine Funktion mit dem Schlüsselwort 'new' aufgerufen wird, dann wird 
// ein neues Objekt erzeugt. Funktionen, die darauf ausgelegt sind in dieser 
// Art aufgerufen zu werden, werden Konstruktoren genannt.
var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Jedes JavaScript-Objekt hat einen Prototyp. Wenn man versucht auf eine 
// Eigenschaft des Objekts zuzugreifen, das nicht im Objekt selbst existiert,
// schaut der Interpreter in dessen Prototyp nach.

// Einige JavaScript-Implementierungen erlauben den direkten Zugriff auf den 
// Prototyp eines Objekts durch die magische Eigenschaft __proto__. Obwohl das
// nützlich ist, um  Prototypen im Allgemeinen zu erklären, ist das nicht Teil
// des Standards; zum Standard-Weg der Nutzung von Prototypen kommen wir 
// später.
var myObj = {
    myString: "Hello world!",
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};
myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// Das funktioniert auch bei Funktionen.
myObj.myFunc(); // = "hello world!"

// Sollte die Eigenschaft nicht im Prototypen des Objekts enthalten sein, dann
// wird im Prototypen des Prototypen nachgesehen und so weiter.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Dafür wird nichts hin und her kopiert; jedes Objekt speichert eine Referenz
// auf seinen Prototypen. Das heißt wenn der Prototyp geändert wird, dann 
// werden die Änderungen überall sichtbar.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// Es wurde bereits erwähnt, dass __proto__ nicht zum Standard gehört und es 
// gibt ebenso keinen Standard-Weg, um den Prototyp eines existierenden Objekts
// zu ändern. Es gibt dennoch zwei Wege, wie man ein neues Objekt mit einem 
// gegebenen Prototypen erzeugt.

// Der erste Weg ist die Methode Object.create, die eine jüngere Ergänzung des
// JavaScript-Standards ist und daher noch nicht in allen Implementierungen 
// verfügbar.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// Der zweite Weg, der immer funktioniert, hat mit den Konstruktoren zu tun. 
// Konstruktoren haben eine Eigenschaft, die Prototyp heißt.  Dabei handelt es
// sich *nicht* um den Prototypen der Konstruktor-Funktion; stattdessen handelt
// es sich um den Prototypen, der einem neuen Objekt mitgegeben wird, wenn es 
// mit dem Konstruktor und dem Schlüsselwort 'new' erzeugt wird.
MyConstructor.prototype = {
    getMyNumber: function(){
        return this.myNumber
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5

// Alle primitiven Typen, also strings und numbers, haben auch Konstruktoren, 
// die zu dem Typ äquivalente Wrapper-Objekte erzeugen. 
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Genau genommen: Sie sind nicht exakt äquivalent.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Dieser Teil wird nicht ausgeführt, weil 0 'falsy' ist.
}

// Das Wrapper-Objekt und die regulären, eingebauten Typen, teilen sich einen 
// Prototyp; so ist es möglich zum Beispiel einem String weitere Funktionen 
// hinzuzufügen.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"

// Diese Tatsache wird häufig bei einer Methode mit dem Namen 'polyfilling'
// verwendet: Dabei wird ein neues Feature von JavaScript in einer älteren 
// Untermenge der Sprache integriert, so dass bestimmte Funktionen auch in 
// älteren Umgebungen und Browsern verwendet werden können.

// Ein Beispiel: Es wurde erwähnt, dass die Methode Object.create nicht in 
// allen Umgebungen verfügbar ist - wir können sie dennoch verwenden, mit einem
// 'polyfill':
if (Object.create === undefined){ // überschreib nichts, was eventuell bereits
                                  // existiert
    Object.create = function(proto){
        // erstelle einen vorübergehenden Konstruktor mit dem richtigen
        // Prototypen
        var Constructor = function(){};
        Constructor.prototype = proto;
        // verwende es dann, um ein neues Objekt mit einem passenden 
        // Prototypen zurückzugeben
        return new Constructor();
    }
}
```

## Zur weiteren Lektüre (englisch)

Das [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) bietet eine ausgezeichnete Dokumentation für die Verwendung von JavaScript im Browser. Es ist außerdem ein Wiki und ermöglicht es damit anderen zu helfen, wenn man selbst ein wenig Wissen angesammelt hat.

MDN's [A re-introduction to JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript) führt sehr viele der hier vorgestellten Konzepte im Detail aus. 

Dieses Tutorial hat nur die Sprache JavaScript vorgestellt; um mehr über den  Einsatz in Websites zu lernen, ist es ein guter Start etwas über das [Document Object Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core) zu lernen.

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) ist eine tiefgehende Einführung in die kontra-intuitiven Parts der Sprache.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/) ist ein Klassiker unter den Referenzen.

Zusätzlich zu direkten Beiträgen zu diesem Artikel ist der Inhalt in Anlehnung an Louie Dinh's Python-Tutorial auf dieser Seite und das [JS Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript) des Mozilla Developer Network entstanden.
