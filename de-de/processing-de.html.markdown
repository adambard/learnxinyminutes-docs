---
language: processing
filename: learnprocessing.pde
contributors:
    - ["Phone Thant Ko", "http://github.com/phonethantko"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
translators:
  - ["caminsha", "https://github.com/caminsha"]
filename: processing-de.md
lang: de-de
---

## Einführung

Processing ist eine Programmiersprache, welche es ermöglicht, digitale Kunst
und multimediale Inhalte zu erstellen. Mit Processing können Personen ohne
Programmiererfahrung die Grundlagen der Computerprogrammierung in einem
visuellen Kontext erlernen.

Obwohl Processing von Java beeinflusst wurde und auf Java basiert, ist die Syntax
sowohl von Java als auch Javascript beeinflusst worden. Weitere Informationen
sind [hier](https://processing.org/reference/) zu finden.

Die Programmiersprache wird statisch programmiert und kommt mit einer eigenen
offiziellen IDE, damit die Programme kompiliert und ausgeführt werden können.

```
/* ------------
   Mehrzeilige Kommentare werden so gemacht
*/

// Einzeilige Kommentare funktionieren so //

/*
   Da Processing von Java abstammt, ist die Syntax für Kommentare gleich
   wie bei Java (wie du vielleicht oben bemerkt hast)!
   Mehrzeilige Kommentare werden wie hier umschloßen.
*/

/* -------------------------------------------------
   Schreiben und Ausführen von Processing Programmen
   -------------------------------------------------
*/

// In Processing ist der Startpunkt eines Programms die Funktion `setup()` 
// mit dem Rückgabetyp `void`.
// Beachte: Die Syntax ist derjenigen von C++ ziemlich ähnlich.
void setup() {
    // Dies gibt beim Ausführen "Hallo Welt!" auf der Konsole aus.
    println("Hallo Welt!"); // eine weitere Sprache mit einem Semikolon am Ende.
}

// Normalerweise wird der Code für statische Elemente innerhalb der Methode 
// `setup()` geschrieben, da diese lediglich einmal ausgeführt wird.
// Dies kann zum Beispiel das Setzen der Hintergrundfarbe oder das Bestimmen
// der Canvas-Größe sein.
background(color); // Setze die Hintergrundfarbe 
size(width, height, [renderer]); // bestimme die Canvasgröße mit dem optionalen
                                 // Parameter `renderer`.
// Du wirst innerhalb dieses Dokuments noch weitere Parameter sehen.

// Wenn du möchstest, dass Code unendlich oft ausgeführt wird, so muss dieser
// Code innerhalb der `draw()`-Methode stehen.
// `draw()` muss existieren, wenn du möchtest, dass das Programm durchgehend
// läuft. Die `draw()`-Methode darf nur einmal vorkommen.

int i = 0;
void draw() {
    // Dieser Codeblock wird ausgeführt bis er gestoppt wird.
    print(i);
    i++; // Inkrement-Operator
}

// Da wir nun wissen, wie man ein funktionierendes Skript erstellen kann und wie
// dieses ausgeführt wird, fahren wir mit den unterschiedlichen Datentypen und
// Collections weiter, welche in Processing unterstützt werden.

/* -------------------------------------------------
   Datentypen und Collections
   -------------------------------------------------
*/

// Gemäß den Angaben in der Processingreferenz, unterstützt Processing die
// folgenden acht primitiven Datentypen:
boolean booleanValue = true; // Boolean
byte byteValueOfA = 23; // Byte
char charValueOfA = 'A'; // Char (einzelnes Zeichen)
color colorValueOfWhiteM = color(255, 255, 255); // Farben (angegeben durch die
                                                 // `color()`-Methode)
color colorValueOfWhiteH = #FFFFFF; // Farbe (angegeben mit der Hexadezimal-
                                    // schreibweise.)
int intValue = 5; // Integer (ganze Zahl)
long longValue = 2147483648L; // "L" wird hinzugefügt, um es als `long` zu 
                              // markieren.
float floatValue = 1.12345; // Float (32-Bit Gleitkommazahl)
double doubleValue = 1.12345D // Double (64-Bit Gleitkommazahl)

//BEACHTE!
// Auch wenn es die Datentypen "long" und "double" gibt und auch funktionieren,
// verwenden Processing-Funktionen diese Datentypen nicht. Das bedeutet, dass
// diese zu "int" resp. "float" konvertiert werden müssen.
// Dies geschieht, indem man `(int)` oder `(float)` vor die Variable schreibt, 
// bevor diese einer Funktion übergeben werden.

// Es gibt eine ganze Reiher zusammengesetzter Datentypen, welche in Processing
// gebraucht werden können. Um Zeit zu sparen, gehen wir in diesem Tutorial
// lediglich die wichtigsten durch.

// String
// Während der Datentyp `char` einfache Anzührungszeichen (' ') braucht, haben
// Strings doppelte Anführungszeichen (" ").
String sampleString = "Hallo, Processing!";
// Strings können auch durch ein Array von `char`s erstellt werden.
// Wir werden Arrays gleich anschauen.
char source = {'H', 'A', 'L', 'L', 'O'};
String stringFromSource = new String(source); // HALLO
// Wie auch in Java können in Processing Strings auch zusammengefügt werden
// mit dem +-Operator.
print("Hallo " + "Welt!"); // => Hallo Welt!


// Arrays
// In Processing können Arrays jeden Datentypen beinhalten, sogar Objekte.
// Da Arrays ähnlich wie Objekte sind, müssen diese mit dem Schlüsselwort `new`
// erstellt werden.
int[] intArray = new int[5];
int[] intArrayWithValues = {1, 2, 3} // Arrays können auch mit Daten gefüllt 
                                     // werden.
// ArrayList
// Die Funktionen einer ArrayList sind ähnlich wie die eines Arrays und können
// auch jegliche Datentypen beinhalten. Der einzige Unterschied zwischen Arrays
// und `ArrayList`s ist, dass eine `ArrayList` die Größe dynamisch anpassen kann, 
// da es eine Implementierung des "List" Interface in Java ist.
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// Objekte
// Da Processing auf Java basiert, unterstützt Processing die Objektorientierte
// Programmierung. Dies bedeutet, dass du grundsätzlich jegliche Datentypen
// selber erstellen kannst und diese nach deinen Bedürfnissen manipulieren kannst.
// Selbstverständlich muss eine Klasse definiert werden bevor du ein Objekt
// davon instanzieren kannst.
// Format: ClassName InstanceName
SomeRandomClass myObject // hier musst du das Objekt später instazieren
// Hier wird das Objekt direkt instanziert:
SomeRandomClass myObjectInstantiated = new SomeRandomClass(); 

// Processing hat noch weitere Collections (wie zum Beispiel Dictionaries und 
// Listen). Aus Einfachheitsgründen wird dies in diesem Tutorial weggelassen.

/* -------------------------------------------------
   Mathematik
   -------------------------------------------------
*/

// Arithmetik
1 + 1 // => 2
2 -1 // => 1
2 * 3 // => 6
3 / 2 // => 1
3.0 / 2 // => 1.5
3.0 % 2 // => 1.0 (Modulo)

// Processing beinhaltet auch einige Funktionen, welche mathematische
// Operationen vereinfachen
float f = sq(3); // Quadrat => f = 9.0
float p = pow(3, 3); // Potenz => p = 27.0
int a = abs(-13); // Absolute Zahl => a = 13
int r1 = round(3.1); // Runden => r1 = 3
int r2 = round(3.7); // Runden => r2 = 4
int sr = sqrt(25); // Quadratwurzel => sr = 5.0

// Vektoren
// Processing bietet eine einfache Möglichkeit an, mit Vektoren zu arbeiten mit
// der Klasse PVector. Die Klasse kann zwei- und dreidimensionale Vektoren 
// darstellen und bietet Methoden an, welche nützlich sein können für Matrizen-
// Operationen. Weitere Informationen findest du hier: 
// (https://processing.org/reference/PVector.html)

// Trigonometrie
// Processing unterstützt auch trigonometrische Operationen mit Hilfe dieser
// Funktionen: `sin()`, `cos()`, `tan()`, `asin()`, `atan()`. Für die einfache
// Konvertierung gibt es außerdem noch die Funktionen `degrees()` und `radians()`.
// Die trigonometrischen Funktionen rechnen mit dem Winkelmaß Radian, wodurch
// die Gradzahlen zuerst konvertiert werden müssen.
float one = sin(PI/2); // => one = 1.0
// Wie du vielleicht bemerkt hast, existieren einige Konstanten für trigo-
// metrische Operationen; `PI`, `HALF_PI`, `QUARTER_PI` und so weiter ...

/* -------------------------------------------------
   Kontrollstrukturen
   -------------------------------------------------
*/

// Bedingte Anweisungen
// Bedinge Anweisungen werden gleich wie in Java geschrieben.
if (author.getAppearence().equals("hot")) {
    print("Narzissmus vom Feinsten!")
} else {
    // Du kannst hier weitere Bedingungen prüfen.
    print("Irgendetwas ist falsch hier!");
}
// Für die `if`-Anweisungen gibt es auch eine Kurzschreibweise
// Dies sind sogenannte ternäre Operatoren.
int i = 3;
String value = (i > 5) ? "Groß" : "Klein"; // => "Klein"

// Die Switch-Case-Anweisung kann verwendet werden, um mehrere Bedingungen
// zu prüfen.
// Wichtig ist, dass nach jeder Bedingung ein `break`-Statement verwendet wird, 
// sonst werden alle folgenden ausgeführt und es wird nicht mehr überprüft, ob
// die Bedingung wahr ist.
int value = 2;
switch(value) {
    case 0:
        print("Auf keinen Fall!"); // Dies wird nicht ausgeführt.
        break; // Geht zum nächsten Statement und prüft dieses
    case 1:
        print("Wir kommen näher..."); // Auch dies wird nicht ausgeführt
        break;
    case 2:
        print("Bravo!"); // Dies wird ausgeführt.
        break;
    default:
        print("Nicht gefunden."); // Diese Zeile wird ausgeführt, wenn keine
                                  // der anderen Operatoren wahr sind.
        break;
}

// Wiederholungen
// For-Schleifen - Auch hier ist die Syntax wieder gleich wie in Java
for(int i = 0; i < 5; i++) {
    print(i); // Gibt die Zahlen 0 bis 4 aus.
}

// While-Statements
int j = 3; 
while(j > 0) {
    print(j);
    j--; // Dies ist wichtig, dass der Code nicht unendlich lange läuft.
}

// `loop()` | `noloop()` | `redraw()` | `exit()`
// Dies sind spezifische Funktionen, welche in Processing verwendet werden
// können, um den Programmablauf zu steuern.
loop(); // erlaubt es der `draw()`-Methode immer zu laufen, während
noloop(); // dies nur für einmal erlaubt.
redraw(); // führt die `draw()`-Methode noch einmal aus.
exit(); // Diese Methode stoppt das Programm. Dies kann nützlich sein, wenn die
        // Methode `draw()` immer läuft.
```

## Mit Processing zeichnen

Da du nun die Grundsätze der Programmiersprache verstanden hast, schauen wir 
uns nun das Beste an Processing an - Das Zeichnen!

```

/* -------------------------------------------------
   Figuren
   -------------------------------------------------
*/

// 2D-Figuren

// Punkte
point(x,y); // im zweidimensionalen Raum
point(x, y, z); // im dreidimensionalen Raum
// Diese Befehle zeichnen einen Punkt an der Koordinate.

// Linien
line(x1, y1, x2, y2); // im zweidimensionalen Raum
// Dies zeichnet eine Linie, welche durch die zwei Punkte (x1, y1) und (x2, y2)
// definiert wird.
line(x1, y1, z1, x2, y2, z2); // im dreidimensionalen Raum
// Analog wird hier eine Linie gezeichnet mit drei Punkten

// Dreieck
triangle(x1, y1, x2, y2, x3, y3);
// Zeichnet ein Dreieck, welches als Eckpunkte die drei Koordinaten hat.

// Rechteck
rect(a, b, c, d, [r]); // Mit dem optionalen Parameter kann der Winkel aller
                       // vier Ecken definiert werden
rect(a, b, c, d, [tl, tr, br, bl]); // Mit weiteren optionalen Parametern kann 
                                    // jeder Winkel des Rechtecks definiert werden.
// Dies zeichnet ein Quadrat mit der Koordinate {a, b} als linke obere Ecke
// die Parameter c und d sind für die Breite und Höhe.

// Vierecke
quad(x, y, x2, y2, x3, y3, x4, y4);                                             
// Dies zeichnet ein Viereck, welches die einzelnen Koordinaten als Eckpunkte hat.

// Ellipse
ellipse(x, y, width, height);
// Zeichnet eine Ellipse beim Punkt {x. y}. Die Breite und die Höhe werden durch 
// die Parameter width und height definiert.

// Arc
arc(x, y, width, height, start, stop, [mode]);
// Die ersten vier Parameter sollten selbsterklärend sein.
// start und end definieren die Winkel, bei welchen `arc` starten resp. enden
// (in Radians)
// Der optionale Parameter `mode` definiert, ob der Kreisbogen gefüllt wird 
// oder nicht.
// Die möglichen Optionen für `mode` sind: PIE, CHORD und OPEN.

// Kurven
// Processing bietet zwei mögliche Kurven an, welche verwendet werden können.
// Da es hier darum geht, dass es möglichst simpel ist, werden hier keine 
// weiteren Details genannt. Wenn du Kurven in deinem Programm verwenden möchtest,
// sind die folgenden Links empfehlenswert:
// https://processing.org/reference/curve_.html
// https://processing.org/reference/bezier_.html


// 3D-Figuren

// Der dreidimensionale Raum kann aktiviert werden, indem man den Renderer-
// Parameter in der Methode `size()` zu "P3D" setzt.
size(width, height, P3D);
// Im dreidimensionalen Raum müssen die Koordinaten übersetzt werden, damit
// diese korrekt gerendert werden.

// Box
box(size); // Würfel mit der Seitenlänge `size`
box(w, h, d); // Quader definiert durch Breite, Höhe und Tiefe

// Kugel
sphere(radius); // Die Größe wird definiert durch den Parameter `radius`
// Der Mechanismus hinter dem Rendern von Kugeln wurde durch mosaikartige
// Dreiecke implementiert. 
// Mit der folgenden Funktion ist es möglich, zu bestimmen wie detailliert die
// Kugel gerendert wird.
// spereDetail(res);
// Weitere Informationen sind hier zu finden: (https://processing.org/reference/sphereDetail_.html)

// Unregelmäßige Figuren
// Was ist, wenn du etwas zeichnen möchtest, was nicht durch Processing-Funktionen
// abgedeckt ist?
// Es ist möglich, die Funktionen `beginShape()`, `endShape()` und `vertex(x,y)
// zu verwenden.
// Weitere Informationen findest du hier: (https://processing.org/reference/beginShape_.html)
// Du kannst selber gemachte Formen auch verwenden mit der PShape-Klasse.
// Informationen zu PShape gibt es hier: (https://processing.org/reference/PShape.html)

/* -------------------------------------------------
   Transformationen
   -------------------------------------------------
*/

// Tranformationen sind nützlich, um ständig zu wissen, wo die Koordinaten und
// die Ecken einer Form sind, welche du gezeichnet hast. Grundsätzlich sind dies
// Matrizenoperationen. `pushMatrix()`, `popMatrix()` und `translate()`.
pushMatrix(); // Speichert das aktuelle Koordinatensystem auf dem Stack
              // alle Transformationen werden hier angewendet.
popMatrix(); // Stellt das gespeicherte Koordinatensystem wieder her.
// Wenn du diese Funktionen verwendest, kann das Koordinatensystem gespeichert
// und visualisiert werden, ohne dass es Konflikte gibt.

// Translate
translate(x,y); // Setzt den Ursprung zu diesem Punkt.
translate(x, y, z); // Pendant zu der oberen Funktion im dreidimensionalen Raum

// Rotationen
rotate(angle); // Rotiere, um den Betrag, welcher spezifiert wurde.
// Es gibt drei Pendants im dreidimensionalen Raum. 
// Namentlich sind dies: `rotateX(angle)`, `rotateY(angle)` und `rotateZ(angle)`

// Skalierung
scale(s); // Skaliert das Koordinatensystem (entweder erweitern oder verkleinern)

/* -------------------------------------------------
   Styling und Texturen
   -------------------------------------------------
*/

// Farben
// Wie ich zuvor schon erklärt habe, kann die Hintergrundfarbe mit der Funktion
// `background()` definiert werden. Außerdem ist es möglich, dass man zuerst 
// eine Farbe definiert und diese erst danach der Funktion übergeben wird.
color c = color(255, 255, 255); // WEISS!
// Standardmäßig verwendet Processing das RGB-Farbschema, aber dies kann 
// zu HSB konfiguriert werden, indem die Funktion `colorMode()` verwendet wird.
// Weitere Informationen findest du hier: (https://processing.org/reference/colorMode_.html)
background(c); // Ab jetzt ist der Hintergrund in weiß.
// Du kannst die Funktion `fill()` verwenden, um die Farbe auszuwählen, mit
// welcher die Formen ausgefüllt werden. 
// Dies muss konfiguriert werden bevor Formen und Figuren gezeichnet werden.
fill(color(0, 0, 0));
// Wenn du nur die Farbe der Umrandungen definieren möchtest, kannst du die 
// Funktion `stroke()` verwenden.
stroke(255, 255, 0, 200); // Linienfarbe wird zu gelb mit einer höheren
                          // Transparenz geändert.

// Bilder
// Processing kann Bilder rendern und diese unterschiedlich verwenden. Die
// meisten Bilder sind im Datentyp `PImage` gespeichert.
filter(shader); // Processing unterstützt mehrere Filter-Funktionen, damit
                // Bilder verändert werden können.
texture(image); // PImage kann als Argument, weiteren Funktionen übergeben
                // werden, um die Figuren zu "Text" zu machen.
```

Wenn du weitere Dinge mit Processing kennenlernen willst, dann gibt es unzählige
Dinge, welche du mit Processing machen kannst. Das Rendern von Modellen,
Schattierungen und viele mehr. Für ein kurzes Tutorial bietet Processing zu viel,
daher verweise ich dich, falls du interessiert bist, auf die offizielle
Dokumentaion.

```
// Bevor wir weiterfahren, werde ich einige Aspekte zum Importieren von 
// Bibliotheken und Paketen sagen, damit du Processing erweitern kannst..

/* -------------------------------------------------
   Import
   -------------------------------------------------
*/

// Die Macht von Processing kann besser veranschaulicht werden, wenn wir 
// Bibliotheken und Pakete importieren.
// Die Import-Anweisung kann wie unten geschrieben zu Beginn des Quelltextes 
// geschrieben werden.
import processing.something.*;
``` 

## Beispielprogramm

Lass uns ein Beispiel von openprocessing.org ansehen, welches verdeutlicht,
was man in Processing mit nur wenigen Zeilen Code machen kann.

Kopiere den nachfolgenden Code in deine Processing IDE.

```
// Disclaimer: Ich habe das Porgramm nicht selbst geschriben. Diese Skizze
// stammt aus openprocessing, allerdings soll dieses Programm zeigen, wie wenig
// Zeilen Code notwendig sind, um etwas Cooles zu machen.
// Abgerufen von: (https://www.openprocessing.org/sketch/559769)

float theta;
float a;
float col;
float num;

void setup() {
  size(600,600);
}

void draw() {
  background(#F2F2F2);
  translate(width/2, height/2);
  theta = map(sin(millis()/1000.0), -1, 1, 0, PI/6);

  float num=6;
  for (int i=0; i<num; i++) {
    a =350;
    rotate(TWO_PI/num);
    branch(a);
  }
}

void branch(float len) {
  col=map(len, 0, 90, 150, 255);
  fill(col, 0, 74);
  stroke (col, 0, 74);
  line(0, 0, 0, -len);
  ellipse(0, -len, 3, 3);
  len*=0.7;

  if (len>30) {
    pushMatrix();
    translate(0, -30);
    rotate(theta);
    branch(len);
    popMatrix();

    pushMatrix();
    translate(0, -30);
    rotate(-theta);
    branch(len);
    popMatrix();
  }
}
```

Processing ist einfach zu erlernen und ist vorallem nützlich, um Multimedia-
Inhalte (auch in 3D) zu erstellen ohne viel Code zu schreiben. Es ist so einfach
gehalten, dass man den Code durchlesen kann und man versteht den Programmablauf
bereits.

Wenn du externe Bibliotheken, Pakete oder eigene Klassen einbindest, kann ein
Programm, welches mit Processing geschrieben wurde, durchaus auch kompliziert 
werden.

## Einige nützliche Links

- [Processing Webseite](http://processing.org)
- [Processing Sketches](http://openprocessing.org)
