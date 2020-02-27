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

Processing ist eine Programmiersprache, welche es ermöglicht digitale Kunst
und multimediale Inhalte zu erstellen. Mit Processing können Personen ohne
Programmiererfahrung die Grundlagen der Computererfahrung in einem visuellen 
Kontext erlernen.

Obwohl Processing von Java beeinflusst wurde und auf Java basiert, ist die Syntax
sowohl von Java als auch Javascript beeinflusst worden. Weitere Informationen
sind [hier](https://processing.org/reference/) zu finden.

Die Programmiersprache wird statisch programmiert und kommt mit einer eigenen
offiziellen IDE, damit die Programmie kompiliert und ausgeführt werden können.

```
/* ------------
   Mehrzeilige Kommentare werden so gemacht
*/

// Einzeilige Kommentare funktionieren so //

/*
   Da Processing von Java abstammt, ist die Syntax für Kommentare gleich
   wie bei Java (wie du vielleicht oben bemerkt hast)!
   Mehrzeilige Kommentare werden wie hier umschlossen.
*/

/* -------------------------------------------------
   Schreiben und Ausführen von Processing Programmen
   -------------------------------------------------
*/

// In Processing ist der Startpunkt eines Programms die Funktion `setup()` 
// mit dem Rückgabewert `void`.
// Beachte: Die Syntax ist derjenigen von C++ ziemlich ähnlich.
void setup() {
    // Dies gibt beim Ausführen "Hallo Welt!" auf der Konsole aus.
    println("Hallo Welt!"); // eine weitere Sprache mit einem Semikolon am Ende.
}

// Normalerweise wird der Code für statische Elemente innerhalb der Methode 
// `setup()` geschrieben, da diese lediglich einmal ausgeführt wird.
// Dies kann zum Beispiel das Setzen der Hintergrundfarbe oder das Bestimmen
// der Canvas-Grösse sein.
background(color); // Setze die Hintergrundfarbe 
size(width, height, [renderer]); // bestimme die Canvasgrösse mit dem optionalen
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
    i++; // Inkrement Operator!
}

// Da wir nun wissen, wie man ein funktionierendes Skript erstellen kann und wie
// dieses ausgeführt wird, fahren wir mit den unterschiedlichen Datentypen und
// Collections weiter, welche in Processing unterstützt werden.

/* -------------------------------------------------
   Datentypen und Collections
   -------------------------------------------------
*/

// Gemäss den Angaben in der Processingreferenz, unterstützt Processing die
// folgenden acht primitive Datentypen:
boolean booleanValue = true; // Boolean
byte byteValueOfA = 23; // Byte
char charValueOfA = 'A'; // Char (einzelnes Zeichen)
color colorValueOfWhiteM = color(255, 255, 255); // Farben (angegeben durch die
                                                 // `color()`-Methode)
color colorValueOfWhiteH = #FFFFFF; // Farbe (angegeben mit der Hexadezimal-
                                    // schreibweise.
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
// und `ArrayList`s ist, dass eine ArrayList die Grösse dynamisch anpassen kann, 
// da es eine Implementierung des "List" Interface in Java ist.
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// Objekte
// Da Processing auf Java basiert, unterstützt Processing die Objektorientierte
// Programmierung. Dies bedeutet, dass du grundsätzlich jegliche Datentypen
// selber erstellen kannst und diese nach deinen Bedürfnissen manipulieren kannst.
// Selbstverständlich muss eine Klasse definiert werden bevor du ein Objekt
// instanzieren kannst.
// Format: ClassName InstanceName
SomeRandomClass myObject // hier musst du das Objekt später instazieren
// Hier wird das Objekt direkt instanziert:
SomeRandomClass myObjectInstantiated = new SomeRandomClass(); 

// Processing hat noch weitere Collection (wie zum Beispiel Dictionaries und 
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
// Konvertierung gibt es ausserdem noch die Funktionen `degrees()` und `radians()`.
// Die trigonometrischen Funktionen rechnen mit dem Winkelmass Radian, wodurch
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
String value = (i > 5) ? "Gross" : "Klein"; // => "Klein"

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
// For-Statements - Auch hier ist die Syntax wieder gleich wie in Java
for(int i = 0; i < 5; i++){
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
