---
language: c++
filename: learncpp-de.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "http://github.com/connorwaters"]
    - ["Ankush Goyal", "http://github.com/ankushg07"]
    - ["Jatin Dhankhar", "https://github.com/jatindhankhar"]
    - ["Maximilian Sonnenburg", "https://github.com/LamdaLamdaLamda"]
lang: de-de
---

C++ ist eine Systemprogrammiersprache die, 

[laut dem Begründer Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote)
entworfen wurde um,

- "besseres C" zu sein
- Datenabstraktion zu unterstützen
- Objektorientierung zu unterstützen
- generische Programmierung zu unterstützen

Durch seinen Syntax kann sie durchaus schwieriger und komplexer als neuere Sprachen sein.
Sie ist weit verbeitet, weil sie in Maschinen-Code compiliert, welches direkt vom Prozessor ausgeführt
werden kann und somit eine strikte Kontrolle über die Hardware bietet und gleichzeitig
High-Level-Features wie generics, exceptions und Klassen enthält. (wie C) 
Diese Kombination aus Geschwindigkeit und Funktionalität bildet C++ und ist eine der
weitverbreitesten Programmiersprachen.

```c++
//////////////////
// Vergleich zu C
//////////////////

// C++ ist fast eine Untermenge von C and teilt sich grundsätzlich den 
// Syntax für Variablen Deklarationen, primitiven Typen und Funktionen. 

// Wie in C ist der Programmeinsprungpunkt eine Funktion, welche "main" genannt wird und
// einen Ineteger als Rückgabetyp besitzt.
// Dieser Wert fungiert als Beendigungsstatus des Programms.
// Siehe: https://de.wikipedia.org/wiki/Return_Code für weitere Informationen
int main(int argc, char** argv)
{
    // Kommandozeilen Argumente werden genauso wie in C über argc und argv übergeben
    // argc entspricht der Anzahl von Argumenten und argv ist ein Array von C-style 
    // strings (char*), welche die Argumente repräsentieren.
    // Das erste Argument ist der Name des Programms welches aufgerufen wird.
    // Argc und argv können, wenn nicht benötigt, weg gelassen werden, indem
    // die Funktionssignatur "int main()" verwendet wird.

    //  Ein Rückgabewert von 0 repräsentiert die erfolgreiche Ausführung.
    return 0;
}

// C++ unterscheidet sich in einigen Punkten:

// In C++ sind Zeichen-Literale chars
sizeof('c') == sizeof(char) == 1

// In C sind Zeichen-Literale ints
sizeof('c') == sizeof(int)

// C++ verwendet striktes prototyping
void func(); // Funktion ohne Argumente

// In C
void func(); // Funktion mit beliebiger Anzahl von Argumenten

// Verwende nullptr, anstatt von NULL!!!
int* ip = nullptr;

// C standard headers sind in C++ verfügbar.
// C header enden mit .h, während 
// C++ header das Präfix "c" besitzen und kein ".h" Suffix verwenden.

// Die C++ Standard Version:
#include <cstdio>

// Die C Standard Version:
#include <stdio.h>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////////////////
// Funktionsüberladung
///////////////////////

// C++ unterstützt Funktionsüberladung
// Jede Funktion kann unterschiedliche Parameter erhalten.
void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d", myInt);
}

int main()
{
    print("Hello"); // Wird aufgelöst zu "void print(const char*)"
    print(15); // Wird aufgelöst zu "void print(int)"
}

/////////////////////////////
// Standard Funktionsargumente
/////////////////////////////

// Argumente können per Standard für eine Funktion gesetzt werden,
// wenn diese beim Aufruf nicht bereitgestellt werden.

void doSomethingWithInts(int a = 1, int b = 4)
{
    // führe Anweisungen mit "ints" aus.
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// Standard-Argumente müssen am Ende der Liste der Argumente stehen.
void invalidDeclaration(int a = 1, int b) // Fehler!
{
}


/////////////
// Namespaces (Namesräume)
/////////////

// Namespaces stellen einen getrennten Gültigkeitsbereich für Variablen,
// Funktionen und andere Deklarationen zur Verfügung.
// Namespaces können geschachtelt werden.
namespace First 
{
    namespace Nested 
    {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // Ende des Namespaces "Nested"
} // Ende des Namespaces "First"

namespace Second 
{
    void foo()
    {
        printf("This is Second::foo\n");
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // Fügt all Symbole aus dem namespace Second in den aktuellen Gültigkeitsbereich (scope).
    // "foo()" wird nun nicht länger funktionieren, da es nun doppeldeutig ist, ob foo aus
    // dem namespace foo oder darüberliegenden aufgerufen wird.
    using namespace Second;

    Second::foo(); // Gibt "This is Second::foo" aus.
    First::Nested::foo(); // Gibt "This is First::Nested::foo" aus.
    ::foo(); // Gibt "This is global foo" aus.
}

///////////////
// Eingabe/Ausgabe
///////////////

// C++ verwendet für die Eingabe und Ausgabe streams.
// cin, cout und cerr repräsentieren stdin, stdout und stderr.
// << ist der Einfügeoperator und >> ist der Extraktionsoperator.

#include <iostream> // Include für Eingabe/Ausgabe (I/O) streams

using namespace std; // Streams befinden sich im std namespace (standard library)

int main()
{
   int myInt;

   // Ausgabe auf stdout (oder Terminal/Bildschirm)
   cout << "Enter your favorite number:\n";

   // Empfängt Eingabe
   cin >> myInt;

   // cout kann ebenfalls formatiert werden
   cout << "Your favorite number is " << myInt << "\n";
   // Gibt "Your favorite number is <myInt>" aus

    cerr << "Used for error messages";
}

//////////
// Zeichenketten (Strings)
//////////

// Strings in C++ sind Objekte und haben diverse member-functions
#include <string>

using namespace std; // Strings sind ebenfalls im namespace std (Standard Bibliothek)

string myString = "Hello";
string myOtherString = " World";

// + wird für das Anhängen von strings verwendet.
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// C++ strings sind mutable.
myString.append(" Dog");
cout << myString; // "Hello Dog"


/////////////
// Referenzen
/////////////

// Zusätzlich zu Pointern, wie jene in C.
// C++ besitzt _Referenzen_.
// Diese sind Pointer-Typen, welche nicht erneut zugewiesen werden können
// und nicht Null sein können.
// Sie besitzen den selben Synthax wie Variablen.
// Für die Dereferenzierung ist kein * notwendig und 
// & (die Adresse) wird nicht für die Zuweisung verwendet.

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // Erzeugt eine Referenz auf foo.
fooRef += ". Hi!"; // Verändert foo durch die Referenz
cout << fooRef; // Gibt "I am foo. Hi!" aus.


// Weist "fooRef" nicht erneut zu. Dies ist dasselbe, wie "foo = bar" und 
// foo == "I am bar"
// nach dieser Zeile 
cout << &fooRef << endl; // Gibt die Adresse von foo aus
fooRef = bar;
cout << &fooRef << endl; // Gibt ebenfalls die Adresse von foo aus
cout << fooRef;  // Gibt "I am bar" aus

// Die Adresse von fooRef verbleibt die selbe, sie verweist immer noch auf foo


const string& barRef = bar; // Erzeugt konstante Referenz auf bar.
// Wie in C, können konstante Werte ( und Pointer bzw. Referenzen) nicht verändert werden.

barRef += ". Hi!"; // Fehler: konstante Referenzen können nicht verändert werden.

// Hinweis: bevor wir genauer Referenzen besprechen, schauen wir uns zuerst ein Konzept an
// welches als "temporäres Objekt" bezeichnet wird. Gehen wir von folgenden Code aus:
string tempObjectFun() { ... }
string retVal = tempObjectFun();

// Was passiert nun in der zweiten Zeile:
//  - ein String Objekt wird von tempObjectFun zurückgegeben 
//  - ein neuer String wird mit dem zurückgegebenen Objekt als Argument für den Konstruktor erzeugt.
//  - das zurückgegebene Objekt wird zerstört
// Das zurückgegbene Objekt wird temporäres Objekt genannt. Temporäre Objekte werden erzeugt
// wann immer eine Funktion ein Objekt zurückgibt. Zerstört werden diese am Ende der Auswertung des Ausdrucks
// (dies schreibt der Standard vor, aber Compiler sind berechtigt dieses Verhalten zu ändern. Siehe "return value optimization"
// für Details). Wie in diesem Code:
foo(bar(tempObjectFun()))

// Nehmen wir an foo und bar existieren. Das Objekt wird von "tempObjectFun" zurückgegeben,
// wird an bar übergeben und ist zerstört bevor foo aufgerufen wird.

// Zurück zu Referenzen. Die Ausnahme, dass die "am Ende des Ausdrucks" Regel ist gültig,
// wenn das temporäre Objekt an eine konstante Referenz gebunden ist, in welchem Fall das 
// Leben auf den aktuellen Gültigkeitsbereich erweitert wird.

void constReferenceTempObjectFun() {
  // constRef erhält das temporäre Objekt und ist gültig bis ans Ende der Funktion 
  const string& constRef = tempObjectFun();
  ...
}

// Eine andere Art von Referenzen wird in C++11 eingeführt und ist speziell für 
// temporäre Objekte. Es ist nicht möglich Variablen des Typs zu besitzen, aber 
// Vorrechte bei der Auflösung.
void someFun(string& s) { ... }  // Reguläre Referenz
void someFun(string&& s) { ... }  // Referenz auf ein temporäres Objekt

string foo;
someFun(foo);  // Ruft die Funktion mit der regulären Referenz auf
someFun(tempObjectFun());  // Ruft die Funktion mit der temporären Referenz auf

// Zum Beispiel existieren diese zwei Varianten von Konstruktoren für
// std::basic_string:
basic_string(const basic_string& other);
basic_string(basic_string&& other);

// Nehmen wir an, wir erzeugen einen neuen String eines temporären Objekts (welches später 
// zerstört wird), hierbei existiert ein effizienterer Konstruktor. Dieses Konzept wird 
// als "move semantics" bezeichnet (bewegen eines Objekts in ein anderes in C++).

/////////////////////
// Enumerations (Aufzählungstypen)
/////////////////////

// Enums sind eine einfachere Art und Weise einen Wert einer Konstante zu zuweisen.
// Häufig wird dies verwendet, um den Code lesbarer zu gestalten bzw. zu visualisieren.
enum ECarTypes
{
  Sedan,
  Hatchback,
  SUV,
  Wagon
};

ECarTypes GetPreferredCarType()
{
	return ECarTypes::Hatchback;
}

// Mit C++11 existiert eine einfache Möglichkeit einem Typ dem Enum zu zuweisen. Dies
// kann durchaus sinnvoll bei der Serialisierung von Daten sein, oder bei der Konvertierung
// zwischen Typen bzw. Konstanten.
enum ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // Serialisierung von InputValue in eine Datei
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
    // Das enum wird implizit zu einem "uint8_t" konvertiert. Bedingt dadurch, dass
    // es sich um ein enum handelt.
	WriteByteToFile(InputCarType);
}

// Nicht immer ist es gewünscht, dass enums zu einem Integer oder zu einem anderen 
// enum umgewandelt werden. Daher ist es möglich eine enum-Klasse zu erzeugen, welche 
// nicht implizit umgewandelt wird.
enum class ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // Serialisierung von InputValue in eine Datei
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
    // Wird nicht kompilieren, da ECarTypes ein "uint8_t" ist, da das enum 
    // als "enum class" deklariert wurde!
	WriteByteToFile(InputCarType);
}

//////////////////////////////////////////
// Klassen und objekorientierte Programmierung
//////////////////////////////////////////

// Erstes Beispiel einer Klasse
#include <iostream>

// Deklaration einer Klasse.
// Klassen werden üblicherweise im header (.h oder .hpp) deklariert.
class Dog
{
    // Member Variablen und Funktionen sind private per default (standard).
    std::string name;
    int weight;

// Alle nachfolgenden member sind "public" bis
// "private:" oder "protected:" auftritt.
public:

    // Standard Konstruktor
    Dog();

    // Member-Funktonensdeklaration (Implementierung folgt)
    // Bemerkung: std::string statt der Verwendung von namespace std;
    // "using namespace" sollte niemals in einem header verwendet werden.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // Funktionen, die Objekte nicht ändern sollte mit const deklariert werden.
    // Funktionen müssen explizit als "virtual" deklariert werden, um in einer 
    // abgeleiteten Klassen überschrieben zu werden.
    // Aus performance Gründen sind Funktionen nicht per default virtual.
    virtual void print() const;

    // Funktionen können ebenfalls im class body definiert werden.
    // Derart definierte Funktionen sind automatisch "inline".
    void bark() const { std::cout << name << " barks!\n"; }

    // Neben Konstruktoren, bietet C++ Destruktoren.
    // Diese werden aufgerufen, wenn ein Objekt freigegeben wird oder 
    // seinen Wertebereich verlässt. 
    // Dies ermöglicht mächtige Paradigmen, wie auch RAII.
    // Destruktoren sollten virtual sein, wenn eine Klasse von ihr
    // abgeleitet wird. Ist dieser nicht virtual, dann wird der
    // Destruktor der abgeleiteten Klasse nicht aufgerufen, insofern 
    // das Objekt durch eine Referenz/Pointer der Basisklasse entfernt wird.
    virtual ~Dog();

}; // Ein Semikolon schließt die Definition der Klasse ab.  

// Klassen-Member-Funktionen sind üblicherweise in der .cpp Datei implmentiert. 
Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// Objekte sollten als Referenz übergeben werden und wenn diese nicht 
// verändert werden sollen, sollte das Objekt als const Referenz übergeben werden.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// "Virtual" wird nur bei der Deklaration benötigt und nicht bei der Definition.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

Dog::~Dog()
{
    std::cout << "Goodbye " << name << "\n";
}

int main() 
{
    Dog myDog; // Ausgabe: "A dog has been constructed"
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.print(); // Ausgabe: "Dog is Barkley and weighs 10 kg"
    return 0;
} // Ausgabe: "Goodbye Barkley"

// Vererbung:

// Diese Klasse erbt alles was public bzw. protected ist von der Dog-Klasse
// und darüber hinaus auch private Methoden/Attribute, jedoch kann auf diese
// nicht direkt zugegriffen werden. Lediglich über public/procted getter/setter.
class OwnedDog : public Dog {

public:
    void setOwner(const std::string& dogsOwner);

    // Überschreibt das Verhalten der "print" Funktion für alle "OwnedDogs".
    // Siehe: http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping
    // für eine grundlegende Einführung in "Subtype Polymorphismus".
    // Das "override" Schlüsselwort ist optional, aber stellt sicher, dass die 
    // Methode der Basisklasse tatsächlich überschrieben wurde.
    void print() const override;

private:
    std::string owner;
};

// Die zugehörige .cpp Datei
void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Ruft die "print" Funktion der Basisklasse auf.
    std::cout << "Dog is owned by " << owner << "\n";
    // Ausgaben: "Dog is <name> and weights <weight>"
    //           "Dog is owned by <owner>"
}

//////////////////////////////////////////
// Initialisierung und Operatorüberladung
//////////////////////////////////////////

// In C++ können Operatoren wie: +, -, *, / etc. überladen werden.
// Dies wird umgesetzt, indem eine entsprechende Funktion definiert wird,
// welche immer dann aufgerufen wird, sobald der Operator verwendet wird.
#include <iostream>
using namespace std;

class Point
{
public:
    // Member Variablen können mit einem default Wert initialisiert werden.
    double x = 0;
    double y = 0;

    // Definition des Standard Konstruktor, welcher nichts tut
    // außer den Punkt auf den default Wert (0,0) zu setzen.
    Point() { };

    // Die nachfolgende Syntax ist bekannt als "initialization list"
    // und ist eine gängige Art Klassen-Member zu initialisieren.
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Außschließliche Initialisierung der Werte */ }

    // Überladung des "+" Operator.
    Point operator+(const Point& rhs) const;

    // Überladung des "+=" Operator
    Point& operator+=(const Point& rhs);

    // Sinnhaft wäre es an dieser Stelle den "-" und "-=" Operator
    // ebenfalls zu überladen.
};

Point Point::operator+(const Point& rhs) const
{
    // Erzeugung eines neuen Punkts, welcher die Summe aus sich
    // selbst und "rhs" bildet
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () 
{
    Point up (0,1);
    Point right (1,0);

    // Ruft den + Operator mit den entsprechenden Parametern auf.
    Point result = up + right;
    // Ausgabe: "Result is upright (1,1)"
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

/////////////////////
// Templates
/////////////////////

// Templates in C++ werden in erster Linie dafür verwendet generisch zu programmieren.
// Sie unterstützen explizite und partielle Spezialisierung und darüber hinaus können
// sie für funktionale Klassen verwendet werden.
// Tatsächlich bilden templates die Turing-Vollständigkeit 
// (universelle Programmierbarkeit) ab.


// Zu Beginn ein einführendes Beispiel der generischen Programmierung.
// Die Definition einer Klasse bzw. Funktion, welche mit dem Typ T parametriert wird.
template<class T>
class Box 
{
public:
    // T repräsentiert an dieser Stelle einen beliebigen Typen.
    void insert(const T&) { ... }
};

// Während der Kompilierung generiert der Kompiler Kopien für jedes Template, wobei 
// hierbei die Parameter substituiert werden. Somit muss bei jedem Aufruf die gesamte 
// Definition der Klasse zur Verfügung stehen. Aus diesem Grund wird ein Template 
// komplett im header definiert.

// Erzeugung einer Template-Klasse auf dem stack:
Box<int> intBox;

// eine der zu erwartenden Verwendungen:
intBox.insert(123);

// Verschachtelungen von Templates sind möglich.
Box<Box<int> > boxOfBox;
boxOfBox.insert(intBox);

// Bis C++11 war es erforderlich ein Leerzeichen zwischen '>' einzufügen,
// andernfalls wurde es als '>>' geparsed (right shift).

// Manchmal ist folgende Notation anzutreffen:
// template<typename T>
// Das 'class' Schlüsselwort und das 'typename' Schlüsselwort
// sind fast identisch hinsichtlich der Funktionalität. Weitere 
// Informationen auf: http://en.wikipedia.org/wiki/Typename

// Eine template-Funktion:
template<class T>
void barkThreeTimes(const T& input)
{
    input.bark();
    input.bark();
    input.bark();
}

// Hierbei ist zu beachten, dass an dieser Stelle nichts über den Typen des Parameters
// definiert wurde. Der Kompiler wird bei jedem Aufruf bzw. jeder Erzeugung den Typen
// prüfen. Somit funktioniert die zuvor definiert Funktion für jeden Typ 'T', die die 
// const Methode 'bark' implementiert hat.

Dog fluffy;
fluffy.setName("Fluffy")
barkThreeTimes(fluffy); // Gibt "Fluffy barks" dreimal aus.

// Template Parameter müssen keine Klassen sein.
template<int Y>
void printMessage() 
{
  cout << "Learn C++ in " << Y << " minutes!" << endl;
}

// Des Weiteren können templates aus Effizienzgründen genauer spezifiziert werden.
// Selbstverständlich sind reale-Problemen, welche genauer spezifiziert werden nicht
// derart trivial. Auch wenn alle Parameter explizit definiert wurden, muss die 
// Funktion oder Klasse als template deklariert werden.
template<>
void printMessage<10>()
{
  cout << "Learn C++ faster in only 10 minutes!" << endl;
}

printMessage<20>();  // Gibt "Learn C++ in 20 minutes!" aus.
printMessage<10>();  // Gibt "Learn C++ faster in only 10 minutes!" aus.


/////////////////////
// Ausnahme Behandlungen (Exception-Handling)
/////////////////////

// Die Standard Bibliothek bietet einige Exceptions.
// Siehe: http://en.cppreference.com/w/cpp/error/exception.
// Grundsätzlich können alle Typen als Exception geworfen werden.
#include <exception>
#include <stdexcept>

// Alle Exceptions, die in dem "try" Block geworfen werden, können mittels 
// "catch" abgefangen werden.
try 
{
    // Exceptions sollten nicht auf dem heap mithilfe 
    // von "new" allokiert werden.
    throw std::runtime_error("A problem occurred");
}

// Exceptions sollten als const Referenz abgefangen werden
// insofern diese Objekte sind.
catch (const std::exception& ex)
{
    std::cout << ex.what();
}

// Abfangen aller Exceptions, welche zuvor nicht abgefangen wurden.
catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // Erneutes werfen der exception
}

///////
// RAII
///////

// RAII steht für "Resource Acquisition Is Initialization".
// Oft wird dies als eines der wichtigsten Paradigmen in C++ betrachtet.
// RAII beschreibt das Konzept, dass der Konstruktor für ein Objekt 
// die Ressourcen akquiriert und der Destruktor diese freigibt.

// Zum Verständnis, warum dies sinnvoll ist, nachfolgend
// ein einführendes Beispiel:
void doSomethingWithAFile(const char* filename)
{
    // Wir nehmen an, dass nichts schiefgehen wird.
    FILE* fh = fopen(filename, "r"); // Öffnen der Datei im read-mode.

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // Schließen des file-handle.
}

// Unglücklicherweise ist die Fehlerbehandlung äußerst kompliziert.
// Sollte fopen fehlschlagen und "doSomethingWithTheFile" bzw.
// "doSomethingElseWithIt", geben diese einen Fehlercode zurück.
// (Exceptions sind eine bevorzugte Möglichkeit Fehler abzufangen
// , allerdings bei einigen Programmierern, besonders solchen die einen C
// background besitzen, ein unbeliebtes Mittel zur Fehlerbehandlung).
// Nun müssen wir jeden Aufruf auf mögliche auftretende Fehler überprüfen.
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Öffnet die Datei im read-mode
    if (fh == nullptr) // Der Pointer ist bei einem Fehler NULL .
        return false; // Benachrichtigt den Aufrufer über den Fehler.

    // Wir nehmen an, dass jede Funktion false zurückgibt, in einem Fehlerfall
    if (!doSomethingWithTheFile(fh))
    {
        fclose(fh); // File handle schließen.
        return false; // Fehler "melden".
    }

    if (!doSomethingElseWithIt(fh)) 
    {
        fclose(fh); // File handle schließen.
        return false; // Fehler "melden".
    }

    fclose(fh); // File handle schließen.
    return true; // Erfolg "melden".
}

// C-Programmierer handhaben dies häufig durch goto-Anweisungen:
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // File handle schließen.
    return true; // Erfolg "melden".

failure:
    fclose(fh);
    return false; // Fehler "melden".
}

// Insofern Funktionen Fehler durch exceptions indizieren,
// ist dies "sauberer", aber immer noch suboptimal.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Öffnet die Datei im read-mode
    if (fh == nullptr)
        throw std::runtime_error("Could not open the file.");

    try
    {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) 
    {
        // Im Fehlerfall sollte sichergestellt sein, dass die 
        // Datei geschlossen wird.
        fclose(fh); 
        throw; // Erneutes werfen der Exception
    }

    fclose(fh); // Schließen der Datei
}

// Folgendes ist mit der C++ file stream Klasse (fstream) zu vergleichen.
// fstream verwendet den Destruktor, um die Datei zu schließen.
// Der obige Destruktor wird automatisch aufgerufen, sobald das Objekt
// den Gültigkeitsbereich verlässt.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream entspricht der Kurzform von "input file stream".
    std::ifstream fh(filename); // Öffnen der Datei

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // Die Datei wird automatisch vom Destruktor geschlossen.

// Diese Vorgehensweise bietet massive Vorteile:
// 1. Egal was passiert, die Ressource (das Datei-Handle) wird aufgelöst, 
//    insofern der Destruktor korrekt beschrieben wurde. Es ist möglich
//    zu vergessen das Datei-Handle zu schließen, was zu einem "leak" der
//    entsprechenden Ressource führt.
// 2. Der Code selbst ist wesentlich "sauberer".
//    Der Destruktor wird das Datei-Handle im Hintergrund schließen und der
//    Programmierer muss sich darum keinerlei Sorgen machen.
// 3. Der Code ist "exception sicher".
//    Egal wo die exception geworfen wird, das Aufäumen wird definitv vollzogen.

// Der gesamte idiomatische C++ Code verwendet RAII für alle Ressourcen.
// Weitere Beispiele:
// - Speicher verwenden "unique_ptr" und "shared_ptr".
// - Container - verkettete Listen (linked list), vector (selbst organisierende
//   Arrays), hash maps, etc., entfernen deren Inhalt, wenn diese außerhalb des 
//   Gültigkeitsbereichs laufen.
// - Mutex´s verwenden lock_guard und unique_lock.

/////////////////////
// Container
/////////////////////

// Die Container der Standard Tenplate Bibliothek beinhaltet einige vordefinierter templates.
// Diese verwalten die Speicherbereiche für die eigenen Elemente und stellen Member-Funktionen
// für den Zugriff und die Maniplulation bereit.

// Beispielhafte Container:

// Vector (dynamisches array)
// Erlaubt das Definieren von Arrays oder Listen zur Laufzeit
#include <vector>
string val;
vector<string> my_vector; // Initialisierung des Vectors.
cin >> val;
my_vector.push_back(val); // Fügt den Wert "val" zum Vektor "my_vector" hinzu.
my_vector.push_back(val); // Fügt den Wert "val" zum Vektor "my_vector" hinzu (nun zwei Elemente).

// Für die Iteration über Vektoren stehen zwei Methodiken zu Verfügung:
// Entweder die klassische Iteration über den Index:
for (int i = 0; i < my_vector.size(); i++)
{
	cout << my_vector[i] << endl; // Zugriff auf die Elemente des Vektors über den [] Operator
}

// Oder die Verwendung von Iteratoren:
vector<string>::iterator it; // Initialisierng des Iterators.
for (it = my_vector.begin(); it != my_vector.end(); ++it) 
{
	cout << *it  << endl;
}

// Set (Mengen)
// Sets sind Container, welche einzigartige Elemente beinhalten die einer
// spezifischen Ordnung folgen.

#include<set>
set<int> ST;    // Initialisierung des Sets mit einem Integer Datentyp.
ST.insert(30);  // Einfügen des Werts 30 in das Set ST
ST.insert(10);  // Einfügen des Werts 10 in das Set ST
ST.insert(20);  // Einfügen des Werts 20 in das Set ST
ST.insert(30);  // Einfügen des Werts 30 in das Set ST
// Folgende Elemente befinden sich nun in dem Set:
//  10 20 30

// Entfernen eines Elements:
ST.erase(20); 

// Set ST: 10 30
// Für das iterieren verwenden wir Iteratoren:
set<int>::iterator it;

for(it=ST.begin();it<ST.end();it++) 
{
	cout << *it << endl;
}

// Ausgabe:
// 10
// 30

// Zum leeren des gesmten Container wird die Methode 
// Container._name.clear() verwendet.
ST.clear();
cout << ST.size();  // Ausgabe der Set-Größe

// Ausgabe: 0

// Bemerkung: für mehrdeutige Elemente werden multisets verwendet.
// Für hash-Sets sollten unordered_set´s verwendet werden, da diese
// wesentlich effizienter sind, allerdings keiner Ordnung folgen. 
// Verfügbar sind diese Features ab C++11.

// Map
// Maps speichern Elemente, welche einer Kombination aus "Key"
// und "Value" folgen.

#include<map>
map<char, int> mymap;  // Initialisierung der Map: char -> Key, int -> Value.

mymap.insert(pair<char,int>('A',1)); // Einfügen des Werts "1" für den Key "A".

mymap.insert(pair<char,int>('Z',26)); // Einfügen des Werts "26" für den Key "Z".

// Das Iterieren über Maps: 
map<char,int>::iterator it;
for (it=mymap.begin(); it!=mymap.end(); ++it)
    std::cout << it->first << "->" << it->second << '\n';

// Ausgabe:
// A->1
// Z->26

// Für das Finden des dazugehörigen Value des Keys.
it = mymap.find('Z');
cout << it->second;

// Ausabe: 26

// Bemerkung: für "hash maps" sollten die "unordered_map´s" verwendet werden. Diese
// sind effizienter und benötigen keine Reihenfolge. "unordered_maps" sind ab
// C++11 verfügbar.

// Container für nicht-primitve Datentypen benötigen Vergleichsfunktionen im Objekt selbst,
// oder als Funktionspointer. Primitive Datentypen besitzen default-Vergleichsfunktionen.
// Allerdings können diese überschrieben werden.
class Foo 
{
public:
    int j;
    Foo(int a) : j(a) {}
};

struct compareFunction 
{
    bool operator()(const Foo& a, const Foo& b) const 
    {
        return a.j < b.j;
    }
};

// Folgender Code ist nicht valide, könnte aber von einigen Compilern 
// als valide angesehen werden:
// std::map<Foo, int> fooMap;
std::map<Foo, int, compareFunction> fooMap;
fooMap[Foo(1)]  = 1;
fooMap.find(Foo(1)); // Wahr


///////////////////////////////////////
// Lambda Ausdrücke (C++11 und höher)
///////////////////////////////////////

// Lambdas sind eine gängige Methodik um anonyme Funktionen an dem
// Ort der Verwendung zu definieren. Darüber hinaus auch bei der 
// Verwendung von Funktionen als Argument einer Funktion.

// Nehmen wir an es soll ein Vektor von "pairs" (Paaren) mithilfe 
// des zweiten Werts des "pairs" sortiert werden.

vector<pair<int, int> > tester;
tester.push_back(make_pair(3, 6));
tester.push_back(make_pair(1, 9));
tester.push_back(make_pair(5, 0));

// Übergabe des Lambda-Ausdrucks als drittes Argument für die nachfolgende Sortierfunktion.
sort(tester.begin(), tester.end(), [](const pair<int, int>& lhs, const pair<int, int>& rhs)
{
        return lhs.second < rhs.second;
});

// Beachte den Syntax von Lambda-Ausdrücken.
// Die [] im Lambda Ausdruck werden für die Variablen verwendet.
// Diese so genannte "Capture List" definiert, was außerhalb des Lambdas
// innerhalb der Funktion verfügbar sein soll und in welcher Form.
// Dies kann folgendes sein:
//     1. ein Wert [x]
//     2. eine Referenz [&x]
//     3. eine beliebige Variable, welche sich im Gültigkeitsbereich durch 
//        die Referenz [&] befindet.
//     4. wie bei 3. aber mithilfe des Werts [=]
// Beispiel:

vector<int> dog_ids;

for(int i = 0; i < 3; i++) 
{
	dog_ids.push_back(i);
}

int weight[3] = {30, 50, 10};

// Nehmen wir an wir möchten die "dog_ids" gemäß des Gewichts des Hundes sortieren.
// So sollten sich die "dog_ids" wie folgt verhalten: [2, 0, 1]

// Hier werden Lambdas praktisch:
sort(dog_ids.begin(), dog_ids.end(), [&weight](const int &lhs, const int &rhs)
{
        return weight[lhs] < weight[rhs];
});


// Weiterführender Link über Lambda-Ausdrücke:
// http://stackoverflow.com/questions/7627098/what-is-a-lambda-expression-in-c11

///////////////////////////////
// Range For (C++11 und höher)
///////////////////////////////

// Range-For Schleifen können verwendet werden, um über Container zu iterieren.
int arr[] = {1, 10, 3};

for(int elem: arr)
{
	cout << elem << endl;
}

// Insofern "auto" verwendet wird, muss der Typ nicht weiter beachtet werden.

for(auto elem: arr) 
{
	// Anweisungen ...
}

/////////////////////
// Weiteres:
/////////////////////

// Einige Aspekte von C++ sind für Neueinsteiger häufig überraschend (aber auch für
// C++ Veteranen).
// Der nachfolgende Abschnitt ist leider nicht vollständig:
// C++ ist eine der Sprachen, bei der es ein leichtes ist sich selbst ins Bein zu schießen.

// Private-Methoden können überschrieben werden
class Foo 
{
  virtual void bar();
};

class FooSub : public Foo 
{
  virtual void bar();  // Überschreibt Foo::bar!
};


// 0 == false == NULL 
bool* pt = new bool;
*pt = 0; // Setzt den Wert des Pointers 'pt' auf false.
pt = 0;  // Setzt 'pt' auf den "null-pointer". Keine Compiler-Warnung.

// nullptr sollte dieses Problem nicht lösen:
int* pt2 = new int;
*pt2 = nullptr; // Kompiliert nicht.
pt2 = nullptr;  // Setzt pt2 auf null.

// Eine Ausnahme bilden bools.
// Dies erlaubt es "null-pointer" zu testen: if(!ptr)
// Die Konsequenz ist jedoch, dass dem nullptr ein bool zugewiesen werden kann.
*pt = nullptr;  // Kompiliert auch wenn '*pt' ein bool ist!


// '=' != '=' != '='!
// Ruft Foo::Foo(const Foo&) auf, oder den Kopierkonstruktor
Foo f2;
Foo f1 = f2;

// Ruft Foo::Foo(const Foo&) auf, aber kopiert lediglich den "Foo" Teil von 
// "fooSub". Alle zusätzlichen Member werden verworfen. Diese eigenartige Verhalten
// wird auch "object slicing" genannt.
FooSub fooSub;
Foo f1 = fooSub;

// Ruft Foo::operator=(Foo&) oder eine andere Variante auf.
Foo f1;
f1 = f2;

///////////////////////////////////////
// Tuple (C++11 und höher)
///////////////////////////////////////

#include<tuple>

// Konzeptionell sind Tuples alten Datenstrukturen sehr ähnlich, allerdings haben diese keine 
// benamten Daten-Member, sondern werden durch die Reihenfolge angesprochen.

// Erstellen des Tuples und das Einfügen eines Werts. 
auto first = make_tuple(10, 'A');
const int maxN = 1e9;
const int maxL = 15;
auto second = make_tuple(maxN, maxL);

// Ausgabe der Elemente des "first" Tuple.
cout << get<0>(first) << " " << get<1>(first) << "\n"; // Ausgabe : 10 A

// Ausgabe der Elemente des "second" Tuple.
cout << get<0>(second) << " " << get<1>(second) << "\n"; // Ausgabe: 1000000000 15

int first_int;
char first_char;
tie(first_int, first_char) = first;
cout << first_int << " " << first_char << "\n";  // Ausgabe : 10 A

// Tuple können auch wie folgt erzeugt werden:

tuple<int, char, double> third(11, 'A', 3.14141);
// tuple_size  gibt die Anzahl der Elemente in einem Tuple zurück.
// Als "constexpr".

cout << tuple_size<decltype(third)>::value << "\n"; // prints: 3

// tuple_cat fügt die Elemente eines Tuples aneinander (in der selben Reihenfolge).

auto concatenated_tuple = tuple_cat(first, second, third);
// concatenated_tuple wird zu = (10, 'A', 1e9, 15, 11, 'A', 3.14141)

cout << get<0>(concatenated_tuple) << "\n"; // Ausgabe: 10
cout << get<3>(concatenated_tuple) << "\n"; // Ausgabe: 15
cout << get<5>(concatenated_tuple) << "\n"; // Ausgabe: 'A'


///////////////////////////////////
// Logische- und Bitoperatoren
//////////////////////////////////

// Die meisten Operatoren in C++ entsprechen denen aus anderen Sprachen

// Logische Operatoren.

// C++ verwendet so genannte "Short-circuit" Evaluierung für boolean-Ausdrücke.
// Das zweite Argument wird ausgeführt bzw. evaluiert, wenn das erste Argument genügt,
// um den Ausdruck zu bestimmen.

true && false // Führt **logisches und** aus.
true || false // Führt **logisches oder** aus.
! true        // Führt **logisches nicht** aus.

// Anstelle von Symbolen können auch Schlüsselwörter verwendet werden.
true and false // Führt **logisches und** aus.
true or false  // Führt **logisches oder** aus.
not true       // Führt **logisches nicht** aus.

// Bitoperationen

// **<<** Links-Shift
// **>>** Rechts-Shift

~4    // Führt bitweises nicht aus.
4 | 3 // Führt bitweises oder aus.
4 & 3 // Führt bitweises und aus.
4 ^ 3 // Führt bitweises xor aus.

// Gleichwertige Schlüsselwörter:
compl 4    // Führt bitweises nicht aus.
4 bitor 3  // Führt bitweises oder aus.
4 bitand 3 // Führt bitweises und aus.
4 xor 3    // Führt bitweises xor aus.


```
Weiterführende Literatur:

* Aktuelle Sprachen-Referenz [CPP Reference](http://cppreference.com/w/cpp).
* Zusätzliches: [CPlusPlus](http://cplusplus.com).
* Grundlagen Tutorial: [TheChernoProject - C++](https://www.youtube.com/playlist?list=PLlrATfBNZ98dudnM48yfGUldqGD0S4FFb).
