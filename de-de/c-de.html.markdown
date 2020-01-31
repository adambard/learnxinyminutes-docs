---
language: c
filename: learnc.c
contributors:
    - ["caminsha", "https://github.com/caminsha"]
lang: de-de
---

Ach, C. Immer noch **die** Sprache für modernes High-Performance Computing.

C ist wahrscheinlich die niedrigste Programmiersprache, welche die meisten
Programmierer je brauchen werden. Die Geschwindigkeit von C ist enorm, allerdings
muss man sich stets der maneullen Speicherverwaltung bewusst sein.


> **Über Compiler Flags**
> 
> Standardmässig sind `gcc` und `clang` ziemlich ruhig bezüglich Warnungen und 
> Fehlern, obwohl dies sehr nützliche Informationen sein können. Es wird
> empfohlen, strengere Compiler Flags zu verwenden. Hier sind einige empfohlene
> Standards:
> `-Wall -Wextra -Werror -O2 -std=c99 -pedantic`
>
> Für weitere Informationen, was diese und weitere Optionen genau machen,
> sollte die Man-Page des C-Compilers aufgerufen werden (z.B. `man 1 gcc`).
> Alternativ kann auch online nach den unterschiedlichen Optionen gesucht werden.

```c
// einzeilige Kommentare starten mit // - nur in C99 und später vorhanden.

/*
mehrzeilige Kommentare sehen so aus. Diese funktionieren auch in C89
*/

/* 
mehrzeilige Kommentare können nicht verschaltelt werden /* Sei Vorsichtig! */ // Kommentar endet auf dieser Linie ...
*/ // ... nicht bei dieser!

// Konstanten: #define <keyword>
// Konstanten werden laut der Konvention immer in GROSSBUCHSTABEN geschrieben
#define TAGE_IM_JAHR 365

// Konstanten können auch als Aufzählungskonstanten (Enums) definiert werden.
// Alle Anweisungen müssen mit einem Semikolon beendet werden.
enum tage {SO=1, MO, DI, MI, DO, FR, SA};
// MO wird automatisch zu 2, DI zu 3 etc.

// Importiere Header-Dateien mit #include
#include <stdlib.h>
#include <stio.h>
#include <string.h>

// Dateien, welche zwischen <spitzen Klammern> stehen, sind Header-Dateien aus
// der C-Standard-Bibliothek.
// Für deine eigenen Header müssen Anführungszeichen verwendet werden, z.B.:
// #include "mein_header.h"

// Funktionssignaturen werden entweder vorher in einer .h-Datei deklariert oder
// am Anfang der .c-Datei.
void funktion_1();
int funktion_2(void);

// Es muss ein Funktionsprototyp deklariert werden vor der `main()` Funktion,
// wenn die Funktion nach der `main()` Funktion gebraucht wird.
int addiere_zwei_integer(int x1, int x2); // Funktionsprototyp
// Auch wenn der Ausdrck `int addiere_zwei_integer(int, int)` auch valid wäre, 
// ist es empfohlen, dass man die Namen der Argumente hinschreibt für eine
// einfachere Analyse.

// Der Einstiegspunkt deines Programms ist eine Funktion mit dem Namen main und 
// einem Integer als Rückgabewert.
int main(void){
    // dein Programm
}

// Die Kommandozeilenargumente, welche gebraucht werden, damit dein Programm läuft,
// werden als Argumente der `main`-Funktion mitgegeben.
// argc steht für die Anzahl von Argumenten. - Der Programmname ist das erste Argument.
// argv ist ein Array von Zeichenarrays, welche die Argumente beinhaltet.
// argv[0] = Name des Programms
// argv[1] = erstes Argument usw.
int main (int argc, char** argv){
    // Ausgabe mit Hilfe von printf (print formatted)
    // %d ist ein Integer.
    // \n steht für eine neue Zeile
    printf("%d\n",0); // => Gibt 0 aus.

    ////////////////////////////////////////////////
    // Typen
    ////////////////////////////////////////////////

    // Alle Variable müssen am Anfang des jetzigen Blocks deklariert werden.
    // Wir deklarieren die Variablen dynamisch im Code um die Lesbarkeit im
    // Tutorial zu verbessern.
    // C99-Konforme Compiler erlauben die Variablendeklaration an dem Punkt, an
    // welchem die Variable verwendet wird.

    // integer sind normalerweise 4 Bytes gross
    int x_int = 0;

    // shorts sind normalerweise 2 Bytes gross
    short x_short = 0;

    // chars sind garantiert 1 Byte gross
    char x_char = 0;
    char y_char = 'y'; // Charakterliterale werden mit '' gekennzeichnet.

    // longs sind oft 4 bis 8 Bytes gross. long long sind garantiert mindestens 
    // 8 Bytes gross.
    long x_long = 0;
    long long x_long_long = 0;

    // floats sind normalerweise 32-Bit Gleitkommazahlen
    float x_float = 0.0f; // 'f'-Suffix beschreibt eine Gleitkommazahl.

    // doubles sind normalerweise 64-Bit Gleitkommazahlen
    double x_double = 0.0; // echte Zahlen ohne Suffix sind vom Typ double

    // integer-Typen können vorzeichenlos (unsigned) sein  (grösser oder kleiner als 0)
    unsigned short ux_short;
    unsigned int ux_int;
    unsigned long long ux_long_long;

    // chars innerhalb von einfachen Anführungszeichen sind Integers im
    // Maschinenzeichensatz
    '0'; // => 48 im ASCII-Zeichensatz
    'A'; // => 65 im ASCII-Zeichensatz

    // sizeof(T) gibt die Grösse einer Variablen des Typen T in Bytes zurück.
    // sizeof(obj) ergibt die Grösse des Ausdrucks (Variable, Literal usw.)

    printf("%zu\n", sizeof(int)); // => 4 (auf den meisten Rechnern mit einem 4-Byte-Wort)

    // Wenn das Argument des `sizeof`-Operator ein Ausdruck ist, dann wird das
    // Argument nicht ausgewertet (ausser Arrays mit variabler Länge)
    // Der Wert, der in diesem Fall zurückgegeben wird, ist eine Konstante zur 
    // Kompillierzeit.

    int a = 1;
    //size_t ist ein vorzeichenloser Integer Typ mit mindestens 2 Byte um die 
    // Grösse eines Objekts zu repräsentieren.
    size_t size = sizeof(a++); // a++ wird nicht ausgewertet
    printf("sizeof(a++) = %zu, wobei a=%d ist\n", size, a);
    // Gibt "sizeof(a++) = 4, wobei a=1 ist" aus (mit einer 32-Bit-Architektur)

    // Arrays müssen mit einer Grösse initialisiert werden.
    char mein_char_array[20]; // Dieses Array beinhaltet 1 * 20 = 20 Bytes
    int mein_int_array[20]; // Dieses Array beinhaltet 4 * 20 = 80 Bytes.
    // unter der Voraussetzung eines 4-Byte-Worts.

    // Ein Array kann auf diese Weise mit 0 initialisiert werden.
    char mein_array[20] = {0};
    // Hierbei ist der Teil "{0}" der "Array Initialisierer".
    // Beachte, dass die Länge des Arrays nicht explizit definiert werden muss, 
    // wenn er auf derselben Linie initialisiert wird.
    // Folgende Deklaration ist gleichwertig:
    char mein_array[] = {0};
    // Allerdings muss die Länge des Arrays dann zur Laufzeit ausgewertet werden:
    size_t mein_array_size = sizeof(mein_array) / sizeof(mein_array[0]);
    // WARNUNG: Wenn dieser Ansatz gewählt wird, muss man sicherstellen, dass die
    // Grösse des Arrays ermittelt werden *bevor* dieser einer Funktion als
    // Argument weitergegeben wird (siehe Diskussion weiter unten), weil Arrays
    // einer Funktion nur als Zeiger übergeben werden. => Das obere Statement
    // würde innerhalb einer Funktion ein falsches Resultat liefern.

    // Das Indexieren eines Arrays funktioniert wie in anderen Sprache - resp.
    // in anderen Sprachen funktioniert es gleich wie in C.
    mein_array[0]; // => 0
    
    // Arrays sind veränderbar; es ist nur Arbeitsspeicher!
    mein_array[1] = 2;
    printf("%d\n", mein_array[1]); // => 2

    // In C99 (und als optionales Feature in C11) können Arrays mit variabler
    // Länge deklariert werden. Die Grösse eines solchen Array muss eine Konstante
    // zur Kompilierzeit sein.
    printf("Geben Sie die Arraygrösse an: "); //Frag den Benutzer nach der Arraygrösse
    int array_size;
    fcsanf(stdin, "%d", &array_size);
    int var_length_array[array_size]; // deklariere Array mit variabler Länge
    printf("sizeof array =%zu\n", sizeof var_length_array);

    // Zum Beispiel:
    // > Geben Sie die Arraygrösse an: 10
    // > sizeof array = 40

    // Strings sind lediglich Arrays von `chars`, welche mit einem Null-Byte
    // (0x00) beendet werden. In Strings wird das Nullbyte durch das Zeichen \0
    // repräsentiert. Wir müssen das Null-Byte nicht angeben in String-Literalen;
    // Der Compiler fügt es am Ende des Array automatisch hinzu.
    char ein_string[20] = "Das ist ein String";
    printf("%s\n", ein_string); // %s formattiert einen String

    printf("%d\n", ein_string[18]); // => 0
    // Hier ist das Byte #19 0 (wie auch Byte #20)

    // Wenn wir Zeichen zwischen einfachen Anführungszeichen haben, ist es ein
    // Zeichenliteral vom Typ int und *nicht* char. (aus historischen Gründen)
    int cha = 'a'; // Ok
    char chb = 'a'; // auch ok (implizite Umwandlung von int zu char)

    // Mehrdimensionale Arrays:
    int multi_array[2][5] = {
        {1,2,3,4,5},
        {6,7,8,9,0}
    };
    // Auf Elemente zugreifen:
    int array_int = multi_array[0][2]; // => 3

    ////////////////////////////////////////////////
    // Operatoren
    ////////////////////////////////////////////////
    
    // Kurzschreibweise für mehrere Deklarationen
    int i1 = 1, i2 = 2;
    flaot f1 = 1.0, f2 = 2.0;

    int b,c;
    b = c = 0;

    // Arithmetik ist unkompliziert
    i1 + i2; // => 3
    i2 - i1; // => 1
    i2 * i1; // => 2
    i1 / i2; // 0 (0.5, aber abgeschnitten, da es int sind.

    // Man muss mindestens ein Integer to einen float konvertieren, damit man als
    // Resultat eine Gleitkommazahl erhält.
    (float)i1 / i2; // => 0.5f
    i1 / (double)i2; // => 0.5 // das gleiche mit dem Typ `double`
    f1 / f2; // => 0.5, plus oder minus Epsilon
    // Gleitkommazahlen und deren Berechnungen sind nicht exakt.

    // Es gibt auch die Möglichkeit, Modulo zu rechnen
    11 % 3; // => 2

    // Vergleichsoperatoren sind vielleicht schon bekannt, aber in C gibt es keinen
    // Boolean-Typ. In C verwenden wir `int`. (Oder _Bool oder bool in C99.)
    // 0 ist falsch, alles andere ist wahr (Die Vergleichsoperatoren ergeben
    // immer 1 oder 0.
    3 == 2; // => 0 (falsch)
    3 != 2; // => 1 (wahr)
    3 > 2; // => 1
    3 < 2; // => 0
    2 <= 2; // => 1
    2 >= 2; // => 1

    // C ist nicht Python - Vergleiche können nicht verkettet werden.
    // Warnung: die folgende Zeile wird kompilieren, aber es bedeutet `(0 < a) < 2`.
    // Dieser Ausdruck ist immer wahr, weil (0 < a) kann entweder 1 oder 0 sein.
    // In diesem Falle ist es 1, weil (0 < 1).
    int zwischen_0_und_2 = 0 < a < 2;
    // Benutze stattdessen folgende Schreibweise:
    int zwischen_0_und_2 = 0 < a && a < 2;

    // Logik funktioniert auch mit ints
    !3; // => 0 (logisches Nicht)
    !0; // => 1
    1 && 1; // => 1 (logisches Und)
    0 && 1; // => 0
    0 || 1; // => 1 (logisches Oder)
    0 || 0; // => 0

    // Bedingter ternärer Ausdruck ( ? : )
    int e = 5;
    int f = 10;
    int z;
    z = ( e > f) ? e : f; // => // => 10 "wenn e > f ist, gib e zurück, sonst f."

    // Inkrementierungs- und Dekrementierungsoperatoren
    int j = 0;
    int s = j++; // gib j zurück und erhöhe danach j. (s = 0, j = 1)
    s = ++j; // erhöhe zuerst j und gib dann j zurück (s = 2, j = 2)
    // das gleiche gilt für j-- und --j

    // Bitweise Operatoren
    ~0x0F; // => 0xFFFFFFF0 (Bitweise Negation, "Einer-Komplement", Beispielresultat für 32-Bit int)
    0x0F & 0xF0; // => 0x00 (Bitweises UND)
    0x0F | 0xF0; // => 0xFF (Bitweises ODER)
    0x04 ^ 0x0F; // => 0x0B (Bitweises XOR)
    0x01 << 1; // => 0x02 (Bitweises  Linksshift (left shift) (um 1))
    0x02 >> 1; // => 0x01 (Bitweises Rechtsshift (right shift) (um 1))

    // Sei vorsichtig beim Shift mit vorzeichenbehafteten Integern - folgende Ausdrücke sind nicht definiert:
    // - Verschiebung in das Vorzeichenbit (int a = 1 << 31)
    // - Linksshift einer negativen Zahl (int a = -1 << 2)
    // - Shift um einen Offset, welcher >= die Breite des linken Ausdrucks ist.
    // int a = 1 << 32; // undefiniertes Verhalten, wenn int 32-Bit ist.
    
}
