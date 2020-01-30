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
    char my_char_array[20]; // Dieses Array beinhaltet 1 * 20 = 20 Bytes
    int my_int_array[20]; // Dieses Array beinhaltet 4 * 20 = 80 Bytes.
    // unter der Voraussetzung eines 4-Byte-Worts.

    // Ein Array kann auf diese Weise mit 0 initialisiert werden.
    char my_array[20] = {0};
    // Hierbei ist der Teil "{0}" der "Array Initialisierer".

}
