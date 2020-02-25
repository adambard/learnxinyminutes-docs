---
language: c
filename: learnc-de.c
contributors:
    - ["caminsha", "https://github.com/caminsha"]
lang: de-de
---

Ach, C. Immer noch **die** Sprache für modernes High-Performance Computing.

C ist wahrscheinlich die Programmiersprache mit dem niedrigsten Abstraktionsnvieau,
welche die meisten Programmierer je brauchen werden.
Die Geschwindigkeit von C ist enorm, allerdings muss man sich stets der
manuellen Speicherverwaltung bewusst sein.


> **Über Compiler Optionen**
> 
> Standardmäßig sind `gcc` und `clang` ziemlich ruhig bezüglich Warnungen und 
> Fehlern, obwohl dies sehr nützliche Informationen sein können. Es wird
> empfohlen, strengere Compiler Optionen zu verwenden. Hier sind einige empfohlene
> Standards:
> `-Wall -Wextra -Werror -O2 -std=c99 -pedantic`
>
> Da gewisse Optionen (inbesondere der C-Standard) sehr stark vom Projekt
> abhängen, lohnt es sich, wenn die unterschiedlichen Optionen genauer
> angeschaut werden. Eine Übersicht über die Compiler-Optionen findet man unter
> [diesem](https://stackoverflow.com/questions/3375697/useful-gcc-flags-for-c) Stackoverflow-Beitrag.
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
mehrzeilige Kommentare können nicht verschachtelt werden /* Sei Vorsichtig! */ // Kommentar endet auf dieser Linie ...
*/ // ... nicht bei dieser!

// Konstanten: #define <keyword>
// Konstanten werden laut der Konvention immer in GROSSBUCHSTABEN geschrieben
#define DAYS_IN_YEAR 365

// Konstanten können auch als Aufzählungskonstanten (Enums) definiert werden.
// Alle Anweisungen müssen mit einem Semikolon beendet werden.
enum days {SUN = 1, MON, TUE, WED, THU, FRI, SAT};
// MON wird automatisch zu 2, TUE zu 3 etc.

// Importiere Header-Dateien mit #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Dateien, welche zwischen <spitzen Klammern> stehen, sind Header-Dateien aus
// der C-Standard-Bibliothek.
// Für deine eigenen Header müssen Anführungszeichen verwendet werden, z.B.:
// #include "mein_header.h"

// Funktionssignaturen werden entweder vorher in einer .h-Datei deklariert oder
// am Anfang der .c-Datei.
void function_1();
int funkcion_2(void);

// Es muss ein Funktionsprototyp deklariert werden vor der `main()` Funktion,
// wenn die Funktion nach der `main()` Funktion gebraucht wird.
int add_two_ints(int x1, int x2); // Funktionsprototyp
// Auch wenn der Ausdrck `int add_two_ints(int, int)` auch valid wäre, 
// ist es empfohlen, dass man die Namen der Argumente hinschreibt für eine
// einfachere Analyse.

// Der Einstiegspunkt deines Programms ist eine Funktion mit dem Namen main und 
// einem Integer als Rückgabewert.
int main(void) {
    // dein Programm
}

// Die Kommandozeilenargumente, welche gebraucht werden, damit dein Programm 
// läuft, werden als Argumente der `main`-Funktion mitgegeben.
// argc (argument counter) steht für die Anzahl von Argumenten.
// Der Programmname ist das erste Argument.
// argv (argument vector) ist ein Array von Zeichenarrays, welche die
// Argumente beinhaltet.
// argv[0] = Name des Programms
// argv[1] = erstes Argument usw.
int main (int argc, char** argv) {
    // Ausgabe mit Hilfe von printf (print formatted)
    // %d ist ein Integer.
    // \n steht für eine neue Zeile
    printf("%d\n",0); // => Gibt 0 aus.

    ////////////////////////////////////////////////
    // Operatoren
    ////////////////////////////////////////////////
    
    // Kurzschreibweise für mehrere Deklarationen
    int i1 = 1, i2 = 2;
    flaot f1 = 1.0, f2 = 2.0;

    int b,c;
    b = c = 0;

    // Arithmetik ist unkompliziert
    1 + 2; // => 3
    2 - 1; // => 1
    2 * 1; // => 2
    1 / 2; // 0 (0.5, aber abgeschnitten, da es int sind.)

    // Man muss mindestens ein Integer zu einen float konvertieren, damit man als
    // Resultat eine Gleitkommazahl erhält.
    (float)1 / 2; // => 0.5f
    1 / (double)2; // => 0.5 // das gleiche mit dem Typ `double`
    1.0 / 2.0; // => 0.5, plus oder minus Epsilon
    // Gleitkommazahlen und deren Berechnungen sind nicht exakt.

    // Es gibt auch die Möglichkeit, Modulo zu rechnen
    11 % 3; // => 2

    // Vergleichsoperatoren sind vielleicht schon bekannt, aber in C gibt es
    // keinen Boolean-Typ. In C verwenden wir `int`. (Oder _Bool oder bool in C99)
    // 0 ist falsch, alles andere ist wahr (Die Vergleichsoperatoren ergeben
    // immer 1 oder 0.
    3 == 2; // => 0 (falsch)
    3 != 2; // => 1 (wahr)
    3 > 2; // => 1
    3 < 2; // => 0
    2 <= 2; // => 1
    2 >= 2; // => 1

    // C ist nicht Python - Vergleiche können nicht einfach verkettet werden.
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
    z = ( e > f ) ? e : f; // => // => 10 "wenn e > f ist, gib e zurück, sonst f."

    // Inkrementierungs- und Dekrementierungsoperatoren
    int j = 0;
    int s = j++; // gib j zurück und erhöhe danach j. (s = 0, j = 1)
    s = ++j; // erhöhe zuerst j und gib dann j zurück (s = 2, j = 2)
    // das gleiche gilt für j-- und --j

    // Bitweise Operatoren
    ~0x0F; // => 0xFFFFFFF0 (Bitweise Negation, "Einer-Komplement",
           // Beispielresultat für 32-Bit int)
    0x0F & 0xF0; // => 0x00 (Bitweises UND)
    0x0F | 0xF0; // => 0xFF (Bitweises ODER)
    0x04 ^ 0x0F; // => 0x0B (Bitweises XOR)
    0x01 << 1; // => 0x02 (Bitweises  Linksverschiebung (left shift) (um 1))
    0x02 >> 1; // => 0x01 (Bitweises Rechtsverschiebung (right shift) (um 1))

    // Sei vorsichtig beim Shift mit vorzeichenbehafteten Integern
    // folgende Ausdrücke sind nicht definiert:
    // - Verschiebung in das Vorzeichenbit (int a = 1 << 31)
    // - Linksshift einer negativen Zahl (int a = -1 << 2)
    // - Shift um einen Offset, welcher >= die Breite des linken Ausdrucks ist.
    // int a = 1 << 32; // undefiniertes Verhalten, wenn int 32-Bit ist.
    

    ////////////////////////////////////////////////
    // Typen
    ////////////////////////////////////////////////

    // Compiler, welche nicht C99-kompatibel sind, verlangen, dass sämtliche
    // Variablen zu Beginn des Blocks deklariert werden.
    // C99-Konforme Compiler erlauben die Variablendeklaration an dem Punkt, an
    // welchem die Variable verwendet wird.
    // Wir deklarieren die Variablen dynamisch im Code um die Lesbarkeit im
    // Tutorial zu verbessern.

    // integer sind normalerweise 4 Bytes groß
    int x_int = 0;

    // shorts sind normalerweise 2 Bytes groß
    short x_short = 0;

    // chars sind garantiert 1 Byte groß
    char x_char = 0;
    char y_char = 'y'; // Charakterliterale werden mit '' gekennzeichnet.

    // longs sind oft 4 bis 8 Bytes groß. long long sind garantiert mindestens 
    // 8 Bytes groß.
    long x_long = 0;
    long long x_long_long = 0;

    // floats sind normalerweise 32-Bit Gleitkommazahlen
    float x_float = 0.0f; // 'f'-Suffix beschreibt eine Gleitkommazahl.

    // doubles sind normalerweise 64-Bit Gleitkommazahlen
    double x_double = 0.0; // echte Zahlen ohne Suffix sind vom Typ double

    // integer-Typen können vorzeichenlos (unsigned) sein
    // (größer oder kleiner als 0)
    unsigned short ux_short;
    unsigned int ux_int;
    unsigned long long ux_long_long;

    // Zeichen innerhalb von einfachen Anführungszeichen sind Integers im
    // Maschinenzeichensatz
    '0'; // => 48 im ASCII-Zeichensatz
    'A'; // => 65 im ASCII-Zeichensatz

    // sizeof(T) gibt die Größe einer Variablen des Typen T in Bytes zurück.
    // sizeof(obj) ergibt die Größe des Ausdrucks (Variable, Literal usw.)

    printf("%zu\n", sizeof(int)); // => 4 (auf den Rechnern mit einem 4-Byte-Wort)

    // Wenn das Argument des `sizeof`-Operator ein Ausdruck ist, dann wird das
    // Argument nicht ausgewertet (außer Arrays mit variabler Länge)
    // Der Wert, der in diesem Fall zurückgegeben wird, ist eine Konstante zur 
    // Kompillierzeit.

    int a = 1;
    //size_t ist ein vorzeichenloser Integer Typ mit mindestens 2 Byte um die 
    // Größe eines Objekts zu repräsentieren.
    size_t size = sizeof(a++); // a++ wird nicht ausgewertet
    printf("sizeof(a++) = %zu, wobei a=%d ist\n", size, a);
    // Gibt "sizeof(a++) = 4, wobei a=1 ist" aus (mit einer 32-Bit-Architektur)

    // Arrays müssen mit einer Größe initialisiert werden.
    char my_char_array[20]; // Dieses Array beinhaltet 1 * 20 = 20 Bytes
    int my_int_array[20]; // Dieses Array beinhaltet 4 * 20 = 80 Bytes.
    // unter der Voraussetzung eines 4-Byte-Worts.

    // Ein Array kann auf diese Weise mit 0 initialisiert werden.
    char my_array[20] = {0};
    // Hierbei ist der Teil "{0}" der "Array Initialisierer".
    // Beachte, dass die Länge des Arrays nicht explizit definiert werden muss, 
    // wenn er auf derselben Linie initialisiert wird.
    // Folgende Deklaration ist gleichwertig:
    char my_array[] = {0};
    // Allerdings muss die Länge des Arrays dann zur Laufzeit ausgewertet werden:
    size_t my_array_size = sizeof(my_array) / sizeof(my_array[0]);
    // WARNUNG: Wenn dieser Ansatz gewählt wird, muss man sicherstellen, dass die
    // Größe des Arrays ermittelt werden *bevor* dieser einer Funktion als
    // Argument weitergegeben wird (siehe Diskussion weiter unten), weil Arrays
    // einer Funktion nur als Zeiger übergeben werden. => Das obere Statement
    // würde innerhalb einer Funktion ein falsches Resultat liefern.

    // Das Indexieren eines Arrays funktioniert wie in anderen Sprache - resp.
    // in anderen Sprachen funktioniert es gleich wie in C.
    my_array[0]; // => 0
    
    // Arrays sind veränderbar; es ist nur Arbeitsspeicher!
    my_array[1] = 2;
    printf("%d\n", my_array[1]); // => 2

    // In C99 (und als optionales Feature in C11) können Arrays mit variabler
    // Länge deklariert werden. Die Größe eines solchen Array muss eine Konstante
    // zur Kompilierzeit sein.
    printf("Geben Sie die Arraygröße an: "); //Frag den Benutzer nach
                                              // der Arraygröße
    int array_size;
    fcsanf(stdin, "%d", &array_size);
    int var_length_array[array_size]; // deklariere Array mit variabler Länge
    printf("sizeof array =%zu\n", sizeof var_length_array);

    // Zum Beispiel:
    // > Geben Sie die Arraygröße an: 10
    // > sizeof array = 40

    // Strings sind lediglich Arrays von `chars`, welche mit einem Null-Byte
    // (0x00) beendet werden. In Strings wird das Nullbyte durch das Zeichen \0
    // repräsentiert. Wir müssen das Null-Byte nicht angeben in String-Literalen;
    // der Compiler fügt es am Ende des Array automatisch hinzu.
    char a_string[20] = "Das ist ein String";
    printf("%s\n", a_string); // %s formattiert einen String

    printf("%d\n", a_string[18]); // => 0
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
    // Kontrollstrukturen
    ////////////////////////////////////////////////
    if (0) {
        printf("Ich werde nie ausgeführt.");
    }
    else if (0) {
        printf("Ich werde auch nie ausgeführt.");
    }
    else {
        printf("Ich gebe etwas aus.");
    }

    // While-Schleifen existieren auch
    int ii = 0;
    while (ii < 10) { // JEDER Wert unter zehn ist wahr
        printf("%d, " ii++); //i++ inkrementiert ii NACHDEM der Wert gebraucht
                             // wurde.
    } // => gibt folgendes aus: "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    int kk = 0;
    do {
        printf("%d, ", kk);
    } while(++kk < 10); //++kk inkrementiert kk BEVOR der Wert gebraucht wurde.
    // => gibt folgendes aus: "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // In C gibt es auch for-Schleifen
    int jj; 
    for (jj = 0; jj < 10; jj++) {
        printf("%d, ", jj);
    } // => gibt folgendes aus: "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "
    
    printf("\n");

    // **Merke**
    // Schleifen und Funktionen müssen einen Rumpf haben. Wenn kein Rumpf gebraucht
    // wird, kann folgendes gemacht werden:
    int i; 
    for (i = 0; i <= 5; i++) {
        ; // Semikolon wird als Rumpf behandelt (Null-Anweisung)
    }
    // Alternativ kann auch folgendes geschrieben werden:
    for (i = 0; i <= 5; i++);

    // Verzweigungen mit mehreren Möglichkeiten: `switch()`
    switch (a) {
        case 0: // Labels müssen integrale *konstante* Ausdrücke sein (z.B. Enums)
            printf("Hey, 'a' ist gleich 0!\n");
            break; //Wenn du kein break einsetzt, so geht der Kontrollfluss
                   // durch die Labels
        case 1:
            printf("Huh, 'a' ist gleich 1!\n");
            break;
            // Sei vorsichtig - wenn man das `break` vergisst, werden alle 
            // Anweisungen ausgeführt bis das nächste `break` erscheint.
        case 3:
        case 4:
            printf("Schau mal ... 'a' ist entweder 3 oder 4.\n");
            break;
        default:
            // wenn der Ausdruck `a` auf kein Label zutrifft.
            fputs("Fehler!\n", stderr);
            exit(-1);
            break;
    }

    ////////////////////////////////////////////////
    // Typenumwandlung
    ////////////////////////////////////////////////

    // Jeder Wert in C hat einen bestimmten Typen, aber es ist möglich, ein 
    // Wert in einen anderen Typ umzuwandeln (mit einigen Einschränkungen).

    int x_hex = 0x01; // Es ist möglich, Variablen Hexadezimalwerten zuzuweisen.

    // Bei der Umwandlung zwischen Typen wird versucht, den numerischen Wert
    // beizubehalten.
    printf("%d\n", x_hex); // => 1
    printf("%d\n", (short) x_hex); // => 1
    printf("%d\n", (char) x_hex); // => 1

    // Typen werden überlaufen (overflow) ohne jegliche Warnung
    printf("%d\n", (unsigned char) 257); // => 1 (Max char=255 wenn char 8 Bit ist)

    // Um den maximalen Wert eines `char`, `signed char` oder `unsigned char` 
    // herauszufinden, können die Makros `CHAR_MAX`, `SCHAR_MAX` und `UCHAR_MAX`
    // aus der Header-Datei `<limits.h>` verwendet werden.

    // Integer-Typen können zu Gleitkommazahlen und umgekehrt umgewandelt werden.
    printf("%f\n", (double) 100); // %f formattiert immer zu einem `double`...
    printf("%f\n", (flaot) 100); // ... auch mit einem `float`
    printf("%d\n", (char)100.0); 

    ////////////////////////////////////////////////
    // Zeiger (aka Pointer)
    ////////////////////////////////////////////////

    // In diesem Tutorial wird das deutsche Wort Zeiger nicht verwendet, da es
    // bei einer weiteren Recherche einfacher ist, wenn man von Pointern ausgeht.
    // Außerdem ist der Begriff Pointer auch im deutschen Sprachgebrauch zu finden.

    // Ein Pointer ist eine Variable, welche deklariert wurde, um eine Speicher-
    // adresse zu speichern. Die Deklaration eines Pointers wird auch zeigen,
    // auf welche Art von Daten der Pointer zeigt. Man kann die Speicheradresse
    // von Variablen abrufen und dann mit diesen herumspielen.

    int x = 0; 
    printf("%p\n", (void *)&x); // verwende & um die Adresse der Variable
    // zu erhalten
    // %p  formattiert einen Objektpointer des Typen void*)
    // => Gibt eine Adresse im Speicher aus

    // Pointer starten mit einem * zu Beginn der Deklaration.
    int *px, not_a_pointer; // px ist ein Pointer zu einem int.
    px = &x; // Speichert die Adresse von x in px
    printf("%p\n", (void *)px); // => Gibt eine Adresse im Speicher aus
    printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
    // Gibt auf einem typischen 64-Bit-System folgendes aus: "8, 4"

    // Um den Wert einer Adresse, auf welche ein Pointer zeigt, herauszufinden, 
    // muss man vor die Variable ein * setzen, um sie zu dereferenzieren.
    // Notiz: Ja, es kann verwirrend sein, dass '*' sowohl für das Deklarieren
    // als auch das Derefenzieren verwendet werden kann.
    printf("%d\n", *px); // => 0, der Wert von x

    // Man kann den Wert, auf welchen ein Pointer zeigt, auch verändern.
    // Man muss die Dereferenzierung in Klammern setzen, weil ++ eine höhere 
    // Priorität als * hat.
    (*px)++; // Inkrementiere den Wert, auf welchen px zeigt, um 1
    printf("%d\n", *px); // => 1
    printf("%d\n", x); // => 1

    // Arrays sind eine gute Möglichekit, einen zusammenhängenden Block von
    // Speicher zu allozieren.
    int x_array[20]; // deklariert einen Array der Größe 20 (Größe kann
    // nicht geändert werden.)
    int xx;
    for (xx =0; xx < 20; xx++) {
        x_array[xx]  20 -xx;
    } // Initialisiere x_array zu 20, 19, 18, ... 2, 1

    // Deklariere ein Pointer des Typs int und initalisiere ihn, um auf `x_array`
    // zu zeigen.
    int *x_ptr = x_array;
    // x_ptr zeigt jetzt auf den ersten Wert innerhalb des Arrays (int 20)
    // Das funktioniert, weil Arrays oft zu Pointern reduziert werden, welche
    // auf das erste Element zeigen.
    // Zum Beispiel: Wenn ein Array einer Funktion mitgegeben wird oder einem
    // Pointer zugewiesen wird, wird es zu einem Pointer reduziert (implizites Casting)
    // Ausnahme: Wenn das Array das Argument des Operators `&` ist.
    int arr[10];
    int (*ptr_to_arr)[10] = &arr; //`&arr` ist nicht vom Typ `int *`!
    // Es ist vom Typem "Pointer auf Array" (aus zehn `int`s)
    // oder wenn das Array ein Stringliteral ist, welches gebraucht wird um ein
    // `char`-Array zu initialisieren.
    char other_arr[] = "foobarbazquirk";
    // oder wenn es das Argument des `sizeof` oder `alignof` Operators ist.
    int third_array[10];
    int *ptr = third_array; // gleich wie: `int *ptr = &arr[0]`
    printf("%zu, %zu\n", sizeof(third_array), sizeof(ptr));
    // Gibt wahrscheinlich "40, 4" oder "40, 8" aus

    // Pointer werden basierend auf dem Typ in- und dekrementiert
    // Dies wird Pointer-Arithmetik genannt.
    printf("%d\n", *(x_ptr + 1)); // => 19
    printf("%d\n", x_array[1]); // => 19

    // Man kann zusammenhängende Speicherblöcke auch mit der Funktion `malloc`
    // aus der Standardbibliothek dynamisch allozieren. Der Funktion `malloc` 
    // muss ein Argument des Typs `size_t` übergeben werden, welches bestimmt, 
    // wie viele Bytes alloziert werden sollen. (Normalerweise geschieht dies
    // aus dem Heap - dies kann auf eingebetteten Systemen unterschiedlichen sein.
    // Der C Standard sagt nichts darüber.)
    int *my_ptr = malloc(sizeof(*my_ptr) * 20);
    for (xx = 0; xx < 20; xx++) {
        *(my_ptr + xx) = 20 -xx; //my_ptr[xx] = 20-xx
    } // initialisiere Speicher zu 20, 19, 18, 17, ... 2, 1 (als `int`)

    // Sei vorsichtig beim Übergeben von Benutzerdefinierten Werten an `malloc`.
    // Wenn du sicher sein willst, kannst du die Funktion `calloc` nutzen, welche
    // (nicht wie `malloc`) auch den Speicher nullt.
    int *my_other_ptr = calloc(20, sizeof(int));

    // Merke, dass es in C keinen Standard-Weg gibt, um die Länge eines dynamisch
    // allozierten Arrays zu bestimmen. Auf Grund dessen sollte eine Variable 
    // erstellt werden, welche sich die Anzahl der Elemente im Array merkt, wenn
    // die Arrays mehrmals im Programm gebraucht werden.
    // Weitere Informationen stehen im Abschnitt Funktionen.
    size_t size = 10;
    int *my_array = calloc(size, sizeof(int));
    // Füge dem Array ein Element hinzu 
    size++;
    my_array = realloc(my_array, sizeof(int) *size);
    if (my_array == NULL) {
        // Denke daran, realloc-Fehler zu prüfen
        return
    }
    my_array[10] = 5;

    // Das Dereferenzieren von nicht alloziertem Speicher führt zu einem 
    // Undefinierten Verhalten.
    printf("%d\n", *(my_ptr + 21)); // Gibt irgendwas aus.
    // Das Programm kann auch abstürzen

    // Nachdem du fertig mit einem Block bist, welcher `malloc` verwendet hat, 
    // muss der Speicher befreit werden. Ansonsten kann dieser Speicherbereich
    // niemand nutzen bis dein Programm beendet wird.
    // Dies wird auch als "Speicherleck" (engl: memory leak) bezeichnet.
    free(my_ptr);

    // Obwohl Strings normalerweise als Pointer-to-Char (Pointer zum ersten
    // Zeichen des Arrays) repräsentiert werden, sind Strings Arrays aus `char`s.
    // Es ist eine gute Praxis, `const char *` zu verwenden, wenn man ein
    // String-Literal referenziert, da String-Literale nicht modifiziert werden
    // sollten (z.B. "foo"[0] = 'a' ist ILLEGAL)
    const char *my_str = "Das ist mein eigener String";
    printf("%c\n", *my_str); // => D

    // Dies ist nicht der Fall, wenn der String ein Array (möglicherweise mit
    // einem String-Literal initialisiert) ist, welcher im beschreibbaren Speicher
    // bleibt, wie zum Beispiel in:
    char foo[] = "foo";
    foo[0] = 'a'; // Dies ist legal, foo enthält jetzt "aoo"

    function_1();
} // Ende der `main`-Funktion

////////////////////////////////////////////////
// Funktionen
////////////////////////////////////////////////

// Syntax einer Funktionsdeklaration
// <rueckgabe_wert> <funktions_name>(<args>)

int add_two_ints(int x1, int x2) {
    return x1 + x2; // verwendet return, um einen Wert zurückzugeben
}

/* 
Funktionen werden auf Grund des Wertes aufgerufen (call-by-value). Wenn eine 
Funktion aufgerufen wird, sind die Argumente Kopien der ursprünglichen Werte
(ausgenommen Arrays). Alles, was man innerhalb einer Funktion mit den Werten 
macht, hat keinen Einfluss auf die Originalwerte als die Funktion aufgerufen
wurde.

Verwende Pointer, um den Originalinhalt zu bearbeiten.

Beispiel:
*/

// Eine `void`-Funktion gibt keinen Wert zurück
void str_reverse(char *str_in) {
    char tmp;
    size_t ii = 0; 
    size_t size = strlen(str_in);
    // `strlen()` ist ein Teil der C Standard-Bibliothek.
    // Merke: Die Länge, welche von `strlen` zurückgegeben wird, ist ohne den 
    // Null-Byte Terminator.
    for (ii = 0; i < size /2; ii++) { // in C99 kann man `ii` hier deklarieren.
        tmp = str_in[ii];
        str_in[ii] = str_in[size - ii - 1]; //#ii'tes Zeichen vom Ende her
        str_in[size - ii- 1] = tmp;
    }
}
// Merke: Die `string.h`-Headerdatei muss inkludiert werden, bevor `strlen()`
// verwendet werden kann.

/*
char c[] = "Das ist ein Test";
str_reverse(c);
printf("%s\n", c), => "tseT nie tsi saD"
*/

// Weil wir lediglich eine Variable zurückgeben können, kann zum Ändern mehrerer
// Variablen das Konzept call-by-reference verwendet werden.
void swap_two_numbers(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}
int first = 10; 
int seconde = 20;
printf("Erste Zahl: %d\n Zweite Zahl: %d\n", first, second);
swap_two_numbers(&first, &second);
printf("Erste Zahl: %d\n Zweite Zahl: %d\n", first, second);
// Werte sind vertauscht.

/*
Wenn man Arrays betrachtet, so werden diese immer als Pointer übergeben. Auch
wenn die Arrays statisch alloziert werden (wie zum Beispiel `arr[10]`), werden
diese als Pointer zum ersten Element des Arrays übergeben.
Auch hier soll noch einmal erwähnt werden, dass es keinen Standard gibt, wie die 
Größe eines dynamischen Arrays herausgefunden werden kann.
*/
// Die Größe des Arrays muss unbedingt mitgegeben werden.
// Sonst hat die Funktion keine Ahnung wie groß das Array ist.
void print_int_arrray(int *arr, size_t size) {
    int i;
    for (i = 0; i < size; i++) {
        printf("arr[%d] ist %d\n", i, arr[i]);
    }
}

int my_array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
int size = 10;
print_int_array(my_array, size);
// Wird folgendes ausgeben: "arr[0] ist 1" usw.

// Wenn man auf externe Variable (außerhalb der Funktion) referenziert, sollte
// man das Schlüsselwort `extern` verwenden.
int i = 0;
void test_function() {
    extern int i; // i braucht nun die externe Variable i
}

// Das Schlüsselwort `static` macht, dass eine Variable außerhalb der Kompilier-
// einheit nicht zugreifbar ist.  (Auf den meisten Systemen ist eine Kompiliereinheit
// eine `.c`-Datei.) Das Schlüsselwort `static` kann sowohl bei globalen
// (zur Kompiliereinheit gehörende) Variablen, Funktionen und Funktionslokale
// Variablen angewendet werden.
// Wenn man `static` bei lokalen Variablen verwendet, so ist diese Variable global
// erreichbar und behält dessen Wert über Funktionsaufrufe hinweg, aber sie ist 
// nur innerhalb der deklarierten Funktion verfügbar. Außerdem werden statische
// Variablen mit 0 initialisiert, wenn sie nicht mit einem anderen Startwert 
// initialisiert werden.
// Es ist auch möglich, Funktionen als statisch zu deklarieren, damit diese
// `private` sind. Privat heißt, dass sie nur in diesem Kontekt sichtbar sind.


////////////////////////////////////////////////
// Benutzerdefinierte Typen und Strukturen (Structs)
////////////////////////////////////////////////

// `typedef`s können verwendet werden, um Typenaliase zu erstellen.
typedef int my_type;
my_type my_type_var = 0;

// Structs sind lediglich Sammlungen von Daten, die Inhalte werden
// (in der Reihenfolge wie sie geschrieben wurden) sequentiell alloziert.
struct rectangle {
    int width;
    int height;
};

// Allgemein ist es nicht so, dass folgender Ausdruck wahr ist.
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// Dies ist so, weil potentiell ein Padding zwischen den Struktur-Inhalten
// möglich ist). (siehe [1, Englisch])

void function_1() {
    struct rectangle my_rectangle;

    // Greife auf Struct-Inhalte mit `.` zu.
    my_rectangle.width = 10;
    my_rectangle.height = 20;

    // Du kannst Pointer zu Structs deklarieren.
    struct rectangle *my_rectangle_ptr = &my_rectangle;

    // Verwende Dereferenzierung, um Struct-Inhalte zu bearbeiten
    (*my_rectangle_ptr).width = 30; 

    //Noch besser: Verwende die Kurzschreibweise ->, um die Lesbarkeit zu
    // verbessern.
    my_rectangle_ptr->height = 10; // Gleich wie: (*my_rectangle_ptr).height = 10;
}

// Aus Bequemlichkeitsgründen ist es möglich einem `struct` ein `typedef` hinzuzufügen.
typedef struct rectangle rect;

int area(rect r) {
    return r.width * r.height;
}

// Wenn du große Structs hast, kannst du diese mit dem Pointer kopieren, 
// damit große Kopiervorgänge vermieden werden.
int area_ptr(const rect *r) {
    return r->width * r->height;
}

////////////////////////////////////////////////
// Funktionspointer
////////////////////////////////////////////////

/* 
Zur Laufzeit sind Funktionen in einer Speicheradresse gespeichert. 
Funktionspointer sind wie normale Pointer (es wird einfach eine Speicheradresse 
gespeichert). Funktionspointer können verwendet werden, um Funktionen und
Handler (oder Callback-Funktionen) direkt aufzurufen.
Wie auch immer, die Syntax kann zu Beginn verwirrend wirken.

Zum Beispiel: Verwende str_reverse von einem Pointer 
*/
void str_reverse_through_pointer(char *str_in) {
    // Definiere eine Funktionspointer-Variable, welche f genannt wird.
    void (*f)(char *); // Signatur sollte genau der Funktion entsprechen.
    f = &str_reverse; // weise die Adresse der wirklichen Funktion zu
                      // (zur Laufzeit bestimmt)
    // `f = str_reverse;` würde auch funktionieren, da Funktionen zu Pointern
    // reduziert werden (ähnlich wie Arrays)
    (*f)(str_in); // Die Funktion einfach mit dem Pointer aufrufen
    // f(str_in); // Dies ist eine weitere gültige Alternative um eine Funktion
                  // auzurufen.
}

/*
Solange die Signaturen der Funktionen übereinstimmen, kann man sämtliche Funktionen
demselben Pointer zuweisen. Funktionspointer sind auf Grund der Einfacheit und
Leserlichkeit normalerweise wie folgt `typedef`d 
*/
typedef void (*my_fnp_type)(char *);
// Danach werden diese genutzt, um die wirkliche Pointervariable zu deklarieren.
// ..
// my_fnp_type f;

// Spezialzeichen
// Im folgenden sin die englischen Begriffe jeweils in Klammern geschrieben,
// da diese Begriffe auch im deutschten Sprachgebrauch verwendet werden.
'\a'; // Alarmzeichen (alert (bell) character)
'\n'; // Zeichen für neue Linie (newline character)
'\t'; // Tab (tab character (left justifies text))
'\v'; // Vertikaler Tab (vertical tab)
'\f'; // Neue Seite (new page (form feed))
'\r'; // Wagenrücklauf (carriage return)
'\b'; // Backspace-Zeichen (backspace character)
'\0'; // Null-Byte (NULL character). In C wird dieses Zeichen normalerweise am
// Ende eines Strings gesetzt.
// Beispiel: Hallo\n\0. "\0"  wird per Konvention verwendet, um das Ende
// eines Strings zu kennzeichnen.
'\\'; // Backslash (backslash)
'\?'; // Fragezeichen (question mark)
'\''; // einfaches Anführungszeichen (single quote)
'\"'; // doppeltes Anführungszeichen (double quote)
'\xhh'; // Hexadezimale Zahl (hexadecimal number.) Beispiel:
        // '\xb' = Zeichen für vertikalen Tab 
'\0oo'; // Oktalzahl (octal number). Beispiel \013 = Zeichen für vertikalen Tab

//Ausgabeformatierung
"%d";    // Integer
"%3d";   // Integer mit einer minimalen Länge von drei Zeichen.
"%s";    // String
"%f";    // Gleitkommazahl (float)
"%ld";   // genauere Gleitkommazahl (long)
"%3.2f"; // Mindestens drei Zeichen vor und drei nach dem Komma.
"%7.4s"; // (Kann auch mit Strings gemacht werden)
"%c";    // einzelnes Zeichen (char)
"%p";    // Pointer. Merke: man muss den Pointer zu void umwandeln,
         // bevor `printf` funktioniert.
"%x";    // Hexadezimal
"%o";    // Oktalzahl
"%%";    // Gibt % aus

////////////////////////////////////////////////
// Reihenfolge der Auswertung von Operatoren
////////////////////////////////////////////////

//-------------------------------------------------------//
//        Operatoren                 | Assoziativität    //
//-------------------------------------------------------//
// () [] -> .                        | linksassoziativ   //
// ! ~ ++ -- + = *(type)sizeof       | rechtsassoziativ  //
// * / %                             | linksassoziativ   //
// + -                               | linksassoziativ   //
// << >>                             | linksassoziativ   //
// < <= > >=                         | linksassoziativ   //
// == !=                             | linksassoziativ   //
// &                                 | linksassoziativ   //
// ^                                 | linksassoziativ   //
// |                                 | linksassoziativ   //
// &&                                | linksassoziativ   //
// ||                                | linksassoziativ   //
// ?:                                | rechtsassoziativ  //
// = += -= *= /= %= &= ^= |= <<= >>= | rechtsassoziativ  //
// ,                                 | linksassoziativ   //
//-------------------------------------------------------//


////////////////////////////////////////////////
// Header-Dateien
////////////////////////////////////////////////

/*
Header-Dateien sind ein wichtiger Teil von C, da sie eine Verbindung zwischen 
unterschiedlichen C-Quelldateien herstellen. Außerdem vereinfachen Header-Dateien
den Code und Definitionen, da diese in separaten Dateien geschrieben werden können.

Header-Dateien sind von der Syntax her ähnlich zu C-Quelldateien, allerdings haben
die Header-Dateien die Dateiendung `.h`. Header-Dateien können im Quellcode mit
der `#include`-Anweisung eingebunden werden z.B. `#include "beispiel.h". Die
vorherige Anweisung geht davon aus, dass sich die Header-Datei im selben Ordner
befindet wie die C-Quelldatei.
*/

// Eine sichere Möglichkeit, einen Header mehrere Male zu definieren bietet, das
// folgende Statement. Die mehrfache Definition geschieht, wenn Kreisabhängigkeiten
// bestehen.
#ifndef EXAMPLE_H /* Wenn EXAMPLE_H noch nicht definiert wurde */
#define EXAMPLE_H /* definiere das Makro EXAMPLE_H */

// Es könenn weitere Header innerhalb eines Headers eingebunden werden, was dazu
// führt, dass diese bereits in anderen Dateien eingebunden wurden. So kann eine
// Header-Datei in mehreren Dateien eingebunden werden. zum Beispiel:
#include <string.h>

// Wie in den Quelldateien können auch in den Header-Dateien Makros definiert
// werden und in anderen Dateien verwendet werden, welche diesen Header einbinden.
#define EXAMPLE_NAME "Dennis Ritchie"

// Funktionsmakros können auch definiert werden.
#define ADD(a, b) ((a) + (b))

// Beachte die Klammern, welche um die Argumente geschrieben wurden - diese sind
// wichtig, damit sichergestellt werden kann, dass a und b nicht unerwartet 
// erweitert werden. Zum Beispiel: `MUL (x,y) (x * y)`; Bei der Verwendung von 
// `MUL(1 + 2, 3)` würde dies wie folgt erweitert werden: `(1 + 2 * 3)`, was zu
// einem falschen Resultat führt.

// Strukturen und Typendefinitionen können verwendet werden, um die Konsistenz
// zwischen unterschiedlichen Dateien beizubehalten.
typedef struct Node {
    int value;
    struct Node *next;
}Node;

// Dies kann auch mit Aufzählungen gemacht werden.
enum traffic_light_state {GREEN, YELLOW, RED};

// Funktionsprototypen könenn auch in Header-Dateien definiert werden, um die
// Funktion in unterschiedlichen Dateien zu verwenden, aber dies wird als schlechte
// Praxis angesehen. Definitionen sollten in einer C-Datei erstellt werden.
Node create_linked_list(int *value, int length);

// Außer den oben genannten Elementen, sollten weitere Definitionen in einer
// C-Datei gemacht werden. Übermäßige Includes und Definitionen sollten auch 
// nicht einer Header-Datei gemacht werden. Stattdessen wird es empfohlen, diese
// in eine separate Header-Datei oder in eine C-Quelldatei zu schreiben.

#endif /* Ende der Präprozessordirektive */
```
## Weiterführende Literatur

Das Beste wird es sein, wenn man sich ein Exemplar des Buches
["The C Programming Language"](https://de.wikipedia.org/wiki/The_C_Programming_Language) besorgt.
Dieses Buch gilt als **das** Buch über die Programmiersprache C und wurde
von Dennis Ritchie, dem Erfinder der Programmiersprache C, und Brian Kernighan
geschrieben.
Sei vorsichtig, da dieses Buch mittlerweile schon etwas älter ist und gewisse
Unkorrektheiten (d.h. Ideen, welche nicht mehr als gut empfunden werden.) oder
mittlerweile geänderte Praktiken enthält. [Hinweis: Das Buch wurde auf Englisch
geschrieben, es gibt aber auch eine Übersetzung davon]

Eine weitere gute Ressource ist [Learn C The Hard Way](http://learncodethehardway.org/c/).
[Englisch]

Solltest du Fragen zu C haben, so lies die FAQ [compl.lang.c Frequently Asked Questions](http://c-faq.com).[Englisch]

Außerdem ist es wichtig, eine saubere Einrückung zu verwenden. Des weiteren ist
es wichtig, dass der Codestil möglichst konsistent ist. Es ist wichtiger, lesbaren
Code zu schreiben als Code, welcher clever und schnell ist. Es lohnt sich ein
Blick auf den [Codestil des Linuxkernel](https://www.kernel.org/doc/Documentation/process/coding-style.rst) zu werfen. [Englisch]

[1] [Why isn't sizeof for a struct equal to the sum of sizeof of each member?](http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member)
