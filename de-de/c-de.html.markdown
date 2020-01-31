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
    
    ////////////////////////////////////////////////
    // Kontrollstrukturen
    ////////////////////////////////////////////////
    if (0) {
        printf("Ich werde nie ausgeführt.");
    }
    else if (0){
        printf("Ich werde auch nie ausgeführt.");
    }
    else {
        printf("Ich gebe etwas aus.");
    }

    // While-Schleifen existieren auch
    int ii = 0;
    while (ii < 10){ // JEDER Wert unter zehn ist wahr
        printf("%d, " ii++); //i++ inkrementiert ii NACHDEM der Wert gebraucht wurde.
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
    for (jj = 0; jj < 10; jj++){
        printf("%d, ", jj);
    } // => gibt folgendes aus: "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "
    
    printf("\n");

    // **Merke**
    // Schleifen und Funktionen müssen einen Body haben. Wenn kein Body gebraucht
    // wird, kann folgendes gemacht werden:
    int i; 
    for (i = 0; i <= 5; i++){
        ; // Semikolon wird als Body behandelt (Null-Anweisung)
    }
    // Alternativ kann auch folgendes geschrieben werden:
    for (i = 0; i <= 5; i++);

    // Verzweigungen mit mehreren Möglichkeiten: `switch()`
    switch (a){
        case 0: //labels müssen integrale *konstante* Ausdrücke sein (z.B. Enums)
            printf("Hey, 'a' ist gleich 0!\n");
            break; //Wenn du kein break einsetzt, so geht der Kontrollfluss durch die Labels
        case 1:
            printf("Huh, 'a' ist gleich 1!\n");
            break;
            // Sei vorsichtig - wenn man das `break` vergisst, werden alle Anweisungen
            // ausgeführt bis das nächste `break` erscheint.
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

    // Verwendung von "goto" in C
    typedef enum { false, true } bool;
    bool desaster = false;
    int i, j; 
    for(i=0; i < 100; ++i){
        for (j=0; j < 100; ++j){
            if ((i + j ) >= 150){
                desaster = true;
            }
            if (desaster){
                goto error;
            }
        }
    }
error:
    printf("Ein Fehler ist aufgetreten bei i = %d & j ? %d\n", i, j);

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
    printf("%d\n", (unsigned char) 257); // => 1 (Max char = 255 wenn char 8 Bit lang ist)

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
    // Ausserdem ist der Begriff Pointer auf im deutschen Sprachgebrauch zu finden.

    // Ein Pointer ist eine Variable, welche deklariert wurde, um eine Speicher-
    // Adresse zu speichern. Die Deklaration eines Pointers wird auch zeigen,
    // auf welche Art von Daten der Pointer zeigt. Man kann die Speicheradresse
    // von Variablen abrufen und dann mit diesen herumspielen.

    int x = 0; 
    printf("%p\n", (void *)&x); // verwende & um die Adresse der Variable zu erhalten
    // %p  formattiert einen Objektpointer des Typen void*)
    // => Gibt eine Adresse im Speicher aus

    // Pointer starten mit einem * zu Beginn der Deklaration.
    int *px, kein_pointer; // px ist ein Pointer zu einem int.
    px = &x; // Speichert die Adresse von x in px
    printf("%p\n", (void *)px); // => Gibt eine Adresse im Speicher aus
    printf("%zu, %zu\n", sizeof(px), sizeof(kein_pointer));
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
    int x_array[20]; // deklariert einen Array der Grösse 20 (Grösse kann
    // nicht geändert werden.)
    int xx;
    for (xx =0; xx < 20; xx++){
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
    int (*ptr_zu_arr)[10] = &arr; //`&arr` ist nicht vom Typ `int *`!
    // Es ist vom Typem "Pointer auf Array" (aus zehn `int`s)
    // oder wenn das Array ein Stringliteral ist, welches gebraucht wird um ein
    // `char`-Array zu initialisieren.
    char anderer_arr[] = "foobarbazquirk";
    // oder wenn es das Argument des des `sizeof` oder `alignof` Operators ist.
    int dritter_array[10];
    int *ptr = dritter_array; // gleich wie: `int *ptr = &arr[0]`
    printf("%zu, %zu\n", sizeof(dritter_array), sizeof(ptr));
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
    int *mein_ptr = malloc(sizeof(*mein_ptr) * 20);
    for (xx = 0; xx < 20; xx++){
        *(mein_ptr + xx) = 20 -xx; //mein_ptr[xx] = 20-xx
    } // initialisiere Speicher zu 20, 19, 18, 17, ... 2, 1 (als `int`)

    // Sei vorsichtig beim Übergeben von Benutzerdefinierten Werten an `malloc`.
    // Wenn du sicher sein willst, kannst du die Funktion `calloc` nutzen, welche
    // (nicht wie `malloc`) auch den Speicher nullt.
    int *mein_anderer_ptr = calloc(20, sizeof(int));

    // Merke, dass es in C keinen Standard-Weg gibt, um die Länge eines dynamisch
    // allozierten Arrays zu bestimmen. Auf Grund dessen sollte eine Variable 
    // erstellt werden, welche sich die Anzahl der Elemente im Array merkt, wenn
    // die Arrays mehrmals im Programm gebraucht werden.
    // Weitere Informationen stehen im Abschnitt Funktionen.
    size_t groesse = 10;
    int *mein_array = calloc(groesse, sizeof(int));
    // Füge dem Array ein Element hinzu 
    groesse++;
    mein_array = realloc(mein_array, sizeof(int) *groesse);
    if (mein_array == NULL){
        // Denke daran, realloc-Fehler zu prüfen
        return
    }
    mein_array[10] = 5;

    // Das Dereferenzieren von nicht alloziertem Speicher führt zu einem 
    // Undefinierten Verhalten.
    printf("%d\n", *(mein_ptr + 21)); // Gibt irgendwas aus. Das Programm kann auch abstürzen

    // Nachdem du fertig mit einem Block bist, welcher `malloc` verwendet hat, 
    // muss der Speicher befreit werden. Ansonsten kann dieser Speicherbereich
    // niemand nutzen bis dein Programm beendet wird.
    // Dies wird auch als "Speicherleck" (engl: memory leak) bezeichnet.
    free(mein_ptr);

    // Obwohl Strings normalerweise als Pointer-to-Char (Pointer zum ersten
    // Zeichen des Arrays) repräsentiert werden, sind Strings sind Arrays aus `char`s.
    // Es ist eine gute Praxis, `const char *` zu verwenden, wenn man ein
    // String-Literal referenziert, da String-Literale nicht modifiziert werden
    // sollten (z.B. "foo"[0] = 'a' ist ILLEGAL)
    const char *mein_str = "Das ist mein eigener String";
    printf("%c\n", *mein_str); // => D

    // Dies ist nicht der Fall, wenn der String ein Array (möglicherweise mit
    // einem String-Literal initialisiert) ist, welcher im beschreibbaren Speicher
    // bleibt, wie zum Beispiel in:
    char foo[] = "foo";
    foo[0] = 'a'; // Dies ist legal, foo enthält jetzt "aoo"

    funktion_1();
} // Ende der `main`-Funktion

////////////////////////////////////////////////
// Funktionen
////////////////////////////////////////////////

// Syntax einer Funktionsdeklaration
// <rueckgabe_wert> <funktion_name>(<args>)

int addiere_zwei_integer(int x1, int x2){
    return x1 + x2; // verwendet return, um einen Wert zurückzugeben
}

/* 
 * Funktionen werden auf Grund des Wertes aufgerufen (call-by-value). Wenn eine 
 * Funktion aufgerufen wird, sind die Argumente Kopien der ursprüunglichen Werte
 * (ausgenommen Arrays). Alles, was man innerhalb einer Funktion mit den Werten 
 * macht, hat keinen Einfluss auf die Originalwerte als die Funktion aufgerufen
 *  wurde

 * Verwende Pointer, um den Originalinhalt zu bearbeiten.

 * Beispiel:
*/

// Eine `void`-Funktion gibt keinen Wert zurück
void str_reverse(char *str_in){
    char tmp;
    size_t ii = 0; 
    size_t laenge = strlen(str_in);
    // `strlen()` ist ein Teil der C Standard-Bibliothek.
    // Merke: Die Länge, welche von `strlen` zurückgegeben wird, ist ohne den 
    // Null-Byter Terminatur.
    for (ii = 0; i < laenge /2; ii++){ // in C99 kann man `ii` direkt hier deklarieren.
        tmp = str_in[ii];
        str_in[ii] = str_in[laenge - ii - 1]; //#ii'tes Zeichen vom Ende her
        str_in[laenge - ii- 1] = tmp;
    }
}
// Merke: Die `string.h`-Headerdatei muss inkludiert werden, bevor `strlen()`
// verwendet werden kann.

/*
   * char c[] = "Das ist ein Test";
   * str_reverse(c);
   * printf("%s\n", c), => "tseT nie tsi saD"
*/

// Weil wir lediglich eine Variable zurückgeben können, kann zum Ändern mehrerer
// Variablen das Konzept call-by-reference verwendet werden.
void tausche_zwei_zahlen(int *a, int *b){
    int temp = *a;
    *a = *b;
    *b = temp;
}
int erste_zahl = 10; 
int zweite_zahl = 20;
printf("Erste Zahl: %d\n Zweite Zahl: %d\n", erste_zahl, zweite_zahl);
tausche_zwei_zahlen(&erste_zahl, &zweite_zahl);
printf("Erste Zahl: %d\n Zweite Zahl: %d\n", erste_zahl, zweite_zahl);
// Werte sind vertauscht.

/*
Wenn man Arrays betrachtet, so werden diese immer als Pointer übergeben. Auch
wenn die Arrays statisch alloziert werden (wie zum Beispiel `arr[10]`), werden
diese als Pointer zum ersten Element des Arrays übergeben.
Auch hier soll noch einmal erwähnt werden, dass keinen Standard gibt, wie die 
Grösse eines dynamischen Arrays herausgefunden werden kann.
*/
// Die Grösse des Arrays muss unbedingt mitgegeben werden.
// Sonst hat die Funktion keine Ahnung wie gross das Array ist.
void ausgabe_int_array(int *arr, size_t size){
    int i;
    for (i = 0; i < size; i++){
        printf("arr[%d] ist %d\n", i, arr[i]);
    }
}

int mein_array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
int groesse = 10;
ausgabe_int_array(mein_array, groesse);
// Wird folgendes ausgeben: "arr[0] ist 1" usw.

// Wenn man auf externe Variable (ausserhalb der Funktion) referenziert, sollte
// man das Schlüsselwort `extern` verwenden.
int i = 0;
void test_funktion(){
    extern int i; //i braucht nun die externe Variable i
}

// Das Schlüsselwort `static` macht, dass eine Variable ausserhalb der Kompilier-
// einheit nicht zugreifbar ist.  (Auf den meisten Systemen ist eine Kompiliereinheit
// eine `.c`-Datei.) Das Schlüsselwort `static` kann sowohl global (zur Kompiliereinheit gehörende)
// Variablen, Funktionen und Funktionslokale Variablen angewendet werden.
// Wenn man `static` bei lokalen Variablen verwendet, so ist diese Variable global
// erreichbar und behält dessen Wert über Funktionsaufrufe hinweg, aber sie ist 
// nur innerhalb der deklarierten Funktion verfügbar. Ausserdem werden statische
// Variablen mit 0 initialisiert, wenn sie nicht mit einem anderen Startwert 
// initialisiert werden.
// Es ist auch möglich, Funktionen als statisch zu deklarieren, damit diese
// `private` sind. Private heisst, dass sie nur in diesem Kontekt sichtbar sind.


////////////////////////////////////////////////
// Benutzerdefinierte Typen und Strukturen (Structs)
////////////////////////////////////////////////

// `typdef`s können verwendet werden, um Typenaliase zu erstellen.
typedef int mein_typ;
mein_typ meine_typ_var = 0;

// Structs sind lediglich Sammlungen von Daten, die Inhalte werden
// (in der Reihenfolge wie sie geschrieben wurden) sequentiell alloziert.
struct rechteck{
    int breite;
    int hoehe;
};

// Allgemein ist es nicht so, dass folgender Ausdruck wahr ist.
// sizeof(struct rechteck) == sizeof(int) + sizeof(int)
// Dies ist so, weil potentiell ein Padding zwischen den Struktur-Inhalten
// möglich ist). siehe [1]

void funktion_1(){
    struct rechteck mein_rechteck;

    // Greife auf Struct-Inhalte mit `.` zu.
    mein_rechteck.breite = 10;
    mein_rechteck.hoehe = 20;

    // Du kannst Pointer zu Structs deklarieren.
    struct rechteck *mein_rechteck_ptr = &mein_rechteck;

    // Verwende Dereferenzierung, um Struct-Inhalte zu bearbeiten
    (*mein_rechteck_ptr).breite = 30; 

    //Noch besser: Verwende die Kurzschreibweise ->, um die Lesbarkeit zu verbessern.
    mein_rechteck_ptr->hoehe = 10; // Das gleiche wie: (*mein_rechteck_ptr).hoehe = 10;
}

// Aus Bequemlichkeitsgründen ist es möglich einem `struct` ein `typedef`hinzuzufügen.
typedef struct rechteck recht;

int flaeche(recht r){
    return r.breite * r.hoehe;
}

// Wenn du grosse Structs hast, kannst du diese mit dem Pointer kopieren, 
// damit grosse Kopiervorgänge vermieden werden.
int flaecheptr(const recht *r){
    return r->breite * r->hoehe;
}

[1] [Why isn't sizeof for a struct equal to the sum of sizeof of each member?](http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member)
