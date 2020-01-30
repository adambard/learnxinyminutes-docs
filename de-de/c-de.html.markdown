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
}
