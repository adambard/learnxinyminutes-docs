---
contributors:
  - ["Adam Bard", "http://adambard.com/"]
  - ["Árpád Goretity", "http://twitter.com/H2CO3_iOS"]
  - ["Jakub Trzebiatowski", "http://cbs.stgn.pl"]
  - ["Marco Scannadinari", "https://marcoms.github.io"]
  - ["Zachary Ferguson", "https://github.io/zfergus2"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Joshua Li", "https://github.com/JoshuaRLi"]
  - ["Dragos B. Chirila", "https://github.com/dchirila"]
  - ["Heitor P. de Bittencourt", "https://github.com/heitorPB/"]
translators:
    - ["lele25811", "https://github.com/lele25811"]
filename: learnc-it.c
---

Ah, C. Ancora **il** linguaggio per il moderno calcolo ad alte prestazioni. 

C è il linguaggio di più basso livello che la maggior parte dei programmatori utilizzerà mai,
ma compensa ampiamente con la sua velocità pura. 
Basta essere consapevoli della gestione manuale della memoria e C ti porterà ovunque tu abbia bisogno

```c
// Una singola riga di commenti comincia con // - utilizzabile in C99 e superiori

/*
I commenti multi-riga appaiono come questo. Funzionano anche in C89.
*/

/*
I commenti multi-riga non si nidificano /* fai attenzione */ // commento termina con questa linea
*/ // ...non con questa!

// Costanti: #define <parolachiave>
// Le costanti sono scritte in MAIUSCOLO per convenzione
#define DAYS_IN_YEAR 365

// Le enumerazioni sono anche sono modi per dichiarare costanti. 
// Tutte le dichiarazioni devono finire con un punto e virgola
enum days {SUN, MON, TUE, WED, THU, FRI, SAT};
// SUN ottiene 0, MON ottiene 1, TUE ottiene 2, etc.

// Il valore delle enumerazioni può essere anche specificato
enum days {SUN = 1, MON, TUE, WED = 99, THU, FRI, SAT};
// MON ottiene 2 automaticamente, TUE ottiene 3, etc.
// WED ottiene 99, THU ottiene 100, FRI ottiene 101, etc.

// Importa le intestazioni con #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// I nomi dei file tra <parentesi angolari> dicono al compilatore dove guardare nel tuo sistema.
// di librerie per l'intestazioni.
// Per le tue intestazioni, usa le doppi virgolette invece di parentesi angolari 
// e fornisci il percorso
#include "my_header.h" 		// file locale
#include "../my_lib/my_lib_header.h" // percorso relativo

// Dichiara le segnature della funzione in anticipo in un file.h o nella parte superiore
// del tuo file .c
void function_1();
int function_2(void);

// Almeno, è necessario dichiarare un 'prototipo di funzione' prima del suo utilizzo in qualsiasi funzione. 
// Normalmente, i prototipi sono nella parte superiore del file prima di qualsiasi definizione di funzione.
int add_two_ints(int x1, int x2); // prototipo di funzione
// Sebbene 'int add_two_ints(int, int);' è valido (non è necessario nominare gli argomenti),
// si consiglia di nominare anche gli argomenti nel prototipo per un'ispezione più semplice.

// Prototipi di funzione non sono necessari se la definizione della funzione avviene prima
// qualsiasi altra funzione che chiama quella funzione. Comunque, è pratica standard 
// aggiungere sempre il prototipo di funzione a un file di intestazione (*.h) e quindi #define
// il file sopra. Ciò impedisce ogni errore dove una funzione potrebbe essere chiamata
// prima che il compilatore sappia della sua esistenza, dando anche allo sviluppatore
// una intestazione pulita da condividere con il resto del progetto.

// Il tuo punto di accesso al programma è una funzione chiamata 'main'. Il tipo di ritorno 
// può essere qualsiasi, tuttavia molti sistemi operativi si aspettano un tipo di ritorno 'int'
// per l'elaborazione del codice di errore.
int main(void) {
  // il tuo programma
}

// Gli argomenti della riga di comando utilizzati per eseguire il programma vengono anche passati al main 
// argc è il numero di argomenti: il nome del programma conta come 1
// argv è un array di array di caratteri - contiene gli argomenti stessi
// argv[0] = nome del programma
// argv[1] = primo argomento, ecc
int main (int argc, char** argv)
{
  // stampa l'output usanto 'printf', per la formattazione di stampa
  // %d è un intero, \n è un a capo
  printf("%d\n", 0); // => Stampa 0

  // prende un input utilizzando 'scanf'
  // '&' viene utilizzato per definire la posizione
  // dove vogliamo archiviare il valore di input
  int input;
  scanf("%d", &input);

  ///////////////////////////////////////
  // Tipi
  ///////////////////////////////////////

  // I compilatori che non sono uniformi a C99 richiedono che le variabili DEVONO essere
  // dichiarate nella parte superiore del blocco corrente.
  // I compilatori che sono uniformi a C99 permettono la dichiarazione delle variabili vicino 
  // al punto in cui viene utilizzato il valore.
  // Per motivi di tutorial, le variaibli sono dichiarate dinamicamente sotto lo standard coforme a C99.

  // gli int (interi) sono generalmente di 4 byte (usa l'operatore `sizeof` per controllare)
  int x_int = 0;

  // gli short sono generalmente di 2 byte (usa l'operatore `sizeof` per controllare)
  short x_short = 0;
  
  // i char (letterali) sono definiti come le unità indirizzabili più piccole per un processore.
  // Questo generalmente è 1 byte, ma per alcuni sistemi può essere più, 
  // (es. per TMS320 da TI è 2 byte).
  char x_char = 0;
  char y_char = 'y'; // i char sono citati con ''

  // i long sono generalmente di 4 o 8 byte; i lunghi long sono garantiti almeno per essere 8 byte
  long x_long = 0;
  long long x_long_long = 0;

  // i float sono generalmente 32 bit, numeri a virgola mobile
  float x_float = 0.0f; // il suffisso 'f' indica 'floating' per floating point, quindi numeri a virgola mobile 

  // i double sono generalmente 64 bit, numeri a virgola mobile
  double x_double = 0.0; // i numeri reali senza nessun suffiso sono double

  // i tipi interi possono essere 'unsigned', sono firmati (maggiore o uguale a zero)
  unsigned short ux_short;
  unsigned int ux_int;
  unsigned long long ux_long_long;

  // i char all'interno delle virgolette singole sono numeri interi nel insieme di caratteri della macchina.
  '0'; // => 48 nel insieme di caratteri ASCII.
  'A'; // => 65 nel insieme di caratteri ASCII.

  // sizeof(T) ti dà la dimensione di una variabile con tipo T in byte
  // sizeof(OBJ) produce la dimensione dell'espressione (variabile, letterale, ecc)
  printf("%zu\n", sizeof(int)); // => 4 (sulla maggior parte delle macchine con parole a 4 byte)

  // Se l'argomento del operatore `sizeof` è un espressione, allora il suo argomento 
  // non viene valutato (trannte VLA, vedi sotto)
  // Il valore che produce in questo caso è una costante a tempo di compilazione.
  int a = 1;
  // size_t è un tipo intero non firmato di almeno 2 byte usati per rappresentare
  // la dimensione di un oggetto.
  size_t size = sizeof(a++); // a++ non viene valutato
  printf("sizeof(a++) = %zu where a = %d\n", size, a);
  // la stampa "sizeof(a++) = 4 dove a=1" (su un'archiettura a 32 bit)

  // Gli array devono essere inizializzati con una dimensione fissa
  char my_char_array[20]; // Questo array occupa 1 * 20 = 20 bytes
  int my_int_array[20]; // Questo array 4 * 20 = 80 bytes
  // (assumendo parole a 4 byte)

  // Puoi inizializzare un array di venti int dove sono tutti 0 così:
  int my_array[20] = {0};
  // dove la parte '{0}' è chiamata 'Initializer array' (inizializzatore di array)
  // Tutti gli elementi (se presenti) oltre quelli nell'inizializzatore sono inizializzati a 0:
  int my_array[5] = {1, 2};
  // Quindi my_array ora ha cinque elementi, tutti tranne i primi due sono 0:
  // [1, 2, 0, 0, 0]
  // NOTE: se non dichiari esplicitamente la dimensione del array
  // puoi inizializzarlo comunque sulla stessa riga
  int my_array[] = {0};
  // NOTE: quando non si dichiara la lunghezza, la lunghezza è il numero
  // degli elementi nell'inizializzatore. Con '{0}' my_array è ora di dimensioni: [0].
  // Per valutare le dimensioni dell'array in fase di esecuzione, 
  // dividere le dimensioni del byte per le dimensioni del byte del tipo di elemento:
  size_t my_array_size = sizeof(my_array) / sizeof(my_array[0]);
  // ATTENZIONE: È necessario valutare la dimensione prima di iniziare a passare l'array
  // alle funzioni (vedi discussione successiva) perchè gli array vengono 'declassati'
  // a puntatori grezzi quando vengono passati alle funzioni 
  // (quindi l'affermazione sopra produrra il risultato errato all'interno della funzione)

  // Indicizzare un array è come negli altri linguaggi 
  // o meglio, gli altri linguaggi sono come il C.
  my_array[0]; // => 0

  // Gli array sono immutabili, È solo memorial!
  my_array[1] = 2;
  printf("%d\n", my_array[1]); // => 2

  // Nel C99 (e come caratteristica opzionale in C11), gli array a lunghezza variabile (VLA)
  // possono essere dichiarati. La dimensione di tale array non deve essere specificata a tempo di compilazione.
  printf("Enter the array size: "); // chiede al utente la lunghezza del array
  int array_size;
  fscanf(stdin, "%d", &array_size);
  int var_length_array[array_size]; // dichiarazione di VLA
  printf("sizeof array = %zu\n", sizeof var_length_array);

  // Example:
  // > Enter the array size: 10  -> (Inserisci la lunghezza del array: 10)
  // > sizeof array = 40 -> (sizeof array = 40)

  // Le Stringhe sono solo array di caratteri terminati da un byte null (0x00),
  // rappresentato nelle stringhe come il carattere speciale '\0'
  // (non dobbiamo includere il byte null nei letterali delle stringhe,
  // il compilatore lo inserisce alla fine dell'array per noi).
  char a_string[20] = "This is a string";
  printf("%s\n", a_string); // %s formattazione di una stringa

  printf("%d\n", a_string[16]); // => 0
  // i.e., byte #17 è 0 (come sono 18, 19, e 20)

  // Se noi abbiamo caratteri tra le vergolette doppie (""), quei caratteri sono un letterale.
  // È di tipo `int` e *non* `char` (per motivi storici)
  int cha = 'a'; // ok
  char chb = 'a'; // ok lo stesso (implicitamente la conversione da int a char)

  // Array di più dimensioni:
  int multi_array[2][5] = {
    {1, 2, 3, 4, 5},
    {6, 7, 8, 9, 0}
  };
  // accesso agli elementi:
  int array_int = multi_array[0][2]; // => 3

  ///////////////////////////////////////
  // Operatori
  ///////////////////////////////////////

  // versione corta per molteplici dichiarazioni:
  int i1 = 1, i2 = 2;
  float f1 = 1.0, f2 = 2.0;

  int b, c;
  b = c = 0;

  // Aritmetica semplice
  i1 + i2; // => 3
  i2 - i1; // => 1
  i2 * i1; // => 2
  i1 / i2; // => 0 (0.5, ma troncato è 0)

  // Hai bisogno di un cambio di tipo (cast) da `int` a `float` per ottenere un risultato in virgola mobile
  (float)i1 / i2; // => 0.5f
  i1 / (double)i2; // => 0.5 // Lo stesso per i double
  f1 / f2; // => 0.5, più o meno epsilon

  // I numeri a virgola mobile sono definiti da IEEE 754, quindi non possono archiviare perfettamente
  // valori esatti. Ad esempio, quanto segue non produce risultati previsti
  // perchè 0.1 potrebbe effettivamente essere 0,09999999999 dentro il computer,
  // e 0.3 potrebbe essere memorizzato come 0.300000000001.
  (0.1 + 0.1 + 0.1) != 0.3; // => 1 (vero)
  // e non è associato per ragioni di sopra menzionate.
  1 + (1e123 - 1e123) != (1 + 1e123) - 1e123; // => 1 (vero)
  // questa notazione è la notazione scientifica per i numeri: 1e123 = 1*10^123

  // È importante considerare che la maggior parte dei sistemi utilizza IEEE 754 per 
  // rappresentare i numeri in virgola mobile, Anche in python viene utilizzato per il calcolo scientifico
  // alla fine chiama C che utilizza IEEE754. È mensionato in questo modo non per indicare che l'implementazione 
  // è scarsa, ma invece come avvertimento per quando si effettuano confronti in virgola mobile
  // un po di errore (Epsilon) deve essere considerato.

  // Il modulo è presente, ma fai attenzione se gli argomenti sono negativi
  11 % 3;    // => 2 come 11 = 2 + 3*x (x=3)
  (-11) % 3; // => -2, come ci si aspetterebbe
  11 % (-3); // => 2 e non -2, ed è abbastanza intuitivo 

  // Gli operatori di confronto sono probabilmente famigliari, 
  // ma non esiste nessun tipo di booleano in C. Usiamo invece 'int'.
  // (C99 ha introdotto il tipo _bool fornito in stdbool.h)
  // 0 è falso, qualsiasi altra cosa è vera. 
  // (il confronto tra operatori produce sempre 0 o 1 -> falso o vero) 
  3 == 2; // => 0 (falso)
  3 != 2; // => 1 (vero)
  3 > 2;  // => 1
  3 < 2;  // => 0
  2 <= 2; // => 1
  2 >= 2; // => 1

  // C non è python - i confronti non sono a catena.
  // ATTENZIONE: la riga seguente si compilerà, ma significa '(0 a) 2'.
  // Questa espressione è sempre vera, perchè (0 a) potrebbe essere 1 o 0.
  // In questo caso è 1, perchè (0 1).
  int between_0_and_2 = 0 < a < 2;
  // Invece utilizza:
  int between_0_and_2 = 0 < a && a < 2;

  // La logica lavora sugli 'int'
  !3; // => 0 (Logica not)
  !0; // => 1
  1 && 1; // => 1 (Logica and)
  0 && 1; // => 0
  0 || 1; // => 1 (Logica or)
  0 || 0; // => 0

  // Espressione di condizione ternaria ( ? : )
  int e = 5;
  int f = 10;
  int z;
  z = (e > f) ? e : f; // => 10 "if e > f return e, else return f."

  // Incremento e decremento degli operatori:
  int j = 0;
  int s = j++; // Return j THEN incremento j. (s = 0, j = 1)
  s = ++j; // Incremento j THEN return j. (s = 2, j = 2)
  // stesso con j-- e --j

  // Operatori bitwise!
  ~0x0F; // => 0xFFFFFFF0 (negazione bitwise, "complemento di 1", nel esempio il risultato per int a 32 bit)
  0x0F & 0xF0; // => 0x00 (bitwise AND)
  0x0F | 0xF0; // => 0xFF (bitwise OR)
  0x04 ^ 0x0F; // => 0x0B (bitwise XOR)
  0x01 << 1; // => 0x02 (bitwise spostamento a sinistra (di 1))
  0x02 >> 1; // => 0x01 (bitwise spostamento a destra (di 1))

  // Fai attenzione quando si spostano numeri interi: i seguenti sono indefiniti:
  // - spostamento nel bit di segno di un numero intero (int a = 1 << 31)
  // - spostamento a sinistra di un numero negativo (int a = -1 << 2)
  // - spostamento di un offset che è >= la larghezza del tipo di LHS:
  // int a = 1 << 32; // oltre la grandezza di a se l'intero è largo 32 bit

  ///////////////////////////////////////
  // Strutture di controllo
  ///////////////////////////////////////

  if (0) {
    printf("I am never run\n");
  } else if (0) {
    printf("I am also never run\n");
  } else {
    printf("I print\n");
  }

  // Esistono i while loop innestati
  int ii = 0;
  while (ii < 10) { // qualsiasi valore inferiore a dieci è vero.
    printf("%d, ", ii++); // il ii++ incrementa ii DOPO aver utilizzato il suo valore corrente.
  } // => stampa "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  int kk = 0;
  do {
    printf("%d, ", kk);
  } while (++kk < 10); // il ++kk incrementa kk PRIMA di utilizzare il suo valore corrente.
  // => stampa "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  // Esistono anche i ciclo for
  int jj;
  for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
  } // => stampa "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  // *****NOTES*****:
  // Cicli e funzioni devono avere un corpo. Se nessun corpo è necessario:
  int i;
  for (i = 0; i <= 5; i++) {
    ; // usa le ';' per fungere da corpo (null statement)
  }
  // oppure
  for (i = 0; i <= 5; i++);

  // ramificazione con più scelte: switch ()
  switch (a) {
  case 0: // Le etichette devono essere *espessioni costanti* (come gli enums)
    printf("Hey, 'a' equals 0!\n");
    break; // se non si rompe il flusso (break) continua sulle etichette successive
  case 1:
    printf("Huh, 'a' equals 1!\n");
    break;
    // Stai attendo: senza 'break' l'esecuzione continua fino a che non viene 
    // raggiunta la prossima pausa
  case 3:
  case 4:
    printf("Look at that.. 'a' is either 3, or 4\n");
    break;
  default:
    // se il valore di a non corrisponde a nessuna delle etichette
    fputs("Error!\n", stderr);
    exit(-1);
    break;
  }
  /*
    Usiamo il "goto" in C
  */
  typedef enum { false, true } bool;
  // per il C che non ha booleani come tipi di dato prima del C99 :(
  bool disaster = false;
  int i, j;
  for(i=0; i<100; ++i)
  for(j=0; j<100; ++j)
  {
    if((i + j) >= 150)
        disaster = true;
    if(disaster)
        goto error;  // esci da entrambi i cicli for
  }
  error: // questa è un etichetta a cui puoi "saltare" con `goto error`
  printf("Error occurred at i = %d & j = %d.\n", i, j);
  /*
    https://ideone.com/GuPhd6
    Questo stamperà l'errore 'Error occured at i=51 & j=99.'
  */
  /*
    È generalmente considerato una cattiva pratica farlo, 
    tranne se davvero sai cosa stai facendo, vedi:
    https://en.wikipedia.org/wiki/Spaghetti_code#Meaning
  */

  ///////////////////////////////////////
  // Typecasting
  ///////////////////////////////////////

  // Ogni valore in C ha un tipo, ma puoi lanciare un valore in un altro tipo
  // se vuoi (con alcuni vincoli)

  int x_hex = 0x01; // Puoi assegnare variabili con caratteri esadecimali
                    // il binario non è nello standard, ma consentito da alcuni
                    // compilatori (x_bin = 0b0010010110)

  // I casting tra i tipi tenterà di preservare i loro valori numerici
  printf("%d\n", x_hex); // => Stampa 1
  printf("%d\n", (short) x_hex); // => Stampa 1
  printf("%d\n", (char) x_hex); // => Stampa1 1

  // Se si assegna un valore superiore a un tipo di massimo, il rollover senza preavviso.
  printf("%d\n", (unsigned char) 257); // => 1 (Max char = 255 se char è lunga 8 bits)

  // Per determinare il valore massimo di un `char`, un char unsigned è un char senza sengno,
  // rispettivamente, usa i macro char_max, schar_max e uchar_max da limiti.h

  // I tipi integrati possono essere cambiati (cast) in virgola mobile e vice versa
  printf("%f\n", (double) 100); // %f formatta sempre un doppio...
  printf("%f\n", (float)  100); // ...anche con un vigola mobile.
  printf("%d\n", (char)100.0);

  ///////////////////////////////////////
  // Puntatori (Pointers)
  ///////////////////////////////////////

  // Un puntatore è una variabile chiamata per archiviare un indirizzo di memoria.
  // La sua dichiarazione dirà il tipo di dato a cui è indicato.
  // Puoi recuperare l'indirizzo di memoria dalle tue variabili, poi potrai utilizzarle.

  int x = 0;
  printf("%p\n", (void *)&x); // Usa e recupera l'indirizzo di una variabile
  // (%p formattazione di un puntatore, oggetto di tipo void *)
  // => Stampa alcuni indirizzi in memoria;

  // I puntatori iniziano con * nella loro dichiarazione
  int *px, not_a_pointer; // px è un puntatore ad un int
  px = &x; // memorizza l'indirizzo di x in px
  printf("%p\n", (void *)px); // => stampa un indirizzo di memoria
  printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
  // => Stampa '8, 4' su un tipico sistema a 64 bit

  // Per recuperare il valore all'indirizzo che un puntatore sta puntando
  // metti * davanti alla referenza
  // Note: si, potrebbe confondere il fatto che '*' si usa per entrambe,
  // dichiarazione a puntatore e referenza ad esso.
  printf("%d\n", *px); // => Stampa 0, la variabile di x

  // Puoi anche modificare il valore a cui punta il puntatore.
  // dovremmo avvolgere la referenza tra parentesi perchè
  // ++ ha una precedenza maggiore di *.
  (*px)++; // Incrementa il valore che px punta di 1
  printf("%d\n", *px); // => Stampa 1
  printf("%d\n", x); // => Stampa 1

  // Gli array sono un buon modo di allocare blocchi di memoria contigui
  int x_array[20]; // dichiarazione di un array di dimensione 20 (non può essere cambiata la dimensione)
  int xx;
  for (xx = 0; xx < 20; xx++) {
    x_array[xx] = 20 - xx;
  } // Inizializza x_array a 20, 19, 18,... 2, 1

  // dichiara un puntatore di tipo int e inizializzalo per indicare x_array 
  int* x_ptr = x_array;
  // x_ptr ora indica il primo elemento nell'array (il numero intero 20).
  // Funziona perchè gli array spesso decadono nei puntatori del loro primo elemento.
  // Ad esempio, quando un array viene passato a una funzione o viene assegnato a un puntatore,
  // decade in (implicitamente convertito in) un puntatore.
  // Eccezioni: quando l'array è l'argomento dell'operatore `&` (indirizzo di): 
  int arr[10];
  int (*ptr_to_arr)[10] = &arr; // &arr non è un tipo`int *`!
  // È un tipo 'puntatore all'array' (del decimo 'int').
  // o quando l'array è una stringa di caratteri utilizzata per l'inizializzazione di un array di char:
  char otherarr[] = "foobarbazquirk";
  // oppure quando è un argomento di un operatore `sizeof` o `alignof`:
  int arraythethird[10];
  int *ptr = arraythethird; // equivalente con int *ptr = &arr[0];
  printf("%zu, %zu\n", sizeof(arraythethird), sizeof(ptr));
  // probabilmente stamperà "40, 4" or "40, 8"

  // I puntatori sono incrementati e decrementati in base al loro tipo
  // (questo si chiama puntatore aritmetico)
  printf("%d\n", *(x_ptr + 1)); // => Stampa 19
  printf("%d\n", x_array[1]); // => Stampa 19

  // E ancora possibile allocare dinamicamente blocchi di memoria contigui con la
  // funzione di libreria standard malloc, che prende un argomento di tipo `size_t`
  // che rappresenta il numero di byte da allocare (di solito dalla heap, sebbene questo
  // potrebbe non essere vero su alcuni sistemi embedded - il C standard non dice nulla al riguardo).
  int *my_ptr = malloc(sizeof(*my_ptr) * 20);
  for (xx = 0; xx < 20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
  } // Inizializza la memoria a 20, 19, 18, 17... 2, 1 (come int)

  // Fai attenzione a passare i valori forniti dall'utente alla malloc!
  // Se vuoi essere al sicuro, puoi usare calloc invece (che, a differenza di malloc,
  // inizializza tutti zero in memorial)
  int* my_other_ptr = calloc(20, sizeof(int));

  // Nota che non esiste un modo standard per ottenere la lunghezza di un array allocato dinamicamente in C.
  // Per questo motivo, se i tuoi array sono passati attraverso il programma per molto tempo, 
  // avrai bisogno di un altra variabile per tenere traccia del numero di elementi (dimensione) del array.
  // Vedi la sezione delle funzioni per maggiori informazioni.
  size_t size = 10;
  int *my_arr = calloc(size, sizeof(int));
  // Aggiungi un elemento all'array
  size++;
  my_arr = realloc(my_arr, sizeof(int) * size);
  if (my_arr == NULL) {
    // Ricordati di verificare il fallimento di realloc!
    return
  }
  my_arr[10] = 5;

  // la Memoria di dereferenziazione che non hai assegnato darà 'risultati imprevedibili'
  // - Si dice che il programma invochi 'comportamento indefinito'  
  printf("%d\n", *(my_ptr + 21)); // => Stampa chi-sa-cosa? potrebbe anche andare in crash.

  // Quando hai finito con un blocco di memoria malloc, devi liberarlo,
  // altrimenti nessun altro può usarlo fino a quando il programma non termina
  // (questo è chiamato perdita di memoria (= memory leak))
  free(my_ptr);

  // Le stringhe sono array di char, ma solitamente sono rappresentate come
  // puntatori-al-char (che è un puntatore al primo elemento dell'array).
  // È una buona pratica usare `const chat *` quando si fa riferimento a una stringa di caratteri,
  // poichè i caratteri di una stringa non devono essere modificati 
  // (ovvero 'foo' [0] = 'a' è illegale)
  const char *my_str = "This is my very own string literal";
  printf("%c\n", *my_str); // => 'T'

  // Questo non è il caso se la stringa è un array 
  // (potenzialmente inizializzato con una stringa di caratteri)
  // che risiede nella memoria scrivibile, come in:
  char foo[] = "foo";
  foo[0] = 'a'; // questo è legale, foo ora contiene 'aoo'.

  function_1();
} // fine della funzione main

///////////////////////////////////////
// Funzioni (Functions)
///////////////////////////////////////

// Sintassi della dichiarazione di una funzione:
// <tipo ritorno> <nome funzione>(<argomenti>)

int add_two_ints(int x1, int x2)
{
  return x1 + x2; // Utilizzare return per restituire un valore
}

/*
Le funzioni sono chiamate per valore. Quando viene chiamata una funzione,
gli argomenti passati alla funzione sono copie di argomenti originali (tranne gli array).
Qualunque cosa tu fai l'argomento nella funzione non cambia il valore originale rispetto 
a quando è stata chiamata.

Usa i puntatori se te hai bisogno di modificare l'argomento originale  
(gli array sono sempre passati come puntatori).

Esempio: inversione della stringa.
*/

// Una funzione void in ritorna nulla
void str_reverse(char *str_in)
{
  char tmp;
  size_t ii = 0;
  size_t len = strlen(str_in); // `strlen()` è parte della libreria standard
                               // NOTE: la lunghezza ritornata da `strlen` NON
                               //       include la terminazione con il NULL byte ('\0')
  // nella versione C99 e superiori, puoi direttamente dichiamare le variabili nel controllo del ciclo
  // nelle parentesi del loop e.g. `for (size_t ii = 0; ...`
  for (ii = 0; ii < len / 2; ii++) {
    tmp = str_in[ii];
    str_in[ii] = str_in[len - ii - 1]; // ii-iesimo chat dalla fine
    str_in[len - ii - 1] = tmp;
  }
}
//NOTE: string.h il file di intestazione ha bisogno di includere

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/
/*
come possiamo ritornare solo una variabile
per cambiare valori in più di una variabile possiamo utilizzare le referenze
*/
void swapTwoNumbers(int *a, int *b)
{
    int temp = *a;
    *a = *b;
    *b = temp;
}
/*
int first = 10;
int second = 20;
printf("first: %d\nsecond: %d\n", first, second);
swapTwoNumbers(&first, &second);
printf("first: %d\nsecond: %d\n", first, second);
// i valori verranno scambiati
*/

// Restituire più valori.
// Il linguaggio C non permette di restituire più valori utilizzando l'istruzione return.
// Se si desidera restituire più valori, la funzione deve ricevere in ingresso le variabili
// in cui salvare i risultati. Queste variabili devono essere passate come puntatori,
// in modo che la funzione possa modificarne direttamente il contenuto.
int return_multiple( int *array_of_3, int *ret1, int *ret2, int *ret3)
{
    if(array_of_3 == NULL)
        return 0; // ritorna il codice d'errore (falso)

    // mettiamo le varibili nel puntatore in modo da modificare il suo valore
   *ret1 = array_of_3[0];
   *ret2 = array_of_3[1];
   *ret3 = array_of_3[2];

   return 1; //return codice d'errore (vero)
}

/*
Per qunato riguarda gli array, saranno sempre passati alla funzioni come indicatori.
Anche se allochi staticamente un array come `arr[10]`
Viene ancora passato come puntatore al primo elemento in qualsiasi chiamata di funzioni.
Ancora una volta non esiste un modo standard per ottenere le dimensioni di un array allocato dinamicamente in C. 
*/
// La grandezza deve essere passata!
// Altrimenti, questa funzione non ha modo di sapere quanto sia grande l'array.
void printIntArray(int *arr, size_t size) {
    int i;
    for (i = 0; i < size; i++) {
        printf("arr[%d] is: %d\n", i, arr[i]);
    }
}
/*
int my_arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
int size = 10;
printIntArray(my_arr, size);
// stamperà "arr[0] is: 1" etc
*/

// Se si fa riferimento a variaibli esterne, fuori dalla funzione, 
// è necessario utilizzare la parola chiave extern
int i = 0;
void testFunc() {
  extern int i; //i qui ora sta usando la variabile esterna i
}

// rendi le variabili esterne private per il file di origine con static:
static int j = 0; // altri file che utilizzano testFunc2() non possono accedere alla variabile j
void testFunc2() {
  extern int j;
}
// La parola chiave `static` rende una variabile inaccessibile al codice al di fuori del
// unità di compilazione. (Su quasi tutti i sistemi, un'unità di compilazione è un file .c).
// `static` si può applicare sia alla variabili globali (all'unità di compilazione),
// funzioni e variabili e variabili di funzioni locali.
// Quando si usa `static` con variabili di funzioni locali, la variabile è effettivamente globale 
// e conserva il suo valore tra le chiamate di funzione, ma è accessibile solo all'interno della funzione 
// in cui è dichiarata.
// In più, le variabili statiche sono inizializzate a 0 se non dichiarate con qualche valore di partenza.
// **Puoi dichiarare funzioni statiche per renderle private**

///////////////////////////////////////
// Tipi definiti dal utente e strutture
///////////////////////////////////////

// Typedefs può essere utilizzato per creare alias di tipo
typedef int my_type;
my_type my_type_var = 0;

// Le strutture sono delle raccolte di dati, i membri vengono allocati in memorial in sequenza,
// nell'ordine sono scritti:
struct rectangle {
  int width;
  int height;
};

// non è generalmente vero
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// a causa del possibile padding tra i membri 
// della struttura (necessario per motivi di allineamento) [1]

void function_1()
{
  struct rectangle my_rec = { 1, 2 }; // Campo che può essere inizializzato immediatamente

  // Accesso alla struttura con .
  my_rec.width = 10;
  my_rec.height = 20;

  // Puoi dichiarare puntatori alle strutture
  struct rectangle *my_rec_ptr = &my_rec;

  // Usa la dereferenziazione per impostare i membri del puntatore...
  (*my_rec_ptr).width = 30;

  // ... o anche meglio: preferisci la -> per motivi di legibilità
  my_rec_ptr->height = 10; // Stesso di (*my_rec_ptr).height = 10;
}

// Puoi applicare typedef alla struttura per convenienza
typedef struct rectangle rect;

int area(rect r)
{
  return r.width * r.height;
}

// I Typedefs possono anche essere definiti a destra durante la definizione della struttura 
typedef struct {
  int width;
  int height;
} rect;
// Come prima, fare questo significa che puoi scrivere
rect r;
// invece di dover digitare
struct rectangle r;

// Se hai strutture di grandi dimensioni, puoi passarle `per puntatore` 
// per evitare di copiare l'intera struttura:
int areaptr(const rect *r)
{
  return r->width * r->height;
}

///////////////////////////////////////
// Puntatori a funzioni
///////////////////////////////////////
/*
In fase di esecuzioni, le funzioni si trovano su indirizzi di memoria noti. 
I puntatori della funzione sono proprio come qualsiasi altro puntatore 
(memorizzano solo un indirizzo di memoria), ma possono essere utilizzati
per invocare le funzioni direttamente (o le funzioni di callback).
Tuttavia, la sintassi della definizione può essere inizialmente confusa

Esempio: usa str_reverse da un puntatore:
*/
void str_reverse_through_pointer(char *str_in) {
  // Definire una variabile puntatore della funzione, denominata f.
  void (*f)(char *); // La segnatura dovrebbe corrispondere esattamente alla funzione target.
  f = &str_reverse; // Assegna l'indirizzo per la funzione effettiva (determinato a tempo d'esecuzione)
  // f = str_reverse; funzionerebbe anche: le funzioni decadono nei puntatori, simili agli array
  (*f)(str_in); // Per chiamare la funzione attraverso il puntatore
  // f(str_in); // Questa è una sintassi alternativa ma ugualmente valida per chiamarla
}

/*
Finchè le segnature delle funzioni corrispondono, è possibile assegnare qualsiasi funzione allo stesso puntatore.
I puntatori della funzione sono generalmente scritti per semplicità e leggibilità come segue:
*/

typedef void (*my_fnp_type)(char *);

// quindi utilizzato quando si dichiara la variabile del puntatore effettivo:
// ...
// my_fnp_type f;


/////////////////////////////
// Stampando caratteri con printf()
/////////////////////////////

//Caratteri speciali:
/*
'\a'; // carattere di allarme
'\n'; // carattere di nuova riga  
'\t'; // carattere di tabulazione (allinea il testo a sinistra)  
'\v'; // tabulazione verticale  
'\f'; // nuova pagina (form feed)  
'\r'; // ritorno a capo (carriage return)  
'\b'; // carattere di backspace (cancella il carattere precedente)  
'\0'; // carattere NULL. Di solito viene utilizzato alla fine delle stringhe in C.  
//   Esempio: "hello\n\0". \0 viene usato per convenzione per indicare la fine della stringa.  
'\\'; // barra inversa (backslash)  
'\?'; // punto interrogativo  
'\''; // apostrofo (singolo apice)  
'\"'; // doppio apice  
'\xhh'; // numero esadecimale. Esempio: '\xb' corrisponde al carattere di tabulazione verticale  
'\0oo'; // numero ottale. Esempio: '\013' corrisponde al carattere di tabulazione verticale  

// Formattazione della stampa:  
"%d";    // intero  
"%3d";   // intero con una lunghezza minima di 3 cifre (allineato a destra)  
"%s";    // stringa  
"%f";    // numero in virgola mobile (float)  
"%ld";   // numero long  
"%3.2f"; // numero float con almeno 3 cifre prima della virgola e 2 dopo  
"%7.4s"; // applicabile anche alle stringhe  
"%c";    // carattere  
"%p";    // puntatore. NOTA: è necessario effettuare un cast a (void *) prima di passarlo come argomento a `printf`.  
"%x";    // numero esadecimale  
"%o";    // numero ottale  
"%%";    // stampa il carattere %  
*/


///////////////////////////////////////
// Ordine di valutazione
///////////////////////////////////////

// Da sopra a sotto, il sopra ha la precedenza 
//----------------------------------------------------------//
//        Operatori                  | Associatività        //
//----------------------------------------------------------//
// () [] -> .                        | da sinistra a destra //
// ! ~ ++ -- + = *(tipo) sizeof      | da destra a sinistra //
// * / %                             | da sinistra a destra //
// + -                               | da sinistra a destra //
// << >>                             | da sinistra a destra //
// < <= > >=                         | da sinistra a destra //
// == !=                             | da sinistra a destra //
// &                                 | da sinistra a destra //
// ^                                 | da sinistra a destra //
// |                                 | da sinistra a destra //
// &&                                | da sinistra a destra //
// ||                                | da sinistra a destra //
// ?:                                | da destra a sinistra //
// = += -= *= /= %= &= ^= |= <<= >>= | da destra a sinistra //
// ,                                 | da sinistra a destra //
//----------------------------------------------------------//


/******************************* File d'intestazione **********************************

I file d'intestazione sono una parte importante del C, lo permettono la connesione di 
file sorgenti in C e possono semplificare il codice e le definizioni separandoli in file diversi.

I file d'intestazione sono sintatticamente simili ai file di sorgente C ma risiedono in '.h'
Possono essere inclusi nel tuo file sorgente utilizzando il pre-processore con la direttiva #include 'Esempio.h'
dato che 'esempio.h' esiste nella stessa cartella come file c. 
*/

/* Una protezione per evitare che l'header venga definito troppe volte. Questo */
/* accade in caso di dipendenze circolari, quando il contenuto dell'header è   */
/* già stato definito.                                                         */
#ifndef EXAMPLE_H /* Se EXAMPLE_H non è ancora stato definito. */
#define EXAMPLE_H /* Definisce la macro EXAMPLE_H. */

/* Altri header possono essere inclusi negli header e quindi inclusi in modo  */
/* transitivo nei file che includono questo header.                           */
#include <string.h>

/* Come per i file sorgente in C, le macro possono essere definite negli header */
/* e utilizzate nei file che includono questo file header.                      */
#define EXAMPLE_NAME "Dennis Ritchie"

/* Anche le macro funzione possono essere definite. */
#define ADD(a, b) ((a) + (b))

/* Nota le parentesi che racchiudono gli argomenti: sono importanti per evitare */
/* che `a` e `b` vengano espansi in modo inaspettato. Ad esempio, considera    */
/* MUL(x, y) (x * y); MUL(1 + 2, 3) verrebbe espanso in (1 + 2 * 3), dando un  */
/* risultato errato.                                                            */

/* Le struct e i typedef possono essere usati per garantire coerenza tra i file. */
typedef struct Node
{
    int val;
    struct Node *next;
} Node;

/* Anche le enumerazioni possono essere definite qui. */
enum traffic_light_state {GREEN, YELLOW, RED};

/* I prototipi di funzione possono essere definiti qui per l'uso in più file,  */
/* ma è una cattiva pratica definire la funzione direttamente nell'header.     */
/* Le definizioni dovrebbero essere inserite in un file C separato.            */
Node createLinkedList(int *vals, int len);

/* Oltre agli elementi sopra citati, altre definizioni dovrebbero essere lasciate */
/* a un file sorgente C. Inoltre, non bisognerebbe includere troppi file o      */
/* definizioni in un unico header, ma organizzarli in più header separati o in  */
/* un file C.                                                                   */

#endif /* Fine della direttiva preprocessor if. */

```

## Ulteriori letture

È meglio procurarsi una copia di [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language). È il libro sul C, scritto da Dennis Ritchie, il creatore del linguaggio, e Brian Kernighan. Tuttavia, fai attenzione: è piuttosto datato e contiene alcune imprecisioni (o meglio, idee che oggi non sono più considerate valide) e pratiche che nel tempo sono cambiate.

Un'altra buona risorsa è [Learn C The Hard Way](http://learncodethehardway.org/c/) (non è gratuito).

Se hai una domanda, consulta le [compl.lang.c Frequently Asked Questions](http://c-faq.com).

È molto importante utilizzare una corretta spaziatura, indentazione e mantenere uno stile di codifica coerente in generale.
Un codice leggibile è migliore di un codice "intelligente" o veloce. Per uno stile di codifica chiaro e ben strutturato, puoi fare riferimento al [Linux kernel coding style](https://www.kernel.org/doc/Documentation/process/coding-style.rst).

[1] [Why isn't sizeof for a struct equal to the sum of sizeof of each member?](https://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member)
