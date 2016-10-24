---
language: c++
filename: learncpp-it.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "http://github.com/connorwaters"]
translators:
    - ["Robert Margelli", "http://github.com/sinkswim/"]
    - ["Tommaso Pifferi", "http://github.com/neslinesli93/"]
lang: it-it
---

Il C++ è un linguaggio di programmazione il quale,
[secondo il suo inventore Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),
è stato progettato per

- essere un "miglior C"
- supportare l'astrazione dei dati
- supportare la programmazione orientata agli oggetti
- supportare la programmazione generica

Nonostante la sintassi possa risultare più difficile o complessa di linguaggi più recenti,
è usato in maniera vasta poichè viene compilato in istruzioni macchina che possono
essere eseguite direttamente dal processore ed offre un controllo stretto sull'hardware (come il linguaggio C)
ed allo stesso tempo offre caratteristiche ad alto livello come i generici, le eccezioni, e le classi.
Questa combinazione di velocità e funzionalità rende il C++
uno dei più utilizzati linguaggi di programmazione.

```c++
//////////////////
// Confronto con il C
//////////////////

// Il C++ è _quasi_ un superset del C e con esso condivide la sintassi di base per
// la dichiarazione di variabili, tipi primitivi, e funzioni.

// Proprio come nel C, l'inizio del programma è una funzione chiamata
// main con un intero come tipo di ritorno,
// Questo valore serve come stato d'uscita del programma.
// Vedi http://it.wikipedia.org/wiki/Valore_di_uscita per maggiori informazioni.
int main(int argc, char** argv)
{
    // Gli argomenti a linea di comando sono passati tramite argc e argv così come
    // avviene in C.
    // argc indica il numero di argomenti,
    // e argv è un array di stringhe in stile-C (char*)
    // che rappresenta gli argomenti.
    // Il primo argomento è il nome che è stato assegnato al programma.
    // argc e argv possono essere omessi se non hai bisogno di argomenti,
    // in questa maniera la funzione avrà int main() come firma.

    // Lo stato di uscita 0 indica successo.
    return 0;
}

// Tuttavia, il C++ varia nei seguenti modi:

// In C++, i caratteri come letterali sono dei char.
sizeof('c') == sizeof(char) == 1

// In C, i caratteri come letterali sono degli interi.
sizeof('c') == sizeof(int)


// C++ ha prototipizzazione rigida
void func(); // funziona che non accetta argomenti

// In C
void func(); // funzione che può accettare un qualsiasi numero di argomenti

// Usa nullptr invece di NULL in C++
int* ip = nullptr;

// Gli header C standard sono disponibili in C++,
// ma sono prefissati con "c" e non hanno il suffisso ".h".
#include <cstdio>

int main()
{
    printf("Ciao, mondo!\n");
    return 0;
}

///////////////////////////////
// Overloading per le funzioni
//////////////////////////////

// Il C++ supporta l'overloading per le funzioni
// sia dato che ogni funzione accetta parametri diversi.

void print(char const* myString)
{
    printf("Stringa %s\n", myString);
}

void print(int myInt)
{
    printf("Il mio int è %d", myInt);
}

int main()
{
    print("Ciao"); // Viene chiamata void print(const char*)
    print(15); //  Viene chiamata void print(int)
}

////////////////////////
// Argomenti di default
///////////////////////

// Puoi fornire argomenti di default per una funzione
// se non sono forniti dal chiamante.

void faiQualcosaConInteri(int a = 1, int b = 4)
{
    // fai qualcosa con gli interi qui
}

int main()
{
    faiQualcosaConInteri();      // a = 1,  b = 4
    faiQualcosaConInteri(20);    // a = 20, b = 4
    faiQualcosaConInteri(20, 5); // a = 20, b = 5
}

// Gli argomenti di default devono essere alla fine della lista degli argomenti.

void dichiarazioneInvalida(int a = 1, int b) // Errore!
{
}


/////////////
// Namespaces
/////////////

// I namespaces forniscono visibilità separata per dichiarazioni di variabili, funzioni,
// ed altro.
// I namespaces possono essere annidati.

namespace Primo {
    namespace Annidato {
        void foo()
        {
            printf("Questa è Primo::Annidato::foo\n");
        }
    } // fine di namespace Annidato
} // fine di namespace Primo

namespace Secondo {
    void foo()
    {
        printf("Questa è Secondo::foo\n");
    }
}

void foo()
{
    printf("Questa è foo globale\n");
}

int main()
{
    // Include tutti i simboli del namespace Secondo nello scope attuale.
    // Osserva che chiamare semplicemente foo() non va più bene perché è ambiguo:
    // bisogna specificare se vogliamo chiamare foo definita nel namespace Secondo
    // o foo definita nel livello principale del programma.

    using namespace Secondo;

    Secondo::foo(); // stampa "Questa è Secondo::foo"
    Primo::Annidato::foo(); // stampa "Questa è Primo::Annidato::foo"
    ::foo(); // stampa "Questa è foo globale"
}

///////////////
// Input/Output
///////////////

// L'input e l'output in C++ utilizza gli streams
// cin, cout, e cerr i quali rappresentano stdin, stdout, e stderr.
// << è l'operatore di inserzione >> è l'operatore di estrazione.

#include <iostream> // Include gli streams di I/O

using namespace std; // Gli streams sono nel namespace std (libreria standard)

int main()
{
   int myInt;

   // Stampa su stdout (o terminalee/schermo)
   cout << "Inserisci il tuo numero preferito:\n";
   // Prende l'input
   cin >> myInt;

   // cout può anche essere formattato
   cout << "Il tuo numero preferito è " << myInt << "\n";
   // stampa "Il tuo numero preferito è <myInt>"

    cerr << "Usato per messaggi di errore";
}

////////////
// Stringhe
///////////

// Le stringhe in C++ sono oggetti ed hanno molte funzioni membro
#include <string>

using namespace std; // Anche le stringhe sono contenute nel namespace std (libreria standard)

string myString = "Ciao";
string myOtherString = " Mondo";

// + è usato per la concatenazione.
cout << myString + myOtherString; // "Ciao Mondo"

cout << myString + " Bella"; // "Ciao Bella"

// le stringhe in C++ possono essere modificate.
myString.append(" Mario");
cout << myString; // "Ciao Mario"


///////////////
// Riferimenti
//////////////

// Oltre ai puntatori come quelli in C,
// il C++ ha i _riferimenti_.
// Questi non sono tipi puntatori che non possono essere riassegnati una volta settati
// e non possono essere null.
// Inoltre, essi hanno la stessa sintassi della variabile stessa:
// * non è necessario per la dereferenziazione e
// & ("indirizzo di") non è usato per l'assegnamento.

using namespace std;

string foo = "Io sono foo";
string bar = "Io sono bar";


string& fooRef = foo; // Questo crea un riferimento a foo.
fooRef += ". Ciao!"; // Modifica foo attraverso il riferimento
cout << fooRef; // Stampa "Io sono foo. Ciao!"

// Non riassegna "fooRef". Questo è come scrivere "foo = bar", e
//   foo == "Io sono bar"
// dopo questa riga.
cout << &fooRef << endl; // Stampa l'indirizzo di foo
fooRef = bar;
cout << &fooRef << endl; // Stampa lo stesso l'indirizzo di foo
cout << fooRef;  // Stampa "Io sono bar"

// L'indirizzo di fooRef rimane lo stesso, ovvero si riferisce ancora a foo.


const string& barRef = bar; // Crea un riferimento const a bar.
// Come in C, i valori const (i puntatori e i riferimenti) non possono essere modificati.
barRef += ". Ciao!"; // Errore, i riferimenti const non possono essere modificati.

// Facciamo un piccolo excursus: prima di approfondire ancora i riferimenti, è necessario
// introdurre il concetto di oggetto temporaneo. Supponiamo di avere il seguente codice:
string tempObjectFun() { ... }
string retVal = tempObjectFun();

// Nella seconda riga si ha che:
//   - un oggetto di tipo stringa viene ritornato da tempObjectFun
//   - viene costruita una nuova stringa, utilizzando l'oggetto ritornato come
//     argomento per il costruttore
//   - l'oggetto ritornato da tempObjectFun viene distrutto
// L'oggetto ritornato da tempObjectFun viene detto oggetto temporaneo.
// Un oggetto temporaneo viene creato quando una funzione ritorna un oggetto, e viene
// distrutto quando l'espressione che lo racchiude termina la sua esecuzione - questo
// comportamento viene definito dallo standard, ma i compilatori possono modificarlo
// a piacere. Cerca su google "return value optimization" se vuoi approfondire.
// Dunque nel seguente codice:
foo(bar(tempObjectFun()))

// dando per scontato che foo e bar esistano, l'oggetto ritornato da tempObjectFun
// è passato a bar ed è distrutto prima dell'invocazione di foo.

// Tornando ai riferimenti, c'è un'eccezione a quanto appena detto.
// Infatti un oggetto temporaneo "viene distrutto quando l'espressione
// che lo racchiude termina la sua esecuzione", tranne quando è legato ad un
// riferimento di tipo const. In tal caso la sua vita viene estesa per tutto
// lo scope attuale:

void constReferenceTempObjectFun() {
    // constRef riceve l'oggetto temporaneo, che non viene distrutto fino
    // alla fine di questa funzione.
    const string& constRef = tempObjectFun();
    ...
}

// Un altro tipo di riferimento introdotto nel C++11 è specifico per gli
// oggetti temporanei. Non puoi dichiarare una variabile di quel tipo, ma
// ha la precedenza nella risoluzione degli overload:

void someFun(string& s) { ... }  // Riferimento normale
void someFun(string&& s) { ... }  // Riferimento ad un oggetto temporaneo

string foo;
someFun(foo);  // Chiama la versione con il riferimento normale
someFun(tempObjectFun());  // Chiama la versione con il riferimento temporaneo

// Ad esempio potrai vedere questi due costruttori per std::basic_string:
basic_string(const basic_string& other);
basic_string(basic_string&& other);

// L'idea è che se noi costruiamo una nuova stringa a partire da un oggetto temporaneo
// (che in ogni caso verrà distrutto), possiamo avere un costruttore più efficiente
// che in un certo senso "recupera" parti di quella stringa temporanea.
// Ci si riferisce a questo concetto come "move semantics".

/////////////////////
// Enum
/////////////////////

// Gli enum sono un modo per assegnare un valore ad una costante, e sono
// principalmente usati per rendere il codice più leggibile.
enum ETipiMacchine
{
  AlfaRomeo,
  Ferrari,
  SUV,
  Panda
};

ETipiMacchine GetPreferredCarType()
{
    return ETipiMacchine::Ferrari;
}

// Dal C++11 in poi c'è un modo molto semplice per assegnare un tipo ad un enum,
// che può essere utile per la serializzazione dei dati o per convertire gli enum
// tra il tipo desiderato e le rispettive costanti.
enum ETipiMacchine : uint8_t
{
  AlfaRomeo, // 0
  Ferrari, // 1
  SUV = 254, // 254
  Ibrida // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // Serializza InputValue in un file
}

void WritePreferredCarTypeToFile(ETipiMacchine InputCarType)
{
    // L'enum viene implicitamente convertito ad un uint8_t poiché
    // è stato dichiarato come tale
    WriteByteToFile(InputCarType);
}

// D'altro canto potresti voler evitare che un enum venga accidentalmente convertito
// in un intero o in un altro tipo, quindi è possibile create una classe enum che
// impedisce la conversione implicita.
enum class ETipiMacchine : uint8_t
{
  AlfaRomeo, // 0
  Ferrari, // 1
  SUV = 254, // 254
  Ibrida // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // Serializza InputValue in un file
}

void WritePreferredCarTypeToFile(ETipiMacchine InputCarType)
{
    // Il compilatore darà errore anche se ETipiMacchine è un uint8_t: questo
    // perchè abbiamo dichiarato l'enum come "enum class"!
    WriteByteToFile(InputCarType);
}

//////////////////////////////////////////////////
// Classi e programmazione orientata agli oggetti
/////////////////////////////////////////////////

// Primo esempio delle classi
#include <iostream>

// Dichiara una classe.
// Le classi sono in genere dichiara in un header file (.h o .hpp).
class Cane {
    // Variabili e funzioni membro sono private di default.
    std::string nome;
    int peso;

// Tutti i membri dopo questo sono pubblici (public)
// finchè "private:" o "protected:" non compaiono.
public:

    // Costruttore di default
    Cane();

    // Dichiarazioni di funzioni membro (le implentazioni sono a seguito)
    // Nota che stiamo usando std::string invece di porre
    // using namespace std;
    // sopra.
    // Mai usare uno statement "using namespace" in uno header.
    void impostaNome(const std::string& nomeCane);

    void impostaPeso(int pesoCane);

    // Le funzioni che non modificano lo stato dell'oggetto
    // dovrebbero essere marcate come const.
    // Questo permette di chiamarle con un riferimento const all'oggetto.
    // Inoltre, nota che le funzioni devono essere dichiarate espliciamente come _virtual_
    // per essere sovrascritte in classi derivate.
    // Le funzioni non sono virtual di default per motivi di performance.
    virtual void print() const;

    // Le funzioni possono essere definite anche all'interno del corpo della classe.
    // Le funzioni definite in questo modo sono automaticamente inline.
    void abbaia() const { std::cout << nome << " abbaia!\n"; }

    // Assieme con i costruttori, il C++ fornisce i distruttori.
    // Questi sono chiamati quando un oggetto è rimosso o esce dalla visibilità.
    // Questo permette paradigmi potenti come il RAII
    // (vedi sotto)
    // I distruttori devono essere virtual per permettere a classi di essere
    // derivate da questa; altrimenti, il distruttore della classe derivata
    // non viene chiamato se l'oggetto viene distrutto tramite un riferimento alla
    // classe da cui ha ereditato o tramite un puntatore.
    virtual ~Dog();

}; // Un punto e virgola deve seguire la definizione della funzione

// Le funzioni membro di una classe sono generalmente implementate in files .cpp .
Cane::Cane()
{
    std::cout << "Un cane è stato costruito\n";
}

// Gli oggetti (ad esempio le stringhe) devono essere passati per riferimento
// se li stai modificando o come riferimento const altrimenti.
void Cane::impostaNome(const std::string& nomeCane)
{
    nome = nomeCane;
}

void Cane::impostaPeso(int pesoCane)
{
    peso = pesoCane;
}

// Notare che "virtual" è solamente necessario nelle dichiarazioni, non nelle definizioni.
void Cane::print() const
{
    std::cout << "Il cane è " << nome << " e pesa " << peso << "kg\n";
}

Cane::~Cane()
{
    cout << "Ciao ciao " << nome << "\n";
}

int main() {
    Cane myDog; // stampa "Un cane è stato costruito"
    myDog.impostaNome("Barkley");
    myDog.impostaPeso(10);
    myDog.print(); // stampa "Il cane è Barkley e pesa 10 kg"
    return 0;
} // stampa "Ciao ciao Barkley"

// Ereditarietà:

// Questa classe eredita tutto ciò che è public e protected dalla classe Cane,
// ma anche ciò che privato: tuttavia non potrà accedere direttamente a membri/metodi
// privati se non c'è un metodo pubblico o privato che permetta di farlo.
class MioCane : public Cane {

    void impostaProprietario(const std::string& proprietarioCane);

    // Sovrascrivi il comportamento della funzione print per tutti i MioCane. Vedi
    // http://it.wikipedia.org/wiki/Polimorfismo_%28informatica%29
    // per una introduzione più generale se non sei familiare con
    // il polimorfismo.
    // La parola chiave override è opzionale ma fa sì che tu stia effettivamente
    // sovrascrivendo il metodo nella classe base.
    void print() const override;

private:
    std::string proprietario;
};

// Nel frattempo, nel file .cpp corrispondente:

void MioCane::impostaProprietario(const std::string& proprietarioCane)
{
    proprietario = proprietarioCane;
}

void MioCane::print() const
{
    Cane::print(); // Chiama la funzione print nella classe base Cane
    std::cout << "Il cane è di " << proprietario << "\n";
    // stampa "Il cane è <nome> e pesa <peso>"
    //        "Il cane è di <proprietario>"
}

///////////////////////////////////////////////////
// Inizializzazione ed Overloading degli Operatori
//////////////////////////////////////////////////

// In C++ puoi sovrascrivere il comportamento di operatori come +, -, *, /, ecc...
// Questo è possibile definendo una funzione che viene chiamata
// ogniqualvolta l'operatore è usato.

#include <iostream>
using namespace std;

class Punto {
public:
    // Così si assegna alle variabili membro un valore di default.
    double x = 0;
    double y = 0;

    // Definisce un costruttore di default che non fa nulla
    // ma inizializza il Punto ai valori di default (0, 0)
    Punto() { };

    // La sintassi seguente è nota come lista di inizializzazione
    // ed è il modo appropriato di inizializzare i valori membro della classe
    Punto (double a, double b) :
        x(a),
        y(b)
    { /* Non fa nulla eccetto inizializzare i valori */ }

    // Sovrascrivi l'operatore +.
    Punto operator+(const Punto& rhs) const;

    // Sovrascrivi l'operatore +=
    Punto& operator+=(const Punto& rhs);

    // Avrebbe senso aggiungere gli operatori - e -=,
    // ma li saltiamo per rendere la guida più breve.
};

Punto Punto::operator+(const Punto& rhs) const
{
    // Crea un nuovo punto come somma di questo e di rhs.
    return Punto(x + rhs.x, y + rhs.y);
}

Punto& Punto::operator+=(const Punto& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Punto su (0,1);
    Punto destro (1,0);
    // Questo chiama l'operatore + di Punto
    // Il Punto su chiama la funzione + con destro come argomento
    Punto risultato = su + destro;
    // Stampa "Risultato è spostato in (1,1)"
    cout << "Risultato è spostato (" << risultato.x << ',' << risultato.y << ")\n";
    return 0;
}

/////////////////
// Templates
////////////////

// Generalmente i templates in C++ sono utilizzati per programmazione generica, anche se
// sono molto più potenti dei costrutti generici in altri linguaggi. Inoltre,
// supportano specializzazione esplicita e parziale, classi in stile funzionale,
// e sono anche complete per Turing.

// Iniziamo con il tipo di programmazione generica con cui forse sei familiare. Per
// definire una classe o una funzione che prende un parametro di un dato tipo:
template<class T>
class Box {
public:
    // In questa classe, T può essere usato come qualsiasi tipo.
    void inserisci(const T&) { ... }
};

// Durante la compilazione, il compilatore in effetti genera copie di ogni template
// con i parametri sostituiti, e così la definizione completa della classe deve essere
// presente ad ogni invocazione. Questo è il motivo per cui vedrai le classi template definite
// interamente in header files.

// Per instanziare una classe template sullo stack:
Box<int> intBox;

// e puoi usarla come aspettato:
intBox.inserisci(123);

//Puoi, ovviamente, innestare i templates:
Box<Box<int> > boxOfBox;
boxOfBox.inserisci(intBox);

// Fino al C++11, devi porre uno spazio tra le due '>', altrimenti '>>'
// viene visto come l'operatore di shift destro.

// Qualche volta vedrai
// template<typename T>
// invece. La parole chiavi 'class' e 'typename' sono _generalmente_
// intercambiabili in questo caso. Per una spiegazione completa, vedi
// http://en.wikipedia.org/wiki/Typename
// (si, quella parola chiave ha una sua pagina di Wikipedia propria).

// Similmente, una funzione template:
template<class T>
void abbaiaTreVolte(const T& input)
{
    input.abbaia();
    input.abbaia();
    input.abbaia();
}

// Nota che niente è specificato relativamente al tipo di parametri. Il compilatore
// genererà  e poi verificherà il tipo di ogni invocazione del template, così che
// la funzione di cui sopra funzione con ogni tipo 'T' che ha const 'abbaia' come metodo!

Cane fluffy;
fluffy.impostaNome("Fluffy")
abbaiaTreVolte(fluffy); // Stampa "Fluffy abbaia" tre volte.

// I parametri template non devono essere classi:
template<int Y>
void stampaMessaggio() {
  cout << "Impara il C++ in " << Y << " minuti!" << endl;
}

// E poi esplicitamente specializzare i template per avere codice più efficiente. Ovviamente,
// la maggior parte delle casistiche reali non sono così triviali.
// Notare che avrai comunque bisogna di dichiarare la funzione (o classe) come un template
// anche se hai esplicitamente specificato tutti i parametri.
template<>
void stampaMessaggio<10>() {
  cout << "Impara il C++ più velocemente in soli 10 minuti!" << endl;
}

printMessage<20>();  // Stampa "impara il C++ in 20 minuti!"
printMessage<10>();  // Stampa "Impara il C++ più velocemente in soli 10 minuti!"                                   
                                        
////////////////////////////
// Gestione delle eccezioni
///////////////////////////

// La libreria standard fornisce un paio di tipi d'eccezioni
// (vedi http://en.cppreference.com/w/cpp/error/exception)
// ma ogni tipo può essere lanciato come eccezione
#include <exception>
#include <stdexcept>

// Tutte le eccezioni lanciate all'interno del blocco _try_ possono essere catturate dai successivi 
// handlers _catch_.
try {
    // Non allocare eccezioni nello heap usando _new_.
    throw std::runtime_error("C'è stato un problema.");
}

// Cattura le eccezioni come riferimenti const se sono oggetti
catch (const std::exception& ex)
{
    std::cout << ex.what();
}

// Cattura ogni eccezioni non catturata dal blocco _catch_ precedente
catch (...)
{
    std::cout << "Catturata un'eccezione sconosciuta";
    throw; // Rilancia l'eccezione
}

///////
// RAII
///////

// RAII sta per "Resource Allocation Is Initialization".
// Spesso viene considerato come il più potente paradigma in C++.
// È un concetto semplice: un costruttore di un oggetto
// acquisisce le risorse di tale oggetto ed il distruttore le rilascia.

// Per comprendere come questo sia vantaggioso,
// consideriamo una funzione che usa un gestore di file in C:
void faiQualcosaConUnFile(const char* nomefile)
{
    // Per cominciare, assumiamo che niente possa fallire.

    FILE* fh = fopen(nomefile, "r"); // Apri il file in modalità lettura.

    faiQualcosaConIlFile(fh);
    faiQualcosAltroConEsso(fh);

    fclose(fh); // Chiudi il gestore di file.
}

// Sfortunatamente, le cose vengono complicate dalla gestione degli errori.
// Supponiamo che fopen fallisca, e che faiQualcosaConUnFile e
// faiQualcosAltroConEsso ritornano codici d'errore se falliscono.
//  (Le eccezioni sono la maniera preferita per gestire i fallimenti,
//   ma alcuni programmatori, specialmente quelli con un passato in C,
//   non sono d'accordo con l'utilità delle eccezioni).
// Adesso dobbiamo verificare che ogni chiamata per eventuali fallimenti e chiudere il gestore di file
// se un problema è avvenuto.
bool faiQualcosaConUnFile(const char* nomefile)
{
    FILE* fh = fopen(nomefile, "r"); // Apre il file in modalità lettura
    if (fh == nullptr) // Il puntatore restituito è null in caso di fallimento.
        return false; // Riporta il fallimento al chiamante.

    // Assumiamo che ogni funzione ritorni false se ha fallito
    if (!faiQualcosaConIlFile(fh)) {
        fclose(fh); // Chiude il gestore di file così che non sprechi memoria.
        return false; // Propaga l'errore.
    }
    if (!faiQualcosAltroConEsso(fh)) {
        fclose(fh); // Chiude il gestore di file così che non sprechi memoria.
        return false; // Propaga l'errore.
    }

    fclose(fh); // Chiudi il gestore di file così che non sprechi memoria.
    return true; // Indica successo
}

// I programmatori C in genere puliscono questa procedura usando goto:
bool faiQualcosaConUnFile(const char* nomefile)
{
    FILE* fh = fopen(nomefile, "r");
    if (fh == nullptr)
        return false;

    if (!faiQualcosaConIlFile(fh))
        goto fallimento;

    if (!faiQualcosAltroConEsso(fh))
        goto fallimento;

    fclose(fh); // Chiude il file
    return true; // Indica successo

fallimento:
    fclose(fh);
    return false; // Propaga l'errore
}

// Se le funzioni indicano errori usando le eccezioni,
// le cose sono un pò più pulite, ma sono sempre sub-ottimali.
void faiQualcosaConUnFile(const char* nomefile)
{
    FILE* fh = fopen(nomefile, "r"); // Apre il file in modalità lettura
    if (fh == nullptr)
        throw std::runtime_error("Errore nell'apertura del file.");

    try {
        faiQualcosaConIlFile(fh);
        faiQualcosAltroConEsso(fh);
    }
    catch (...) {
        fclose(fh); // Fai sì che il file venga chiuso se si ha un errore.
        throw; // Poi rilancia l'eccezione.
    }

    fclose(fh); // Chiudi il file
    // Tutto è andato bene
}

// Confronta questo con l'utilizzo della classe C++ file stream (fstream)
// fstream usa i distruttori per chiudere il file.
// Come detto sopra, i distruttori sono automaticamente chiamati
// ogniqualvolta un oggetto esce dalla visibilità.
void faiQualcosaConUnFile(const std::string& nomefile)
{
    // ifstream è l'abbreviazione di input file stream
    std::ifstream fh(nomefile); // Apre il file

    // Fai qualcosa con il file
    faiQualcosaConIlFile(fh);
    faiQualcosAltroConEsso(fh);

} // Il file viene chiuso automaticamente chiuso qui dal distruttore

// Questo ha vantaggi _enormi_:
// 1. Può succedere di tutto ma
//    la risorsa (in questo caso il file handler) verrà ripulito.
//    Una volta che scrivi il distruttore correttamente,
//    È _impossibile_ scordarsi di chiudere l'handler e sprecare memoria.
// 2. Nota che il codice è molto più pulito.
//    Il distruttore gestisce la chiusura del file dietro le scene
//    senza che tu debba preoccupartene.
// 3. Il codice è sicuro da eccezioni.
//    Una eccezione può essere lanciata in qualunque punto nella funzione e la ripulitura
//    avverrà lo stesso.

// Tutto il codice C++ idiomatico usa RAII in maniera vasta su tutte le risorse.
// Esempi aggiuntivi includono
// - Utilizzo della memoria con unique_ptr e shared_ptr
// - I contenitori - la lista della libreria standard,
//   vettori (i.e. array auto-aggiustati), mappe hash, e così via
//   sono tutti automaticamente distrutti con i loro contenuti quando escono dalla visibilità.
// - I mutex usano lock_guard e unique_lock

// I contenitori che utilizzano chiavi non-primitive (classi personalizzate)
// richiedono la funzione di confronto nell'oggetto stesso, o tramite un puntatore a funzione.
// Le chiavi primitive hanno funzioni di confronto già definite, ma puoi sovrascriverle.
class Foo {
public:
    int j;
    Foo(int a) : j(a) {}
};
struct funzioneDiConfronto {
    bool operator()(const Foo& a, const Foo& b) const {
        return a.j < b.j;
    }
};
// Questo non è permesso, anche se qualche compilatore potrebbe non dare problemi
//std::map<Foo, int> fooMap;
std::map<Foo, int, funzioneDiConfronto> fooMap;
fooMap[Foo(1)]  = 1;
fooMap.find(Foo(1)); -- vero

///////////////////////////////////////
// Espressioni Lambda (C++11 e superiori)
///////////////////////////////////////

// Le espressioni lambda (più semplicemente "lambda") sono utilizzate
// per definire una funzione anonima nel punto in cui viene invocata, o
// dove viene passata come argomento ad una funzione

// Ad esempio, consideriamo l'ordinamento di un vettore costituito da una
// coppia di interi, utilizzando il secondo elemento per confrontare
vector<pair<int, int> > tester;
tester.push_back(make_pair(3, 6));
tester.push_back(make_pair(1, 9));
tester.push_back(make_pair(5, 0));

// Passiamo una lambda come terzo argomento alla funzione di ordinamento
// `sort` è contenuta nell'header <algorithm>
sort(tester.begin(), tester.end(), [](const pair<int, int>& lhs, const pair<int, int>& rhs) {
    return lhs.second < rhs.second;
});

// Nota bene la sintassi utilizzata nelle lambda:
// [] serve per "catturare" le variabili.
// La "Lista di Cattura" definisce tutte le variabili esterne che devono essere disponibili
// all'interno della funzione, e in che modo.
// La lista può contenere:
//     1. un valore: [x]
//     2. un riferimento: [&x]
//     3. qualunque variabile nello scope corrente, per riferimento [&]
//     4. qualunque variabile nello scope corrente, per valore [=]
// Esempio:

vector<int> id_cani;
// numero_cani = 3;
for(int i = 0; i < 3; i++) {
    id_cani.push_back(i);
}

int pesi[3] = {30, 50, 10};

// Mettiamo che vuoi ordinare id_cani in base al peso dei cani
// Alla fine, id_cani sarà: [2, 0, 1]

// Le lambda vengono in aiuto

sort(id_cani.begin(), id_cani.end(), [&pesi](const int &lhs, const int &rhs) {
    return pesi[lhs] < pesi[rhs];
});
// Nota come abbiamo catturato "pesi" per riferimento nell'esempio.
// Altre informazioni sulle lambda in C++: http://stackoverflow.com/questions/7627098/what-is-a-lambda-expression-in-c11

///////////////////////////////
// Ciclo For semplificato(C++11 e superiori)
///////////////////////////////

// Puoi usare un ciclo for per iterare su un tipo di dato contenitore
int arr[] = {1, 10, 3};

for(int elem: arr) {
    cout << elem << endl;
}

// Puoi usare "auto" senza preoccuparti del tipo degli elementi nel contenitore
// Ad esempio:

for(auto elem: arr) {
    // Fai qualcosa con `elem`
}

///////////////////////
// Roba divertente
//////////////////////

// Aspetti del C++ che potrebbero sbalordire i nuovi arrivati (e anche qualche veterano).
// Questa sezione è, sfortunatamente, selvaggiamente incompleta; il C++ è uno dei linguaggi
// più facili con cui puoi spararti da solo nel piede.

// Puoi sovrascrivere metodi privati!
class Foo {
  virtual void bar();
};
class FooSub : public Foo {
  virtual void bar();  // Sovrascrive Foo::bar!
};


// 0 == false == NULL (la maggior parte delle volte)!
bool* pt = new bool;
*pt = 0; // Setta il valore puntato da 'pt' come falso.
pt = 0;  // Setta 'pt' al puntatore null. Entrambe le righe vengono compilate senza warnings.

// nullptr dovrebbe risolvere alcune di quei problemi:
int* pt2 = new int;
*pt2 = nullptr; // Non compila
pt2 = nullptr;  // Setta pt2 a null.

// C'è un'eccezione per i bool.
// Questo permette di testare un puntatore a null con if(!ptr), ma
// come conseguenza non puoi assegnare nullptr a un bool direttamente!
*pt = nullptr;  // Questo compila, anche se '*pt' è un bool!


// '=' != '=' != '='!
// Chiama Foo::Foo(const Foo&) o qualche variante (vedi "move semantics")
// del costruttore di copia.
Foo f2;
Foo f1 = f2;

// Chiama Foo::Foo(const Foo&) o qualche variante, ma solo copie di 'Foo' che fanno parte di
// 'fooSub'. Ogni altro membro di 'fooSub' viene scartato. Questo comportamento
// orribile viene chiamato "object slicing."
FooSub fooSub;
Foo f1 = fooSub;

// Chiama Foo::operator=(Foo&) o una sua variante.
Foo f1;
f1 = f2;


///////////////////////////////////////
// Tuple (C++11 e superiori)
///////////////////////////////////////

#include<tuple>

// Concettualmente le tuple sono simili alle strutture del C, ma invece di avere
// i membri rappresentati con dei nomi, l'accesso agli elementi avviene tramite
// il loro ordine all'interno della tupla.

// Cominciamo costruendo una tupla.
// Inserire i valori in una tupla
auto prima = make_tuple(10, 'A');
const int maxN = 1e9;
const int maxL = 15;
auto seconda = make_tuple(maxN, maxL);

// Vediamo gli elementi contenuti nella tupla "prima"
cout << get<0>(prima) << " " << get<1>(prima) << "\n"; // stampa : 10 A

// Vediamo gli elementi contenuti nella tupla "seconda"
cout << get<0>(seconda) << " " << get<1>(seconda) << "\n"; // stampa: 1000000000 15

// Estrarre i valori dalla tupla, salvandoli nelle variabili
int primo_intero;
char primo_char;
tie(primo_intero, primo_char) = prima;
cout << primo_intero << " " << primo_char << "\n";  // stampa : 10 A

// E' possibile creare tuple anche in questo modo
tuple<int, char, double> terza(11, 'A', 3.14141);

// tuple_size ritorna il numero di elementi in una tupla (come constexpr)
cout << tuple_size<decltype(terza)>::value << "\n"; // stampa: 3

// tuple_cat concatena gli elementi di tutte le tuple, nell'esatto ordine
// in cui sono posizionati all'interno delle tuple stesse
auto tupla_concatenata = tuple_cat(prima, seconda, terza);
// tupla_concatenata diventa = (10, 'A', 1e9, 15, 11, 'A' ,3.14141)

cout << get<0>(tupla_concatenata) << "\n"; // stampa: 10
cout << get<3>(tupla_concatenata) << "\n"; // stampa: 15
cout << get<5>(tupla_concatenata) << "\n"; // stampa: 'A'


/////////////////////
// Contenitori
/////////////////////

// I Contenitori della "Standard Template Library", ovvero la libreria standard
// dei template contenuti nel C++, sono template predefiniti.
// I Contenitori si occupano di come allocare lo spazio per gli elementi contenuti,
// e forniscono funzioni per accedervi e manipolarli

// Vediamo alcuni tipi di contenitori:

// Vector (array dinamici/vettori)
// Permettono di definire un vettore, o una lista di oggetti, a runtime
#include<vector>
vector<Tipo_Dato> nome_vettore; // usato per inizializzare un vettore
cin >> val;
nome_vettore.push_back(val); // inserisce il valore di "val" nel vettore

// Per iterare in un vettore, abbiamo due possibilità:
// Ciclo normale
for(int i=0; i<nome_vettore.size(); i++)
// Cicla dall'indice zero fino all'ultimo

// Iteratore
vector<Tipo_Dato>::iterator it; // inizializza l'iteratore per il vettore
for(it=nome_vettore.begin(); it!=nome_vettore.end();++it)
// Nota che adesso non cicla più sugli indici, ma direttamente sugli elementi!

// Per accedere agli elementi del vettore
// Operatore []
var = nome_vettore[indice]; // Assegna a "var" il valore del vettore all'indice dato


// Set (insiemi)
// Gli insiemi sono contenitori che memorizzano elementi secondo uno specifico ordine.
// Gli insiemi vengono per lo più utilizzati per memorizzare valori unici, secondo
// un ordine, senza scrivere ulteriore codice.

#include<set>
set<int> insieme;    // Inizializza un insieme di interi
insieme.insert(30);  // Inserisce il valore 30 nell'insieme
insieme.insert(10);  // Inserisce il valore 10 nell'insieme
insieme.insert(20);  // Inserisce il valore 20 nell'insieme
insieme.insert(30);  // Inserisce il valore 30 nell'insieme
// Gli elementi dell'insieme sono:
//  10 20 30

// Per cancellare un elemento
insieme.erase(20);  // Cancella l'elemento con valore 20
// L'insieme contiene adesso: 10 30

// Per iterare su un insieme, usiamo gli iteratori
set<int>::iterator it;
for(it=insieme.begin();it<insieme.end();it++) {
    cout << *it << endl;
}
// Stampa:
// 10
// 30

// Per svuotare il contenitore usiamo il metodo "clear"
insieme.clear();
cout << insieme.size();
// Stampa: 0

// Nota: per permettere elementi duplicati, possiamo usare "multiset"

// Map (mappa/tabella di hash)
// Le mappe servono per memorizzare un elemento, detto chiave, a cui viene
// associato un valore, il tutto secondo uno specifico ordine.

#include<map>
map<char, int> mia_mappa;  // Inizializza una mappa che usa i char come chiave, e gli interi come valore

mia_mappa.insert(pair<char,int>('A',1));
// Inserisce il valore 1 per la chiave A
mia_mappa.insert(pair<char,int>('Z',26));
// Inserisce il valore 26 per la chiave Z

// Per iterare
map<char,int>::iterator it;
for (it=mia_mappa.begin(); it!=mia_mappa.end(); ++it)
    std::cout << it->first << "->" << it->second << '\n';
// Stampa:
// A->1
// Z->26

// Per trovare il valore corrispondente ad una data chiave
it = mia_mappa.find('Z');
cout << it->second;
// Stampa: 26


///////////////////////////////////
// Operatori logici e bitwise(bit-a-bit)
//////////////////////////////////

// La maggior parte di questi operatori in C++ sono gli stessi degli altri linguaggi

// Operatori logici

// Il C++ usa la "Short-circuit evaluation" per le espressioni booleane. Cosa significa?
// In pratica, in una condizione con due argomenti, il secondo viene considerato solo se
// il primo non basta a determinate il valore finale dell'espresione.

true && false // Effettua il **and logico** e ritorna falso
true || false // Effettua il **or logico** e ritorna vero
! true        // Effettua il **not logico** e ritorna falso

// Invece di usare i simboli, si possono usare le keyword equivalenti
true and false // Effettua il **and logico** e ritorna falso
true or false  // Effettua il **or logico** e ritorna vero
not true       // Effettua il **not logico** e ritorna falso

// Operatori bitwise(bit-a-bit)

// **<<** Operatore di Shift a Sinistra
// << sposta i bit a sinistra
4 << 1 // Sposta a sinistra di 1 i bit di 4, ottenendo 8
// x << n in pratica realizza x * 2^n


// **>>** Operatore di Shift a Destra
// >> sposta i bit a destra
4 >> 1 // Sposta a destra di 1 i bit di 4, ottenendo 2
// x >> n in pratica realizza x / 2^n

~4    // Effettua il NOT bit-a-bit
4 | 3 // Effettua il OR bit-a-bit
4 & 3 // Effettua il AND bit-a-bit
4 ^ 3 // Effettua il XOR bit-a-bit

// Le keyword equivalenti sono
compl 4    // Effettua il NOT bit-a-bit
4 bitor 3  // Effettua il OR bit-a-bit
4 bitand 3 // Effettua il AND bit-a-bit
4 xor 3    // Effettua il XOR bit-a-bit

```
Letture consigliate:

Un riferimento aggiornato del linguaggio può essere trovato qui
<http://cppreference.com/w/cpp>

Risorse addizionali possono essere trovate qui <http://cplusplus.com>
