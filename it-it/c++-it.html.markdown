---
language: c++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
translators:
    - ["Robert Margelli", "http://github.com/sinkswim/"]
lang: it-it
---

Il C++ e' un linguaggio di programmazione il quale,
[secondo il suo inventore Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),
e' stato progettato per

- essere un "miglior C"
- supportare l'astrazione dei dati
- supportare la programmazione orientata agli oggetti
- supportare la programmazione generica

Nonostante la sintassi possa risultare piu' difficile o complessa di linguaggi piu' recenti,
e' usato in maniera vasta poiche' viene compilato in istruzioni macchina che possono
essere eseguite direttamente dal processore ed offre un controllo stretto sull'hardware (come il linguaggio C)
ed allo stesso tempo offre caratteristiche ad alto livello come i generici, le eccezioni, e le classi.
Questa combinazione di velocita' e funzionalita' rende il C++
uno dei piu' utilizzati linguaggi di programmazione.

```c++
//////////////////
// Confronto con il C
//////////////////

// Il C++ e' _quasi_ un superset del C e con esso condivide la sintassi di base per
// la dichiarazione di variabili, tipi primitivi, e funzioni.

// Proprio come nel C, l'inizio del programma e' una funzione chiamata
// main con un intero come tipo di ritorno,
// nonostante void main() sia anch'essa accettata dalla maggior parte dei compilatori(gcc, clang, ecc...)
// Questo valore serve come stato d'uscita del programma.
// Vedi http://it.wikipedia.org/wiki/Valore_di_uscita per maggiori informazioni.
int main(int argc, char** argv)
{
    // Gli argomenti a linea di comando sono passati tramite argc e argv cosi' come
    // avviene in C.
    // argc indica il numero di argomenti,
    // e argv e' un array in stile-C di stringhe (char*)
    // che rappresenta gl iargomenti.
    // Il primo argomento e' il nome che e' stato assegnato al programma.
    // argc e argv possono essere omessi se non hai bisogno di argomenti,
    // in questa maniera la funzione avra' int main() come firma.

    // Lo stato di uscita 0 indica successo.
    return 0;
}

// Tuttavia, il C++ varia nei seguenti modi:

// In C++, i caratteri come letterali sono da un byte.
sizeof('c') == 1

// In C, i caratteri come letterali sono della stessa dimensione degli interi.
sizeof('c') == sizeof(10)


// C++ ha prototipizzazione rigida
void func(); // funziona che non accetta argomenti

// In C
void func(); // funzione che puo' accettare un qualsiasi numero di argomenti

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
    printf("Il mio int e' %d", myInt);
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

// I namespaces forniscono visibilita' separata per dichiarazioni di variabili, funzioni,
// ed altro.
// I namespaces possono essere annidati.

namespace Primo {
    namespace Annidato {
        void foo()
        {
            printf("Questa e' Primo::Annidato::foo\n");
        }
    } // fine di namespace Annidato
} // fine di namespace Primo

namespace Secondo {
    void foo()
    {
        printf("Questa e' Secondo::foo\n")
    }
}

void foo()
{
    printf("Questa e' foo globale\n");
}

int main()
{
    // Assume che tutto venga dal namespace "Secondo"
    // a meno che non venga dichiarato altrimenti.
    using namespace Secondo;

    foo(); // stampa "Questa e' Secondo::foo"
    Primo::Annidato::foo(); // stampa "Questa e' Primo::Annidato::foo"
    ::foo(); // stampa "Questa e' foo globale"
}

///////////////
// Input/Output
///////////////

// L'input e l'output in C++ utilizza gli streams
// cin, cout, e cerr i quali rappresentano stdin, stdout, e stderr.
// << e' l'operatore di inserzione >> e' l'operatore di estrazione.

#include <iostream> // Include for I/O streams

using namespace std; // Gli streams sono nel namespace std (libreria standard)

int main()
{
   int myInt;

   // Stampa su stdout (o terminalee/schermo)
   cout << "Inserisci il tuo numero preferito:\n";
   // Prende l'input
   cin >> myInt;

   // cout puo' anche essere formattato
   cout << "Il tuo numero preferito e' " << myInt << "\n";
   // stampa "Il tuo numero preferito e' <myInt>"

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

// + e' usato per la concatenazione.
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
// * non e' necessario per la dereferenziazione e
// & ("indirizzo di") non e' usato per l'assegnamento.

using namespace std;

string foo = "Io sono foo";
string bar = "Io sono bar";


string& fooRef = foo; // Questo crea un riferimento a foo.
fooRef += ". Ciao!"; // Modifica foo attraverso il riferimento
cout << fooRef; // Stampa "Io sono foo. Ciao!"

// Non riassegna "fooRef". Questo e' come scrivere "foo = bar", e
//   foo == "Io sono bar"
// dopo questa riga.
fooRef = bar;

const string& barRef = bar; // Crea un riferimento const a bar.
// Come in C, i valori const (i puntatori e i riferimenti) non possono essere modificati.
barRef += ". Ciao!"; // Errore, i riferimenti const non possono essere modificati.

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
// finche' "private:" o "protected:" non compaiono.
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
    // Questi sono chiamati quando un oggetto e' rimosso o esce dalla visibilita'.
    // Questo permette paradigmi potenti come il RAII
    // (vedi sotto)
    // I distruttori devono essere virtual per permettere a classi di essere derivate da questa.
    virtual ~Dog();

}; // Un punto e virgola deve seguire la definizione della funzione

// Class member functions are usually implemented in .cpp files.
void Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// Objects (such as strings) should be passed by reference
// if you are modifying them or const reference if you are not.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Notice that "virtual" is only needed in the declaration, not the definition.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

void Dog::~Dog()
{
    cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // prints "A dog has been constructed"
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.printDog(); // prints "Dog is Barkley and weighs 10 kg"
    return 0;
} // prints "Goodbye Barkley"

// Inheritance:

// This class inherits everything public and protected from the Dog class
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner)

    // Override the behavior of the print function for all OwnedDogs. See
    // http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping
    // for a more general introduction if you are unfamiliar with
    // subtype polymorphism.
    // The override keyword is optional but makes sure you are actually
    // overriding the method in a base class.
    void print() const override;

private:
    std::string owner;
};

// Meanwhile, in the corresponding .cpp file:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Call the print function in the base Dog class
    std::cout << "Dog is owned by " << owner << "\n";
    // Prints "Dog is <name> and weights <weight>"
    //        "Dog is owned by <owner>"
}

//////////////////////////////////////////
// Initialization and Operator Overloading
//////////////////////////////////////////

// In C++ you can overload the behavior of operators such as +, -, *, /, etc.
// This is done by defining a function which is called
// whenever the operator is used.

#include <iostream>
using namespace std;

class Point {
public:
    // Member variables can be given default values in this manner.
    double x = 0;
    double y = 0;

    // Define a default constructor which does nothing
    // but initialize the Point to the default value (0, 0)
    Point() { };

    // The following syntax is known as an initialization list
    // and is the proper way to initialize class member values
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Do nothing except initialize the values */ }

    // Overload the + operator.
    Point operator+(const Point& rhs) const;

    // Overload the += operator
    Point& operator+=(const Point& rhs);

    // It would also make sense to add the - and -= operators,
    // but we will skip those for brevity.
};

Point Point::operator+(const Point& rhs) const
{
    // Create a new point that is the sum of this one and rhs.
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // This calls the Point + operator
    // Point up calls the + (function) with right as its paramater
    Point result = up + right;
    // Prints "Result is upright (1,1)"
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

////////////////////////////
// Gestione delle eccezioni
///////////////////////////

// La libreria standard fornisce un paio di tipi d'eccezioni
// (vedi http://en.cppreference.com/w/cpp/error/exception)
// ma ogni tipo puo' essere lanciato come eccezione
#include <exception>

// Tutte le eccezioni lanciate all'interno del blocco _try_ possono essere catturate dai successivi 
// handlers _catch_.
try {
    // Non allocare eccezioni nello heap usando _new_.
    throw std::exception("E' avvenuto un problema");
}
// Cattura le eccezioni come riferimenti const se sono oggetti
catch (const std::exception& ex)
{
  std::cout << ex.what();
// Cattura ogni eccezioni non catturata dal blocco _catch_ precedente
} catch (...)
{
    std::cout << "Catturata un'eccezione sconosciuta";
    throw; // Rilancia l'eccezione
}

///////
// RAII
///////

// RAII sta per Resource Allocation Is Initialization.
// Spesso viene considerato come il piu' potente paradigma in C++.
// E' un concetto semplice: un costruttore di un oggetto
// acquisisce le risorse di tale oggetto ed il distruttore le rilascia.

// Per comprendere come questo sia vantaggioso,
// consideriamo una funzione che usa un gestore di file in C:
void faiQualcosaConUnFile(const char* nomefile)
{
    // Per cominciare, assumiamo che niente possa fallire.

    FILE* fh = fopen(nomefile, "r"); // Apri il file in modalita' lettura.

    faiQualcosaConUnFile(fh);
    faiQualcosAltroConEsso(fh);

    fclose(fh); // Chiudi il gestore di file.
}

// Unfortunately, things are quickly complicated by error handling.
// Suppose fopen can fail, and that doSomethingWithTheFile and
// doSomethingElseWithIt return error codes if they fail.
// (Exceptions are the preferred way of handling failure,
//  but some programmers, especially those with a C background,
//  disagree on the utility of exceptions).
// We now have to check each call for failure and close the file handle
// if a problem occurred.
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Open the file in read mode
    if (fh == nullptr) // The returned pointer is null on failure.
        return false; // Report that failure to the caller.

    // Assume each function returns false if it failed
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // Close the file handle so it doesn't leak.
        return false; // Propagate the error.
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // Close the file handle so it doesn't leak.
        return false; // Propagate the error.
    }

    fclose(fh); // Close the file handle so it doesn't leak.
    return true; // Indicate success
}

// C programmers often clean this up a little bit using goto:
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // Close the file
    return true; // Indicate success

failure:
    fclose(fh);
    return false; // Propagate the error
}

// If the functions indicate errors using exceptions,
// things are a little cleaner, but still sub-optimal.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Open the file in read mode
    if (fh == nullptr)
        throw std::exception("Could not open the file.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // Be sure to close the file if an error occurs.
        throw; // Then re-throw the exception.
    }

    fclose(fh); // Close the file
    // Everything succeeded
}

// Compare this to the use of C++'s file stream class (fstream)
// fstream uses its destructor to close the file.
// Recall from above that destructors are automatically called
// whenever an object falls out of scope.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream is short for input file stream
    std::ifstream fh(filename); // Open the file

    // Do things with the file
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // The file is automatically closed here by the destructor

// This has _massive_ advantages:
// 1. No matter what happens,
//    the resource (in this case the file handle) will be cleaned up.
//    Once you write the destructor correctly,
//    It is _impossible_ to forget to close the handle and leak the resource.
// 2. Note that the code is much cleaner.
//    The destructor handles closing the file behind the scenes
//    without you having to worry about it.
// 3. The code is exception safe.
//    An exception can be thrown anywhere in the function and cleanup
//    will still occur.

// All idiomatic C++ code uses RAII extensively for all resources.
// Additional examples include
// - Memory using unique_ptr and shared_ptr
// - Containers - the standard library linked list,
//   vector (i.e. self-resizing array), hash maps, and so on
//   all automatically destroy their contents when they fall out of scope.
// - Mutexes using lock_guard and unique_lock
```
Letture consigliate:

Un riferimento aggiornato del linguaggio puo' essere trovato qui
<http://cppreference.com/w/cpp>

Risorse addizionali possono essere trovate qui <http://cplusplus.com>
