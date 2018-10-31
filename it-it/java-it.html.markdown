---
language: java
filename: LearnJava-it.java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Madison Dickson", "http://github.com/mix3d"]
translators:
    - ["Ivan Sala","http://github.com/slavni96"]
    - ["Tommaso Pifferi","http://github.com/neslinesli93"]
lang: it-it
---

Java è un linguaggio di programmazione orientato ad oggetti,
concorrente, basato su classi e adatto a svariati scopi. 
[Per saperne di più](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// I commenti su singola linea incominciano con //
/*
I commenti su più linee invece sono così
*/
/**
I commenti per la documentazione JavaDoc si fanno così.
Vengono usati per descrivere una classe o alcuni suoi attributi.
*/

// Per importare la classe ArrayList contenuta nel package java.util
import java.util.ArrayList;
// Per importare tutte le classi contenute nel package java.security
import java.security.*;

// Ogni file .java contiene una classe pubblica, con lo stesso nome del file
public class LearnJava {

    // Un programma deve avere un metodo main come punto di partenza.
    // Tuttavia si possono creare anche file senza main, che però
    // per essere usati devono essere richiamati da altri file.
    public static void main (String[] args) {

        // Per stampare a schermo si usa System.out.println
        System.out.println("Ciao Mondo!");
        System.out.println(
            "Intero [integer]: " + 10 +
            " Reale [double]: " + 3.14 +
            " Booleano [boolean]: " + true);

        // Se non si vuole andare a capo, si puo' usare System.out.print
        System.out.print("Ciao ");
        System.out.print("Mondo ");

        // Per stampare del testo formattato, si può usare System.out.printf
        System.out.printf("pi greco = %.5f", Math.PI); // => pi greco = 3.14159

        ///////////////////////////////////////
        // Variabili
        ///////////////////////////////////////

        /*
        *  Dichiarazione delle Variabili
        */
        // Per dichiarare una variabile basta fare <tipoDato> <nomeVariabile>
        int fooInt;
        // Per dichiarare più di una variabile dello lo stesso tipo si usa:
        // <tipoDato> <nomeVariabile1>, <nomeVariabile2>, <nomeVariabile3>
        int fooInt1, fooInt2, fooInt3;

        /*
        *  Inizializzazione delle Variabili
        */

        // Per inizializzare una variabile si usa
        // <tipoDato> <nomeVariabile> = <valore>
        int fooInt = 1;
        // Per inizializzare più di una variabile dello lo stesso tipo
        // si usa <tipoDato> <nomeVariabile1>, <nomeVariabile2>, <nomeVariabile3> = <valore>
        int fooInt1, fooInt2, fooInt3;
        fooInt1 = fooInt2 = fooInt3 = 1;

        /*
        *  Tipi di Variabili
        */
        // Byte - intero con segno a 8 bit (in complemento a 2)
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - intero con segno a 16 bit (in complemento a 2)
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - intero con segno a 32 bit (in complemento a 2)
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - intero con segno a 64 bit (in complemento a 2)
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L viene usato per indicare che il valore è di tipo Long;
        // altrimenti il valore viene considerato come intero.

        // Nota: Java non dispone di interi senza segno.

        // Float - Numero in virgola mobile a 32 bit con precisione singola (IEEE 754)
        // 2^-149 <= float <= (2-2^-23) * 2^127
        float fooFloat = 234.5f;
        // f o F indicano the la variabile è di tipo float;
        // altrimenti il valore viene considerato come double.

        // Double - Numero in virgola mobile a 64 bit con precisione doppia (IEEE 754)
        // 2^-1074 <= x <= (2-2^-52) * 2^1023
        double fooDouble = 123.4;

        // Boolean - Può assumere il valore vero (true) o falso (false)
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - Un singolo carattere Unicode a 16-bit
        char fooChar = 'A';

        // Le variabili precedute da final possono essere inizializzate una volta sola,
        final int HOURS_I_WORK_PER_WEEK = 9001;
        // però è possibile dichiararle e poi inizializzarle in un secondo momento.
        final double E;
        E = 2.71828;


        // BigInteger - Interi a precisione arbitraria
        //
        // BigInteger è un tipo di dato che permette ai programmatori di
        // gestire interi più grandi di 64 bit. Internamente, le variabili
        // di tipo  BigInteger vengono memorizzate come un vettore di byte e 
        // vengono manipolate usando funzioni dentro la classe BigInteger.
        //
        // Una variabile di tipo BigInteger può essere inizializzata usando
        // un array di byte oppure una stringa.
        
        BigInteger fooBigInteger = new BigDecimal(fooByteArray);

        // BigDecimal - Numero con segno, immutabile, a precisione arbitraria
        //
        // Una variabile di tipo BigDecimal è composta da due parti: un intero
        // a precisione arbitraria detto 'non scalato', e un intero a 32 bit
        // che rappresenta la 'scala', ovvero la potenza di 10 con cui
        // moltiplicare l'intero non scalato.
        //
        // I BigDecimal permettono un controllo completo sull'arrotondamento
        // dei numeri. Essi sono molto usati in ambito finanziario, nella
        // gestione delle valute, e in ogni altro posto in cui serve
        // precisione esatta.
        //
        // Le variabili di tipo BigDecimal possono essere inizializzate con un
        // int, long, double o String, oppure con un intero non scalato
        // (di tipo BigInteger) e una scala (int).

        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, fooInt);



        // Stringhe
        String fooString = "Questa e' la mia stringa!";

        // \n è un carattere di escape che rappresenta l'andare a capo
        String barString = "Stampare su una nuova riga?\nNessun problema!";
        // \t è un carattere di escape che aggiunge un tab
        String bazString = "Vuoi aggiungere un tab?\tNessun problema!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Vettori
        // La dimensione di un array deve essere decisa in fase di
        // istanziazione. Per dichiarare un array si può fare in due modi:
        // <tipoDato>[] <nomeVariabile> = new <tipoDato>[<dimensioneArray>];
        // <tipoDato> <nomeVariabile>[] = new <tipoDato>[<dimensioneArray>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Un altro modo per dichiarare ed insieme inizializzare un vettore.
        int[] y = {9000, 1000, 1337};
        String names[] = {"Gianni", "Anna", "Luca", "Cristina"};
        boolean bools[] = new boolean[] {true, false, false};

        // Per accedere ad un elemento di un vettore
        System.out.println("intArray @ 0: " + intArray[0]);

        // I vettori non sono immutabili (ma la loro dimensione si!)
        // e gli indici partono da 0.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Ci sono altri tipo di dato interessanti.
        // ArrayList - Simili ai vettori, però offrono altre funzionalità,
        //             e la loro dimensione può essere modificata.
        // LinkedList - Si tratta di una lista linkata doppia, e come tale
        //              implementa tutte le operazioni del caso.
        // Map - Un insieme di oggetti che fa corrispondere delle chiavi
        //       a dei valori. Non permette l'inserimento di chiavi uguali.
        // HashMap - Questa classe usa una tabella di hash per implementare
        //           l'interfaccia di tipo Map. Questo permette di effettuare
        //           operazioni basilari, come inserimento e cancellazione,
        //           in tempo costante anche su insiemi molto grandi.

        ///////////////////////////////////////
        // Operatori
        ///////////////////////////////////////
        System.out.println("\n->Operatori");

        int i1 = 1, i2 = 2; // Dichiarazone multipla in contemporanea

        // L'aritmetica è lineare.
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 
            // (con 0.5 arrotonda per difetto)

        // Modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Operatori di confronto
        System.out.println("3 == 2? " + (3 == 2)); // => falso
        System.out.println("3 != 2? " + (3 != 2)); // => vero
        System.out.println("3 > 2? " + (3 > 2)); // => vero
        System.out.println("3 < 2? " + (3 < 2)); // => falso
        System.out.println("2 <= 2? " + (2 <= 2)); // => vero
        System.out.println("2 >= 2? " + (2 >= 2)); // => vero

        // Operatori binari orientati ai bit
        // effettuano le operazioni logiche confrontando, i bit degli operandi:
        /*
        ~       complemento 
        <<      shift sinistro con segno
        >>      shift destro con segno
        >>>     shift destro senza segno
        &       AND Binario Bitwise AND
        ^       OR Esclusivo
        |       OR Incusivo
        */

        // Incrementare e Decrementare
        int i = 0;
        System.out.println("\n->Incrementare/Decrementare");
        // Gli operatori ++ e -- incrementano e decrementano rispettivamente di 1.
        // Se posizionati prima della variabile, incrementano, quindi riportano.
        // Se si trovano dopo la variabile, riporano, e quindi incrementano. 
        System.out.println(i++); //i = 1, Stampa 0 (post-incremento)
        System.out.println(++i); //i = 2, Stampa 2 (pre-incremento)
        System.out.println(i--); //i = 1, Stampa 2 (post-decremento)
        System.out.println(--i); //i = 0, Stampa 0 (pre-decremento)

        ///////////////////////////////////////
        // Strutture di controllo
        ///////////////////////////////////////
        System.out.println("\n->Strutture di controllo");

        // La dichiarazione dell'If è C-like.
        int j = 10;
        if (j == 10){
            System.out.println("Io vengo stampato");
        } else if (j > 10) {
            System.out.println("Io no");
        } else {
            System.out.println("E io neppure");
        }

        // Struttura While
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            //System.out.println(fooWhile);
            //Incrementa il contatore
            //Si ripete per 100 volte, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("Valore di fooWhile: " + fooWhile);

        // Struttura Do While
        int fooDoWhile = 0;
        do
        {
            //System.out.println(fooDoWhile);
            //Incrementa il contaore
            //Si repete per 99 volte, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("Valore di fooWhile: " + fooDoWhile);

        // Struttura For
        int fooFor;
        //Struttura For => for(<Situazione iniziale>; <Condizione>; <passo>)
        for(fooFor=0; fooFor<10; fooFor++){
            //System.out.println(fooFor);
            //Itera 10 volte, fooFor 0->9
        }
        System.out.println("Valore di fooFor: " + fooFor);

        // Struttura For Each
        // Una iterazione automatica attraverso un array o una lista di oggetti
        int[] fooList = {1,2,3,4,5,6,7,8,9};
        //struttura for each => for(<oggetto> : <oggetto dell'attay>)
        //si legge: per ogni oggetto dell'array fai...
        //Nota: il tipo dell'oggetto deve essere uguale a quello dell'array

        for( int bar : fooList ){
            //System.out.println(bar);
            //Itera 9 volte e stampa 1-9 andando a capo.
        }

        // Struttura Switch Case
        // La struttura switch lavora con byte, short, char e int.
        // Se funziona con i char funzionera ovviamente anche con le stringhe.
        int mese = 3;
        String stringaMese;
        switch (mese){
            case 1:
                    stringaMese = "Genneio";
                    break;
            case 2:
                    stringaMese = "Febbraio";
                    break;
            case 3:
                    stringaMese = "Marzo";
                    break;
            default:
                    stringaMese = "Altri mesi";
                    break;
        }
        System.out.println("Risultato del costrutto switch: " + stringaMese);

        // Condizioni brevi
        // Si può usare l'operatore '?' per un rapido assegnamento
        // o per operazioni logiche.
        // Si legge: 
        // Se (condizione) è vera, usa <primo valore>, altrimenti usa <secondo valore>
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println("Se la condizione e' vera stampa A: "+bar); 
        // Stampa A, perché la condizione è vera.


        /////////////////////////////////////////
        // Convertire i tipi di dati e Typecasting
        /////////////////////////////////////////

        // Convertire tipi di dati

        // Stringhe ad interi
        Integer.parseInt("123");//Riporta una versione intera di "123"

        // Interi a Stringhe
        Integer.toString(123);//Riporta la stringa "123"
        // Per altre conversioni guarda le seguenti classi
        // Double
        // Long
        // String

        // Typecasting
        // Vi sono molti dettagli che non si possono spiegare qui, 
        // java dispone di una ottima documentazione
        // Sentiti libero di leggerla
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Classi e funzioni
        ///////////////////////////////////////

        System.out.println("\n->Classi & Funzioni");

        // (Di seguito la definizione della classe Bicicletta)

        // Instanziare una nuova classe
        Bicicletta percorso = new Bicicletta();

        // Chiamare metodi
        percorso.accellera(3); // Si usano sempre metodi set... get...
        percorso.setCadenza(100);

        // toString riporta la rappresenzazione dell'oggetto 
        // come se fosse una stringa
        System.out.println("percorso info: " + percorso.toString());

    } // Fine metodo main
} // Fine classe LearnJava


// Si possono inculdere altre anche delle classi non pubbliche (private)
// oltre a quella pubblica principale, in un file .java

// Sintassi per dichiarare una classe:
// <public/private/protected> class <Nome classe>{
//    //dati, variabili, costruttori, funzioni, tutto qua.
//    //le funzioni sono chiamate come i metodi.
// }

class Bicicletta {

    // Variabili della bicicletta
    public int cadenza; 
      // Public: Può essere richiamato da qualsiasi classe
    private int velocita; 
      // Private: è accessibile solo dalla classe dove è stato inizializzato
    protected int ingranaggi; 
      // Protected: è visto sia dalla classe che dalle sottoclassi
    String nome; 
      // default: è accessibile sono all'interno dello stesso package

    // I costruttori vengono usati per creare variabili
    // Questo è un costruttore
    public Bicicletta() {
        ingranaggi = 1;
        cadenza = 50;
        velocita = 5;
        nome = "Bontrager";
    }

    // Questo è un costruttore che richiede parametri
    public Bicicletta(int cadenza, int velocita, int ingranaggi, String nome) {
        this.ingranaggi = ingranaggi;
        this.cadenza = cadenza;
        this.velocita = velocita;
        this.nome = nome;
    }

    // Sintassi delle funzioni:
    // <public/private/protected> <tipo di ritorino> <nome della funzione>(<parametri>)

    // Le classi in java spesso implementano delle funzioni o metodo
    // 'get...' o 'set...' 

    // Dichiarazione di un metodo
    // <scope> <tipo di ritorno> <nome del metodo>(<parametri>)
    public int getCandenza() {
        return cadenza;
    }

    // i medodi (void) non necessitano di riportare un valore
    public void setCadenza(int nuovoValore) {
        cadenza = nuovoValore;
    }

    public void setIngranaggi(int nuovoValore) {
        ingranaggi = nuovoValore;
    }

    public void accellera(int incrementa) {
        velocita += incrementa;
    }

    public void decellera(int decrementa) {
        velocita -= decrementa;
    }

    public void setNome(String nuovoNome) {
        nome = nuovoNome;
    }

    public String getNome() {
        return nome;
    }

    //Medoto per visualizzare gli attributi dell'oggetto
    @Override
    public String toString() {
        return "Ingranaggi: " + ingranaggi +
                " Cadenza: " + cadenza +
                " Velocita: " + velocita +
                " Nome: " + nome;
    }
} // Fine classe bicicletta

// PennyFarthing è una sottoclasse della bicicletta
class PennyFarthing extends Bicicletta {
    // (Sono quelle biciclette con un unica ruota enorme
    // Non hanno ingranaggi.)

    public PennyFarthing(int cadenzaIniziale, int velocitaIniziale){
        // Richiamo il costruttore del padre con super
        super(cadenzaIniziale, velocitaIniziale, 0, "PennyFarthing");
    }

    // Bisogna contrassegnre un medodo che si sta riscrivendo 
    // con una @annotazione
    // Per saperne di più sulle annotazioni
    // Vedi la guida: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setIngranaggi(int ingranaggi) {
        ingranaggi = 0;
    }

}
/*
//Interfacce
//Sintassi per dichiarare una interfaccia
//<livello di accesso> interface <nome dell'interfaccia> extends <super-interfaccia> {
//    	//Costanti
//		//Dichiarazioni dei metodi
//}

//Esempi- Cibo:
interface Commestibile {
	public void mangia(); 
        //Ogni classe che implementa questa interfaccia
        //deve implementare questo metodo.
        }
interface Digeribile {
	public void digerisci();
}

//Possiamo quindi creare una classe che implementa entrambe le interfaccie
class Frutta implements Commestibile, Digestibile {
	public void mangia() {
		//...
	}

	public void digerisci() {
		//... 
	}
}

//In Java si può estendere solo una classe, ma si possono implementare 
//più interfaccie, per esempio:
class ClasseEsempio extends AltraClasse implements PrimaInterfaccia, SecondaInterfaccia {
	public void MetodoPrimaInterfaccia() {

	}

	public void MetodoSecondaInterfaccia() {

	}
}
*/
```
## Letture future

I link di seguito sono solo per capire l'argomento, cerca pure su Google degli esempi specifici


**Guida ufficiale di Oracle [solo in inglese]**:

* [Java Tutorial Trail from Sun / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconv-138413.html)


**Tutorial Online [in inglese]**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Libri [in italiano]** :

* [Java la guida completa](http://www.amazon.it/Java-guida-completa-Herbert-Schildt/dp/8838667667/ref=sr_1_1?ie=UTF8&qid=1393422296&sr=8-1&keywords=java)

* [Thinking in java](http://www.amazon.it/Thinking-Java-1-Bruce-Eckel/dp/8871923030/ref=sr_1_8?ie=UTF8&qid=1393422296&sr=8-8&keywords=java)

* [Manuale di Java 7](http://www.amazon.com/gp/product/0071606300)
