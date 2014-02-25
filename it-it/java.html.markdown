---

language: java
filename: LearnJava.java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Madison Dickson", "http://github.com/mix3d"]
translators:
    - ["Ivan Sala","http://github.com/dev-sala"]
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

// Per importare la classe ArrayList conenuta nel package java.util
import java.util.ArrayList;
// Per importare tutte le classi contenute nel package java.security
import java.security.*;

// Ogni filce .java contiene una classe pubblica, con lo stesso nome del file
public class LearnJava {

    // Un programma deve avere un metodo main come punto di partenza
    public static void main (String[] args) {

        // Per stampare a schermo si usa System.out.println
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Se non si vuole andare a capo, si può usare System.out.print
        System.out.print("Hello ");
        System.out.print("World");


        ///////////////////////////////////////
        // Tipi e Variabili
        ///////////////////////////////////////
        // Si dichiara una variabile usando <tipo> <nome>
        // Byte - 8-bit signed two's complement integer
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - 16-bit signed two's complement integer
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 32-bit signed two's complement integer
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - 64-bit signed two's complement integer
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L is used to denote that this variable value is of type Long;
        // anything without is treated as integer by default.

        // Nota: Java non dispone di variabili senza segno

        // Float - Single-precision 32-bit IEEE 754 Floating Point
        float fooFloat = 234.5f;
        // f is used to denote that this variable value is of type float;
        // otherwise it is treated as double.

        // Double - Double-precision 64-bit IEEE 754 Floating Point
        double fooDouble = 123.4;

        // Boolean - true & false
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - A single 16-bit Unicode character
        char fooChar = 'A';

        // final - Costanti, non possono essere riassegnate ad un altro oggetto
        final int HOURS_I_WORK_PER_WEEK = 9001;

        // Stringhe
        String fooString = "My String Is Here!";

        // \n è un carattere speciale che permette di andare a capo.
        String barString = "Printing on a new line?\nNo Problem!";
        // \t è un carattere speciale che permette di aggiungere un 'Tab'
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Vettori
        //La lunghezza del vettore deve essere decisa quando viene istanziato
        //Si può dichiarare come segue:
        //<tipodato> [] <nomevariabile> = new <tipodato>[<grandezza vettore>];
        //<tipodato> <nomevariabile>[] = new <tipodato>[<grandezza vettore>];
        int [] intArray = new int[10];
        String [] stringArray = new String[1];
        boolean boolArray [] = new boolean[100];

        // Un altro modo per dichiarare & inizializzare un vettore
        int [] y = {9000, 1000, 1337};
        String names [] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = new boolean[] {true, false, false};

        // Accesso diretto ad un elemento
        System.out.println("intArray @ 0: " + intArray[0]);

        // I vettori vengono indicizzati a parire dallo 0, ma sono mutabili
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Altro da aggiungere:
        // Liste di array - come i vettori ma vi sono più funzioni 
        // e la grandezza può variare in corso di esecuzione
        // Liste concatenate
        // Maps
        // HashMaps

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

        // Bitwise operators! 
        // Operatori binari
        /*
        ~       Unary bitwise complement
        <<      Signed left shift
        >>      Signed right shift
        >>>     Unsigned right shift
        &       AND Binario Bitwise AND
        ^       OR Esclusivo
        |       OR Incusivo
        */

        // Incrementare
        int i = 0;
        System.out.println("\n->Incrementare/Decrementare");
        // Gli operatori ++ e -- incrementano e decrementano ripettivamente di 1.
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
        // Se funziona con i char funzionerà ovviamente anche con le stringhe.
        int mese = 3;
        String stringaMese;
        switch (mese){
            case 1:
                    stringaMese = "Genneio";
                    break;
            case 2:
                    strigaMese = "Febbraio";
                    break;
            case 3:
                    stringaMese = "Marzo";
                    break;
            default:
                    stringaMese = "Altri mesi";
                    break;
        }
        System.out.println("Risultato del costrutto switch:: " + stringaMese);

        // Condizioni brevi
        // Si può usare l'operatore '?' per un rapido assegnamento
        // o per operazioni logiche.
        // Si legge: 
        // Se (condizione) è vera, usa <primo valore>, altrimenti usa <secondo valore>
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Stampa A, perchè la condizione è vera.


        /////////////////////////////////////////
        // Convertire i tipi di tati e Typcasting
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
        Bicycle trek = new Bicycle();

        // Chiamare metodi
        trek.speedUp(3); // Si usano sempre metodi set... get...
        trek.setCadence(100);

        // toString riporta la rappresenzazione dell'oggetto 
        // come se fosse una stringa
        System.out.println("trek info: " + trek.toString());

    } // Fine metodo main
} // Fine classe ImparareJavas


// Si possono inculdere altre anche delle classi non pubbliche (private)
// oltre a quella pubblica principale, in un file .java

// Sintassi per dichiarare una classe:
// <public/private/protected> class <Nome classe>{
//    //dati, variabili, costruttori, funzioni, tutto qua.
//    //le funzioni sono chiamate come i metodi.
// }

class Bicicletta {

    // Variabili della bicicletta
    public int cadence; 
      // Public: Può essere richiamato da qualsiasi classe
    private int speed; 
      // Private: è accessibile solo dalla classe dove è stato inizializzato
    protected int gear; 
      // Protected: è visto sia dalla classe che dalle sottoclassi
    String name; 
      // default: è accessibile sono all'interno dello stesso package

    // I costruttori vengono usati per creare variabili
    // Questo è un costruttore
    public Bicicletta() {
        ingranaggi = 1;
        ritmo = 50;
        velocità = 5;
        nome = "Bontrager";
    }

    // Questo è un costruttore che richiede parametri
    public Bicycle(int startCadence, int startSpeed, int startGear, String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Sintassi delle funzioni:
    // <public/private/protected> <return type> <function name>(<args>)

    // Le classi in java spesso implementano delle funzioni o metodo
    // 'get...' o 'set...' 

    // Dichiarazione di un metodo
    // <scope> <return type> <method name>(<args>)
    public int getCadence() {
        return cadence;
    }

    // i medodi (void) non necessitano di riportare un valore
    public void setCadence(int newValue) {
        cadence = newValue;
    }

    public void setGear(int newValue) {
        gear = newValue;
    }

    public void speedUp(int increment) {
        speed += increment;
    }

    public void slowDown(int decrement) {
        speed -= decrement;
    }

    public void setName(String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    //Medoto per visualizzare gli attributi dell'oggetto
    @Override
    public String toString() {
        return "gear: " + gear +
                " cadence: " + cadence +
                " speed: " + speed +
                " name: " + name;
    }
} // Fine classe bicicletta

// PennyFarthing è una sottoclasse della bicicletta
class PennyFarthing extends Bicycle {
    // (Sono quelle biciclette con un unica ruota enorme
    // Non hanno ingranaggi.)

    public PennyFarthing(int startCadence, int startSpeed){
        // Richiamo il costruttore del padre con super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // You should mark a method you're overriding with an @annotation
    // Bisogna contrassegnre un medodo che si sta riscrivendo 
    // con una @annotazione
    // Per saperne di più sulle annotazioni
    // Vedi la guida: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }

}

//Interfaccie
//Sintassi per dichiarare una interfaccia
//<access-level> interface <interface-name> extends <super-interfaces> {
//		//Constants
//		//Method declarations
//}

//Example - Food:
public interface Edible {
	public void eat(); //Any class that implements this interface, must implement this method
}

public interface Digestible {
	public void digest();
}

//Possiamo quindi creare una classe che implementa entrambe le interfaccie
public class Fruit implements Edible, Digestible {
	public void eat() {
		//...
	}

	public void digest() {
		//... 
	}
}

//In Java si può estendere solo una classe, ma si possono implementare 
//più interfaccie, per esempio:
public class ExampleClass extends ExampleClassParent implements InterfaceOne, InterfaceTwo {
	public void InterfaceOneMethod() {

	}

	public void InterfaceTwoMethod() {

	}
}

```

## Letture future

I link di seguito sono solo per capire l'argomento, cerca pure su Google degli esempi specifici

**Guida ufficiale di Oracle**:

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

**Tutorial Online**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Libri**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)


