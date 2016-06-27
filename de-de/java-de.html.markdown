---
language: java
filename: LearnJavaDe.java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Jakukyo Friel", "http://weakish.github.io"]
    - ["Madison Dickson", "http://github.com/mix3d"]
    - ["Simon Morgan", "http://sjm.io/"]
translators:
    - ["Michael Dähnert", "http://github.com/JaXt0r"]
lang: de-de
---

Java ist eine Programmiersprache für vielfältige Aufgaben. Sie ist imperative und objektorientiert.
Oftmals wird sie für Desktop- Webapplikationen sowie als Programmiersprache im Betriebssystem Android verwendet.
[Weitere Informationen \(Englisch\](http://docs.oracle.com/javase/tutorial/java/)

```java
// Einzeilige Kommentare starten mit //
/*
Mehrzeilige Kommentare sehen so aus.
*/
/**
JavaDoc Kommentare haben dieses Format. Sie werden verwendet um Klassen, Attribute sowie Methoden zu beschreiben.
*/

// Importieren der Klasse ArrayList aus dem Paket java.util
import java.util.ArrayList;
// Importieren aller Klassen innerhalb des Paketes java.security
import java.security.*;

// Jede .java Datei besteht aus einer äußeren öffentlichen (public) Klasse.
// Der Name der Klasse muss identisch des Dateinamens sein.
public class LearnJavaDe {

    // Ein Programm muss eine main Methode als Eintrittspunkt besitzen.
    public static void main (String[] args) {

        // System.out.println() wird zum Schreiben von zeilenweisen Ausgaben verwendet.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Zum Schreiben von Ausgaben ohne Zeilenumbruch wird System.out.print() verwendet.
        System.out.print("Hello ");
        System.out.print("World");


        ///////////////////////////////////////
        // Typen & Variablen
        ///////////////////////////////////////

        // Zum Deklarieren einer Variable nutze <type> <name>
        // Byte - 8-bit vorzeichenbehaftete (signed), binäre Ganzzahl
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - 16-bit vorzeichenbehaftete (signed), binäre Ganzzahl
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 32-bit vorzeichenbehaftete (signed), binäre Ganzzahl
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - 64-bit vorzeichenbehaftete (signed), binäre Ganzzahl
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L wird verwendet um zu kennzeichnen, dass ein Variablenwert vom Typ long ist.
        // Ohne diesen Buchstaben wird die Zahl automatisch als Integer behandelt.

        // Hinweis: Java besitzt keine vorzeichenlosen (unsigned) Typen.

        // Float - Typ mit einfacher Genauigkeit (Single-precision), 32-bit IEEE 754 Fließkommazahl
        float fooFloat = 234.5f;
        // f wird verwendet um zu kennzeichnen, dass ein Variablenwert vom Typ float ist;
        // Ohne diesen Buchstaben wird die Zahl automatisch als Integer behandelt.

        // Double - Typ mit doppelter Genauigkeit (Double-precision), 64-bit IEEE 754 Fließkommazahl
        double fooDouble = 123.4;

        // Boolean - Wahr & Falsch (true & false)
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - Ein einfacher 16-bit Unicode Buchstabe
        char fooChar = 'A';

        // final Variablen können von einem anderen Objekt nicht erneut zugeordnet werden.
        final int HOURS_I_WORK_PER_WEEK = 9001;

        // Zeichenketten (Strings)
        String fooString = "My String Is Here!";

        // \n ist ein Escape Zeichen welcher eine neue Zeile startet.
        String barString = "Schreiben auf einer neuen Zeile?\nKein Problem!";
        // \t ist ein Escape Zeichen welcher einen Tab-Zeichen anhängt.
        String bazString = "Möchtest du einen Tabulator anhängen?\tKein Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Arrays
        // Die Arraygröße muss bei Instanziierung entschieden werden.
        // Das folgende Format funktioniert bei Deklaration eines Arrays
        // <datentyp>[] <variablenname> = new <datentyp>[<arraygröße>];
        // <datentyp> <variablenname>[] = new <datentyp>[<arraygröße>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Eine weitere Möglichkeit ein Array zu deklarieren & initialisieren.
        int[] y = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = new boolean[] {true, false, false};

        // Indexierung eines Arrays - Zugriff auf ein Element
        System.out.println("intArray @ 0: " + intArray[0]);

        // Arrays sind 0-indexiert und veränderbar.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Weitere nennenswerte Typen
        // ArrayLists - Ähnlich Arrays, allerdings werden mehr Funktionen geboten,
        //            ebenso ist die Arraygröße verwänderbar
        // LinkedLists - Implementierung einer doppelt verlinkten Liste.
        //            Alle Operationen funktioneren so, wie es von einer doppelt verlinkten Liste erwartet wird.
        //            Weitere Informationen: https://de.wikipedia.org/wiki/Liste_(Datenstruktur)#Doppelt_.28mehrfach.29_verkettete_Liste
        // Maps - Eine Sammlung von Objekten, welche eine Verknüpfung von Schlüsseln zu Werten (key => value) vornimmt.
        //            Eine Map kann keine Duplikate enthalten; Jeder Schlüssel kann genau einen Wert beinhalten.
        // HashMaps - Diese Klasse nutzt eine Hashtabelle zur Implementierung eines Map Interfaces.
        //            Dies erlaubt es zur Laufzeit Standardoperationen wie gib (get) und einfügen (insert)
        //            selbst für große Mengen in einer konstanten Zeit auszuführen (Laufzeitverhalten O(n)).

        ///////////////////////////////////////
        // Operatoren
        ///////////////////////////////////////
        System.out.println("\n->Operatoren");

        int i1 = 1, i2 = 2; // Kurform zur Deklaration mehrerer Variablen.

        // Arithmetische Operationen sind einfach nutzbar.
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5 Nachkommazahl abgeschnitten)

        // Modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Vergleichsoperationen
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Bitwise Operatoren!
        /*
        ~      Unäres (unary) bitweise Komplement
        <<     Vorzeichenbehaftete (signed) linke Verschiebung
        >>     Vorzeichenbehaftete (signed) rechte Verschiebung
        >>>    Vorzeichenlose (unsigned) linke Verschiebung
        &      Bitweise UND (AND)
        ^      Bitweise exklusive ODER (OR)
        |      Bitweise inklusive ODER (OR)
        */

        // Inkrementierungen
        int i = 0;
        System.out.println("\n->Inc/Dec-rementierung");
        // Die ++ und -- operatoren inkrementieren und dekrementieren jeweils um 1.
        // Werden sie vor die Variable gesetzt, ink-/dekrementieren sie und geben anschließend ihren Wert zurück.
        // Hinter der Variable geben sie ihren Wert zurück und ändern ihn anschließend.
        System.out.println(i++); // i = 1, schreibt 0 (post-increment)
        System.out.println(++i); // i = 2, schreibt 2 (pre-increment)
        System.out.println(i--); // i = 1, schreibt 2 (post-decrement)
        System.out.println(--i); // i = 0, schreibt 0 (pre-decrement)

        ///////////////////////////////////////
        // Kontrollstrukturen
        ///////////////////////////////////////
        System.out.println("\n->Kontrollstrukturen");

        // If Bedingungen sind wie in den C-Sprachen aufgebaut
        int j = 10;
        if (j == 10){
            System.out.println("Ich wurde geprinted");
        } else if (j > 10) {
            System.out.println("Ich nicht");
        } else {
            System.out.println("Ich auch nicht");
        }

        // While Schleife
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Den Zähler inkrementieren
            // 100x iterieren, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Wert: " + fooWhile);

        // Do While Schleife
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Den Zähler inkrementieren
            // 99x iterieren, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Wert: " + fooDoWhile);

        // For Schleife
        int fooFor;
        // for Schleifenstruktur => for(<start_statement>; <Bedingung>; <Schritt>)
        for (fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // 10x iterieren, fooFor 0->9
        }
        System.out.println("fooFor Wert: " + fooFor);

        // For Each Schleife
        // The for Schleife kann verwendet werden um über Arrays ebenso wie Objekte,
        // welche das Interface Iterable implementieren zu iterieren.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // for each Schleifenstruktur => for (<Objekt> : <iterable>)
        // Wird gelesen als: Iteriere für jedes Element im Iterable
        // Hinweis: Der Objekttyp muss dem Elementtyp des Iterable entsprechen.

        for (int bar : fooList) {
            System.out.println(bar);
            //9x iterieren und die Werte 1-9 auf jeweils einer neuen Zeile schreiben
        }

        // Switch Case
        // A Schalter (switch) funktioniert mit den Datentypen byte, short, char und int.
        // Ebenso kann er für Aufzählungen (Enums) verwendet werden (Enum Typen folgen weiter unten)
        // der String Klasse (ab Java SE7) und ein paar spezielle Klassen, welche die primitiven Typen ummanteln (wrap):
        // Character, Byte, Short, and Integer.
        int monat = 3;
        String monatsString;
        switch (monat) {
            case 1: monatsString = "Januar";
                    break;
            case 2: monatsString = "Februar";
                    break;
            case 3: monatsString = "März";
                    break;
            default: monatsString = "Ein anderer Monat";
                     break;
        }
        System.out.println("Switch Case Ergebnis: " + monatsString);

        // Bedingungsoperator (Conditional Shorthand)
        // Der Operator '?' kann für schnelle Zuweisungen oder logische Verzweigungen genutzt werden.
        // Er ist wie folgt zu lesen: Wenn die Bedingung wahr ist, nutze <erster Wert>
        // ansonsten nutze <zweiter Wert>
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Schreibt A, denn die Bedingung ist wahr.


        ////////////////////////////////////////
        // Typkonvertierung und Type-Casting
        ////////////////////////////////////////

        // Konvertierung von Daten

        // Konvertiere String nach Integer
        Integer.parseInt("123");// Gibt die Integer Repräsentation von "123" zurück

        // Konvertiere String nach Integer
        Integer.toString(123);// Gibt die String Repräsentation von 123 zurück

        // Für andere Konvertierungen sind die folgenden Klassen zu betrachten:
        // Double
        // Long
        // String

        // Tpe-Casting
        // Java Objekte können benfalls konvertiert werden, hierbei gibt es vielfältige Konzepte.
        // Weitere Informationen finden sich hier (englisch):
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Klassen und Funktionen
        ///////////////////////////////////////

        System.out.println("\n->Klassen & Funktionen");

        // (Die Definition der Klasse Fahrrad folgt)

        // Verwendung einer neuen Klasseninstanz
        Fahrrad trek = new Fahrrad();

        // Aufruf von Methoden des Objektes
        trek.erhöheGeschwindigkeit(3); // Es sollten immer getter- und setter- Methoden verwendet werden
        trek.setTrittfrequenz(100);

        // toString gibt die StringRepräsentation des Objektes zurück.
        System.out.println("trek info: " + trek.toString());

    } // Ende der Main Methode
} // Ende der LearnJavaDe Klasse


// In einer .java-Datei können zusätzliche nicht öffentliche (non-public) äüßere Klassen vorhanden sein.


// Syntax der Klassendeklaration:
// <public/private/protected> class <Klassenname> {
//    // Es folgen Datenfelder, Konstruktoren, Funktionen.
//    // Funktionen werden in Java Methoden genannt.
// }

class Fahrrad {

    // Felder/Variablen der Klasse Fahrrad
    public int trittfrequenz; // Public: Kann von überall her angesprochen werden
    private int geschwindigkeit;  // Private: Nur innerhalb der Klasse sichtbar
    protected int gang; // Protected: Erreichbar innerhalb der Klasse oder Subklassen (sub classes)
    String name; // default: Nur innerhalb des Paketes verwendbar

    // Eine Klasse kann mittelst Konstruktoren erstellt werden.
    // Das ist ein Konstruktor
    public Fahrrad() {
        gang = 1;
        trittfrequenz = 50;
        geschwindigkeit = 5;
        name = "Bontrager";
    }

    // Das ist ein Konstruktor mit Argumenten
    public Bicycle(int initialTrittfrequenz, int initialGeschwindigkeit, int initialGang,
        String name) {
        this.gang = initialGang;
        this.trittfrequenz = initialTrittfrequenz;
        this.geschwindigkeit = initialGeschwindigkeit;
        this.name = name;
    }

    // Syntax von Methoden (Funktionen):
    // <public/private/protected> <Rückgabetyp> <Funktionsname>(<Argumente>)

    // Java Klassen implementieren oftmals getter- und setter-Methoden ihrer Felder

    // Syntax von Methodendeklarationen:
    // <Sichtbarkeit> <Rückgabetyp> <Methodenname>(<Argumente>)
    public int getTrittfrequenz() {
        return tri;
    }

    // void Methoden benötigen kein return Statement.
    public void setCadence(int newValue) {
        cadence = newValue;
    }

    public void setGear(int newValue) {
        gear = newValue;
    }

    public void erhöheGeschwindigkeit(int increment) {
        speed += increment;
    }

    public void verringereGeschwindigkeit(int decrement) {
        speed -= decrement;
    }

    public void setName(String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    //Methode zur Darstellung der Attributwerte des Objektes.
    @Override
    public String toString() {
        return "Gang: " + gang + " Trittfrequenz: " + trittfrequenz + " Geschwindigkeit: " + geschwindigkeit +
            " name: " + name;
    }
} // Ende der Klasse Fahrrad

// Hochrad ist eine Subklasse von Fahrrad
class Hochrad extends Fahrrad {
    // (Hochräder sind Fahrräder mit einem extrem großen Vorderrad.
    // Sie haben keine Gänge.)

    public Hochrad(int initialTrittfrequenz, int initialGeschwindigkeit){
        // Aufruf des Vater-Konstruktors (parent constructor) mit dem Wort super.
        super(initialTrittfrequenz, initialGeschwindigkeit, 0, "Hochrad");
    }

    // Überschriebene Methoden sollten die Annotation @Override besitzen.
    // Mehr zu Annotationen und deren Verwendungszwecken kann hier nachgelesen werden:
    // (englisch) http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGang(int gang) {
        gang = 0;
    }
}

// Schnittstellen (Interfaces)
// Interface Deklaration
// <Zugriffsrecht> interface <Name> extends <super-Interface> {
//     // Konstanten
//     // Methodendeklarationen
// }

// Beispiel - Nahrung:
public interface Essbar {
	public void essen(); // Jede Klasse, die dieses Interface implementiert
                       // muss auch diese Methode implementieren.
}                       


public interface Verdaulich {
	public void verdauen();
}


// Nun können wir eine Klasse erstellen, die beide Interfaces implementiert.
public class Frucht implements Essbar, Verdaulich {
    @Override
	public void essen() {
		// ...
	}

    @Override
	public void verdauen() {
		// ...
	}
}

// Mit Java kann man nur eine Klasse erweitern (extends) jedoch mehrere Interfaces implementieren.
// z.B.:
public class BeispielKlasse extends ParentBeispielKlasse implements InterfaceEins,
    InterfaceZwei {
    @Override
	public void methodeInterfaceEins() {
	}

    @Override
	public void methodeInterfaceZwei() {
	}
}
```

## Weitere Informationen (in englisch)

Die folgenden Links dienen lediglich dazu Verständnis für die Kapitel aufzubauen.
Für tiefergreifende Fragen ist Google der beste Startpunkt.

**Offizielle Oracle Guides**:

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

**Online Tutorials**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Bücher**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)
