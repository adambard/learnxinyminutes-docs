---
language: java
contributors:
    - ["Jake Prather", "https://github.com/JakeHP"]
    - ["Jakukyo Friel", "https://weakish.github.io"]
    - ["Madison Dickson", "https://github.com/mix3d"]
    - ["Simon Morgan", "https://sjm.io/"]
    - ["Zachary Ferguson", "https://github.com/zfergus2"]
    - ["Cameron Schermerhorn", "https://github.com/cschermerhorn"]
    - ["Rachel Stiyer", "https://github.com/rstiyer"]
    - ["Michael Dähnert", "https://github.com/JaXt0r"]
    - ["Rob Rose", "https://github.com/RobRoseKnows"]
    - ["Sean Nam", "https://github.com/seannam"]
filename: JavaFr.java
translators:
    - ['Mathieu Gemard', 'https://github.com/mgemard']
lang: fr-fr
---
Ce tutoriel est en cours de traduction (environ 35% de traduit au 16 Juin 2017).

Java est un langage orienté objet, concurrent et très facilement portable. Java
est inspiré du C++ mais ne reprend pas tout les concepts comme par exemple les
pointeurs et en ajoute de nouveaux comme les interfaces.
[En savoir plus.](https://fr.wikipedia.org/wiki/Java_(langage))

```java
// Les commentaires sur une seule ligne commencent par //

/*
Les commentaires sur plusieurs lignes ressemblent à ceci.
*/

/**
 * Les commentaires de la JavaDoc ressemblent à ceci. Ils sont utilisés pour
 * décrire la classe et ses différents attributs.
 * Attributs principaux:
 *
 * @author      Nom (et information de contact comme l'email) de(s) auteur(s).
 * @version     Version actuelle du programme.
 * @since        Date à laquelle cette partie du programme a été ajouté.
 * @param        Décrit les différents paramètres pour d'une méthode.
 * @return        Décrit le retour de la méthode.
 * @deprecated  Indique si le code est déprécié ou ne doit plus être utilisé.
 * @see         Lien vers une autre partie de la documentation.
*/

// Importe la classe ArrayList qui se trouve dans le package java.util
import java.util.ArrayList;
// Importe toutes les classes qui se trouvent dans le package java.security
import java.security.*;

// Chaque fichier .java doit contenir une classe public portant le même nom que
le fichier.
public class JavaFr {

    // Pour exécuter un programme Java, celui-ci doit posséder une méthode main
    // qui fournir un point d'entré.
    public static void main (String[] args) {

    ///////////////////////////////////////
    // Entrée/Sortie
    ///////////////////////////////////////

        /*
        * Sortie
        */

        // Utilisez System.out.println() pour afficher un texte dans la console.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Pour afficher sans retour à la ligne, on utilise System.out.print().
        System.out.print("Hello ");
        System.out.print("World");

        // Utilisez System.out.printf() pour formatter l'affichage.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        /*
         * Entrée
         */

        // Utilisez Scanner pour lire l'entrée
        // Nécessite: import java.util.Scanner;
        Scanner scanner = new Scanner(System.in);

        // Lire une entrée string
        String name = scanner.next();

        // Lire une entrée byte
        byte numByte = scanner.nextByte();

        // Lire une entrée int
        int numInt = scanner.nextInt();

        // Lire une entrée long
        float numFloat = scanner.nextFloat();

        // Lire une entrée double
        double numDouble = scanner.nextDouble();

        // Lire une entrée boolean
        boolean bool = scanner.nextBoolean();

        ///////////////////////////////////////
        // Variables
        ///////////////////////////////////////

        /*
        *  Déclaration de variable
        */
        // Déclarez une variable sous la forme <type> <name>
        int fooInt;
        // Declarez plusieurs variables du même type <type> <name1>, <name2>,
        // <name3>
        int fooInt1, fooInt2, fooInt3;

        /*
        *  Initialisation de variable
        */

        // Initialisez une variable sous la forme <type> <name> = <val>
        int barInt = 1;
        // Initialisez plusieurs variables du même type et avec la même valeur
        // sous la forme <type> <name1>, <name2>, <name3> = <val>
        int barInt1, barInt2, barInt3;
        barInt1 = barInt2 = barInt3 = 1;

        /*
        *  Types de variable
        */
        // byte - Entier signé utilisant la notation en complément à deux sur
        // 8 bits
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Si vous voulez interpréter un byte en entier non-signé, cette simple
        // opération peut vous aider
        int unsignedIntLessThan256 = 0xff & fooByte;
        // cela contraste avec une conversion qui peut être négative.
        int signedInt = (int) fooByte;

        // short - Entier signé utilisant la notation en complément à deux sur
        // 16 bits
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // int - Entier signé utilisant la notation en complément à deux sur
        // 32 bits
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int bazInt = 1;

        // long - Entier signé utilisant la notation en complément à deux sur
        // 64 bits
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L est utilisé pour indiquer que la variable est de type long;
        // le nombre serait traité comme un int sans le L

        // Note: byte, short, int et long sont signés. Ils peuvent avoir des
        // valeurs positives et négatives.
        // Il n'existe pas de variantes non-signées.
        // char, toutefois, est non-signé sur 16 bits

        // float - nombre à virgule flottante selon la norme IEEE 754 utilisant
        // le format simple précision sur 32 bits
        // 2^-149 <= float <= (2-2^-23) * 2^127
        float fooFloat = 234.5f;
        // f ou F sont utilisés pour indiquer que la variable est de type float;
        // autrement elle serait traitée comme un double.

        // double - nombre à virgule flottante selon la norme IEEE 754 utilisant
        // le format double précision sur 64 bits
        // 2^-1074 <= x <= (2-2^-52) * 2^1023
        double fooDouble = 123.4;

        // boolean - vrai & faux
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // char - un caractère Unicode sur 16 bits
        char fooChar = 'A';

        // les variables final ne peuvent pas être réassignés à un autre objet,
        final int HOURS_I_WORK_PER_WEEK = 9001;
        // mais ils peuvent être initialisés plus tard.
        final double E;
        E = 2.71828;

        // BigInteger - entiers immuable de taille arbitraire
        //
        // BigInteger est un type de donné qui autorise les développeurs à
        // manipuler des entiers au delà de 64 bits. Les entiers sont stockés
        // dans un tableau de bytes et sont manipulés grâce à des functions
        // de la classe BigIntger
        //
        // BigInteger peut être initialiser en utilisant un tableau de bytes ou
        // une chaîne de caractère.      
        BigInteger fooBigInteger = new BigInteger(fooByteArray);

        // BigDecimal - entiers immuable et positif de taille arbitraire
        //
        // BigDecimal comprend deux parties: une entier de taille arbitraire
        // (BigInteger) et un entier de 32 bits représantant la position de la
        // virgule.
        //
        // BigDecimal donne aux développeurs un contrôle totale pour l'arrondie
        // à la décimale. Il est recommandé de l'utiliser pour les valeurs
        // monétaires et pour les cas où la value exacte de l'arondie à la
        // décimale est requis.
        //
        // BigInteger peut être initialiser en utilisant un int, long, double ou
        // String.
        // On peut également utiliser un BigInteger et un int pour la
        // position de la virgule.
        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, fooInt);

        // Sachez que la création d'un BigDecimal avec un float ou
        // un double prendra en compte l'inexactitude des représention en float
        // ou double.
        // Préférez String pour une représention excate.
        BigDecimal tenCents = new BigDecimal("0.1");

        // String - Chaîne de caractères
        String fooString = "My String Is Here!";

        // \n est un caractère d'échappement qui indique une nouvelle ligne
        String barString = "Printing on a new line?\nNo Problem!";
        // \t est un caractère d'échappement qui ajoute une tabulation
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Construction de chaînes de caractères
        // #1 - avec l'opérateur +
        // C'est la manière la plus simple et optimisé par le compilateur
        String plusConcatenated = "Strings can " + "be concatenated " + "via + operator.";
        System.out.println(plusConcatenated);
        // Affiche: Strings can be concatenated via + operator.

        // #2 - avec StringBuilder
        // Cette méthode ne nécessite pas d'objet String intermédiaire. elle
        // stocke juste les différentes chaînes de caractères et les assemblent
        // lorsque la méthode toString() est appelée.
        // Attention: Cette classe n'est pas thread-safe (capable de fonctionner
        // correctement lorsqu'il est exécuter dans un thread). Une alternative
        // (avec un impact sur les performances) thread-safe est d'utiliser la
        // classe StringBuffer.
        StringBuilder builderConcatenated = new StringBuilder();
        builderConcatenated.append("You ");
        builderConcatenated.append("can use ");
        builderConcatenated.append("the StringBuilder class.");
        System.out.println(builderConcatenated.toString()); // only now is the string built
        // Affiche: You can use the StringBuilder class.

        // StringBuffer est efficace quand la chaîne de caractères n'est pas
        // utilisée avec la fin de sa construction.
        StringBuilder stringBuilder = new StringBuilder();
        String inefficientString = "";
        for(int i = 0 ; i < 10; i++){
            stringBuilder.append(i).append(" ");
            inefficientString += i + " ";
        }
        System.out.println(inefficientString);
        System.out.println(stringBuilder.toString());
        // inefficientString est moins performant car une chaîne de caractères
        // est créée à chaque itération de la boucle.
        // Les concaténations avec + sont compilés en un StringBuilder et
        // toString().
        // Evitez les concaténations de string dans les boucles.

        // #3 - avec la méthode format() de la classe String.
        // Une autre alternative. Rapide et lisible.
        String.format("%s may prefer %s.", "Or you", "String.format()");
        // Affiche: Or you may prefer String.format().

        // Tableau
        // La taille du tableau doit être précisée à l'instantiation
        // Les formats suivant sont possibles pour déclarer un tableau
        // <datatype>[] <var name> = new <datatype>[<array size>];
        // <datatype> <var name>[] = new <datatype>[<array size>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Une autre manière de déclarer et initialiser un tableau
        int[] y = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = {true, false, false};

        // Accéder à un élément
        System.out.println("intArray @ 0: " + intArray[0]);

        // Les tableau commencent à 0 et sont muables
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Les autres types de donnés utiles sont
        // ArrayList - Identique aux tableaux mais avec plus de fonctionnalités
        //              et de taille muable.
        // LinkedList - Implémentation de listes doublement chaînées. Toutes Les
        //              opérations éffectuées le sont comme attendue pour une
        //              liste doublement chaînée.
        // Map - Une collection d'objets qui fait correspondre une valeur à une
        //       clé. Map est une interface et ne peut pas être instantiée. Le
        //       type des clés et des valeurs doit être précisés à
        //       l'instantiation. Chaque clé doit correspondre à une seule
        //       valeur et chaque clé doit être unique (pas de clés dupliquées).
        // HashMap - Cette classe utilise une table de hachage pour implémenter
        //           l'interface Map. Cela garantie que le temps d'exécution des
        //           opérations basiques, comme get (récuper une valeur) et
        //           insert (insérer une valeur), reste constant quelque soit la
        //           la taille.
        // TreeMap - Cette classe utilise une structure en arbre et est
        //           ordonnée. Elle implémente un arbre bicolore (ou arbre rouge
        //           et noir) et ordonne les éléments en se basant sur la clé ou
        //           en utilisant un comparateur fournit à la création.

        ///////////////////////////////////////
        // Operators
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // Shorthand for multiple declarations

        // Arithmetic is straightforward
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int returns int)
        System.out.println("1/2 = " + (i1 / (double)i2)); // => 0.5

        // Modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Comparison operators
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Boolean operators
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); // => false
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); // => true
        System.out.println("!(3 == 2)? " + (!(3 == 2))); // => true

        // Bitwise operators!
        /*
        ~      Unary bitwise complement
        <<     Signed left shift
        >>     Signed/Arithmetic right shift
        >>>    Unsigned/Logical right shift
        &      Bitwise AND
        ^      Bitwise exclusive OR
        |      Bitwise inclusive OR
        */

        // Increment operators
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        // The ++ and -- operators increment and decrement by 1 respectively.
        // If they are placed before the variable, they increment then return;
        // after the variable they return then increment.
        System.out.println(i++); // i = 1, prints 0 (post-increment)
        System.out.println(++i); // i = 2, prints 2 (pre-increment)
        System.out.println(i--); // i = 1, prints 2 (post-decrement)
        System.out.println(--i); // i = 0, prints 0 (pre-decrement)

        ///////////////////////////////////////
        // Control Structures
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // If statements are c-like
        int j = 10;
        if (j == 10) {
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // While loop
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Increment the counter
            // Iterated 100 times, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Do While Loop
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Increment the counter
            // Iterated 99 times, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // For Loop
        // for loop structure => for(<start_statement>; <conditional>; <step>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Iterated 10 times, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // Nested For Loop Exit with Label
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // breaks out of outer loop instead of only the inner one
            }
          }
        }

        // For Each Loop
        // The for loop is also able to iterate over arrays as well as objects
        // that implement the Iterable interface.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // for each loop structure => for (<object> : <iterable>)
        // reads as: for each element in the iterable
        // note: the object type must match the element type of the iterable.
        for (int bar : fooList) {
            System.out.println(bar);
            //Iterates 9 times and prints 1-9 on new lines
        }

        // Switch Case
        // A switch works with the byte, short, char, and int data types.
        // It also works with enumerated types (discussed in Enum Types), the
        // String class, and a few special classes that wrap primitive types:
        // Character, Byte, Short, and Integer.
        int month = 3;
        String monthString;
        switch (month) {
            case 1: monthString = "January";
                    break;
            case 2: monthString = "February";
                    break;
            case 3: monthString = "March";
                    break;
            default: monthString = "Some other month";
                     break;
        }
        System.out.println("Switch Case Result: " + monthString);

        // Starting in Java 7 and above, switching Strings works like this:
        String myAnswer = "maybe";
        switch(myAnswer) {
            case "yes":
                System.out.println("You answered yes.");
                break;
            case "no":
                System.out.println("You answered no.");
                break;
            case "maybe":
                System.out.println("You answered maybe.");
                break;
            default:
                System.out.println("You answered " + myAnswer);
                break;
        }


        // Try-with-resources (Java 7+)
        // Try-catch-finally statements work as expected in Java but in Java 7+
        // the try-with-resources statement is also available. Try-with-resources
        // simplifies try-catch-finally statements by closing resources
        // automatically.

        // In order to use a try-with-resources, include a an instance of a class
        // in the try statement. The class must implement java.lang.AutoCloseable.
        try(BufferedReader br = new BufferedReader(new FileReader("foo.txt"))) {
            // You can attempt to do something that could throw an exception.
            System.out.println(br.readLine());
            // In Java 7, the resource will always be closed, even if it throws
            // an Exception.
        } catch (Exception ex) {
            //The resource will be closed before the catch statement executes.
            System.out.println("readLine() failed.");
        }
        // No need for a finally statement in this case, the BufferedReader is
        // already closed. This can be used to avoid certain edge cases where
        // a finally statement might not be called.
        // To learn more:
        // https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html


        // Conditional Shorthand
        // You can use the '?' operator for quick assignments or logic forks.
        // Reads as "If (statement) is true, use <first value>, otherwise, use
        // <second value>"
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Prints A, because the statement is true

        ////////////////////////////////////////
        // Converting Data Types And Typecasting
        ////////////////////////////////////////

        // Converting data

        // Convert String To Integer
        Integer.parseInt("123");//returns an integer version of "123"

        // Convert Integer To String
        Integer.toString(123);//returns a string version of 123

        // For other conversions check out the following classes:
        // Double
        // Long
        // String

        // Typecasting
        // You can also cast Java objects, there's a lot of details and deals
        // with some more intermediate concepts. Feel free to check it out here:
        // https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html

        ///////////////////////////////////////
        // Classes And Functions
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (definition of the Bicycle class follows)

        // Use new to instantiate a class
        Bicycle trek = new Bicycle();

        // Call object methods
        trek.speedUp(3); // You should always use setter and getter methods
        trek.setCadence(100);

        // toString returns this Object's string representation.
        System.out.println("trek info: " + trek.toString());

        // Double Brace Initialization
        // The Java Language has no syntax for how to create static Collections
        // in an easy way. Usually you end up in the following way:
        private static final Set<String> COUNTRIES = new HashSet<String>();
        static {
           COUNTRIES.add("DENMARK");
           COUNTRIES.add("SWEDEN");
           COUNTRIES.add("FINLAND");
        }

        // But there's a nifty way to achieve the same thing in an
        // easier way, by using something that is called Double Brace
        // Initialization.
        private static final Set<String> COUNTRIES = new HashSet<String>() {{
            add("DENMARK");
            add("SWEDEN");
            add("FINLAND");
        }}

        // The first brace is creating a new AnonymousInnerClass and the
        // second one declares an instance initializer block. This block
        // is called when the anonymous inner class is created.
        // This does not only work for Collections, it works for all
        // non-final classes.

    } // End main method
} // End LearnJava class

// You can include other, non-public outer-level classes in a .java file,
// but it is not good practice. Instead split classes into separate files.

// Class Declaration Syntax:
// <public/private/protected> class <class name> {
//    // data fields, constructors, functions all inside.
//    // functions are called as methods in Java.
// }

class Bicycle {

    // Bicycle's Fields/Variables
    public int cadence; // Public: Can be accessed from anywhere
    private int speed;  // Private: Only accessible from within the class
    protected int gear; // Protected: Accessible from the class and subclasses
    String name; // default: Only accessible from within this package
    static String className; // Static class variable

    // Static block
    // Java has no implementation of static constructors, but
    // has a static block that can be used to initialize class variables
    // (static variables).
    // This block will be called when the class is loaded.
    static {
        className = "Bicycle";
    }

    // Constructors are a way of creating classes
    // This is a constructor
    public Bicycle() {
        // You can also call another constructor:
        // this(1, 50, 5, "Bontrager");
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }
    // This is a constructor that takes arguments
    public Bicycle(int startCadence, int startSpeed, int startGear,
        String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Method Syntax:
    // <public/private/protected> <return type> <function name>(<args>)

    // Java classes often implement getters and setters for their fields

    // Method declaration syntax:
    // <access modifier> <return type> <method name>(<args>)
    public int getCadence() {
        return cadence;
    }

    // void methods require no return statement
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

    //Method to display the attribute values of this Object.
    @Override // Inherited from the Object class.
    public String toString() {
        return "gear: " + gear + " cadence: " + cadence + " speed: " + speed +
            " name: " + name;
    }
} // end class Bicycle

// PennyFarthing is a subclass of Bicycle
class PennyFarthing extends Bicycle {
    // (Penny Farthings are those bicycles with the big front wheel.
    // They have no gears.)

    public PennyFarthing(int startCadence, int startSpeed) {
        // Call the parent constructor with super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // You should mark a method you're overriding with an @annotation.
    // To learn more about what annotations are and their purpose check this
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        this.gear = 0;
    }
}

// Interfaces
// Interface declaration syntax
// <access-level> interface <interface-name> extends <super-interfaces> {
//     // Constants
//     // Method declarations
// }

// Example - Food:
public interface Edible {
    public void eat(); // Any class that implements this interface, must
                       // implement this method.
}

public interface Digestible {
    public void digest();
    // In Java 8, interfaces can have default method.
    // public void digest() {
    //     System.out.println("digesting ...");
    // }
}

// We can now create a class that implements both of these interfaces.
public class Fruit implements Edible, Digestible {
    @Override
    public void eat() {
        // ...
    }

    @Override
    public void digest() {
        // ...
    }
}

// In Java, you can extend only one class, but you can implement many
// interfaces. For example:
public class ExampleClass extends ExampleClassParent implements InterfaceOne,
    InterfaceTwo {
    @Override
    public void InterfaceOneMethod() {
    }

    @Override
    public void InterfaceTwoMethod() {
    }

}

// Abstract Classes

// Abstract Class declaration syntax
// <access-level> abstract <abstract-class-name> extends <super-abstract-classes> {
//     // Constants and variables
//     // Method declarations
// }

// Marking a class as abstract means that it contains abstract methods that
// must be defined in a child class. Similar to interfaces, abstract classes
// cannot be instantiated, but instead must be extended and the abstract
// methods defined. Different from interfaces, abstract classes can contain a
// mixture of concrete and abstract methods. Methods in an interface cannot
// have a body, unless the method is static, and variables are final by default,
// unlike an abstract class. Also abstract classes CAN have the "main" method.
public abstract class Animal
{
    public abstract void makeSound();

    // Method can have a body
    public void eat()
    {
        System.out.println("I am an animal and I am Eating.");
        // Note: We can access private variable here.
        age = 30;
    }

    // No need to initialize, however in an interface
    // a variable is implicitly final and hence has
    // to be initialized.
    private int age;

    public void printAge()
    {
        System.out.println(age);  
    }

    // Abstract classes can have main function.
    public static void main(String[] args)
    {
        System.out.println("I am abstract");
    }
}

class Dog extends Animal
{
    // Note still have to override the abstract methods in the
    // abstract class.
    @Override
    public void makeSound()
    {
        System.out.println("Bark");
        // age = 30;    ==> ERROR!    age is private to Animal
    }

    // NOTE: You will get an error if you used the
    // @Override annotation here, since java doesn't allow
    // overriding of static methods.
    // What is happening here is called METHOD HIDING.
    // Check out this SO post: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Dog pluto = new Dog();
        pluto.makeSound();
        pluto.eat();
        pluto.printAge();
    }
}

// Final Classes

// Final Class declaration syntax
// <access-level> final <final-class-name> {
//     // Constants and variables
//     // Method declarations
// }

// Final classes are classes that cannot be inherited from and are therefore a
// final child. In a way, final classes are the opposite of abstract classes
// because abstract classes must be extended, but final classes cannot be
// extended.
public final class SaberToothedCat extends Animal
{
    // Note still have to override the abstract methods in the
    // abstract class.
    @Override
    public void makeSound()
    {
        System.out.println("Roar");
    }
}

// Final Methods
public abstract class Mammal()
{
    // Final Method Syntax:
    // <access modifier> final <return type> <function name>(<args>)

    // Final methods, like, final classes cannot be overridden by a child
    // class, and are therefore the final implementation of the method.
    public final boolean isWarmBlooded()
    {
        return true;
    }
}

// Enum Type
//
// An enum type is a special data type that enables for a variable to be a set
// of predefined constants. The variable must be equal to one of the values
// that have been predefined for it. Because they are constants, the names of
// an enum type's fields are in uppercase letters. In the Java programming
// language, you define an enum type by using the enum keyword. For example,
// you would specify a days-of-the-week enum type as:
public enum Day {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY,
    THURSDAY, FRIDAY, SATURDAY
}

// We can use our enum Day like that:
public class EnumTest {
    // Variable Enum
    Day day;

    public EnumTest(Day day) {
        this.day = day;
    }

    public void tellItLikeItIs() {
        switch (day) {
            case MONDAY:
                System.out.println("Mondays are bad.");
                break;
            case FRIDAY:
                System.out.println("Fridays are better.");
                break;   
            case SATURDAY:
            case SUNDAY:
                System.out.println("Weekends are best.");
                break;     
            default:
                System.out.println("Midweek days are so-so.");
                break;
        }
    }

    public static void main(String[] args) {
        EnumTest firstDay = new EnumTest(Day.MONDAY);
        firstDay.tellItLikeItIs(); // => Mondays are bad.
        EnumTest thirdDay = new EnumTest(Day.WEDNESDAY);
        thirdDay.tellItLikeItIs(); // => Midweek days are so-so.
    }
}

// Enum types are much more powerful than we show above.
// The enum body can include methods and other fields.
// You can see more at https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

**Official Oracle Guides**:

* [Java Tutorial Trail from Sun / Oracle](https://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](https://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](https://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](https://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

* New features in Java 8:
    * [Lambda expressions (functional programming)](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
    * [Date and time API (java.time package)](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)

**Online Practice and Tutorials**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)

**Books**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](https://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](https://www.amazon.com/gp/product/0071606300)
