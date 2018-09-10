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
filename: java-fr.java
translators:
    - ['Mathieu Gemard', 'https://github.com/mgemard']
lang: fr-fr
---
Java est un langage orienté objet, concurrent et très facilement portable. Java
est inspiré du C++ mais ne reprend pas tous les concepts comme par exemple les
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
    // qui fournir un point d'entrée.
    public static void main(String[] args) {

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

        // Utilisez System.out.printf() pour formatter les données à afficher.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        /*
         * Entrée
         */

        // Utilisez Scanner pour lire l'entrée
        // Nécessite: import java.util.Scanner;
        Scanner scanner = new Scanner(System.in);

        // Lire une chaîne de caractères
        String name = scanner.next();

        // Lire un byte
        byte numByte = scanner.nextByte();

        // Lire un entier
        int numInt = scanner.nextInt();

        // Lire une entrée de type long
        float numFloat = scanner.nextFloat();

        // Lire une entrée de type double
        double numDouble = scanner.nextDouble();

        // Lire une entrée de type boolean
        boolean bool = scanner.nextBoolean();

        ///////////////////////////////////////
        // Variables
        ///////////////////////////////////////

        /*
        *  Déclaration de variable
        */
        // Déclarez une variable avec la forme <type> <name>
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
        // sous la forme
        // <type> <name1>, <name2>, <name3>
        // <name1> = <name2> = <name3> = <val>
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

        // BigInteger - entier immuable de taille arbitraire
        //
        // BigInteger est un type de donné qui autorise les développeurs à
        // manipuler des entiers au delà de 64 bits. Les entiers sont stockés
        // dans un tableau de bytes et sont manipulés grâce à des functions
        // de la classe BigIntger
        //
        // BigInteger peut être initialiser en utilisant un tableau de bytes ou
        // une chaîne de caractère.
        BigInteger fooBigInteger = new BigInteger(fooByteArray);

        // BigDecimal - entier immuable et positif de taille arbitraire
        //
        // BigDecimal comprend deux parties: une entier de taille arbitraire
        // (BigInteger) et un entier de 32 bits représantant la position de la
        // virgule.
        //
        // BigDecimal donne aux développeurs un contrôle total pour l'arrondie
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
        // Préférez String pour une représention exacte.
        BigDecimal tenCents = new BigDecimal("0.1");

        // String - Chaîne de caractères
        String fooString = "My String Is Here!";

        // \n est un caractère d'échappement qui indique une nouvelle ligne
        String barString = "Printing on a new line?\nNo Problem!";
        // \t est un caractère d'échappement qui indique une tabulation
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
        // Cette méthode ne nécessite pas d'objet String intermédiaire. Elle
        // stocke juste les différentes chaînes de caractères et les assemble
        // lorsque la méthode toString() est appelée.
        // Attention: Cette classe n'est pas thread-safe (l'objet ne peut pas être partagé
        // entre les threads). Une alternative
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
        for (int i = 0 ; i < 10; i++) {
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

        // Les tableaus commencent à 0 et sont muables
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
        // Opérateurs
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // Raccourcis pour des déclarations multiples

        // L'arithmétique
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int returns int)
        System.out.println("1/2 = " + (i1 / (double)i2)); // => 0.5

        // Le modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Opérateurs de comparaison
        System.out.println("3 == 2? " + (3 == 2)); // => faux
        System.out.println("3 != 2? " + (3 != 2)); // => vrai
        System.out.println("3 > 2? " + (3 > 2)); // => vrai
        System.out.println("3 < 2? " + (3 < 2)); // => faux
        System.out.println("2 <= 2? " + (2 <= 2)); // => vrai
        System.out.println("2 >= 2? " + (2 >= 2)); // => vrai

        // Opérateurs boolean
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); // => false
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); // => true
        System.out.println("!(3 == 2)? " + (!(3 == 2))); // => true

        // Opérateurs sur les bits
        /*
        ~      Complément à un
        <<     Décalage des bits vers la gauche
        >>     Décalage des bits vers la droite, le signe est conservé
        >>>    Décalage des bits vers la droite, zéro est utilisé pour les bits
               les plus à gauche
        &      Opérateur ET
        ^      Opérateur OU exlusif
        |      Opérateur OU inclusif
        */

        // Opérateurs d'incrémentation
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        // Les opérateurs ++ et -- incrémentent et décrémentent respectivement
        // de 1.
        // S'ils sont placés avant la variable, ils incrémentent la variable puis
        // retournent la valeur. Placés après la varible, ils retournent la variable
        // puis l'incrémentent.
        System.out.println(i++); // i = 1, affiche 0 (pré-incrément)
        System.out.println(++i); // i = 2, affiche 2 (post-incrément)
        System.out.println(i--); // i = 1, affiche 2 (post-incrément)
        System.out.println(--i); // i = 0, affiche 0 (pré-incrément)

        ///////////////////////////////////////
        // Structures de contôles
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // Les instructions conditionnelle sont identiques aux langage C
        int j = 10;
        if (j == 10) {
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // Bouble while
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Incrémente le compteur
            // Itéré 100 fois, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Boucle do-while
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Incrémente le compteur
            // Itéré 99 fois, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // Boucle for
        // De la forme for(<start_statement>; <conditional>; <step>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Itéré 10 fois, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // Fin d'une boucle for avec un label
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // termine l'itération de la boucle englobante avec le label outer
            }
          }
        }

        // Boucle for-each
        // La boucle for est également capable d'itérer aussi bien sur un
        // tableau que sur des objets qui implémentent l'interface Iterable.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // De la forme: for (<object> : <iterable>)
        // Lu comme: "Pour chaque élément du tableau"
        // note: le type doit correspondre à celui de l'objet itérable
        for (int bar : fooList) {
            System.out.println(bar);
            //Itère 9 fois et affiche les chiffres de 1 à 9
        }

        // Le switch-case
        // Un switch fonctionne avec les données de type byte, short, char et
        // int.
        // On peut également utiliser le type Enum, la classe String et les
        // classes spéciales qui englobent les types primitifs (Character, Byte,
        // Short et Integer).
        // Depuis Java 7, on peut utiliser le type String.
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

        // try-with-resources (Java 7+)
        // Le mécanisme de gestion des erreurs try-catch-finally peut être
        // utilisé mais depuis Java 7 il est également possible d'utiliser
        // try-with-ressources.
        // try-with-resources simplifie try-catch-finally en fermant
        // automatiquement les ressources

        // Pour utiliser un try-with-resources, il suffit d'inclure l'instance
        // d'une classe qui implémente l'interface java.lang.AutoCloseable
        try (BufferedReader br = new BufferedReader(new FileReader("foo.txt"))) {
            // Ici, vous pouvez essayer de faire quelque chose qui lance une
            // exception.
            System.out.println(br.readLine());
            // Avec Java 7, la ressource sera toujours fermé, même si elle lance
            // une exception.
        } catch (Exception ex) {
            // La ressource sera fermé avant que le catch s'exécute.
            System.out.println("readLine() failed.");
        }
        // Il n'y a pas besoin de finally dans ce cas, l'objet BufferedReader
        // sera déjà fermé. Cela peut être utile dans certains cas spécifiques
        // où le code contenu dans finally ne serait pas exécuté.
        // Consulter la documention Oracle pour en savoir plus (en anglais) :
        // https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html


        // Expression ternaire
        // Vous pouvez utiliser l'opérateur ternaire '?' pour faire un
        // assignement rapide avec une condition logique.
        // Il faut lire "Si la (condition) est vraie alors utiliser la
        // <première valeur> sinon utilisez la <deuxième valeur>".
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println("bar : " + bar); // Affiche "bar : A", car la condition est vraie
        // Ou alors plus simplement
        System.out.println("bar : " + (foo < 10 ? "A" : "B")); // Affiche également "bar : A"

        ////////////////////////////////////////
        // Conversion de type
        ////////////////////////////////////////

        // Autoboxing

        // Convertir un objet String en un objet Integer
        Integer.parseInt("123"); // retourne un le type primitif int de 123

        // Convert Integer To String
        Integer.toString(123); // retourne un object String correspondant à"123"

        // Pour les autres conversions, référer vous aux classes suivantes:
        // Double
        // Long
        // String

        ///////////////////////////////////////
        // Classes et fonctions
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (voir plus loin pour la définition de la classe Bicycle)

        // Utilisez new pour instancier une classe
        Bicycle trek = new Bicycle();

        // Pour appeler une méthode de l'objet
        trek.speedUp(3); // !! Il est conseillé de passer par une méthode pour
        // changer la valeur d'une variable.
        trek.setCadence(100);

        // toString retourne une représentation de l'objet en chaîne de caractères.
        System.out.println("trek info: " + trek.toString());

        // Initialisation avec double accolades
        // Le langage Java ne permet pas de créer des collections statiques d'une
        // manière simple. Généralement, on utilise la forme suivante:
        private static final Set<String> COUNTRIES = new HashSet<String>();
        static {
           COUNTRIES.add("DENMARK");
           COUNTRIES.add("SWEDEN");
           COUNTRIES.add("FINLAND");
        }

        // Mais on peut le faire d'une manière plus habile, dite initialisation
        // avec double semi-colonnes
        private static final Set<String> COUNTRIES = new HashSet<String>() {{
            add("DENMARK");
            add("SWEDEN");
            add("FINLAND");
        }}

        // La première semi-colonne crée une classe anonyme et la deuxième est
        // un bloc d'initialisation du bloc. Ce dernier est appelé lorsque Copyright (c)
        // classe anonyme est crée. Cela ne fonctionne pas uniquement pour les
        // collections mais également pour toutes les classes n'étant pas
        // déclarées comme final.

    } // Fin de la méthode main
} // Fin de la class JavaFr

// Vous pouvez inclure des classes qui ne sont pas publics dans un fichier Java.
// Cependant, il est préférable de séparer les
// classes dans des fichiers différents.

// Syntaxe de déclaration des classes:
// <public/private/protected> class <Nom de la classe> {
//    // Les attributs, les constructeurs et les méthodes de la classe vont ici.
//    // Les functions de classes sont appelées méthode.
// }

class Bicycle {

    // Attributs et variables de la classe Bicycle
    public int cadence; // Public: Peut être accesible depuis n'importe où
    private int speed;  // Private: Accisible depuis la classe
    protected int gear; // Protected: Accisible depuis la classe et ses sous-
                        // classes
    String name; // default: Uniquement accesible depuis ce package
    static String className; // Variable de classe static

    // Bloc static
    // Java n'a pas d'implémentation pour les constructeurs statiques mais
    // possède le bloc static qui peut être utilisé pour initialiser les
    // variables de classe.
    // Ce bloc sera appelé lorsque la classe sera chargée.
    static {
        className = "Bicycle";
    }

    // Les constructeurs sont un moyen de créer les classe
    // Ceci est le constructeur de la classe Bicycle
    public Bicycle() {
        // Vous pouvez aussie appeler un autre constructeur. Par exemple en
        // appelant le constructeur de la classe mère (voir héritage):
        // this(1, 50, 5, "Bontrager");
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }
    // Le constructeur peut prendre plusieurs arguments
    public Bicycle(int startCadence, int startSpeed, int startGear,
        String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Syntaxe d'une méthode :
    // <public/private/protected> <type de retour> <nom de la fonction>(
    // <arguments>)

    // Les classes Java possèdent souvent des accesseurs (getters) et mutateurs
    // (setters) pour leurs attributs.

    public int getCadence() {
        return cadence;
    }

    // Les méthodes void ne retourne aucune valeur
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

    // Méthode pour afficher la valeur des attributs de l'objet. @Override est
    // une annotation (voir plus loin).
    @Override //On dit ici qu'on remplace la méthode de la classe Objet.
    public String toString() {
        return "gear: " + gear + " cadence: " + cadence + " speed: " + speed +
            " name: " + name;
    }
} // Fin de la classe Bicycle

// PennyFarthing est une sous-classe de Bicycle
class PennyFarthing extends Bicycle {
    // (Les Penny Farthings sont des bicyclette avec une grande roue avant.
    // Il n'y a pas de roue libre, le cycliste est obligé de pédaler en
    // permanence.)

    public PennyFarthing(int startCadence, int startSpeed) {
        // Appelez le constructeur parent avec la méthode super()
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // Ici nous modifions la méthode setGear() de la classe mère. Il faut donc
    // utiliser l'annotation @Overide. Pour en savoir plus sur les annotations,
    // consulter la documention officiel (en anglais) :
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        this.gear = 0;
    }
}

// Polymorphisme (cast d'objets)
// Comme la classe PennyFarthing héritent de la classe Bicycle, on peut dire
// qu'un PennyFarthing est un Bicycle (un vélo en anglais) et écrire :
// Bicycle bicycle = new PennyFarthing();
// Le polymorphisme est la capacité d'un objet de se faire passer pour un autre.
// Vous pouvez consulter la documentation Oracle pour plus de détails et
// concepts (en anglais) :
// https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html

// Interfaces
// Déclaration d'une interface
// <niveau d'accès> interface <nom de l'interface> extends <nom de l'interface
// mère> {
//     // Constantes
//     // Délaration des méthodes
// }

// Exemple - Toute nourriture peut être mangée et digégée différemment
// L'interface Edible (traduction : comestible) décrit l'action de manger
public interface Edible {
    public void eat(); // Toute classe qui implémente cette interface doit
                       // implémenter cette méthode
}

// L'interface Digestible décrit l'action de digérer
public interface Digestible {
    public void digest();
    // Depuis Java 8, les interfaces peuvent avoir des méthodes par défaut.
    public void defaultMethod() {
        System.out.println("Hi from default method ...");
    }
}

// On peut maintenant créer une classe qui implémente chacune de ces interfaces.
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

// En Java, on peut hériter uniquement d'une classe mais on peut implémenter
// plusieurs interfaces:
public class ExampleClass extends ExampleClassParent implements InterfaceOne,
    InterfaceTwo {
    @Override
    public void InterfaceOneMethod() {
    }

    @Override
    public void InterfaceTwoMethod() {
    }

}

// Classes abstraites

// Syntaxe de déclaration:
// <niveau d'accès> abstract class <nom de la classe abstraite> extends <nom de la
//   classe mère abstraite> {
//     // Constantes et variables
//     // Méthodes
// }

// Une classe abstraite contient au moins une méthode abstraite qui doit être
// définee dans la classe fille. Comme les interfaces, les classes abstraites ne
// peuvent pas être instanciées mais doivent être étendues avec les méthodes
// abstraites implémentées. À la différence des interfaces, une classe abstraite
// peut contenir des méthodes abstraites ou non-abstraites. Les méthodes dans une
// interfaces ne peuvent pas être implémentées à l'exception des méthodes static.
// Les variables d'une classe abstraite sont déclarées comme final par défaut à
// l'opposé des interfaces. Finalement les classes abstraites peuvent avoir une
// méthode main.
public abstract class Animal
{
    public abstract void makeSound();

    // Les méthodes peuvent avoir une implémentation dans une classe abstraite.
    public void eat()
    {
        System.out.println("I am an animal and I am Eating.");
        // Note: On peut accéder à une variable privée ici.
        age = 30;
    }

    // On n'a pas besoin d'initialiser les variables dans les classe abstraites.
    // Cependant, dans une interfaces, les variables sont implicitement
    // déclarées comme final et doivent donc être initialisées.
    private int age;

    public void printAge()
    {
        System.out.println(age);
    }

    // Les classes abstraites peuvent avoir une fonction main.
    public static void main(String[] args)
    {
        System.out.println("I am abstract");
    }
}

class Dog extends Animal
{
    // On doit également utiliser l'annotation @Override lors de la surchage de
    // la méthode abstraite d'une classe abstraite.
    @Override
    public void makeSound()
    {
        System.out.println("Bark");
        // age = 30;    ==> ERREUR!    age est privé et n'est pas accesible.
    }

    // NOTE: Vous obtiendrez une erreur si vous utilisé l'annotation @Override
    // ici car Java n'autorise pas la surcharge de méthodes statiques. Ce qui ce
    // passe est appelé "method hiding". Si vous voulez en savoir plus,
    // consultez cette discussion (en anglais) :
    // http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Dog pluto = new Dog();
        pluto.makeSound();
        pluto.eat();
        pluto.printAge();
    }
}

// Classes finales

// Syntaxe de déclaration
// <niveau d'accès> final <nom de la classe final> {
//     // Constantes et variables
//     // Méthodes déclarations
// }

// Les classe déclarées comme final ne peuvent pas avoir de classe fille. Elles
// peuvent être considérées comme l'opposé des classes abstraites.
public final class SaberToothedCat extends Animal
{
    // On doit également utiliser l'annotation @Override lors de la surchage de
    // la méthode abstraite d'une classe abstraite.
    @Override
    public void makeSound()
    {
        System.out.println("Roar");
    }
}

// Méthodes final
public abstract class Mammal()
{
    // Syntaxe:
    // <niveau d'accès> final <type de retour> <nom de la fonction>(<arguments>)

    // Les méthodes déclarées comme final ne peuvent pas être surchargées par
    // une classe fille et en sont donc l'implémentation finale.
    public final boolean isWarmBlooded()
    {
        return true;
    }
}

// Enumérations
//
// Le type enum est un type de donnée spécial qui permet à une variable de ne
// prendre que certaines valeurs prédéfinies. La variable doit être égales à une
// des valeurs pédéfinies pour celle-ci. En Java, les variables constantes sont
// notées en majuscules.
// On définie un type enum en utilisant le mot clé enum. Par exemple pour les
// jours de l'année:
public enum Day {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY,
    THURSDAY, FRIDAY, SATURDAY
}

// On l'utilise ainsi:
public class EnumTest {
    // On utilise notre énumération
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
        firstDay.tellItLikeItIs(); // => affiche "Mondays are bad"
        EnumTest thirdDay = new EnumTest(Day.WEDNESDAY);
        thirdDay.tellItLikeItIs(); // => affiche "Midweek days are so-so"
    }
}

// Le type enum permet de faire bien plus que ce qui est montré ici. Il ne se
// limite pas à une liste de constante mais peut inclure des champs et méthodes.
// Vous pouvez en savoir plus ici (en anglais):
//https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Pour aller plus loin (en anglais)

Les liens ci-dessous sont données si vous souhaitez approfondir sur le sujet,
n'hésitez pas à consulter Google pour trouver des exemples spécifiques.

**Guides officiels d'Oracle**:

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

* Nouvelles fonctionnalités Java 8:
    * [Lambda expressions (functional programming)](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
    * [Date and time API (java.time package)](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)

**Pratiquer en ligne et tutoriels**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)

**Livres**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](https://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](https://www.amazon.com/gp/product/0071606300)
