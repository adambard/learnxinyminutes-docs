---
name: Java
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
    - ["Shawn M. Hanes", "https://github.com/smhanes15"]
    - ["E. F. Souza Lima", "https://github.com/cosmiclm"]
filename: LearnJava.java
---

Java is a general-purpose, concurrent, class-based, object-oriented computer
programming language.
[Read more here.](https://docs.oracle.com/javase/tutorial/java/)

```java
// Single-line comments start with //

/*
Multi-line comments look like this.
*/

/**
 * JavaDoc comments look like this. Used to describe the Class or various
 * attributes of a Class.
 * Main attributes:
 *
 * @author         Name (and contact information such as email) of author(s).
 * @version     Current version of the program.
 * @since        When this part of the program was first added.
 * @param         For describing the different parameters for a method.
 * @return        For describing what the method returns.
 * @deprecated  For showing the code is outdated or shouldn't be used.
 * @see         Links to another part of documentation.
*/

// Import ArrayList class inside of the java.util package
import java.util.ArrayList;
// Import all classes inside of java.security package
import java.security.*;
// Java to illustrate calling of static members and methods without calling classname
import static java.lang.Math.*;
import static java.lang.System.*;

public class LearnJava {

    // In order to run a java program, it must have a main method as an entry
    // point.
    public static void main(String[] args) {

    ///////////////////////////////////////
    // Input/Output
    ///////////////////////////////////////

        /*
        * Output
        */

        // Use System.out.println() to print lines.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // To print without a newline, use System.out.print().
        System.out.print("Hello ");
        System.out.print("World");

        // Use System.out.printf() for easy formatted printing.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        /*
         * Input
         */

        // use Scanner to read input
        // must import java.util.Scanner;
        Scanner scanner = new Scanner(System.in);

        // read string input
        String name = scanner.next();

        // read byte input
        byte numByte = scanner.nextByte();

        // read int input
        int numInt = scanner.nextInt();

        // read long input
        long numLong = scanner.nextLong();

        // read float input
        float numFloat = scanner.nextFloat();

        // read double input
        double numDouble = scanner.nextDouble();

        // read boolean input
        boolean bool = scanner.nextBoolean();

        ///////////////////////////////////////
        // Variables
        ///////////////////////////////////////

        /*
        *  Variable Declaration
        */
        // Declare a variable using <type> <name>
        int fooInt;
        // Declare multiple variables of the same
        // type <type> <name1>, <name2>, <name3>
        int fooInt1, fooInt2, fooInt3;

        /*
        *  Variable Initialization
        */

        // Initialize a variable using <type> <name> = <val>
        int barInt = 1;
        // Initialize multiple variables of same type with same
        // value <type> <name1>, <name2>, <name3>
        // <name1> = <name2> = <name3> = <val>
        int barInt1, barInt2, barInt3;
        barInt1 = barInt2 = barInt3 = 1;
        // Shorthand for multiple declarations
        int barInt4 = 1, barInt5 = 2; 


        /*
        *  Variable types
        */
        // Byte - 8-bit signed two's complement integer
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // If you would like to interpret a byte as an unsigned integer
        // then this simple operation can help
        int unsignedIntLessThan256 = 0xff & fooByte;
        // this contrasts a cast which can be negative.
        int signedInt = (int) fooByte;

        // Short - 16-bit signed two's complement integer
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 32-bit signed two's complement integer
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int bazInt = 1;

        // Long - 64-bit signed two's complement integer
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L is used to denote that this variable value is of type Long;
        // anything without is treated as integer by default.

        // Note: byte, short, int and long are signed. They can have positive and negative values.
        // There are no unsigned variants.
        // char, however, is 16-bit unsigned.

        // Float - Single-precision 32-bit IEEE 754 Floating Point
        // 2^-149 <= float <= (2-2^-23) * 2^127
        float fooFloat = 234.5f;
        // f or F is used to denote that this variable value is of type float;
        // otherwise it is treated as double.

        // Double - Double-precision 64-bit IEEE 754 Floating Point
        // 2^-1074 <= x <= (2-2^-52) * 2^1023
        double fooDouble = 123.4;

        // Boolean - true & false
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - A single 16-bit Unicode character
        char fooChar = 'A';

        // final variables can't be reassigned,
        final int HOURS_I_WORK_PER_WEEK = 9001;
        // but they can be initialized later.
        final double E;
        E = 2.71828;

        // BigInteger - Immutable arbitrary-precision integers
        //
        // BigInteger is a data type that allows programmers to manipulate
        // integers longer than 64-bits. Integers are stored as an array of
        // bytes and are manipulated using functions built into BigInteger
        //
        // BigInteger can be initialized using an array of bytes or a string.
        byte[] fooByteArray = {2, 5, 6};
        BigInteger fooBigInteger = new BigInteger(fooByteArray);

        // BigDecimal - Immutable, arbitrary-precision signed decimal number
        //
        // A BigDecimal takes two parts: an arbitrary precision integer
        // unscaled value and a 32-bit integer scale
        //
        // BigDecimal allows the programmer complete control over decimal
        // rounding. It is recommended to use BigDecimal with currency values
        // and where exact decimal precision is required.
        //
        // BigDecimal can be initialized with an int, long, double or String
        // or by initializing the unscaled value (BigInteger) and scale (int).
        int scale = 2; // e.g. scale 2 means 2 digits after the decimal point
        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, scale);

        // Be wary of the constructor that takes a float or double as
        // the inaccuracy of the float/double will be copied in BigDecimal.
        // Prefer the String constructor when you need an exact value.
        BigDecimal tenCents = new BigDecimal("0.1");

        // Type inference with 'var'
        var x = 100; // int
        var y = 1.90; // double
        var z = 'a'; // char
        var p = "tanu"; // String
        var q = false; // boolean

        // Strings
        String fooString = "My String Is Here!";

        // Text blocks
        var textBlock = """
                        This is a <Text Block> in Java 
                        """;

        // \n is an escaped character that starts a new line
        String barString = "Printing on a new line?\nNo Problem!";
        // \t is an escaped character that adds a tab character
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // String Building
        // #1 - with plus operator
        // That's the basic way to do it (optimized under the hood)
        String plusConcatenated = "Strings can " + "be concatenated " + "via + operator.";
        System.out.println(plusConcatenated);
        // Output: Strings can be concatenated via + operator.

        // #2 - with StringBuilder
        // This way doesn't create any intermediate strings. It just stores the string pieces, and ties them together
        // when toString() is called.
        // Hint: This class is not thread safe. A thread-safe alternative (with some impact on performance) is StringBuffer.
        StringBuilder builderConcatenated = new StringBuilder();
        builderConcatenated.append("You ");
        builderConcatenated.append("can use ");
        builderConcatenated.append("the StringBuilder class.");
        System.out.println(builderConcatenated.toString()); // only now is the string built
        // Output: You can use the StringBuilder class.

        // StringBuilder is efficient when the fully constructed String is not required until the end of some processing.
        StringBuilder stringBuilder = new StringBuilder();
        String inefficientString = "";
        for (int i = 0 ; i < 10; i++) {
            stringBuilder.append(i).append(" ");
            inefficientString += i + " ";
        }
        System.out.println(inefficientString);
        System.out.println(stringBuilder.toString());
        // inefficientString requires a lot more work to produce, as it generates a String on every loop iteration.
        // Simple concatenation with + is compiled to a StringBuilder and toString()
        // Avoid string concatenation in loops.

        // #3 - with String formatter
        // Another alternative way to create strings. Fast and readable.
        String.format("%s may prefer %s.", "Or you", "String.format()");
        // Output: Or you may prefer String.format().

        // Arrays
        // The array size must be decided upon instantiation
        // The following formats work for declaring an array
        // <datatype>[] <var name> = new <datatype>[<array size>];
        // <datatype> <var name>[] = new <datatype>[<array size>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Another way to declare & initialize an array
        int[] intArray2 = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = {true, false, false};

        // Indexing an array - Accessing an element
        System.out.println("intArray @ 0: " + intArray[0]);

        // Arrays are zero-indexed and mutable.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Other data types worth checking out
        // ArrayLists - Like arrays except more functionality is offered, and
        //              the size is mutable.
        // LinkedLists - Implementation of doubly-linked list. All of the
        //               operations perform as could be expected for a
        //               doubly-linked list.
        // Maps - A mapping of key Objects to value Objects. Map is
        //        an interface and therefore cannot be instantiated.
        //        The type of keys and values contained in a Map must
        //        be specified upon instantiation of the implementing
        //        class. Each key may map to only one corresponding value,
        //        and each key may appear only once (no duplicates).
        // HashMaps - This class uses a hashtable to implement the Map
        //            interface. This allows the execution time of basic
        //            operations, such as get and insert element, to remain
        //            constant-amortized even for large sets.
        // TreeMap - A Map that is sorted by its keys. Each modification
        //           maintains the sorting defined by either a Comparator
        //           supplied at instantiation, or comparisons of each Object
        //           if they implement the Comparable interface.
        //           Failure of keys to implement Comparable combined with failure to
        //           supply a Comparator will throw ClassCastExceptions.
        //           Insertion and removal operations take O(log(n)) time
        //           so avoid using this data structure unless you are taking
        //           advantage of the sorting.

        ///////////////////////////////////////
        // Operators
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2;

        // Arithmetic is straightforward
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int returns int)
        System.out.println("1/2.0 = " + (i1 / (double)i2)); // => 0.5

        // Modulo
        System.out.println("11%3 = " + (11 % 3)); // => 2

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
        while (fooWhile < 100) {
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
            // Iterated 100 times, fooDoWhile 0->99
            fooDoWhile++;
        } while (fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // For Loop
        // for loop structure => for(<start_statement>; <conditional>; <step>)
        // Note: fooFor is declared here, before the loop, so it is still in
        // scope afterwards. A variable declared in the loop's own
        // <start_statement> only lives for the duration of the loop.
        int fooFor;
        for (fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Iterated 10 times, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // Nested For Loop Exit with Label
        // (m, n avoid clashing with the i, j already declared above)
        outer:
        for (int m = 0; m < 10; m++) {
          for (int n = 0; n < 10; n++) {
            if (m == 5 && n == 5) {
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
        // Starting in Java 7 and above, we can also use the String type.
        // Note: Do remember that, not adding "break" at end any particular case ends up in
        // executing the very next case(given it satisfies the condition provided) as well.
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


        // Try-with-resources (Java 7+)
        // Try-catch-finally statements work as expected in Java but in Java 7+
        // the try-with-resources statement is also available. Try-with-resources
        // simplifies try-catch-finally statements by closing resources
        // automatically.

        // In order to use a try-with-resources, include an instance of a class
        // in the try statement. The class must implement java.lang.AutoCloseable.
        try (BufferedReader br = new BufferedReader(new FileReader("foo.txt"))) {
            // You can attempt to do something that could throw an exception.
            String firstLine = br.readLine(); // can throw IOException
            int lineAsNumber = Integer.parseInt(firstLine); // or NumberFormatException
            System.out.println(lineAsNumber);
            // In Java 7, the resource will always be closed, even if it throws
            // an Exception.
        } catch (IOException | NumberFormatException ex) {
            // Java 7+ Multi catch block handle both exceptions
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
        System.out.println("bar : " + bar); // Prints "bar : A", because the
        // statement is true.
        // Or simply
        System.out.println("bar : " + (foo < 10 ? "A" : "B"));


        ////////////////////////////////////////
        // Converting Data Types
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
    } // End main method

    private static class TestInitialization {
        // Double Brace Initialization
        // Before Java 11, the Java Language had no syntax for how to create
        // static Collections in an easy way. Usually you end up like this:
        private static final Set<String> COUNTRIES = new HashSet<String>();
        static {
           COUNTRIES.add("DENMARK");
           COUNTRIES.add("SWEDEN");
           COUNTRIES.add("FINLAND");
        }

        // There's a nifty way to achieve the same thing, 
        // by using something that is called Double Brace Initialization.
        private static final Set<String> COUNTRIES_DOUBLE_BRACE = 
        new HashSet<String>() {{
            add("DENMARK");
            add("SWEDEN");
            add("FINLAND");
        }};

        // The first brace creates a new AnonymousInnerClass and the second 
        // one declares an instance initializer block. This block is called 
        // when the anonymous inner class is created. 
        // However, this is considered an anti-pattern: it creates a new class 
        // for every instance, increasing memory overhead (Metaspace) 
        // and potentially causing memory leaks by holding an implicit 
        // reference to the enclosing class. Use Set.of() in Java 9+.

        // Another option was to initialize the Collection from an array,
        // using Arrays.asList() method:
        private static final List<String> COUNTRIES_AS_LIST = 
                        Arrays.asList("SWEDEN", "DENMARK", "NORWAY");
        // This has one catch: the list we get is internally backed by the array,
        // and since arrays can't change their size, the list backed by the array
        // is not resizeable, which means we can't add new elements to it: 
        public static void main(String[] args) {
            COUNTRIES_AS_LIST.add("FINLAND"); // throws UnsupportedOperationException!
            // However, we can replace elements by index, just like in an array:
            COUNTRIES_AS_LIST.set(1, "FINLAND");
            System.out.println(COUNTRIES_AS_LIST); // prints [SWEDEN, FINLAND, NORWAY]
        }
        // The resizing problem can be circumvented 
        // by creating another Collection from the List:
         private static final Set<String> COUNTRIES_SET = 
                new HashSet<>(Arrays.asList("SWEDEN", "DENMARK", "NORWAY"));
        // It's perfectly fine to add anything to the Set of COUNTRIES now. 
    } // End TestInitialization class

    private static class TestJava11Initialization {
        // Since Java 11, there is a convenient option to initialize Collections:
        // Set.of() and List.of() methods. 
        private static final Set<String> COUNTRIES = 
                Set.of("SWEDEN", "DENMARK", "NORWAY");
        // There is a massive catch, though: Lists and Sets initialized like this 
        // 1) are immutable 
        // 2) can't contain null elements (even check for null elements fails)!
        public static void main(String[] args) {
            COUNTRIES.add("FINLAND"); // throws UnsupportedOperationException
            COUNTRIES.remove("NORWAY"); // throws UnsupportedOperationException 
            COUNTRIES.contains(null); // throws NullPointerException
        }
        private static final Set<String> COUNTRIES_WITH_NULL = 
                    Set.of("SWEDEN", null, "NORWAY"); // throws NullPointerException

    } // End TestJava11Initialization class
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

// Object casting
// Since the PennyFarthing class is extending the Bicycle class, we can say
// a PennyFarthing is a Bicycle and write :
// Bicycle bicycle = new PennyFarthing();
// This is called object casting where an object is taken for another one. There
// are lots of details and deals with some more intermediate concepts here:
// https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html

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
    // Since Java 8, interfaces can have default method.
    public default void defaultMethod() {
        System.out.println("Hi from default method ...");
    }
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
// <access-level> abstract class <abstract-class-name> extends
// <super-abstract-classes> {
//     // Constants and variables
//     // Method declarations
// }

// Abstract Classes cannot be instantiated.
// Abstract classes may define abstract methods.
// Abstract methods have no body and are marked abstract
// Non-abstract child classes must @Override all abstract methods
// from their super-classes.
// Abstract classes can be useful when combining repetitive logic
// with customised behavior, but as Abstract classes require
// inheritance, they violate "Composition over inheritance"
// so consider other approaches using composition.
// https://en.wikipedia.org/wiki/Composition_over_inheritance

public abstract class Animal
{
    private int age;

    public abstract void makeSound();

    // Method can have a body
    public void eat()
    {
        System.out.println("I am an animal and I am Eating.");
        // Note: We can access private variable here.
        age = 30;
    }

    public void printAge()
    {
        System.out.println(age);
    }

    // Abstract classes can have main method.
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
public abstract class Mammal
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

// Records (Java 16+)
//
// A record is a concise way to declare an immutable data carrier class. The
// compiler generates a canonical constructor, private final fields, public
// accessor methods (name(), not getName()), equals(), hashCode() and
// toString() automatically.
public record Person(String name, int age) {
    // A compact constructor lets you validate/normalize the fields that the
    // canonical constructor will assign; you don't restate the assignments.
    public Person {
        if (age < 0) {
            throw new IllegalArgumentException("age can't be negative");
        }
        name = name.trim();
    }

    // Records can have extra fields (must be static), constructors, methods,
    // and can implement interfaces, but cannot extend another class (they
    // implicitly extend java.lang.Record) and cannot declare extra instance
    // fields beyond the ones in the header.
    public String greeting() {
        return "Hi, I'm " + name;
    }

    public static Person unknown() {
        return new Person("Unknown", 0);
    }
}

class RecordDemo {
    public static void main(String[] args) {
        Person p = new Person("Alice", 30);
        System.out.println(p.name()); // => Alice (auto-generated accessor)
        System.out.println(p); // => Person[name=Alice, age=30] (toString)
        System.out.println(p.equals(new Person("Alice", 30))); // => true
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

// Getting Started with Lambda Expressions
//
// New to Java version 8 are lambda expressions. Lambdas are more commonly found
// in functional programming languages, which means they are methods which can
// be created without belonging to a class, passed around as if it were itself
// an object, and executed on demand.
//
// Final note, lambdas must implement a functional interface. A functional
// interface is one which has only a single abstract method declared. It can
// have any number of default methods. Lambda expressions can be used as an
// instance of that functional interface. Any interface meeting the requirements
// is treated as a functional interface. You can read more about interfaces
// above.
//
import java.util.Map;
import java.util.HashMap;
import java.util.function.*;
import java.security.SecureRandom;

public class Lambdas {
    public static void main(String[] args) {
        // Lambda declaration syntax:
        // <zero or more parameters> -> <expression body or statement block>

        // We will use this hashmap in our examples below.
        Map<String, String> planets = new HashMap<>();
            planets.put("Mercury", "87.969");
            planets.put("Venus", "224.7");
            planets.put("Earth", "365.2564");
            planets.put("Mars", "687");
            planets.put("Jupiter", "4,332.59");
            planets.put("Saturn", "10,759");
            planets.put("Uranus", "30,688.5");
            planets.put("Neptune", "60,182");

        // Lambda with zero parameters using the Supplier functional interface
        // from java.util.function.Supplier. The actual lambda expression is
        // what comes after numPlanets =.
        Supplier<String> numPlanets = () -> Integer.toString(planets.size());
        System.out.format("Number of Planets: %s\n\n", numPlanets.get());

        // Lambda with one parameter and using the Consumer functional interface
        // from java.util.function.Consumer. This is because planets is a Map,
        // which implements both Collection and Iterable. The forEach used here,
        // found in Iterable, applies the lambda expression to each member of
        // the Collection. The default implementation of forEach behaves as if:
        /*
            for (T t : this)
                action.accept(t);
        */

        // The actual lambda expression is the parameter passed to forEach.
        planets.keySet().forEach((p) -> System.out.format("%s\n", p));

        // If you are only passing a single argument, then the above can also be
        // written as (note absent parentheses around p):
        planets.keySet().forEach(p -> System.out.format("%s\n", p));

        // Tracing the above, we see that planets is a HashMap, keySet() returns
        // a Set of its keys, forEach applies each element as the lambda
        // expression of: (parameter p) -> System.out.format("%s\n", p). Each
        // time, the element is said to be "consumed" and the statement(s)
        // referred to in the lambda body is applied. Remember the lambda body
        // is what comes after the ->.

        // The above without use of lambdas would look more traditionally like:
        for (String planet : planets.keySet()) {
            System.out.format("%s\n", planet);
        }

        // This example differs from the above in that a different forEach
        // implementation is used: the forEach found in the HashMap class
        // implementing the Map interface. This forEach accepts a BiConsumer,
        // which generically speaking is a fancy way of saying it handles
        // the Set of each Key -> Value pairs. This default implementation
        // behaves as if:
        /*
            for (Map.Entry<K, V> entry : map.entrySet())
                action.accept(entry.getKey(), entry.getValue());
        */

        // The actual lambda expression is the parameter passed to forEach.
        String orbits = "%s orbits the Sun in %s Earth days.\n";
        planets.forEach((K, V) -> System.out.format(orbits, K, V));

        // The above without use of lambdas would look more traditionally like:
        for (String planet : planets.keySet()) {
            System.out.format(orbits, planet, planets.get(planet));
        }

        // Or, if following more closely the specification provided by the
        // default implementation:
        for (Map.Entry<String, String> planet : planets.entrySet()) {
            System.out.format(orbits, planet.getKey(), planet.getValue());
        }

        // These examples cover only the very basic use of lambdas. It might not
        // seem like much or even very useful, but remember that a lambda can be
        // created as an object that can later be passed as parameters to other
        // methods.
    }
}

///////////////////////////////////////
// Generics
///////////////////////////////////////

// Generics let you write classes, interfaces and methods that are
// parameterized over a type, giving compile-time type safety without
// duplicating code for every concrete type you want to support.

// T is a placeholder for a type supplied when Box is used.
class Box<T> {
    private T content;

    public void set(T content) {
        this.content = content;
    }

    public T get() {
        return content;
    }
}

// Multiple type parameters are allowed.
class Pair<K, V> {
    private final K key;
    private final V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }

    public K getKey() { return key; }
    public V getValue() { return value; }
}

// Bounded type parameters restrict T to a type (or a subtype of it). Here T
// must be a Number or one of its subclasses, so doubleValue() is callable.
class NumericBox<T extends Number> {
    private final T number;

    public NumericBox(T number) {
        this.number = number;
    }

    public double doubled() {
        return number.doubleValue() * 2;
    }
}

class GenericsDemo {
    // A generic method: the <T> before the return type declares a type
    // parameter scoped to this method only, inferred from the argument.
    static <T> T firstOf(List<T> list) {
        return list.get(0);
    }

    // Wildcards:
    // ? extends T -> some subtype of T; read-only access ("producer").
    // ? super T   -> some supertype of T; write-only access ("consumer").
    static double sumOfList(List<? extends Number> list) {
        double sum = 0;
        for (Number n : list) {
            sum += n.doubleValue();
        }
        return sum;
    }

    public static void main(String[] args) {
        Box<String> stringBox = new Box<>();
        stringBox.set("hello");
        System.out.println(stringBox.get()); // => hello

        Pair<String, Integer> pair = new Pair<>("age", 30);
        System.out.println(pair.getKey() + " = " + pair.getValue());

        NumericBox<Integer> numBox = new NumericBox<>(21);
        System.out.println(numBox.doubled()); // => 42.0

        System.out.println(firstOf(List.of(1, 2, 3))); // => 1
        System.out.println(sumOfList(List.of(1, 2, 3))); // => 6.0
    }
}

///////////////////////////////////////
// Exceptions
///////////////////////////////////////

// Java distinguishes checked exceptions (subclasses of Exception but not
// RuntimeException; must be declared with `throws` or caught) from
// unchecked exceptions (subclasses of RuntimeException; no declaration
// required) and Errors (serious problems like OutOfMemoryError, not meant
// to be caught).

// A custom checked exception.
class InsufficientFundsException extends Exception {
    public InsufficientFundsException(String message) {
        super(message);
    }
}

// A custom unchecked exception.
class InvalidAmountException extends RuntimeException {
    public InvalidAmountException(String message) {
        super(message);
    }
}

class BankAccount {
    private double balance;

    public BankAccount(double balance) {
        this.balance = balance;
    }

    // Checked exceptions must be declared in the method signature; callers
    // are then forced to catch them or re-declare them.
    public void withdraw(double amount) throws InsufficientFundsException {
        if (amount < 0) {
            // Unchecked - no `throws` declaration required.
            throw new InvalidAmountException("amount can't be negative");
        }
        if (amount > balance) {
            throw new InsufficientFundsException("not enough funds");
        }
        balance -= amount;
    }
}

class ExceptionsDemo {
    public static void main(String[] args) {
        BankAccount account = new BankAccount(100);
        try {
            account.withdraw(150);
        } catch (InsufficientFundsException e) {
            System.out.println("Caught: " + e.getMessage());
        } finally {
            // finally always runs, whether or not an exception was thrown -
            // useful for cleanup that must happen regardless.
            System.out.println("Transaction attempt finished.");
        }
    }
}

///////////////////////////////////////
// Collections & Comparator
///////////////////////////////////////

// The Collections Framework (java.util) is organized around a few core
// interfaces: List (ordered, allows duplicates), Set (no duplicates), Map
// (key -> value), and Queue/Deque (FIFO/LIFO access).

import java.util.Set;
import java.util.HashSet;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.Comparator;
import java.util.Collections;

class CollectionsDemo {
    public static void main(String[] args) {
        // List - ordered, index-accessible, allows duplicates. ArrayList is
        // backed by a resizable array: fast random access, slower inserts.
        List<String> fruits = new ArrayList<>();
        fruits.add("banana");
        fruits.add("apple");
        fruits.add("cherry");

        // Set - no duplicates.
        Set<String> hashSet = new HashSet<>(fruits); // no order guarantee
        Set<String> treeSet = new TreeSet<>(fruits); // sorted, natural order
        System.out.println(treeSet); // => [apple, banana, cherry]

        // Map - keys map to values, keys are unique. TreeMap keeps keys
        // sorted; HashMap (seen earlier) makes no ordering guarantee.
        Map<String, Integer> stock = new TreeMap<>();
        stock.put("apple", 1);
        stock.put("banana", 2);
        System.out.println(stock); // => {apple=1, banana=2}

        // Comparator defines a custom ordering without touching the
        // element's own compareTo(), so the same data can be sorted
        // multiple different ways.
        List<String> byLength = new ArrayList<>(fruits);
        byLength.sort(Comparator.comparingInt(String::length));
        System.out.println(byLength);

        // Comparators compose: sort by length, break ties alphabetically,
        // then reverse the whole ordering.
        byLength.sort(
            Comparator.comparingInt(String::length)
                .thenComparing(Comparator.naturalOrder())
                .reversed());
        System.out.println(byLength);

        // The Collections utility class holds common algorithms that work
        // over any List/Set/Map.
        Collections.sort(fruits);
        Collections.reverse(fruits);
        System.out.println("max: " + Collections.max(fruits));
    }
}

///////////////////////////////////////
// Streams & Optional (Java 8+)
///////////////////////////////////////

import java.util.stream.Collectors;
import java.util.Optional;

class StreamsDemo {
    public static void main(String[] args) {
        List<String> names = List.of("Ann", "Bob", "Cleo", "Dan", "Eve");

        // A Stream describes a pipeline over a source of data. Intermediate
        // operations (filter, map, sorted...) are lazy and return a new
        // Stream; a terminal operation (collect, forEach, reduce...)
        // triggers the actual processing, exactly once.
        List<String> shortNamesUpper = names.stream()
            .filter(name -> name.length() <= 3)
            .map(String::toUpperCase)
            .sorted()
            .collect(Collectors.toList());
        System.out.println(shortNamesUpper); // => [ANN, BOB, DAN, EVE]

        // mapToInt + sum is a common way to reduce a stream to one number.
        int totalLength = names.stream().mapToInt(String::length).sum();
        System.out.println(totalLength);

        // Collectors.groupingBy partitions elements into a Map keyed by a
        // classifier function.
        Map<Integer, List<String>> byLength =
            names.stream().collect(Collectors.groupingBy(String::length));
        System.out.println(byLength); // => {3=[Ann, Bob, Dan, Eve], 4=[Cleo]}

        // Streams are single-use: once a terminal operation runs, that
        // stream is consumed and can't be reused.

        // Optional<T> represents a value that may or may not be present -
        // an explicit, type-checked alternative to returning null.
        Optional<String> zName = names.stream()
            .filter(name -> name.startsWith("Z"))
            .findFirst();
        System.out.println(zName.isPresent()); // => false
        System.out.println(zName.orElse("none found")); // => none found

        // ifPresent avoids an explicit isPresent()/get() pair.
        names.stream().findFirst().ifPresent(System.out::println);
    }
}

///////////////////////////////////////
// Pattern Matching & Sealed Types (Java 16+ / 17+ / 21+)
///////////////////////////////////////

// Sealed interfaces/classes (Java 17+) restrict which classes may implement
// or extend them, listed explicitly via `permits`. That lets the compiler
// verify a switch over the whole hierarchy is exhaustive, with no `default`
// branch needed.
sealed interface Shape permits Circle, Square, Rectangle {}
record Circle(double radius) implements Shape {}
record Square(double side) implements Shape {}
record Rectangle(double width, double height) implements Shape {}

class PatternMatchingDemo {
    // Pattern matching for instanceof (Java 16+): the cast and the new
    // variable declaration happen in one step. `str` is only in scope where
    // the compiler can prove the instanceof check was true.
    static void printLengthIfString(Object obj) {
        if (obj instanceof String str) {
            System.out.println(str.length());
        }
    }

    // Switch expressions (Java 14+) use `->` (no fall-through) and can
    // return a value directly, instead of assigning inside every case.
    static String dayType(int day) {
        return switch (day) {
            case 6, 7 -> "Weekend";
            default -> "Weekday";
        };
    }

    // Pattern matching for switch (Java 21+) combines type patterns, record
    // deconstruction, and exhaustiveness checking over a sealed hierarchy.
    // No default branch is needed: Circle/Square/Rectangle are the only
    // types Shape permits, so the compiler can prove every case is covered.
    static double area(Shape shape) {
        return switch (shape) {
            case Circle c -> Math.PI * c.radius() * c.radius();
            case Square s -> s.side() * s.side();
            // Record deconstruction: pull width/height straight out of the
            // pattern instead of calling the accessors yourself.
            case Rectangle(double w, double h) -> w * h;
        };
    }

    public static void main(String[] args) {
        printLengthIfString("hello"); // => 5
        System.out.println(dayType(6)); // => Weekend
        System.out.println(area(new Circle(2))); // => 12.566370614359172
        System.out.println(area(new Rectangle(3, 4))); // => 12.0
    }
}

///////////////////////////////////////
// Nested, Inner, Local & Anonymous Classes
///////////////////////////////////////

class Outer {
    private int outerField = 10;

    // A static nested class holds no reference to an Outer instance; it's
    // effectively a regular class scoped inside Outer for namespacing.
    static class StaticNested {
        void greet() {
            System.out.println("I'm static, no Outer instance needed.");
        }
    }

    // A (non-static) inner class holds an implicit reference to the Outer
    // instance that created it, so it can read Outer's instance fields.
    class Inner {
        void printOuterField() {
            System.out.println("outerField = " + outerField);
        }
    }

    void demoLocalAndAnonymous() {
        // A local class is declared inside a method body and is only
        // usable there - handy for a one-off helper type.
        class Local {
            void hello() {
                System.out.println("Hello from a local class");
            }
        }
        new Local().hello();

        // An anonymous class declares and instantiates an unnamed subclass
        // (or interface implementation) in a single expression. Common
        // before lambdas existed; still useful when more than a single
        // abstract method needs implementing.
        Runnable r = new Runnable() {
            @Override
            public void run() {
                System.out.println("Running from an anonymous class");
            }
        };
        r.run();
    }
}

class NestedClassesDemo {
    public static void main(String[] args) {
        new Outer.StaticNested().greet();

        Outer outer = new Outer();
        Outer.Inner inner = outer.new Inner(); // note the outer.new syntax
        inner.printOuterField();

        outer.demoLocalAndAnonymous();
    }
}

///////////////////////////////////////
// Virtual Threads (Java 21+)
///////////////////////////////////////

// Virtual threads are lightweight threads scheduled by the JVM rather than
// mapped 1:1 to OS threads. They let you write ordinary thread-per-task
// blocking code (one thread per request/connection) that scales to huge
// numbers of concurrent tasks without the memory cost of platform threads.
class VirtualThreadsDemo {
    public static void main(String[] args) throws InterruptedException {
        Thread vThread = Thread.ofVirtual().start(() -> {
            System.out.println("Running in a virtual thread");
        });
        vThread.join(); // wait for it to finish
    }
}
```

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

### Official Oracle Guides

* [Java Tutorial Trail from Sun / Oracle](https://docs.oracle.com/javase/tutorial/index.html)
* [Java Access level modifiers](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)
* [Object-Oriented Programming Concepts](https://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)
* [Exceptions](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)
* [Interfaces](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)
* [Generics](https://docs.oracle.com/javase/tutorial/java/generics/index.html)
* [Collections Framework](https://docs.oracle.com/javase/tutorial/collections/index.html)
* [Java Code Conventions](https://www.oracle.com/technetwork/java/codeconvtoc-136057.html)
* New features in Java 8:
    * [Lambda expressions (functional programming)](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
    * [Streams API](https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html)
    * [Optional](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html)
    * [Date and time API (java.time package)](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
* New features in Java 16-21:
    * [Records](https://docs.oracle.com/en/java/javase/17/language/records.html)
    * [Sealed Classes](https://docs.oracle.com/en/java/javase/17/language/sealed-classes-and-interfaces.html)
    * [Pattern Matching for switch](https://docs.oracle.com/en/java/javase/21/language/pattern-matching-switch-statements-and-expressions.html)
    * [Virtual Threads](https://docs.oracle.com/en/java/javase/21/core/virtual-threads.html)

### Online Practice and Tutorials

* [Codingbat.com](http://codingbat.com/java)
* [Codewars - Java Katas](https://www.codewars.com/?language=java)
* [University of Helsinki - Object-Oriented programming with Java](http://moocfi.github.io/courses/2013/programming-part-1/)

### Books

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)
* [Thinking in Java](https://www.amazon.com/Thinking-Java-4th-Bruce-Eckel/dp/0131872486/)
* [Objects First with Java](https://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)
* [Java The Complete Reference](https://www.amazon.com/gp/product/0071606300)

### Real-World Java Ecosystem & Projects

To explore Java beyond language fundamentals, here are widely used real-world tools, frameworks, and open-source projects:

* [Awesome Java](https://github.com/akullpp/awesome-java)
* [Spring Boot Guides](https://spring.io/guides)
* [Java Design Patterns](https://github.com/iluwatar/java-design-patterns)
* [OpenJFX Official Documentation](https://openjfx.io/openjfx-docs/)

