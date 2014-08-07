---

language: java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Madison Dickson", "http://github.com/mix3d"]
filename: LearnJava.java

---

Java is a general-purpose, concurrent, class-based, object-oriented computer programming language.
[Read more here.](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// Single-line comments start with //
/*
Multi-line comments look like this.
*/
/**
JavaDoc comments look like this. Used to describe the Class or various
attributes of a Class.
*/

// Import ArrayList class inside of the java.util package
import java.util.ArrayList;
// Import all classes inside of java.security package
import java.security.*;

// Each .java file contains one outer-level public class, with the same name as
// the file.
public class LearnJava {

    // A program must have a main method as an entry point
    public static void main (String[] args) {

        // Use System.out.println to print lines
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // To print without a newline, use System.out.print
        System.out.print("Hello ");
        System.out.print("World");


        ///////////////////////////////////////
        // Types & Variables
        ///////////////////////////////////////

        // Declare a variable using <type> <name> [
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

        // Note: Java has no unsigned types

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

        // final variables can't be reassigned to another object
        final int HOURS_I_WORK_PER_WEEK = 9001;

        // Strings
        String fooString = "My String Is Here!";

        // \n is an escaped character that starts a new line
        String barString = "Printing on a new line?\nNo Problem!";
        // \t is an escaped character that adds a tab character
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Arrays
        //The array size must be decided upon instantiation
        //The following formats work for declaring an arrow
        //<datatype> [] <var name> = new <datatype>[<array size>];
        //<datatype> <var name>[] = new <datatype>[<array size>];
        int [] intArray = new int[10];
        String [] stringArray = new String[1];
        boolean boolArray [] = new boolean[100];

        // Another way to declare & initialize an array
        int [] y = {9000, 1000, 1337};
        String names [] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = new boolean[] {true, false, false};

        // Indexing an array - Accessing an element
        System.out.println("intArray @ 0: " + intArray[0]);

        // Arrays are zero-indexed and mutable.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Others to check out
        // ArrayLists - Like arrays except more functionality is offered,
        //             and the size is mutable
        // LinkedLists
        // Maps
        // HashMaps

        ///////////////////////////////////////
        // Operators
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // Shorthand for multiple declarations

        // Arithmetic is straightforward
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5 truncated down)

        // Modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Comparison operators
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Bitwise operators!
        /*
        ~       Unary bitwise complement
        <<      Signed left shift
        >>      Signed right shift
        >>>     Unsigned right shift
        &       Bitwise AND
        ^       Bitwise exclusive OR
        |       Bitwise inclusive OR
        */

        // Incrementations
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        // The ++ and -- operators increment and decrement by 1 respectively.
        // If they are placed before the variable, they increment then return;
        // after the variable they return then increment.
        System.out.println(i++); //i = 1, prints 0 (post-increment)
        System.out.println(++i); //i = 2, prints 2 (pre-increment)
        System.out.println(i--); //i = 1, prints 2 (post-decrement)
        System.out.println(--i); //i = 0, prints 0 (pre-decrement)

        ///////////////////////////////////////
        // Control Structures
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // If statements are c-like
        int j = 10;
        if (j == 10){
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // While loop
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            //System.out.println(fooWhile);
            //Increment the counter
            //Iterated 100 times, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Do While Loop
        int fooDoWhile = 0;
        do
        {
            //System.out.println(fooDoWhile);
            //Increment the counter
            //Iterated 99 times, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // For Loop
        int fooFor;
        //for loop structure => for(<start_statement>; <conditional>; <step>)
        for(fooFor=0; fooFor<10; fooFor++){
            //System.out.println(fooFor);
            //Iterated 10 times, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // For Each Loop
        // An automatic iteration through an array or list of objects.
        int[] fooList = {1,2,3,4,5,6,7,8,9};
        //for each loop structure => for(<object> : <array_object>)
        //reads as: for each object in the array
        //note: the object type must match the array.

        for( int bar : fooList ){
            //System.out.println(bar);
            //Iterates 9 times and prints 1-9 on new lines
        }

        // Switch Case
        // A switch works with the byte, short, char, and int data types.
        // It also works with enumerated types (discussed in Enum Types),
        // the String class, and a few special classes that wrap
        // primitive types: Character, Byte, Short, and Integer.
        int month = 3;
        String monthString;
        switch (month){
            case 1:
                    monthString = "January";
                    break;
            case 2:
                    monthString = "February";
                    break;
            case 3:
                    monthString = "March";
                    break;
            default:
                    monthString = "Some other month";
                    break;
        }
        System.out.println("Switch Case Result: " + monthString);

        // Conditional Shorthand
        // You can use the '?' operator for quick assignments or logic forks.
        // Reads as "If (statement) is true, use <first value>, otherwise, use <second value>"
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Prints A, because the statement is true


        ///////////////////////////////////////
        // Converting Data Types And Typcasting
        ///////////////////////////////////////

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
        // You can also cast java objects, there's a lot of details and
        // deals with some more intermediate concepts.
        // Feel free to check it out here:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


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
} // End LearnJava class


// You can include other, non-public outer-level classes in a .java file


// Class Declaration Syntax:
// <public/private/protected> class <class name>{
//    //data fields, constructors, functions all inside.
//    //functions are called as methods in Java.
// }

class Bicycle {

    // Bicycle's Fields/Variables
    public int cadence; // Public: Can be accessed from anywhere
    private int speed;  // Private: Only accessible from within the class
    protected int gear; // Protected: Accessible from the class and subclasses
    String name; // default: Only accessible from within this package

    // Constructors are a way of creating classes
    // This is a constructor
    public Bicycle() {
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }

    // This is a constructor that takes arguments
    public Bicycle(int startCadence, int startSpeed, int startGear, String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Function Syntax:
    // <public/private/protected> <return type> <function name>(<args>)

    // Java classes often implement getters and setters for their fields

    // Method declaration syntax:
    // <scope> <return type> <method name>(<args>)
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
    @Override
    public String toString() {
        return "gear: " + gear +
                " cadence: " + cadence +
                " speed: " + speed +
                " name: " + name;
    }
} // end class Bicycle

// PennyFarthing is a subclass of Bicycle
class PennyFarthing extends Bicycle {
    // (Penny Farthings are those bicycles with the big front wheel.
    // They have no gears.)

    public PennyFarthing(int startCadence, int startSpeed){
        // Call the parent constructor with super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // You should mark a method you're overriding with an @annotation
    // To learn more about what annotations are and their purpose
    // check this out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }

}

//Interfaces
//Interface declaration syntax
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


//We can now create a class that implements both of these interfaces
public class Fruit implements Edible, Digestible {
	public void eat() {
		//...
	}

	public void digest() {
		//... 
	}
}

//In java, you can extend only one class, but you can implement many interfaces.
//For example:
public class ExampleClass extends ExampleClassParent implements InterfaceOne, InterfaceTwo {
	public void InterfaceOneMethod() {

	}

	public void InterfaceTwoMethod() {

	}
}

```

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

**Official Oracle Guides**:

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

**Online Practice and Tutorials**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Books**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)


