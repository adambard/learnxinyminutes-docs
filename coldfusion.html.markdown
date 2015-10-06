---
language: ColdFusion
contributors:
    - ["Wayne Boka", "http://wboka.github.io"]
filename: LearnColdFusion.cfm
---

ColdFusion is a scripting language for web development.
[Read more here.](http://www.adobe.com/products/coldfusion-family.html)

```ColdFusion
<!--- Comments start with "<!---" and end with "--->" --->
<!---
    Comments can
    also
    span
    multiple lines
--->

<!--- CFML tags have a similar format to HTML tags. --->

<!--- Variable Declaration: Variables are loosely typed, similar to javascript --->
<cfset myVariable = "myValue" />
<cfset myNumber = 3.14 />

<!--- Displaying simple data --->
<!--- Use <cfoutput> for simple values such as strings, numbers, and expressions --->
<cfoutput>#myVariable#<br /></cfoutput> <!--- myValue --->
<cfoutput>#myNumber#<br /></cfoutput> <!--- 3.14 --->

<hr />

<!--- Declaring complex variables --->
<!--- Declaring an array of 1 dimension: literal or bracket notation --->
<cfset myArray1 = [] />
<!--- Declaring an array of 1 dimension: function notation --->
<cfset myArray2 = ArrayNew(1) />

<!--- Outputting complex variables --->
<cfdump var="#myArray1#" /> <!--- An empty array object --->
<cfdump var="#myArray1#" /> <!--- An empty array object --->

<!--- Operators --->
<!--- Arithmetic --->
<cfoutput>#1 + 1#<br /></cfoutput> = 2
<cfoutput>#10 - 8#<br /></cfoutput> = 2
<cfoutput>#1 * 2#<br /></cfoutput> = 2
<cfoutput>#10 / 5#<br /></cfoutput> = 2
<cfoutput>#12 % 5#<br /></cfoutput> = 0

<hr />

<!--- Comparison --->
<cfoutput>#1 eq 1#<br /></cfoutput> <!--- TRUE --->
<cfoutput>#15 neq 1#<br /></cfoutput> <!--- TRUE --->
<cfoutput>#10 gt 8#<br /></cfoutput> <!--- TRUE --->
<cfoutput>#1 lt 2#<br /></cfoutput> <!--- TRUE --->
<cfoutput>#10 gte 5#<br /></cfoutput> <!--- TRUE --->
<cfoutput>#1 lte 5#<br /></cfoutput> <!--- TRUE --->

<hr />

<!--- Control Structures --->
<cfset myCondition = "Test" />
<cfif myCondition eq "Test">
    <cfoutput>#myCondition#</cfoutput>
<cfelseif myCondition eq "Production">
    <cfoutput>#myCondition#. Proceed Carefully!!!</cfoutput>
<cfelse>
    myCondition is unknown
</cfif>

<hr />

<!--- Loops --->
<cfloop from="0" to="10" index="i">
	<cfoutput>#i# <br /></cfoutput>
</cfloop>

<hr />
```
<!--
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

    // Constructors are a way of creating classes
    // This is a constructor
    public Bicycle() {
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
        return "gear: " + gear + " cadence: " + cadence + " speed: " + speed +
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

    // You should mark a method you're overriding with an @annotation.
    // To learn more about what annotations are and their purpose check this
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
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

// Methods can't have bodies in an interface, unless the method is
// static. Also variables are NOT final by default, unlike an interface.
// Also abstract classes CAN have the "main" method.
// Abstract classes solve these problems.

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
		// age = 30;	==> ERROR!	age is private to Animal
	}

	// NOTE: You will get an error if you used the
	// @Override annotation here, since java doesn't allow
	// overriding of static methods.
	// What is happening here is called METHOD HIDING.
	// Check out this awesome SO post: http://stackoverflow.com/questions/16313649/
	public static void main(String[] args)
	{
		Dog pluto = new Dog();
		pluto.makeSound();
		pluto.eat();
		pluto.printAge();
	}
}
-->

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.
