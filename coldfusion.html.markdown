---
language: ColdFusion
contributors:
    - ["Wayne Boka", "http://wboka.github.io"]
filename: LearnColdFusion.cfm
---

ColdFusion is a scripting language for web development.
[Read more here.](http://www.adobe.com/products/coldfusion-family.html)

```ColdFusion

<em>HTML tags have been provided for output readability</em>

<!--- Comments start with "<!---" and end with "--->" --->
<!---
    Comments can
    also
    span
    multiple lines
--->

<!--- CFML tags have a similar format to HTML tags. --->
<h1>Simple Variables</h1>
<!--- Variable Declaration: Variables are loosely typed, similar to javascript --->
<p>Set <b>myVariable</b> to "myValue"</p>
<cfset myVariable = "myValue" />
<p>Set <b>myNumber</b> to 3.14</p>
<cfset myNumber = 3.14 />

<!--- Displaying simple data --->
<!--- Use <cfoutput> for simple values such as strings, numbers, and expressions --->
<p>Display <b>myVariable</b>: <cfoutput>#myVariable#</cfoutput></p><!--- myValue --->
<p>Display <b>myNumber</b>: <cfoutput>#myNumber#</cfoutput></p><!--- 3.14 --->

<hr />

<h1>Complex Variables</h1>
<!--- Declaring complex variables --->
<!--- Declaring an array of 1 dimension: literal or bracket notation --->
<p>Set <b>myArray1</b> to an array of 1 dimension using literal or bracket notation</p>
<cfset myArray1 = [] />
<!--- Declaring an array of 1 dimension: function notation --->
<p>Set <b>myArray2</b> to an array of 1 dimension using function notation</p>
<cfset myArray2 = ArrayNew(1) />

<!--- Outputting complex variables --->
<p>Contents of <b>myArray1</b></p>
<cfdump var="#myArray1#" /> <!--- An empty array object --->
<p>Contents of <b>myArray2</b></p>
<cfdump var="#myArray2#" /> <!--- An empty array object --->

<!--- Operators --->
<!--- Arithmetic --->
<h1>Operators</h1>
<h2>Arithmetic</h2>
<p>1 + 1 = <cfoutput>#1 + 1#</cfoutput></p>
<p>10 - 7 = <cfoutput>#10 - 7#<br /></cfoutput></p>
<p>15 * 10 = <cfoutput>#15 * 10#<br /></cfoutput></p>
<p>100 / 5 = <cfoutput>#100 / 5#<br /></cfoutput></p>
<p>120 % 5 = <cfoutput>#120 % 5#<br /></cfoutput></p>
<p>120 mod 5 = <cfoutput>#120 mod 5#<br /></cfoutput></p>

<hr />

<!--- Comparison --->
<h2>Comparison</h2>
<h3>Standard Notation</h3>
<p>Is 1 eq 1? <cfoutput>#1 eq 1#</cfoutput></p>
<p>Is 15 neq 1? <cfoutput>#15 neq 1#</cfoutput></p>
<p>Is 10 gt 8? <cfoutput>#10 gt 8#</cfoutput></p>
<p>Is 1 lt 2? <cfoutput>#1 lt 2#</cfoutput></p>
<p>Is 10 gte 5? <cfoutput>#10 gte 5#</cfoutput></p>
<p>Is 1 lte 5? <cfoutput>#1 lte 5#</cfoutput></p>

<h3>Alternative Notation</h3>
<p>Is 1 == 1? <cfoutput>#1 eq 1#</cfoutput></p>
<p>Is 15 != 1? <cfoutput>#15 neq 1#</cfoutput></p>
<p>Is 10 > 8? <cfoutput>#10 gt 8#</cfoutput></p>
<p>Is 1 < 2? <cfoutput>#1 lt 2#</cfoutput></p>
<p>Is 10 >= 5? <cfoutput>#10 gte 5#</cfoutput></p>
<p>Is 1 <= 5? <cfoutput>#1 lte 5#</cfoutput></p>

<hr />

<!--- Control Structures --->
<h1>Control Structures</h1>

<cfset myCondition = "Test" />

<p>Condition to test for: "<cfoutput>#myCondition#</cfoutput>"</p>

<cfif myCondition eq "Test">
    <cfoutput>#myCondition#. We're testing.</cfoutput>
<cfelseif myCondition eq "Production">
    <cfoutput>#myCondition#. Proceed Carefully!!!</cfoutput>
<cfelse>
    myCondition is unknown
</cfif>

<hr />

<!--- Loops --->
<h1>Loops</h1>
<h2>For Loop</h2>
<cfloop from="0" to="10" index="i">
	<p>Index equals <cfoutput>#i#</cfoutput></p>
</cfloop>

<h2>For Each Loop (Complex Variables)</h2>

<p>Set <b>myArray3</b> to [5, 15, 99, 45, 100]</p>

<cfset myArray3 = [5, 15, 99, 45, 100] />

<cfloop array="#myArray3#" index="i">
	<p>Index equals <cfoutput>#i#</cfoutput></p>
</cfloop>

<p>Set <b>myArray4</b> to ["Alpha", "Bravo", "Charlie", "Delta", "Echo"]</p>

<cfset myArray4 = ["Alpha", "Bravo", "Charlie", "Delta", "Echo"] />

<cfloop array="#myArray4#" index="s">
	<p>Index equals <cfoutput>#s#</cfoutput></p>
</cfloop>

<h2>Switch Statement</h2>

<p>Set <b>myArray5</b> to [5, 15, 99, 45, 100]</p>

<cfset myArray5 = [5, 15, 99, 45, 100] />

<cfloop array="#myArray5#" index="i">
	<cfswitch expression="#i#">
		<cfcase value="5,15,45" delimiters=",">
			<p><cfoutput>#i#</cfoutput> is a multiple of 5.</p>
		</cfcase>
		<cfcase value="99">
			<p><cfoutput>#i#</cfoutput> is ninety-nine.</p>
		</cfcase>
		<cfdefaultcase>
			<p><cfoutput>#i#</cfoutput> is not 5, 15, 45, or 99.</p>
		</cfdefaultcase> 
	</cfswitch> 
</cfloop>

<hr />
```
<!--
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
