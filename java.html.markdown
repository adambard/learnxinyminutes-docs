---

language: java

author: Jake Prather

author_url: http://github.com/JakeHP

filename: LearnJava.java

---

Java is a general-purpose, concurrent, class-based, object-oriented computer programming language.
[Read more here.](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// Single-line comments start with //
/*
Multi-line comments look like this.
*/

// Import ArrayList class inside of the java.util package
import java.util.ArrayList;
// Import all classes inside of java.lang package
import java.security.*;

// Inside of the LearnJava class, is your program's
// starting point. The main method.
public class LearnJava
{
    //main method
    public static void main (String[] args)
    {
    	
System.out.println("->Printing");
// Printing, and forcing a new line on next print, use println()
System.out.println("Hello World!");
System.out.println("Integer: "+10+" Double: "+3.14+ " Boolean: "+true);
// Printing, without forcing a new line on next print, use print()
System.out.print("Hello World - ");
System.out.print("Integer: "+10+" Double: "+3.14+ " Boolean: "+true);

///////////////////////////////////////
// Types
///////////////////////////////////////
System.out.println("\n\n->Types");
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

// (Java has no unsigned types)

// Float - Single-precision 32-bit IEEE 754 Floating Point
float fooFloat = 234.5f;

// Double - Double-precision 64-bit IEEE 754 Floating Point
double fooDouble = 123.4;

// Boolean - True & False
boolean fooBoolean = true;
boolean barBoolean = false;

// Char - A single 16-bit Unicode character
char fooChar = 'A';

// Make a variable a constant
final int HOURS_I_WORK_PER_WEEK = 9001;

// Strings
String fooString = "My String Is Here!";
// \n is an escaped character that starts a new line
String barString = "Printing on a new line?\nNo Problem!";
System.out.println(fooString);
System.out.println(barString);

// Arrays
//The array size must be decided upon declaration
//The format for declaring an array is follows:
//<datatype> [] <var name> = new <datatype>[<array size>];
int [] intArray = new int[10];
String [] stringArray = new String[1];
boolean [] booleanArray = new boolean[100];

// Another way to declare & initialize an array
int [] y = {9000, 1000, 1337};

// Indexing an array - Accessing an element
System.out.println("intArray @ 0: "+intArray[0]);

// Arrays are mutable; it's just memory!
intArray[1] = 1;
System.out.println("intArray @ 1: "+intArray[1]); // => 1
intArray[1] = 2;
System.out.println("intArray @ 1: "+intArray[1]); // => 2

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
System.out.println("1+2 = "+(i1 + i2)); // => 3
System.out.println("2-1 = "+(i2 - i1)); // => 1
System.out.println("2*1 = "+(i2 * i1)); // => 2
System.out.println("1/2 = "+(i1 / i2)); // => 0 (0.5, but truncated towards 0)

// Modulo
System.out.println("11%3 = "+(11 % 3)); // => 2

// Comparison operators
System.out.println("3 == 2? "+(3 == 2)); // => 0 (false)
System.out.println("3 != 2? "+(3 != 2)); // => 1 (true)
System.out.println("3 > 2? "+(3 > 2)); // => 1
System.out.println("3 < 2? "+(3 < 2)); // => 0
System.out.println("2 <= 2? "+(2 <= 2)); // => 1
System.out.println("2 >= 2? "+(2 >= 2)); // => 1

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
int i=0;
System.out.println("\n->Inc/Dec-rementation");
System.out.println(i++); //i = 1. Post-Incrementation
System.out.println(++i); //i = 2. Pre-Incrementation
System.out.println(i--); //i = 1. Post-Decrementation
System.out.println(--i); //i = 0. Pre-Decrementation

///////////////////////////////////////
// Control Structures
///////////////////////////////////////
System.out.println("\n->Control Structures");
if (false){
	System.out.println("I never run");
}else if (false) {
	System.out.println("I am also never run");
} else {
	System.out.println("I print");
}

// While loop
int fooWhile = 0;
while(fooWhile < 100)
{
    //System.out.println(fooWhile);
    //Increment the counter
	//Iterated 99 times, fooWhile 0->99
    fooWhile++;
}
System.out.println("fooWhile Value: "+fooWhile);

// Do While Loop
int fooDoWhile = 0;
do
{
    //System.out.println(fooDoWhile);
    //Increment the counter
	//Iterated 99 times, fooDoWhile 0->99
    fooDoWhile++;
}while(fooDoWhile < 100);
System.out.println("fooDoWhile Value: "+fooDoWhile);

// For Loop
int fooFor;
//for loop structure => for(<start_statement>;<conditional>;<step>)
for(fooFor=0;fooFor<100;fooFor++){
    //System.out.println(fooFor);
	//Iterated 99 times, fooFor 0->99
}
System.out.println("fooFor Value: "+fooFor);

// Switch Case
int month = 8;
String monthString;
switch (month){
    case 1:  monthString = "January";
             break;
    case 2:  monthString = "February";
             break;
    case 3:  monthString = "March";
             break;
    case 4:  monthString = "April";
             break;
    case 5:  monthString = "May";
             break;
    case 6:  monthString = "June";
             break;
    case 7:  monthString = "July";
             break;
    case 8:  monthString = "August";
             break;
    case 9:  monthString = "September";
             break;
    case 10: monthString = "October";
             break;
    case 11: monthString = "November";
             break;
    case 12: monthString = "December";
             break;
    default: monthString = "Invalid month";
             break;
}
System.out.println("Switch Case Result: "+monthString);

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
// Feel free to check it out here: http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


///////////////////////////////////////
// Classes And Functions
///////////////////////////////////////

	// Read about the class, and function syntax before
	// reading this.
	System.out.println("\n->Classes & Functions");
	// Call bicycle's constructor
	Bicycle trek = new Bicycle();
	// Manipulate your object
	trek.speedUp(3);
	trek.setCadence(100);
	System.out.println("trek info: "+trek.toString());

	// Classes Syntax:
	// <public/private/protected> class <class name>{
	//    //data fields, constructors, functions all inside
	// }
	// Function Syntax:
	// <public/private/protected> <return type> <function name>(<args>)
	// Here is a quick rundown on access level modifiers (public, private, etc.)
	// http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html

// This bracket ends the main method
}
    // The static field is only required because this class
    // is nested inside of the learnjava.java class.
    public static class Bicycle {

        // Bicycle's Fields/Variables
        public int cadence;
        public int gear;
        public int speed;

        // Constructors are a way of creating classes
        // This is a default constructor
        public Bicycle(){
            gear = 1;
            cadence = 50;
            speed = 5;
        }

        // This is a specified constructor (it contains arguments)
        public Bicycle(int startCadence, int startSpeed, int startGear) {
            gear = startGear;
            cadence = startCadence;
            speed = startSpeed;
        }

        // the Bicycle class has
        // four functions/methods
        public void setCadence(int newValue) {
            cadence = newValue;
        }

        public void setGear(int newValue) {
            gear = newValue;
        }

        public void applyBrake(int decrement) {
            speed -= decrement;
        }

        public void speedUp(int increment) {
            speed += increment;
        }
        
        public String toString(){
        	return "gear: "+Integer.toString(gear)+
        			" cadence: "+Integer.toString(cadence)+
        			" speed: "+Integer.toString(speed);
        }
    // bracket to close nested Bicycle class
    }
// bracket to close learnjava.java
}

```

## Further Reading

Other Topics To Research:

* [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)

* [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)

* [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconv-138413.html)

* The links provided are just to get an understanding of the topic, feel free to google and find specific examples
