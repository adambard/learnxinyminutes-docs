---

language: java

author: Jake Prather

author_url: http://github.com/JakeHP

---

Java is a general-purpose, concurrent, class-based, object-oriented computer programming language.
[Read more here.](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// Single-line comments start with //
/*
Multi-line comments look like this.
*/

// Import Packages
import java.util.ArrayList;
import package.path.here;
// Import all "sub-packages"
import java.lang.Math.*;

// Your program's entry point is a function called main
public class Main
{
    public static void main (String[] args) throws java.lang.Exception
    {
        //stuff here
    }
}

// Printing, and forcing a new line on next print = println()
System.out.println("Hello World");
System.out.println("Integer: "+10+"Double: "+3.14+ "Boolean: "+true);
// Printing, without forcing a new line on next print = print()
System.out.print("Hello World");
System.out.print("Integer: "+10+"Double: "+3.14+ "Boolean: "+true);

///////////////////////////////////////
// Types
///////////////////////////////////////

// Byte - 8-bit signed two's complement integer
// (-128 <= byte <= 127)
byte foo = 100;

// Short - 16-bit signed two's complement integer
// (-32,768 <= short <= 32,767)
short bar = 10000;

//Integer - 32-bit signed two's complement integer
// (-2,147,483,648 <= int <= 2,147,483,647)
int foo = 1;

//Long - 64-bit signed two's complement integer
// (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
long bar = 100000L;

// (Java has no unsigned types)

//Float - Single-precision 32-bit IEEE 754 Floating Point
float foo = 234.5f;

//Double - Double-precision 64-bit IEEE 754 Floating Point
double bar = 123.4;

//Boolean - True & False
boolean foo = true;
boolean bar = false;

//Char - A single 16-bit Unicode character
char foo = 'A';

//Make a variable a constant
final int HOURS_I_WORK_PER_WEEK = 9001;

//Strings
String foo = "Hello World!";
// \n is an escaped character that starts a new line
String foo = "Hello World!\nLine2!";
System.out.println(foo);
//Hello World!
//Line2!

//Arrays
//The array size must be decided upon declaration
//The format for declaring an array is follows:
//<datatype> [] <var name> = new <datatype>[<array size>];
int [] array = new int[10];
String [] array = new String[1];
boolean [] array = new boolean[100];

// Indexing an array - Accessing an element
array[0];

// Arrays are mutable; it's just memory!
array[1] = 1;
System.out.println(array[1]); // => 1
array[1] = 2;
System.out.println(array[1]); // => 2

//Others to check out
//ArrayLists - Like arrays except more functionality is offered,
//             and the size is mutable
//LinkedLists
//Maps
//HashMaps

///////////////////////////////////////
// Operators
///////////////////////////////////////

int i1 = 1, i2 = 2; // Shorthand for multiple declarations

// Arithmetic is straightforward
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5, but truncated towards 0)

// Modulo
11 % 3; // => 2

// Comparison operators
3 == 2; // => 0 (false)
3 != 2; // => 1 (true)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// Bitwise operators!
~       Unary bitwise complement
<<      Signed left shift
>>      Signed right shift
>>>     Unsigned right shift
&       Bitwise AND
^       Bitwise exclusive OR
|       Bitwise inclusive OR

// Incrementations
int i=0;
i++; //i = 1. Post-Incrementation
++i; //i = 2. Pre-Incrementation
i--; //i = 1. Post-Decrementation
--i; //i = 0. Pre-Decrementation

///////////////////////////////////////
// Control Structures
///////////////////////////////////////

if (false) {
      System.out.println("I never run");
    } else if (false) {
      System.out.println("I am also never run");
    } else {
      System.out.println("I print");
    }
}

// While loop
int i = 0;
while(i < 100){
    System.out.println(i);
    //Increment the counter
    i++;
}

// Do While Loop
int i = 0;
do{
    System.out.println(i);
    //Increment the counter
    i++;
}while(i < 100);

// For Loop
int i;
//for loop structure => for(<start_statement>;<conditional>;<step>)
for(i=0;i<100;i++){
    System.out.println(i);
}

// Switch Case
int month = 8;
        String monthString;
        switch (month) {
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
        System.out.println(monthString);

///////////////////////////////////////
// Typecasting
///////////////////////////////////////

// Converting data

//Convert String To Integer
Integer.parseInt("123");//returns an integer version of "123"

//Convert Integer To String
Integer.toString(123);//returns a string version of 123

//For other conversions check out the following classes:
//Double
//Long
//String

// You can also cast java objects, there's a lot of details and
// deals with some more intermediate concepts.
// Feel free to check it out here: http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


///////////////////////////////////////
// Classes And Functions
///////////////////////////////////////

// Classes Syntax shown below.
// Function declaration syntax:
// <public/private/protected> <return type> <function name>(<args>)
// Here is a quick rundown on access level modifiers (public, private, etc.)
// http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html


public class Bicycle {

    // Bicycle's Fields/Variables
    public int cadence;
    public int gear;
    public int speed;

    // Constructors are a way of creating classes
    // This is a default constructor
    public Bicycle(){
        gear = 1;
        cadence = 50;
        startGear = 1;
    }

    // This is a specified constructor (it contains arguments)
    public Bicycle(int startCadence, int startSpeed, int startGear) {
        gear = startGear;
        cadence = startCadence;
        speed = startSpeed;
    }

    // the Bicycle class has
    // four methods
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

}

//Now..Later in the main / driver of your java program
public class Main
{
    public static void main (String[] args) throws java.lang.Exception
    {
        //Call bicycle's constructor
        Bicycle trek = new Bicycle();
        //Manipulate your object
        trek.speedUp(3);
        trek.setCadence(100);
    }
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

* The links provided are just to get an understanding of the topic, feel free to google and find specific examples
