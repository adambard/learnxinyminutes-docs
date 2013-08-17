---

language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
filename: LearnCSharp.cs

---

C# is an elegant and type-safe object-oriented language that enables developers to build a variety of secure and robust applications that run on the .NET Framework.

[Read more here.](http://msdn.microsoft.com/en-us/library/vstudio/z1zx9t92.aspx)

```c#
// Single-line comments start with //
/*
Multi-line comments look like this
*/
/// <summary>
/// This is an XML documentation comment
/// </summary>

// Specify namespaces application will be using
using System;
using System.Collections.Generic;


// defines scope to organize code into "packages"
namespace Learning
{
    // Each .cs file should at least contain a class with the same name as the file
    // you're allowed to do otherwise, but shouldn't for sanity.
    public class LearnCSharp
    {
        // A console application must have a main method as an entry point
        public static void Main(string[] args)
        {
            // Use Console.WriteLine to print lines
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // To print without a new line, use Console.Write
            Console.Write("Hello ");
            Console.Write("World");


            ///////////////////////////////////////////////////
            // Types & Variables
            //
            // Declare a variable using <type> <name>
            ///////////////////////////////////////////////////

            // Sbyte - Signed 8-bit integer
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Unsigned 8-bit integer
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - Signed 16-bit integer
            // (-32,768 <= short <= 32,767)
            short fooShort = 10000;

            // Ushort - Unsigned 16-bit integer
            // (0 <= ushort <= 65,535)
            ushort fooUshort = 10000;

            // Integer - Signed 32-bit integer
            // (-2,147,483,648 <= int <= 2,147,483,647)
            int fooInt = 1;

            // Uinteger - Unsigned 32-bit integer
            // (0 <= uint <= 4,294,967,295)
            uint fooUint = 1;

            // Long - Signed 64-bit integer
            // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            long fooLong = 100000L;
            // L is used to denote that this variable value is of type long or ulong
            // anything without is treated as int or uint depending on size.

            // Ulong - Unsigned 64-bit integer
            // (0 <= ulong <= 18,446,744,073,709,551,615)
            ulong fooUlong = 100000L;

            // Float - Single-precision 32-bit IEEE 754 Floating Point
            // Precision: 7 digits
            float fooFloat = 234.5f;
            // f is used to denote that this variable value is of type float;
            // otherwise it is treated as double.

            // Double - Double-precision 64-bit IEEE 754 Floating Point
            // Precision: 15-16 digits
            double fooDouble = 123.4;

            // Bool - true & false
            bool fooBoolean = true;
            bool barBoolean = false;

            // Char - A single 16-bit Unicode character
            char fooChar = 'A';

            // Strings
            string fooString = "My string is here!";
            Console.WriteLine(fooString);

            // formatting
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);
            Console.WriteLine(fooFormattedString);

            // formatting dates
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // \n is an escaped character that starts a new line
            string barString = "Printing on a new line?\nNo Problem!";
            Console.WriteLine(barString);

            // it can be written prettier by using the @ symbol
            string bazString = @"Here's some stuff
    on a new line!";
            Console.WriteLine(bazString);

            // quotes need to be escaped
            // use \" normally
            string quotedString = "some \"quoted\" stuff";
            Console.WriteLine(quotedString);

            // use "" when strings start with @
            string quotedString2 = @"some MORE ""quoted"" stuff";
            Console.WriteLine(quotedString2);

            // Use const or read-only to make a variable immutable
            // const values are calculated at compile time
            const int HOURS_I_WORK_PER_WEEK = 9001;

            // Nullable types
            // any type can be made nullable by suffixing a ?
            // <type>? <var name> = <value>
            int? nullable = null;
            Console.WriteLine("Nullable variable: " + nullable);

            // In order to use nullable's value, you have to use Value property or to explicitly cast it
            string? nullableString = "not null";
            Console.WriteLine("Nullable value is: " + nullableString.Value + " or: " + (string) nullableString );

            // ?? is syntactic sugar for specifying default value
            // in case variable is null
            int notNullable = nullable ?? 0;
            Console.WriteLine("Not nullable variable: " + notNullable);

            // Var - compiler will choose the most appropriate type based on value
            var fooImplicit = true;

            ///////////////////////////////////////////////////
            // Data Structures
            ///////////////////////////////////////////////////
            Console.WriteLine("\n->Data Structures");

            // Arrays
            // The array size must be decided upon declaration
            // The format for declaring an array is follows:
            // <datatype>[] <var name> = new <datatype>[<array size>];
            int[] intArray = new int[10];
            string[] stringArray = new string[1];
            bool[] boolArray = new bool[100];

            // Another way to declare & initialize an array
            int[] y = { 9000, 1000, 1337 };

            // Indexing an array - Accessing an element
            Console.WriteLine("intArray @ 0: " + intArray[0]);

            // Arrays are zero-indexed and mutable.
            intArray[1] = 1;
            Console.WriteLine("intArray @ 1: " + intArray[1]); // => 1

            // Lists
            // Lists are used more frequently than arrays as they are more flexible
            // The format for declaring a list is follows:
            // List<datatype> <var name> = new List<datatype>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();

            // Another way to declare & initialize a list
            List<int> z = new List<int> { 9000, 1000, 1337 };

            // Indexing a list - Accessing an element
            // Lists are zero-indexed and mutable.
            Console.WriteLine("z @ 0: " + z[2]);

            // Lists don't default to a value;
            // A value must be added before accessing the index
            intList.Add(1);
            Console.WriteLine("intList @ 0: " + intList[0]);


            // Others data structures to check out:
            //
            // Stack/Queue
            // Dictionary
            // Read-only Collections
            // Tuple (.Net 4+)


            ///////////////////////////////////////
            // Operators
            ///////////////////////////////////////
            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // Shorthand for multiple declarations

            // Arithmetic is straightforward
            Console.WriteLine("1+2 = " + (i1 + i2)); // => 3
            Console.WriteLine("2-1 = " + (i2 - i1)); // => 1
            Console.WriteLine("2*1 = " + (i2 * i1)); // => 2
            Console.WriteLine("1/2 = " + (i1 / i2)); // => 0 (0.5 truncated down)

            // Modulo
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Comparison operators
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

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
            Console.WriteLine("\n->Inc/Dec-rementation");
            Console.WriteLine(i++); //i = 1. Post-Incrementation
            Console.WriteLine(++i); //i = 2. Pre-Incrementation
            Console.WriteLine(i--); //i = 1. Post-Decrementation
            Console.WriteLine(--i); //i = 0. Pre-Decrementation


            ///////////////////////////////////////
            // Control Structures
            ///////////////////////////////////////
            Console.WriteLine("\n->Control Structures");

            // If statements are c-like
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("I get printed");
            }
            else if (j > 10)
            {
                Console.WriteLine("I don't");
            }
            else
            {
                Console.WriteLine("I also don't");
            }

            // Ternary operators
            // A simple if/else can be written as follows
            // <condition> ? <true> : <false>
            string isTrue = (true) ? "True" : "False";
            Console.WriteLine("Ternary demo: " + isTrue);


            // While loop
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                //Console.WriteLine(fooWhile);
                //Increment the counter
                //Iterated 99 times, fooWhile 0->99
                fooWhile++;
            }
            Console.WriteLine("fooWhile Value: " + fooWhile);

            // Do While Loop
            int fooDoWhile = 0;
            do
            {
                //Console.WriteLine(fooDoWhile);
                //Increment the counter
                //Iterated 99 times, fooDoWhile 0->99
                fooDoWhile++;
            } while (fooDoWhile < 100);
            Console.WriteLine("fooDoWhile Value: " + fooDoWhile);

            // For Loop
            int fooFor;
            //for loop structure => for(<start_statement>; <conditional>; <step>)
            for (fooFor = 0; fooFor < 10; fooFor++)
            {
                //Console.WriteLine(fooFor);
                //Iterated 10 times, fooFor 0->9
            }
            Console.WriteLine("fooFor Value: " + fooFor);

            // Switch Case
            // A switch works with the byte, short, char, and int data types.
            // It also works with enumerated types (discussed in Enum Types),
            // the String class, and a few special classes that wrap
            // primitive types: Character, Byte, Short, and Integer.
            int month = 3;
            string monthString;
            switch (month)
            {
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
            Console.WriteLine("Switch Case Result: " + monthString);


            ///////////////////////////////////////
            // Converting Data Types And Typcasting
            ///////////////////////////////////////

            // Converting data

            // Convert String To Integer
            // this will throw an Exception on failure
            int.Parse("123");//returns an integer version of "123"

            // try parse will default to type default on failure
            // in this case: 0
            int tryInt;
            int.TryParse("123", out tryInt);

            // Convert Integer To String
            // Convert class has a number of methods to facilitate conversions
            Convert.ToString(123);

            ///////////////////////////////////////
            // Classes And Functions
            ///////////////////////////////////////

            Console.WriteLine("\n->Classes & Functions");

            // (definition of the Bicycle class follows)

            // Use new to instantiate a class
            Bicycle trek = new Bicycle();

            // Call object methods
            trek.speedUp(3); // You should always use setter and getter methods
            trek.setCadence(100);

            // ToString is a convention to display the value of this Object.
            Console.WriteLine("trek info: " + trek.ToString());

            // Instantiate another new Bicycle
            Bicycle octo = new Bicycle(5, 10);
            Console.WriteLine("octo info: " + octo.ToString());

            // Instantiate a new Penny Farthing
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.ToString());

            Console.Read();
        } // End main method


    } // End LearnCSharp class

    // You can include other classes in a .cs file


    // Class Declaration Syntax:
    // <public/private/protected> class <class name>{
    //    //data fields, constructors, functions all inside.
    //    //functions are called as methods in Java.
    // }

    public class Bicycle
    {
        // Bicycle's Fields/Variables
        public int cadence; // Public: Can be accessed from anywhere
        private int _speed;  // Private: Only accessible from within the class
        protected int gear; // Protected: Accessible from the class and subclasses
        internal int wheels; // Internal: Accessible from within the assembly
        string name; // default: Only accessible from within this class

        // readonly values are set at run time
        // they can only be assigned upon declaration or in a constructor
        readonly bool hasCardsInSpokes = false; // read-only private

        // Constructors are a way of creating classes
        // This is a default constructor
        public Bicycle()
        {
            gear = 1;
            cadence = 50;
            _speed = 5;
            name = "Bontrager";
        }

        // This is a specified constructor (it contains arguments)
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes)
        {
            this.gear = startGear; // "this" keyword denotes the current object
            this.cadence = startCadence;
            this._speed = startSpeed;
            this.name = name; // it can be useful when there's a name conflict
            this.hasCardsInSpokes = hasCardsInSpokes;
        }

        // Constructors can be chained
        public Bicycle(int startCadence, int startSpeed) :
            this(startCadence, startSpeed, 0, "big wheels", true)
        {
        }

        // Function Syntax:
        // <public/private/protected> <return type> <function name>(<args>)

        // classes can implement getters and setters for their fields
        // or they can implement properties

        // Method declaration syntax:
        // <scope> <return type> <method name>(<args>)
        public int getCadence()
        {
            return cadence;
        }

        // void methods require no return statement
        public void setCadence(int newValue)
        {
            cadence = newValue;
        }

        // virtual keyword indicates this method can be overridden
        public virtual void setGear(int newValue)
        {
            gear = newValue;
        }

        public void speedUp(int increment)
        {
            _speed += increment;
        }

        public void slowDown(int decrement)
        {
            _speed -= decrement;
        }

        // properties get/set values
        // when only data needs to be accessed, consider using properties.
        // properties may have either get or set, or both
        private bool _hasTassles; // private variable
        public bool HasTassles // public accessor
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // Properties can be auto-implemented
        public int FrameSize
        {
            get;
            // you are able to specify access modifiers for either get or set
            // this means only Bicycle class can call set on Framesize
            private set;
        }

        //Method to display the attribute values of this Object.
        public override string ToString()
        {
            return "gear: " + gear +
                    " cadence: " + cadence +
                    " speed: " + _speed +
                    " name: " + name +
                    " cards in spokes: " + (hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }
    } // end class Bicycle

    // PennyFarthing is a subclass of Bicycle
    class PennyFarthing : Bicycle
    {
        // (Penny Farthings are those bicycles with the big front wheel.
        // They have no gears.)

        // calling parent constructor
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true)
        {
        }

        public override void setGear(int gear)
        {
            gear = 0;
        }
    }
} // End Namespace

```

## Topics Not Covered

 * Enums, Flags
 * Attributes
 * Generics (T), Delegates, Func, Actions, lambda expressions
 * Exceptions, Interfaces, Abstraction
 * LINQ
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)



## Further Reading

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)



[C# Coding Conventions](http://msdn.microsoft.com/en-us/library/vstudio/ff926074.aspx)
