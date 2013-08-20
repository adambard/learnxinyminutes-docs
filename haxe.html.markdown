---
language: haxe
filename: LearnHaxe3.hx
contributors:
    - ["Justin Donaldson", "https://github.com/jdonaldson/"]
---

Haxe is a web-oriented language that provides platform support for C++, C#,
Swf/ActionScript, Javascript, Java, and Neko byte code (also written by the
Haxe author).  Note that this guide is for Haxe version 3.  Some of the guide
may be applicable to older versions, but it is recommended to use other
references.

```haxe
/*
   Welcome to Learn Haxe 3 in 15 minutes.  http://www.haxe.org
   This is an executable tutorial.  You can compile and run it using the haxe
   compiler, while in the same directory as LearnHaxe.hx:
   haxe -main LearnHaxe3 -x out
 */

// Let's start with comments... this is a single line comment

/*
   And this is multiline. Multiline comments are also used to generate
   javadoc-style documentation for haxedoc.  They will be used if they precede
   a class, class function, or class variable.
 */

/*
   A package declaration isn't necessary, but it's useful if you want to
   organize your code into modules later on.  Also worth mentioning, all
   expressions in Haxe must end in a semicolon:
 */
package; // empty package, no namespace.


// if you import code from other files, it must be declared before the rest of
// the code.
import haxe.ds.ArraySort;

// you can import many classes/modules at once with "*"
import haxe.ds.*;

// you can also import classes in a special way, enabling them to extend the
// functionality of other classes.  More on this later.
using StringTools;

// Haxe files typically define classes, although they can also define other
// types of code... more on that later.


class LearnHaxe3{
    /*
       If you want certain code to run automatically, you need to put it in
       a static main function, and specify the class in the compiler arguments.
       In this case, we've specified the "LearnHaxe3" class in the compiler
       arguments above.
     */
    static function main(){
        /*
           Trace is the default method of printing haxe expressions to the
           screen.  Different targets will have different methods of
           accomplishing this.  E.g., java, c++, c#, etc. will print to std
           out.  Javascript will print to console.log, and flash will print to
           an embedded TextField.  All traces come with a default newline.
           Finally, It's possible to prevent traces from showing by using the
           "--no-traces" argument on the compiler.
         */


        trace("Hello World, with trace()!");

        /*
           Trace can handle any type of value or object.  It will try to print
           a representation of the expression as best it can.  You can also
           concatenate strings with the "+" operator:
         */
        trace(
            " Integer: " + 10 +
            " Float: " + 3.14 +
            " Boolean: " + true
            );


        /*
           Remember what I said about expressions needing semicolons? You
           can put more than one expression on a line if you want.
         */
        trace('two expressions..'); trace('one line');


        //////////////////////////////////////////////////////////////////
        // Types & Variables
        //////////////////////////////////////////////////////////////////
        trace("***Types & Variables***");

        /*
           You can save values and references to data structures using the
           "var" keyword:
         */

        var an_integer:Int = 1;
        trace(an_integer + " is the value for an_integer");


        /*
           Haxe is statically typed, so "an_integer" is declared to have an
           "Int" type, and the rest of the expression assigns the value "1" to
           it.  It's not necessary to declare the type in many cases.  Here,
           the haxe compiler is inferring that the type of another_integer
           should be "Int".
         */

        var another_integer = 2;
        trace(another_integer + " is the value for another_integer");

        // The $type() method prints the type that the compiler assigns:
        $type(another_integer);

        // You can also represent integers with hexadecimal:
        var hex_integer = 0xffffff;

        /*
           Haxe uses platform precision for Int and Float sizes.  It also
           uses the platform behavior for overflow.
           (Other numeric types and behavior are possible using special
           libraries)
         */

        /*
           In addition to simple values like Integers, Floats, and Booleans,
           Haxe provides standard library implementations for common data
           structures like strings, arrays, lists, and maps:
         */

        var a_string = "some" + 'string';  // strings can have double or single quotes
        trace(a_string + " is the value for a_string");

        var x = 1;
        var an_interpolated_string = 'the value of x is $x';

        /*
           Strings are immutable, instance methods will return a copy of
           parts or all of the string.
           (See also the StringBuf class).
         */
        var a_sub_string = a_string.substr(0,4);
        trace(a_sub_string + " is the value for a_sub_string");

        /*
           Arrays are zero-indexed, dynamic, and mutable.  Missing values are
           defined as null.
         */
        var a = new Array<String>(); // an array that contains Strings
        a[0] = 'foo';
        trace(a.length + " is the value for a.length");
        a[9] = 'bar';
        trace(a.length + " is the value for a.length (after modification)");
        trace(a[3] + " is the value for a[3]"); //null

        /*
           Arrays are *generic*, so you can indicate which values they contain
           with a type parameter:
         */
        var a2 = new Array<Int>(); // an array of Ints
        var a3 = new Array<Array<String>>(); // an Array of Arrays (of Strings).

        /*
           Maps are simple key/value data structures.  The key and the value
           can be of any type.
         */
        var m = new Map<String, Int>();  // The keys are strings, the values are Ints.
        m.set('foo', 4);
        // You can also use array notation;
        m['bar'] = 5;
        trace(m.exists('bar') + " is the value for m.exists('bar')");
        trace(m.get('bar') + " is the value for m.get('bar')");
        trace(m['bar'] + " is the value for m['bar']");

        var m2 =  ['foo' => 4, 'baz' => 6]; // Alternative map syntax
        trace(m2 + " is the value for m2");

        /*
           Remember, you can use type inference.  The Haxe compiler will
           decide the type of the variable the first time you pass an
           argument that sets a type parameter.
         */
        var m3 = new Map();
        m3.set(6, 'baz'); // m3 is now a Map<Int,String>
        trace(m3 + " is the value for m3");

        /*
           Haxe has many more common datastructures in the haxe.ds module, such as
           List, Stack, and BalancedTree
         */


        //////////////////////////////////////////////////////////////////
        // Operators
        //////////////////////////////////////////////////////////////////

        trace("***OPERATORS***");

        // basic arithmetic
        trace((4 + 3) + " is the value for (4 + 3)");
        trace((5 - 1) + " is the value for (5 - 1)");
        trace((2 * 4) + " is the value for (2 * 4)");
        trace((8 / 4) + " is the value for (8 / 3) (division always produces Floats)");
        trace((12 % 4) + " is the value for (12 % 4)");


        //basic comparison
        trace((3 == 2) + " is the value for 3 == 2");
        trace((3 != 2) + " is the value for 3 != 2");
        trace((3 >  2) + " is the value for 3 > 2");
        trace((3 <  2) + " is the value for 3 < 2");
        trace((3 >= 2) + " is the value for 3 >= 2");
        trace((3 <= 2) + " is the value for 3 <= 2");

        //bitwise operators
        /*
        ~       Unary bitwise complement
        <<      Signed left shift
        >>      Signed right shift
        >>>     Unsigned right shift
        &       Bitwise AND
        ^       Bitwise exclusive OR
        |       Bitwise inclusive OR
        */

        //increments
        var i = 0;
        trace("Increments and decrements");
        trace(i++); //i = 1. Post-Incrementation
        trace(++i); //i = 2. Pre-Incrementation
        trace(i--); //i = 1. Post-Decrementation
        trace(--i); //i = 0. Pre-Decrementation

        //////////////////////////////////////////////////////////////////
        // Control Structures
        //////////////////////////////////////////////////////////////////
        trace("***CONTROL STRUCTURES***");

        // if statements
        var j = 10;
        if (j == 10){
            trace("this is printed");
        } else if (j > 10){
            trace("not greater than 10, so not printed");
        } else {
            trace("also not printed.");
        }

        trace("Looping and Iteration");

        // while loop
        var k = 0;
        while(k < 100){
            // trace(counter); // will print out numbers 0-99
            k++;
        }

        // do-while loop
        var  l = 0;
        do{
            trace("do statement always runs at least once");
        } while (i > 0);

        // for loop
        /*
           There is no c-style for loop in Haxe, because they are prone
           to error, and not necessary.  Instead, Haxe has a much simpler
           and safer version that uses Iterators (more on those later).
         */
        var m = [1,2,3];
        for (val in m){
            trace(val + " is the value for val in the m array");
        }

        // Note that you can iterate on an index using a range
        // (more on ranges later as well)
        var n = ['foo', 'bar', 'baz'];
        for (val in 0...n.length){
            trace(val + " is the value for val (an index for m)");
        }


        trace("Array Comprehensions");

        // Array comprehensions give you the ability to iterate over arrays
        // while also creating filters and modifications.
        var filtered_n = [for (val in n) if (val != "foo") val];
        trace(filtered_n + " is the value for filtered_n");

        var modified_n = [for (val in n) val += '!'];
        trace(modified_n + " is the value for modified_n");

        var filtered_and_modified_n = [for (val in n) if (val != "foo") val += "!"];
        trace(filtered_and_modified_n + " is the value for filtered_and_modified_n");

        //////////////////////////////////////////////////////////////////
        // Switch Statements (Value Type)
        //////////////////////////////////////////////////////////////////
        trace("***SWITCH STATEMENTS (VALUE TYPES)***");

        /*
           Switch statements in Haxe are very powerful.  In addition to working
           on basic values like strings and ints, they can also work on the
           generalized algebraic data types in enums (more on enums later).
           Here's some basic value examples for now:
         */
        var my_dog_name = 'fido';
        var favorite_thing  = '';
        switch(my_dog_name){
            case "fido" : favorite_thing = 'bone';
            case "rex" :  favorite_thing = 'shoe';
            case "spot" : favorite_thing = 'tennis ball';
            case _ : favorite_thing = 'some unknown treat';
        }
        // The "_" case above is a "wildcard" value
        // that will match anything.

        trace("My dog's name is" + my_dog_name
                + ", and his favorite thing is a: "
                + favorite_thing);

        //////////////////////////////////////////////////////////////////
        // Expression Statements
        //////////////////////////////////////////////////////////////////
        trace("***EXPRESSION STATEMENTS***");

        /*
           Haxe control statements are very powerful because every statement
           is also an expression, consider:
        */

        // if statements
        var k = if (true){
            10;
        } else {
            20;
        }

        trace("K equals ", k); // outputs 10

        var other_favorite_thing = switch(my_dog_name) {
            case "fido" : 'teddy';
            case "rex" :  'stick';
            case "spot" : 'football';
            case _ : 'some unknown treat';
        }

        trace("My dog's name is" + my_dog_name
                + ", and his other favorite thing is a: "
                + other_favorite_thing);

        //////////////////////////////////////////////////////////////////
        // Converting Value Types
        //////////////////////////////////////////////////////////////////

        // You can convert strings to ints fairly easily.

        // string to integer
        Std.parseInt("0"); // returns 0
        Std.parseFloat("0.4"); // returns 0.4;

        // integer to string
        Std.string(0); // returns "0";
        // concatenation with strings will auto-convert to string.
        0 + "";  // returns "0";
        true + ""; // returns "true";
        // See documentation for parsing in Std for more details.

        //////////////////////////////////////////////////////////////////
        // Basic Object Oriented Programming
        //////////////////////////////////////////////////////////////////
        trace("***BASIC OBJECT ORIENTED PROGRAMMING***");


        // create an instance of FooClass.  The classes for this are at the
        // end of the file.
        var instance = new FooClass(3);

        // read the public variable normally
        trace(instance.public_any + " is the value for instance.public_any");

        // we can read this variable
        trace(instance.public_read + " is the value for instance.public_read");
        // but not write it
        // instance.public_write = 4; // this will throw an error if uncommented:
        // trace(instance.public_write); // as will this.

        trace(instance + " is the value for instance"); // calls the toString method
        trace(instance.toString() + " is the value for instance.toString()"); // same thing


        // instance has the "FooClass" type, while acceptBaseFoo has the
        // BaseFooClass type.  However, since FooClass extends BaseFooClass,
        // it is accepted.
        BaseFooClass.acceptBaseFoo(instance);
    }

}

/*
   This is the "child class" of the main LearnHaxe3 Class
 */
class FooClass extends BaseFooClass implements BaseFooInterface{
    public var public_any:Int; // public variables are accessible anywhere
    public var public_read (default,null): Int; // use this style to only enable public read
    public var public_write (null, default): Int; // or public write
    public var property (get, set): Int; // use this style to enable getters/setters

    // private variables are not available outside the class.
    // see @:allow for ways around this.
    var _private:Int; // variables are private if they are not marked public

    // a public constructor
    public function new(arg:Int){
        super(); // call the constructor of the parent object, since we extended BaseFooClass

        this.public_any= 0;
        this._private = arg;

    }

    // getter for _private
    function get_property() : Int {
        return _private;
    }

    // setter for _private
    function set_property(val:Int) : Int {
        _private = val;
        return val;
    }

    // special function that is called whenever an instance is cast to a string.
    public function toString(){
        return _private + " with toString() method!";
    }

    // this class needs to have this function defined, since it implements
    // the BaseFooInterface interface.
    public function baseFunction(x: Int) : String{
        // convert the int to string automatically
        return x + " was passed into baseFunction!";
    }
}

/*
    A simple class to extend
*/
class BaseFooClass {
    var base_variable:Int;
    public function new(){
        base_variable = 4;
    }
    public static function acceptBaseFoo(b:BaseFooClass){
    }
}

/*
    A simple interface to implement
*/
interface BaseFooInterface{
    public function baseFunction(x:Int):String;
}

```

