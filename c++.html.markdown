---
language: c++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
lang: en
---

C++ was designed as a systems programming language that

- is a "better C"
- supports data abstraction
- supports object-oriented programming
- supports generic programming

Though its syntax can be more difficult or complex than newer languages,
it is widely used because it compiles to native instructions that can be
directly run by the processor and offers tight control over hardware (like C)
while offering high-level features such as generics, exceptions, and classes.
This combination of speed and functionality makes C++
one of the most widely-used programming languages.

```c++
//////////////////
// Comparison to C
//////////////////

// C++ is _almost_ a superset of C and shares its basic syntax for
// variable declarations, primitive types, and functions.
// However, C++ varies in some of the following ways:

// A main() function in C++ should return an int,
// though void main() is accepted by most compilers (gcc, clang, etc.)
int main() // or int main(int argc, char** argv)
{
    return 0; // Can also end without return statement
}

// In C++, character literals are one byte.
sizeof('c') == 1

// In C, character literals are the same size as ints.
sizeof('c') == sizeof(10)


// C++ has strict prototyping
void func(); // function which accepts no arguments

// In C
void func(); // function which may accept any number of arguments

// Use nullptr instead of NULL in C++
int* ip = nullptr;

// C standard headers are available in C++,
// but are prefixed with "c" and have no .h suffix.
#include <cstdio>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////////////////
// Function overloading
///////////////////////

// C++ supports function overloading
// provided each function takes different parameters.

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d", myInt);
}

int main()
{
    printing("Hello"); // Resolves to void print(const char*)
    printing(15); // Resolves to void print(int)
}

/////////////////////////////
// Default function arguments
/////////////////////////////

void two_ints(int a = 1, int b = 4);

int main()
{
    two_ints();      // a = 1,  b = 4
    two_ints(20);    // a = 20, b = 4
    two_ints(20, 5); // a = 20, b = 5
}


/////////////
// Namespaces
/////////////

// Namespaces provide separate scopes for variable, function,
// and other declarations.
// Namespaces can be nested

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // end namespace Nested
} // end namespace First

namespace Second {
    void foo()
    {
        printf("This is Second::foo\n")
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // Assume everything is from the namespace "Second"
    // unless otherwise specified.
    using namespace Second;

    foo(); // prints "This is Second::foo"
    First::Nested::foo(); // prints "This is First::Nested::foo"
    ::foo(); // prints "This is global foo"
}

///////////////
// Input/Output
///////////////

// C++ input and output uses streams
// cin, cout, and cerr represent stdin, stdout, and stderr.
// << is the insertion operator and >> is the extraction operator.

#include <iostream> // Include for I/O streams

using namespace std;

int main()
{
   int myInt;

   // Prints to stdout (or terminal/screen)
   cout << "Enter your fav number:\n";
   // Takes in input
   cin >> myInt;

   // cout can also be formatted
   cout << "Your fav number is " << myInt << "\n";
   // Your fav number is ##

    cerr << "Used for error messages";
}

//////////
// Strings
//////////

// Strings in C++ are objects and have many member functions
#include <string>

using namespace std; // Strings are in the namespace std (standard library)

string myString = "Hello";
string myOtherString = " World";

// + is used for concatenation.
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// C++ strings are mutable and have value semantics.
myString.append(" Dog");
cout << myString; // "Hello Dog"


/////////////
// References
/////////////

// In addition to pointers like the ones in C,
// C++ has _references_.
// These are pointer types that cannot be reassigned once set
// and cannot be null.
// They also have the same syntax as the variable itself:
// No * is needed for dereferencing and
// & (address of) is not used for assignment.

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // This creates a reference to foo.
fooRef += ". Hi!"; // Modifies foo through the reference
cout << foo; // Prints "I am foo. Hi!"

fooRef = bar; // Error: references cannot be reassigned.

const string& barRef = bar; // Create a const reference to bar.
// Like C, const values (and pointers and references) cannot be modified.
barRef += ". Hi!"; // Error, const references cannot be modified.

//////////////////////////////////////////
// Classes and object-oriented programming
//////////////////////////////////////////

// First example of classes
#include <iostream>

// Declare a class.
// Classes are usually declared in header (.h or .hpp) files.
class Dog {
    // Member variables and functions are private by default.
    std::string name;
    int weight;

// All members following this are public
// until "private:" or "protected:" is found.
public:

    // Default constructor
    Dog();

    // Member function declarations (implementations to follow)
    // Note that we use std::string here instead of placing
    // using namespace std;
    // above.
    // Never put a "using namespace" statement in a header.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // Functions that do not modify the state of the object
    // should be marked as const.
    // This allows you to call them if given a const reference to the object.
    // Also note the functions must be explicitly declared as _virtual_
    // in order to be overridden in derived classes.
    // Functions are not virtual by default for performance reasons.
    virtual void print() const;

    // Functions can also be defined inside the class body.
    // Functions defined as such are automatically inlined.
    void bark() const { std::cout << name << " barks!\n" }

    // Along with constructors, C++ provides destructors.
    // These are called when an object is deleted or falls out of scope.
    // This enables powerful paradigms such as RAII
    // (http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization)
    // Destructors must be virtual to allow classes to be derived from this one.
    virtual ~Dog();

}; // A semicolon must follow the class definition.

// Class member functions are usually implemented in .cpp files.
void Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// Objects (such as strings) should be passed by reference
// if you are modifying them or const reference if you are not.
void Dog::setName(const std::string& dogsName)
{
    name = doggie_name;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Notice that "virtual" is only needed in the declaration, not the definition.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

void Dog::~Dog()
{
    cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // prints "A dog has been constructed"
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.printDog(); // prints "Dog is Barkley and weighs 10 kg"
    return 0;
} // prints "Goodbye Barkley"

// Inheritance:

// This class inherits everything public and protected from the Dog class
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner)

    // Override the behavior of the print function for all OwnedDogs. See
    // http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping
    // for a more general introduction if you are unfamiliar with
    // subtype polymorphism.
    // The override keyword is optional but makes sure you are actually
    // overriding the method in a base class.
    void print() const override;

private:
    std::string owner;
};

// Meanwhile, in the corresponding .cpp file:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Call the print function in the base Dog class
    std::cout << "Dog is owned by " << owner << "\n";
    // Prints "Dog is <name> and weights <weight>"
    //        "Dog is owned by <owner>"
}

//////////////////////////////////////////
// Initialization and Operator Overloading
//////////////////////////////////////////

// In C++ you can overload the behavior of operators such as +, -, *, /, etc.
// This is done by defining a function which is called
// whenever the operator is used.

#include <iostream>
using namespace std;

class Point {
public:
    // Member variables can be given default values in this manner.
    double x = 0;
    double y = 0;

    // Define a default constructor which does nothing
    // but initialize the Point to the default value (0, 0)
    Point() { };

    // The following syntax is known as an initialization list
    // and is the proper way to initialize class member values 
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Do nothing except initialize the values */ }

    // Overload the + operator.
    Point operator+(const Point& rhs) const;

    // Overload the += operator
    Point& operator+=(const Point& rhs);
};

Point Point::operator+(const Point& rhs) const
{
    // Create a new point that is the sum of this one and rhs.
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // This calls the Point + operator
    // Point up calls the + (function) with right as its paramater
    Point result = up + right;
    // Prints "Result is upright (1,1)"
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

/////////////////////
// Exception Handling
/////////////////////

// The standard library provides a few exception types
// (see http://en.cppreference.com/w/cpp/error/exception)
// but any type can be thrown an as exception
#include <exception>

// All exceptions thrown inside the _try_ block can be caught by subsequent
// _catch_ handlers.
try {
    // Do not allocate exceptions on the heap using _new_.
    throw std::exception("A problem occurred");
}
// Catch exceptions by const reference if they are objects
catch (const std::exception& ex)
{
  std::cout << ex.what();
// Catches any exception not caught by previous _catch_ blocks
} catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // Re-throws the exception
}
```
Futher Reading:

An up-to-date language reference can be found at
<http://cppreference.com/w/cpp>

Additional resources may be found at <http://cplusplus.com>
