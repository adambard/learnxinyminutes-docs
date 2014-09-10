---
language: c++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
lang: en
---

I am writing this to highlight the differences and 
additions that C++ has with respect to C. My 
suggestion would be to follow the C tutorial first
then look here for the additions and differences.

```c++
///////////////////////////////////////   
// C++ differences   
///////////////////////////////////////  


//In C++   
//cannot use void main()   
int main() { //or int main(int argc, char **argv)   
    //cannot end with return;   
    return 0;   
    //Can also end without return statement   
}   
   
//In C++  
/*    
  //This could lead to compiler errors and is discouraged    
  //#if 0 #endif pairs are encouraged instead   
*/   
   
//In C++    
sizeof(10) //Typically 4   
sizeof('c') == 1    

//In C   
sizeof('c') == sizeof(10) //true chars are passed as ints   


//In C++ strict prototyping   
void func(); //function which accepts no arguments   

//In C   
void func(); //function which may accept arguments    


//In C++   
for(int i = 0; i < 10; i++) {;}   
//In C must int i must be declared before   


//C++ Supports Function overloading   
//Provided each function takes different   
//parameters    

void printing(char const *myString)    
{printf("String %s\n",myString);} //Hello   

void printing(int myInt)    
{printf("My int is %d",myInt);} //15   

int main ()    
{   
    printing("Hello");   
    printing(15);   
}   
   


//C++ Default Function Arguments   
void two_ints(int a = 1, int b = 4);   

int main()   
{    
    two_ints();            // arguments:  1, 4   
    two_ints(20);          // arguments: 20, 4   
    two_ints(20, 5);       // arguments: 20, 5   
}   


//C++ added the nullptr which is different from 0   
int *ip = nullptr;      // OK   
int value = nullptr;    // error: value is no pointer  


///////////////////////////////////////   
// C++ Additions ontop of C    
///////////////////////////////////////   


///////////////////////////////////////   
// C++ Namespace   
///////////////////////////////////////   

//Namespaces allow you to define your own    
//functions and variables for use    

// Use '::' to change variable (or function) scope   
// Putting '::' before a function or variable will   
// reference a global scope    

// This allows you to make normal c library calls   
// std is for standard library    
using namespace std;   

#include <stdio.h>    

int counter = 50;                // global variable    

int main()   
{   
    for (int counter = 1;        // this refers to the   
    counter < 2;                 // local variable   
    counter++)   
    {   
        printf("Global var %d local var %d\n",    
            ::counter,           // global variable    
            counter);            // local variable    
        // => Global var 50 local var 1   
    }   
}   

// Namespaces can be nested   


namespace myFirstNameSpace   
{   
    namespace myInnerSoul   
    {   
        cos(int x)   
        {   
            printf("My inner soul was made to program.");   
	}   
    }   
}   

namespace anotherNameSpace  
{   
    cos(int x) {;} //does nothing   
}   
   
int main()   
{   
    //Specify the full path because main is outside of both namespaces.    
    //Will print out My inner soul was made to program.    
    myFirstNameSpace::myInnerSoul::cos(60);   
}   


///////////////////////////////////////   
// C++ Strings   
///////////////////////////////////////   

//Strings in C++ are Objects and have many functions  
myString = "Hello";   
myOtherString = " World";   

myString + myOtherString; // => "Hello World"    

myString + ' You'; // => "Hello You"   

myString != myOtherString; //True   

//An example of a string method   
myString.append(" Dog"); // => "Hello Dog"   


///////////////////////////////////////   
// C++ Input Output   
///////////////////////////////////////   

//C++ input and output streams   
//cin, cout, cerr, << is insertion and >> is extraction operator   
#include <iostream>   

using namespace std;   

int main()   
{   

   int myInt;   
    
   //Prints to stdout (or terminal/screen)   
   cout << "Enter your fav number:\n"   
   //Takes in input   
   cin >> myInt;   

   //cout can also be formatted   
   cout << "Your fav number is " << myInt << "\n"   
   //Your fav number is ##  

   cerr << "Used for error messages"   
}   


///////////////////////////////////////   
// C++ Classes   
///////////////////////////////////////


//First example of classes   
#include <iostream>   

//define a class    
class Doggie   
{   
    std::string name;   
    int         weight;   

   // These are only the declarations   
   //Can also have private and protected   
   public:     
       //The public methods (can also include variables)   

   // Default constructor   
   Doggie();   

   void setName(std::string dogsName);   
   void setWeight(int dogsWeight);    
   void printDog();   

   //Can define functions within class declaration too   
   void dogBark() {std::cout << "Bark Bark\n"}   

   //Destructors are methods that free the allocated space   
   ~doggieDestructor();   
   //if no destructor compiler defines the trivial destructor   

//Classes are similar to structs and must close the } with ;    
};   

// This is the implementation of the class methods   
// Also called the definition   
void Doggie::Doggie () {   
    std::cout << "A doggie is born. Woof!\n";   
}   
 
void Doggie::setName (std::string doggie_name) {    
    name = doggie_name;   
}   

void Doggie::setWeight (int doggie_weight) {   
    weight = doggie_weight;   
}   

void Doggie::printDog () {   
    std::cout << "Dog is " << name << " weighs" << weight << "\n";    
}   

void Doggie::~doggieDestructor () {   
    delete[] name;   
    delete weight;    
}   

int main () {    
  Doggie deedee; // prints out a doggie is born. Woof!   
  deedee.setName ("Barkley");    
  deedee.setWeight(1000000);   
  deedee.printDog;   
  //prints => Dog is Barkley weighs 1000000    
  return 0;   
}   


//C++ Class inheritance   

class German_Sheperd   
{
   //This class now inherits everything public and protected from Doggie class   
   Doggie      d_dog;   

   //Good practice to put d_ in front of datatypes in classes   
   std::string d_type;   

   public:  
      void dogType() {d_type = "German Sheperd";}   
};   



///////////////////////////////////////   
// C++ Exception Handling   
///////////////////////////////////////    

try {   
   throw 12.25;  // throws a double no handler declared   
} catch (int errorNum)   
{   
  std::cout << "I caught an int " << errorNum << "\n";   
//default catcher   
} catch (...)   
{   
    std::cout << "I got an error. Not sure what but I can pass it up.";   
    throw;   
}   


///////////////////////////////////////    
// C++ Operator Overloading   
///////////////////////////////////////   

// In C++ you can overload operators such as +, -, new, etc.   

#include <iostream>   
using namespace std;   

class Vector {   
    public:   
        double x,y;   
        Vector () {};   
        Vector (double a, double b) : x(a), y(b) {}   
        Vector operator + (const CVector&);   
        Vector operator += (const CVector&);   
};   
 
Vector Vector::operator+ (const Vector& rhs)    
{   
    Vector temp;   
    temp.x = x + rhs.x;   
    temp.y = y + rhs.y;   
    return temp;   
}   

Vector Vector::operator+= (const Vector& rhs)   
{   
    x += rhs.x;   
    y += rhs.y;   
    return *this;   
}    

int main () {   
    Vector up (0,1);   
    Vector right (1,0);   
    Vector result;    
    // This calls the Vector + operator   
    // Vector up calls the + (function) with right as its paramater   
    result = up + right;   
    // prints out => Result is upright (1,1)   
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";   
    return 0;   
}

```
Futher Reading   

for more resources see: http://www.icce.rug.nl/documents/cplusplus/   
for other reference material: http://www.cplusplus.com/doc/tutorial/  
