---
  language: Oum
  contributors:
    -["Theo Butler", "https://github.com/Theodus"]
  filename: learnoum.oum
---

Oum was created to provide a scripting language which could be implemented
within Java projects or as a stand-alone language with the possibility of sharing
variables and methods with Java classes.

The GitHub repository for the project may be found at (https://github.com/Theodus/Oum)
You can give feedback to TheodusButler [at] [google's email service]

```

// Comments start with two forward slashes, same as Java

///////////////////////////
//  Objects & Operators  //
///////////////////////////

// all values are objects and can be created as objects

a = 3
// or
a = new OumInteger(3)

// basic math

1 + 1 //=> 2
2 - 1 //=> 1
2 * 2 //=> 4

// exponent returns a double

2 ^ 3 //=> 8.0

// division of integers returns an integer

5 / 2  //=> 2
-5 / 2 //=> -2

// if at least one of the nubers is a double value a double will be returned

5.0 / 2 //=> 2.5

// modulus operation returns the remainder of a division 

4 % 3 //=> 1

// parentheses can be used to change order of operation, as they do in mathmatics

(1 + 2) * 3 //=> 9

// boolean operations are the same as Java

true && false //=> false
true || false //=> true
1 == 2 //=> false
3 > 2 //=> true
3 > 3 //=> false
3 >= 3 //=> true

// ! is not

1 != 2 //=> true

// Strings are enclosed in ""

"This is a String"

//////////////////////////////
// Variables & Collections  //
//////////////////////////////

// print to the console

print("Hello!")

// assign values to variables

x = 3

// variables can be assigned to values of different Objects

x = "x now contains a String"

// lists contain a sequence of objects

list = [] // empty list
other_list = [1, 2, 3] // list created with values

// add objects to the list

other_list.add(4) //=> [1, 2, 3, 4]

// access an object in the list with get()

other_list.get(0) //=> 1

// any object can be added to the list

other_list.add("String") //=> [1, 2, 3, 4, "String"]

// check if a value exists in a list with contains()

other_list.contains(3) //=> true

// maps contain a String key which corresponds to a value

map = {}

// add a key value pair to the map with set()

map.set("one", 1) //=> {"one"=1}

// get a list of the keys with keys()

map.keys() //=> ["one"]

// get a list of the values with values()

map.values() //=> [1]

// get value in map with get()

map.get("one") //=> 1

/////////////////////////////////
// Logical Operations & Loops  //
/////////////////////////////////

// if statements are the same as Java

x = 0

if(true)
  x++
  print(x) //=> 1

// if there is only one expression after the if statement, you can place the expression on the same line

if(true) x-- //=> 0

// ifs, else ifs, and elses can be nested

if(false)
  print(x + 2)
else if(true)
  if(false) print(x - 1)
  else print(7) //=> 7
else print("nothing happened")

i = 1
while(i <= 5)
  printf("%d ", i)
  i++
print()

/////////////
// Methods //
/////////////

// mathods are created using the def keyword followed by the method name, parameters, and return type
def stuff(int a) double :
    if(a>0)
        if(a<5) return a^2
        else if(a>5) return a^3
        else return a+3
    else return a+2

print(stuff(3)) //=> 9.0

// void methods don't have a return type
def doSomething() :
  print("something!")

doSomething() //=> something!

```
