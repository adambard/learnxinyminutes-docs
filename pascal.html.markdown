---
language: Pascal
filename: learnpascal.pas
contributors:
    - ["Ganesha Danu", "http://github.com/blinfoldking"]
---


>Pascal is an imperative and procedural programming language, which Niklaus Wirth designed in 1968–69 and published in 1970, as a small, efficient language intended to encourage good programming practices using structured programming and data structuring. It is named in honor of the French mathematician, philosopher and physicist Blaise Pascal. 
source : [wikipedia](https://en.wikipedia.org/wiki/Pascal_(programming_language))



to compile and run a pascal program you could use a free pascal compiler. [Download Here](https://www.freepascal.org/)

```pascal
//Anathomy of a Pascal Program
//this is a comment
{
    this is a 
    multiline comment
}

//name of the program
program learn_pascal; //<-- dont forget a semicolon

type
    {
        this is where you should delcare a custom
        data-types
    }
var
    {
        this is where you should declare a variable
    }

//main program area
begin
    {
        area to declare your instruction
    }
end. // End of a main program area should required a "." symbol
```

```pascal
//declaring variable
//you can do this
var a:integer;
var b:integer;
//or this
var 
    a : integer;
    b : integer;
//or this
var a,b : integer;
```
```pascal
program Learn_More;
//Lets learn about data types and their operations

//Declaring variables
var 
    int : integer; // a variable that contains an integer number data types
    ch  : char;    // a variable that contains a character data types
    str : string; // a variable that contains a string data types
    r   : real;   // a variable that contains a real number data types
    bool : boolean; //a variables that contains a Boolean(True/False) value data types
Begin
    int := 1;// how to assign a value to a variable
    r   := 3.14;
    ch  := 'a';
    str := 'apple';
    bool := true;
    //pascal is not a case-sensitive language
    //arithmethic operation
    int := 1 + 1; // int = 2 overwriting the previous assignment
    int := int + 1; // int = 2 + 1 = 3;
    int := 4 div 2; //int = 2 a division operation which the result will be floored
    int := 3 div 2; //int = 1
    int := 1 div 2; //int = 0

    bool := true or false; // bool = true
    bool := false and true; // bool = false
    bool := true xor true; // bool = false
    
    r := 3 / 2; // a division operator for real
    r := int; // you can assign an integer to a real variable but not the otherwise

    c := str[1]; // assign the first letter of str to c
    str := 'hello' + 'world'; //combining strings
End.
```

```pascal

```