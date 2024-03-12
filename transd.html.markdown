---
category: language
language: Transd
filename: learntransd.td
contributors:
    - ["Albert Berger", "http://github.com/al-berger"]
---

```
// One-line comment

/* Multiline
   comment */
   
/*
  Transd is a strongly typed data oriented programming language. It includes elements of
  the object oriented paradigm, functional programming, and has the strong emphasis on the
  processing of user data in various formats. It has classes, built-in data query sublanguage,
  means for simplified conversion of text data into active program objects, etc.
*/

// The first line of program code in each source file of a Transd program, should be the language marker:

#lang transd

// All code must reside within the top program objects called modules

module MainModule : {

// Variables are declared as follows

var1: Int(),
var2: 5,

// The statement separator in Transd is comma

var3: "Hello, World!",

// The language supports generic containers

vec1: Vector<String>(),
vec2: Vector<Int>( [1,2,3,4,5] ),
vec3: [1.34, 2.36, 7.89, 10.11],

// Associative arrays are called indexes

idx1: Index<Int String>( {1:"one", 2:"two", 3:"three"} ),

// Function definition:

func1: (lambda (textout "Hello, World!")), 

// Function parameters are listed as name/Type pairs without separators,
// expressions are written in prefix notation: ( OPERATOR [OPERANDS] )

func2: (lambda a Int() b Int() (textout (+ a b))),

// The keyword 'lambda' can be shortened with 'λ' symbol

func3: (λ s String() (textout "func3 is called: " s)),

// Every Transd program has an entry function, from which the program starts executing

 _start: (lambda 
     // call the previously defined 'func3' function and pass a string to it
    (func3 "abc")  // <= "func3 is called: abc"
    
    // Local (scoped) variables are declared with the scope operator 'with' 
    
    (with a Int() b 5 
        (= a 10)
        (textout (* a b)) // <= 50
    )
    
    // Containers can be initialized with values at the place of declaration
    (with vi Vector<Int>( [1,2,3] )
      // More concise form of initialization can be used
      vs ["abc", "def", "ghi"]  // 'vs' has type 'Vector<String>'
      
      // Containers can be modified after their creation
      // add to 'vi' vector three elements: 4, 5 and 6
      (add vi 4) (add vi 5) (add vi 6)
      
      // Looping in Transd is done with the familiar 'for' constructtion
      
      (for i in vi do 
          (textout i "; ")) // <= 1; 2; 3; 4; 5; 6;
          
      (for i in Range(3) do
          (textout (get vs i) " - "))  // <= abc - def - ghi -
      
      // 'for' construction can be used with additional 'where' clause
      
      (for i in vi where (not (mod i 2)) do (textout i " ")  // <= 2 4 6
      
      // 'for' construction is also used for list comprehensions
      
      (with vec (for i in Range(22) where (not (mod i 3)) project (* i i) )
        (textout vec)   // <= [0, 9, 36, 81, 144, 225, 324, 441] 
      )
    )
 ) 
}

// Programs can have more than one module

module Module2 : {

// Modules can import other modules' content with the 'import' statement

import : "MainModule",  // now we can call functions defined in MainModule

func4: (lambda 
    (textout "2 + 3 is: " (func2 2 3)) // <= 2 + 3 is: 5
)

// Transd support regular expressions

tstRegex: (lambda 
    (with s "abc   ddd"
        (lout (match s "[[:alpha:]]+\s*d+")) // <= true
    )
    (with s "a1b2.,.,.c3d4"
        (lout (match s "[[:alnum:]]+[.,]+[[:alnum:]]+")) // <= true
    )
),
    
// Transd has full support of Unicode 

tstUnicode: (λ 
    (with s1 "和平"  s2 "和平5"
        (lout "size of " s1 " is: " (size s1))   // <= size of 和平 is: 2
        (lout "second character in " s2 " is: " (subn s1 1))   // <= second character in 和平5 is: 平
        (lout s1 " only contains letters: "
                :boolalpha (match s1 "[[:alpha:]]+"))  // <= 和平 only contains letters: true
                
        (lout s2 " only contains letters: "
                (match s2 "[[:alpha:]]+")))  // <= 和平5 only contains letters: false
    )
)

}

// Transd has classes

class Point : {
    x: Double(),
    y: Double(),
    // The special @init function can be used for initializing class members
    @init: (λ _x Double() _y Double() (= x _x) (= y _y)),
    
    // Classes can have methods
    distance: (lambda pt Point()
					(sqrt (+ (pow (- x pt.x) 2 ) (pow (- y pt.y) 2 ) ) ) )
}

module Module3 : {
    import: "Point",
    
    func5: (lambda 
        // Objects of classes are created like usual built-in types
        (with pt1 Point(5.0 6.0) pt2 Point(2.5 4.5)
            // A class method is called on a class object by specifying the object as the first operand
            // Call the 'Point::distance' method on 'pt1' and pass 'pt2' as an argument
            (distance pt1 pt2) // <= 5.147815
        )
    )
    
    // Transd support functions as data
    
    // Create a typealias for shortening a type name 
    Lii: typealias(Lambda<Int Int>()),
    
    // Define a function object of the 'Lambda' data type, which receives an Int as a parameter and returns Int
    square: Lii(λ i Int() (ret (* i i))),
    
    // This is a function, which receives lambda objects as arguments
    func6: (λ fun Lii() 
      // lambda objects are called with the 'exec' operator
      (ret (exec fun 6))
    )
    
    func7: (lambda 
    	// call the 'func6' function and pass the 'square' lambda to it
    	(textout (func6 square))  // <= 36
    )
}
```

Further reading about Transd and learning the language can be done on the language website:

[http://transd.org](http://transd.org)
