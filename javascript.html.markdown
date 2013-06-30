---
language: javascript
author: Adam Brenecki
author_url: http://adam.brenecki.id.au
---

Javascript was created by Netscape's Brendan Eich in 1995. It was originally
intended as a simpler scripting language for websites, complimenting the use of
Java for more complex web applications, but its tight integration with Web pages
and built-in support in browsers has caused it to become far more common than
Java in web frontends.

Feedback would be highly appreciated! You can reach me at
[@adambrenecki](https://twitter.com/adambrenecki), or
[adam@brenecki.id.au](mailto:adam@brenecki.id.au).

```javascript
// Comments are like C. Single-line comments start with two slashes,
/* and multiline comments start with slash-star
   and end with star-slash */

// Statements can be terminated by ;
doStuff();

// ... but they don't have to be, as semicolons are automatically inserted
// wherever there's a newline, except in certain cases.
doStuff()

// We'll leave semicolons off here; whether you do or not will depend on your
// personal preference or your project's style guide.

/***********
 * 1. Numbers, Strings and Operators
 ***********/

// Javascript has one number type that covers ints and floats.
3 // = 3
1.5 // = 1.5

// All the basic arithmetic works as you'd expect.
1 + 1 // = 2
8 - 1 // = 7
10 * 2 // = 20
35 / 5 // = 7

// Including uneven division.
5 / 2 // = 2.5

// Enforce precedence with parentheses
(1 + 3) * 2 // = 8

// There are three special not-a-real-number values:
Infinity // result of e.g. 1/0
-Infinity // result of e.g. -1/0
NaN // result of e.g. 0/0

// There's also a boolean type.
true
false

// Strings are created with ' or ".
'abc'
"Hello, world"

// Negation uses the ! symbol
!true // = false
!false // = true

// Equality is ==
1 == 1 // = true
2 == 1 // = false

// Inequality is !=
1 != 1 // = false
2 != 1 // = true

// More comparisons
1 < 10 // = true
1 > 10 // = false
2 <= 2 // = true
2 >= 2 // = true

// Strings are concatenated with +
"Hello " + "world!" // = "Hello world!"

// and are compared with < and >
"a" < "b" // = true

// Type coercion is performed for comparisons...
"5" == 5 // = true

// ...unless you use ===
"5" === 5 // = false

// You can access characters in a string with charAt
"This is a string".charAt(0)

// There's also null and undefined
null // used to indicate a deliberate non-value
undefined // used to indicate a value that hasn't been set yet

// null, undefined, NaN, 0 and "" are falsy, and everything else is truthy.
// Note that 0 is falsy and "0" is truthy, even though 0 == "0".

/***********
 * 2. Variables, Arrays and Objects
 ***********/

// Variables are declared with the var keyword. Javascript is dynamically typed,
// so you don't need to specify type. Assignment uses a single = character.
var someVar = 5

// if you leave the var keyword off, you won't get an error...
someOtherVar = 10

// ...but your variable will be created in the global scope, not in the scope
// you defined it in.

// Variables declared without being assigned to are set to undefined.
var someThirdVar // = undefined

// Arrays are ordered lists of values, of any type.
["Hello", 45, true]

// JavaScript's objects are equivalent to 'dictionaries' or 'maps' in other
// languages: an unordered collection of key-value pairs.
{key1: "Hello", key2: "World"}

// Keys are strings, but quotes aren't required if they're a valid
// JavaScript identifier. Values can be any type.
var myObj = {myKey: "myValue", "my other key": 4}

// Object attributes can be accessed using the 'subscript' syntax,
myObj["my other key"] // = 4

// ... or using the dot syntax, provided the key is a valid identifier.
myObj.myKey // = "myValue"

// Objects are mutable; values can be changed and new keys added.
myObj.myThirdKey = true

/***********
 * 3. Control Structures
 ***********/

/***********
 * 5. Functions, Scope and Closures
 ***********/

/***********
 * 6. More about Objects; Constructors and Prototypes
 ***********/

// Objects can contain functions, which can be called using the dot syntax.
myObj = {
    myFunc: function(){
        return "Hello world!"
    }
}
myObj.myFunc() // = "Hello world!"

// When functions are called like this, they can access the object they're
// attached to using the this keyword.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString
    }
}
myObj.myFunc() // = "Hello world!"

// The value of this has to do with how the function is called, not where it's
// defined. So, that doesn't work if the function isn't called in the context of
// the object.
var myFunc = myObj.myFunc
myFunc() // = undefined

// Inversely, a function can be assigned to the object and gain access to it
// through this, even if it wasn't defined as such.
var myOtherFunc = function(){
    return this.myString.toUpperCase()
}
myObj.myOtherFunc = myOtherFunc
myObj.myOtherFunc() // = "HELLO WORLD!"

// When you call a function with the new keyword, a new object is created, and
// made available to the function via this. Functions designed to be called
// like this are called constructors.

var MyConstructor = function(){
    this.myNumber = 5
}
myNewObj = new MyConstructor() // = {myNumber: 5}
myNewObj.myNumber // = 5

// JavaScript objects aren't defined in terms of classes like other languages,
// but you can use prototypes to do many of the same things. When you try to
// access a property of an object that isn't present, its prototype is searched.
var myObj = {}
var myPrototype = {
    meaningOfLife: 42,
    myThirdFunc: function(){
        return this.myString.toLowerCase()
    }
}
myObj.__proto__ = myPrototype
myObj.myThirdFunc() // = "hello world!"

// Of course, if your property isn't on your prototype, the prototype's
// prototype is searched, and so on.
myPrototype.__proto__ = {
    myBoolean: true
}
myObj.myBoolean // = true

// There's no copying involved here; each object stores a reference to its
// prototype. This means we can alter the prototype and our changes will be
// reflected everywhere.
myObj.

// The __proto__ magic property we've used to access prototypes isn't standard,
// and shouldn't be used in real-world code. There is a way to create a new
// object with another given object as its prototype, though:
var myObj = Object.create(myPrototype)
myObj.meaningOfLife // = 42

// Unfortunately, Object.create is quite recent and isn't available in many
// browsers, so you often can't use that, either. The most reliable way to set
// prototypes involves constructors.

// TODO: write about the .prototype property on constructors

// Built-in types' prototypes work like this too, so you can actually change
// the prototype of a string, for instance (although whether you should is
// another matter).
String.prototype.firstCharacter = function(){
    return this.charAt(0)
}
"abc".firstCharacter() // = "a"

```

## Further Reading

The [Mozilla Developer
Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) provides
excellent documentation for JavaScript as it's used in browsers. Plus, it's a
wiki, so as you learn more you can help others out by sharing your own
knowledge.
