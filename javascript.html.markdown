---
language: javascript
author: Adam Brenecki
author_url: http://adam.brenecki.id.au
---

Javascript was created by Netscape's Brendan Eich in 1995. It was originally
intended as a simpler scripting language for web apps, complimenting Java for
more complex ones, but has become far more widely used than Java on the web.

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

// Semicolons are a heated topic in the JavaScript world, but they're really a
// matter of personal or style-guide preference. We'll leave them off here.

/***********
 * 1. Primitive Datatypes and Operators
 ***********/

// Javascript has one number type that covers ints and floats.
3 // = 3
1.5 // = 1.5

// which support all the operations you'd expect.
1 + 1 // = 2
8 - 1 // = 7
10 * 2 // = 20
35 / 5 // = 7

// Uneven division works how you'd expect, too.
5 / 2 # = 2.5

// Enforce precedence with parentheses
(1 + 3) * 2 // = 8

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
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

// Strings are concatenated with +
"Hello " + "world!" // = "Hello world!"

// and are compared with < and >
"a" < "b" // = true

// You can also compare strings with numbers
"5" == 5 // = true

// but this is almost always not what you want, so use === to stop this
"5" === 5 // = false

// You can access characters in a string with charAt
"This is a string".charAt(0)

// There's also a null keyword
null // = null

/***********
 * 2. Variables and Lists
 ***********/

// variables are declared with the var keyword
var some_var = 5

// if you leave them off, you won't get an error...
some_other_var = 10

// but your variable will always end up with the global scope, even if it wasn't
// defined there, so don't do it.

/***********
 * 3. Control Structures
 ***********/

/***********
 * 4. Objects
 ***********/
```
