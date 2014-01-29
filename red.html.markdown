---
name: Red
category: language
language: Red
filename: learnred.red
contributors:
    - ["Arnold van Hofwegen", "https://github.com/iArnold"]
---


Red was created out of the need to get work done, and the tool the author wanted to use, the language of REBOL, had a couple of drawbacks. 
It was not Open Sourced at that time and it is an interpreted language, what means that it is on average slow compared to a compiled language.

Red, together with its C-level dialect Red/System, provides a language that covers the entire programming space you ever need to program something in.
Red is a language heavily based on the language of REBOL. Where Red itself reproduces the flexibility of the REBOL language, the underlying language Red will be built upon, 
Red/System, covers the more basic needs of programming like C can, being closer to the metal. 

Red will be the worlds first Full Stack Programming Language. This means that it will be an effective tool to do (almost) any programming task on every level 
from the metal to the meta without the aid of other stack tools. 
Furthermore Red will be able to cross-compile Red source code without using any GCC like toolchain 
from any platform to any other platform. And it will do this all from a binary executable that is supposed to stay under 1 MB.

Ready to learn your first Red?

```
Red

; Single-line comments start with a semicolon ';'

comment {
  Multi-line comments 
  look like this.
}

; Import files with #include and filenames start with a % sign
#include %includefile.red

; Your program's entry point is the first executable code that is found
; no need to restrict this to a 'main' function.

; Valid variable names start with a letter and can contain numbers, 
; variables containing only capital A thru F and numbers and ending with 'h' are 
; forbidden, because that is how hexadecimal numbers are expressed in Red and 
; Red/System.

; assign a value to a variable using a colon ':'
my-name: "Red"
reason-for-using-the-colon: {This makes the equality sign '=' exclusively usable for comparisons purposes, thus speeding up the original Rebol interpreter.}
is-this-name-valid?: true

; print output using print, or prin for printing without a newline or linefeed at the ; end of the printed text.

prin " My name is " print my-name
My name is Red

print ["My name is " my-name lf]
My name is Red

; In case you haven't already noticed: statements do NOT end with a semicolon ;-)

;
; Datatypes
;
; If you know Rebol, you probably have noticed it has lots of datatypes. Red 
; does not have yet all those types, but as Red want to be close to Rebol it 
; will have a lot of datatypes.
; You can recognize types by the exclamation sign at the end. But beware 
; names ending with an exclamation sign are allowed. 
; Some of the available types are integer! string! block! 

; Declaring variables before using them? 
; Red knows by itself what variable is best to use for the data you want to use it 
; for. 
; A variable declaration is not always necessary. 
; It is considered good coding practise to declare your variables,
; but it is not forced upon you by Red.
; You can declare a variable and specify its type. a variable's type determines its 
; size in bytes.

; Variables of integer! type are usually 4 bytes or 32 bits
my-integer: 0
; Red's integers are signed. No support for unsigned atm but that will come.

; To find out the type of variable use type?
type? my-integer
integer!

; chars are guaranteed to be 1 byte
; A string can be cast to a character 
empty-string: ""
a-string: to-string #"a"

i2: 1 + i1: 1

; Arithmetic is straightforward
i1 + i2 ; result 3
i2 - i1 ; result 1
i2 * i1 ; result 2
i1 / i2 ; result 0 (0.5, but truncated towards 0)

; Comparison operators are probably familiar, and unlike in other languages you 
; only need a single '=' sign for comparison.
; There is a boolean like type in Red. It has values true and false, but also the 
; values on/off or yes/no can be used

3 = 2 ; => false
3 != 2 ; => true
3 > 2 ; => true
3 < 2 ; => false
2 <= 2 ; => true
2 >= 2 ; => true

;
; Control Structures
; @TBD

;
; Functions
;
; In Red almost everything can be seen as a function. Even the IF returns a value.

; Function declaration syntax:
; function-name: function [][]
twice: function [a [integer!] /one return: [integer!]][
        c: 2
        a: a * c
        either one [a + 1][a]
]



```

## Further Reading

The main source for information about Red is [the Red language homepage](http://www.red-lang.org).  

To learn more about Rebol and Red join the [chat on StackOverflow](http://chat.stackoverflow.com/rooms/291/rebol-and-red). 
(You will need 20 points to chat but if you ask questions about Red or Rebol we will help you get those points).

Maybe you want to try Red right away? That is possible on the [try Rebol and Red site](http://tryrebol.esperconsultancy.nl).

You can also learn Red by learning [Rebol](http://www.rebol.com/docs.html). 
