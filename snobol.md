---
name: SNOBOL
contributors:
    - ["Platon Kralin", "http://github.com/10Person"]
filename: learn.sbl
---
```SNOBOL
* SNOBOL, also known for its compiled version: SPITBOL, is a unique programming language from the 1960s 
* best known for its pattern matching and flow control systems.
* 
* Comments are prefixed by *, which also doubles as the multiplication sign, only being able to be declared in the label part of a line..
* 
* Every line in SNOBOL consists of 5 optional parts:
*     [label] [subject] [pattern] [replacement] [goto]
*
* For example let's look at a Hello World sample program:
	OUTPUT = "HELLO, WORLD!"
END

* In the first line, the [label] is omitted with a tab, this is MANDATORY. 
* Then in the [subject] the special variable OUTPUT is declared, which prints out its value whenever it is reassigned.
* There's also the PUNCH variable, which punches out its value to a tape punch whenever it is reassigned.
* OUTPUT is assigned in the [replacement], which always starts with '=' ([goto] starts with ':'), and assigns
* "HELLO, WORLD!" to the OUTPUT variable.
* 
* It is also probably important to note that most modern SNOBOL implementations are case insensitive,
* But traditionally SNOBOL is written in ALL CAPS due to a lack of updates past the 1970s.
* 
* Finally END is a special keyword that ends the program. Everything after it is entirely ignored.
```

```SNOBOL
* On more syntax:
* Assignment is done with:
	variable = value

* For example all of the following are valid:
	number = 5
	string.1 = 'CAT'
	another_number = 5 ** 2 + 5 - (5 * 10) / 2

* Variable names must start with a letter and can then contain letters, numbers, periods and underscores.
* The example also shows us all the possible arithmetic operators, which are as follows in their order of operation:
* [Unary Operators such as negation: -], (Anything in parentheses), Exponentiation: ** or !, Multiplication: *, Division: /, Addition: +, Subtraction: -.
* Unary operators cannot have a space between them and their operand, while arithmetic ones must. As follows:
	negative_number = -5
	calculated_number = 5 * 5

* Arithmetic variable definitions can also contain variables, real numbers (Floats in modern programmer parlance):
	PI = 3.14
	RADIUS = 5.
	RADIUS = 5.0
	CIRC_AREA = PI * RADIUS ** 2
	
* If an integer is multiplied by a real number, the result is an integer.
* Strings can be assigned with single or double quotes, with the other type not counting if encloses:
	Plea = 'He shouted, "Please"'
	With_Apostrophe = "It's good to be able to put '' in a string."

* A blank variable is called a NULL String and can be assigned as:
	NULL = 
* It is the default value of all variables.
* Some strings can also be used in arithmetic expressions, with integer strings only having digits and signs, and real strings having at least one number before the decimal point.
* I.E. All of the following are valid:
	INT_STRING = "-5"
	REAL_STRING = '1.048'
	SUM = INT_STRING + REAL_STRING + '55'

* Strings can be concatenated simply by putting them together in a variable declaration or inside parentheses. I.E. the following would result in "Two Strings."
	Two_Strings = "Two " 'Strings.'
	("Two " 'Strings.')
* This also works with floats and reals.
*
* There are three special variables with strings: OUTPUT, PUNCH, and INPUT. The first two send their values to either be printed or punched out (as before),
* but the last reads in a string. I.E. the following echos the user input:
	OUTPUT = INPUT
*
* 
* Now for the really special part: Pattern Matching. The simplest form of pattern matching is just a [subject] with a [pattern]. Only the [subject] requires parentheses.
	"Hello" "EL"
* The other form of pattern matching is the Replacement Statement, taking the role of [subject] [pattern] = [replacement], modifying a string for every math. I.E.
	WORD = "This is BAD"
	WORD 'BAD' = "GOOD"
* This replaces BAD with GOOD.
* There are also two operations for more complex patterns, alteration with |, the pipe, and concatenation, as explained before. Concatenation has precedence over Alteration. I.E.
	STATEMENT = "The animal goes barks woof"
	STATEMENT ("barks" | "goes") " " ("woof" "yip")

* This succeeds if the statement contains "barks woof", "goes woof", "barks yip" or "goes yip" but not, say, "woof woof".
* You can also compose them with separate variables.

* Additionally if one wants to match a matched pattern to a variable, one can do this with [pattern] . variable
	"HEX" ('HEX' | 'DEC') . BASE
* This would have BASE equal to HEX. But:
	"DEC" ('HEX' | 'DEC') . BASE
* Would have BASE equal to DEC.

* The next unique aspect of SNOBOL is flow control. Every SNOBOL expression can return a success or failure.
* This can then be used to jump to a label depending on it with a goto, or :, in the [goto]. I.E.
	STATEMENT = "The animal goes barks woof"
	STATEMENT ("barks" | "goes") " " ("woof" "yip") :s(IS_A_DOG) f(NOT_A_DOG)
IS_A_DOG OUTPUT = "Yes, the animal is a dog." :(END)
NOT_A_DOG OUTPUT = "No, the animal is not a dog." :(END)

* :(END) is required so that only one of the lines is processed.

END

* This isn't a full description of the language, but should be enough for a beginner to start.
```

## Additional Resources:
- [THE SNOBOL 4 PROGRAMMING LANGUAGE 2ND EDITION](https://ftp.regressive.org/snobol/misc/ftp.cs.arizona.edu/gb.pdf)
- [REGRESSIVE.ORG's SNOBOL RESOURCES](https://www.regressive.org/snobol4/)
- [A x64 SPITBOL IMPLEMENTATION](https://github.com/spitbol/x64)
