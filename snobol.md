---
name: SNOBOL
contributors:
    - ["Platon Kralin", "http://github.com/10Person"]
filename: learn.sbl
---
```SNOBOL
* SNOBOL, also known for its compiled version: SPITBOL, is a unique programming 
* language from the 1960s best known for its pattern matching and flow control 
* systems.
* 
* Comments are prefixed by *, which also doubles as the multiplication sign, 
* only being able to be declared in the label part of a line..
* 
* Every line in SNOBOL consists of 5 optional parts:
*     [label] [subject] [pattern] [replacement] [goto]
*
* For example let's look at a Hello World sample program:
	OUTPUT = "HELLO, WORLD!"
END

* In the first line, the [label] is omitted with a tab, this is MANDATORY. 
* Then in the [subject] the special variable OUTPUT is declared, which prints 
* out its value whenever it is reassigned.
* There's also the PUNCH variable, which punches out its value to a tape punch 
* whenever it is reassigned.
* OUTPUT is assigned in the [replacement], which always starts with '=' ([goto] 
* starts with ':'), and assigns "HELLO, WORLD!" to the OUTPUT variable.
* 
* It is also probably important to note that most modern SNOBOL implementations 
* are case insensitive, but traditionally SNOBOL is written in ALL CAPS due to a
*  lack of updates past the 1970s.
* 
* Finally END is a special keyword that ends the program. Everything after it is
* entirely ignored.
```

```SNOBOL
* On more syntax:
* Assignment is done with:
	variable = value

* For example all of the following are valid:
	number = 5
	string.1 = 'CAT'
	another_number = 5 ** 2 + 5 - (5 * 10) / 2

* Variable names must start with a letter and can then contain letters, numbers, 
* periods and underscores.
* The example also shows us all the possible arithmetic operators, 
* which are as follows in their order of operation:
* [Unary Operators such as negation: -], (Anything in parentheses), 
* Exponentiation: ** or !, Multiplication: *, Division: /, Addition: +, 
* Subtraction: -.
* 
* Unary operators cannot have a space between them and their operand, 
* while arithmetic ones must. As follows:
	negative_number = -5
	calculated_number = 5 * 5

* Arithmetic variable definitions can also contain variables, real numbers 
* (Floats in modern programmer parlance):
	PI = 3.14
	RADIUS = 5.
	RADIUS = 5.0
	CIRC_AREA = PI * RADIUS ** 2
	
* If an integer is multiplied by a real number, the result is a real.
* Strings can be assigned with single or double quotes, with the other type not 
* counting if enclosed by the other:
	Plea = 'He shouted, "Please!"'
	With_Apostrophe = "It's good to be able to put '' in a string."

* A blank variable is called a NULL String and can be assigned as:
	NULL = 
* It is the default value of all variables.
* Some strings can also be used in arithmetic expressions, with integer strings 
* only having digits and signs, and real strings having at least one number 
* before the decimal point.
* I.E. All of the following are valid:
	INT_STRING = "-5"
	REAL_STRING = '1.048'
	SUM = INT_STRING + REAL_STRING + '55'

* Strings can be concatenated simply by putting them together in a variable 
* declaration or inside parentheses. I.E. the following would result in "Two Strings."
	Two_Strings = "Two " 'Strings.'
	("Two " 'Strings.')
* This also works with floats and reals.
*
* There are three special variables with strings: OUTPUT, PUNCH, and INPUT. 
* The first two send their values to either be printed or punched out 
* (as before), but the last reads in a string. I.E. the following echos the user 
* input:
	OUTPUT = INPUT
*
* 
* Now for the really special part: Pattern Matching. The simplest form of 
* attern matching is just a [subject] with a [pattern]. Only the [subject] 
* requires parentheses.
	"Hello" "EL"
* The other form of pattern matching is the Replacement Statement, taking the 
* role of [subject] [pattern] = [replacement], modifying a string for every math:
	WORD = "This is BAD"
	WORD 'BAD' = "GOOD"
* This replaces BAD with GOOD.
* There are also two operations for more complex patterns, alteration with |, 
* the pipe, and concatenation, as explained before. Concatenation has precedence 
* over alteration. For example:
	STATEMENT = "The animal goes barks woof"
	STATEMENT ("barks" | "goes") " " ("woof" "yip")

* This succeeds if the statement contains "barks woof", "goes woof", "barks yip"
* or "goes yip" but not, say, "woof woof".
* You can also compose them with separate variables.

* Additionally if one wants to match a matched pattern to a variable, one can do
*  this with [pattern] . variable. I.E.
	"HEX" ('HEX' | 'DEC') . BASE
* This would have BASE equal to HEX. But:
	"DEC" ('HEX' | 'DEC') . BASE
* Would have BASE equal to DEC.

* The next unique aspect of SNOBOL is flow control. Every SNOBOL expression can 
* return a success or failure.
* This can then be used to jump to a label depending on it with a goto, or :, 
* in the [goto]. I.E.
	STATEMENT = "The animal goes barks woof"
	STATEMENT ("barks" | "goes") " " ("woof" "yip") :s(IS_A_DOG) f(NOT_A_DOG)
IS_A_DOG OUTPUT = "Yes, the animal is a dog." :(PASS)
NOT_A_DOG OUTPUT = "No, the animal is not a dog." :(PASS)

PASS
* :(PASS) is required so that only one of the lines is processed.

* For function definitions:
	DEFINE("PRINT(S)",'D1')		:S(ENDPRINT)
D1 OUTPUT = S :(RETURN)
ENDPRINT

	PRINT("TEST!")
* DEFINE is a built-in function to define the functions, followed the inputs, 
* then a comma and the entry label for the function. By default it's the
* name of the function. One must also define an endpoint for the function in the
* success goto, which can be any free name. The RETURN goto retuns a success.
* Then FRETURN is a special goto that returns a failure. In the end, "TEST!"
* should be printed.

* Function with return values:
	DEFINE("RETURN_SQRT(X, Y)")		:S(ENDSQRT)
RETURN_SQRT RETURN_SQRT = x ** (1.0 / Y) 	:S(RETURN) F(FRETURN)
ENDSQRT

	OUTPUT = RETURN_SQRT(25, 2)
* To give a function a return value, one simply assigns a value to its name as if
* it were a variable. Note that for non-integer numbers  to exist one must use 
* REALS. Here the result should be 5.

END

* This isn't a full description of the language (For example there are more 
* built-in elements than shown here), but should be enough for a beginner to it 
* to start out.
```

## Additional Resources:
- [THE SNOBOL 4 PROGRAMMING LANGUAGE 2ND EDITION](https://ftp.regressive.org/snobol/misc/ftp.cs.arizona.edu/gb.pdf)
- [REGRESSIVE.ORG's SNOBOL RESOURCES](https://www.regressive.org/snobol4/)
- [A x64 SPITBOL IMPLEMENTATION](https://github.com/spitbol/x64)
- [RATFACTOR'S EXCELLENT BLOGPOST ON THE LANGUGAE](https://www.ratfactor.com/snobol/judgement)
