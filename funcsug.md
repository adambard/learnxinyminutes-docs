---
name: FuncSug
contributors:
    - ["cl4cnam", "https://github.com/cl4cnam"]
filename: learnfuncsug.fg
---

FuncSug is a programming language designed (as a complement of JavaScript) to simplify GUI programming.
It enables a more linear code in line with async/await and structured concurrency.
It replaces event-driven syntaxes with easy-to-use concurrent syntaxes.

```gdscript
# Boolean, Numbers, Strings, Compound assignment
var a := true
a := 3.5
a := 'Hello' + " " + `world!`
a += 'and hello everybody!'

# Print onto the console
print('Hello world!)

# Print onto the browser
displayNewMessage('Hello world!')

# Indentation (one tab) is significant as in Python
if n>0:
	doThis()
else:
	doThat()

def myFunction(p_param1,...,p_paramN):
	doThis()

# A "parallel" block ends when all the branches are ended
parallel ||
	doThis()
||
	doThat()

# Retrieve the value of a 'parallel' block
result := parallel ||
	doThis()
||
	doThat()

# Parallel block that ends when N branches are ended
# (the others branches are automatically and definitively interrupted)
parallel exitAfter N finished ||
	doThis1()
||
	[...]
||
	doThisM()

# Parallel block that selects the N branches that reached a certain point
# (the others branches are automatically and definitively interrupted)
# "...-------" is to be written to indicate the point to be reached
parallel(select N) ||
||===========
	doThis1_firstPart()
...---------
	doRest1()
[...]
||===========
	doThisM_firstPart()
...---------
	doRestM()

# Await a variable (that is, await an assignment to it), an event, a duration
awaitBeep myVar
awaitClickBeep(myButton)
awaitClickBeep('#myButtonId')
waitSeconds(numberOfSeconds)

# Await a text by the user
# (like "input()" in Python, but in the browser, not in the console)
var text := awaitHumanText()

# Include JavaScript snippets with a 'js' block;
# var1,...,varN are the variables transmitted from FuncSug to JavaScript
var result := js (var1,...,varN):
	// JavaScript/DOM code
	document.getElementById('myLabel').style.color = var6
	return var1*var2+var3+Math.floor(var4/var5)

var a := 2025
var b := js (a):
	return Math.sqrt(a)
displayNewMessage('sqrt(' + a + ') = ' + b)
```

Simple examples

```gdscript
# Play multiple sounds at the same time
parallel ||
	playSoundFile('sound1.mp3')
||
	playSoundFile('sound2.mp3')
||
	playSoundFile('sound3.mp3')

# Play multiple sounds one at a time
playSoundFile('sound1.mp3')
playSoundFile('sound2.mp3')
playSoundFile('sound3.mp3')

# Hello you
displayNewMessage('What is your name?')
var theName := awaitHumanText()
displayNewMessage('Hello, ' + theName + '!')

# Choice
displayNewMessage('<button id="A">Button A</button><button id="B">Button B</button>')

parallel(select 1) ||
||=================
	awaitClickBeep('#A')
...---
	displayNewMessage("You've chosen A")
||================
	awaitClickBeep('#B')
...---
	displayNewMessage("You've chosen B")

# Quiz question with timeout
displayNewMessage('4+5=?')
parallel exitAfter 1 finished ||
	var answered := awaitHumanText()
	if answered = 9:
		displayNewMessage('Yes!')
	else:
		displayNewMessage('No! 9')
||
	waitSeconds(5)
	displayNewMessage('Too late!')

# Quiz question with timeout (better version)
displayNewMessage('4+5=?')
parallel(select 1) ||
||==================================
	var answered := awaitHumanText()
...---------------------------------
	if answered = 9:
		displayNewMessage('Yes!')
	else:
		displayNewMessage('No! 9')
||================
	waitSeconds(5)
...---------------
	displayNewMessage('Too late!')

# Guess the number
var numberToGuess := randomIntBetween(1, 100)
var triedNumber := 0
displayNewMessage('Guess my number (between 1 and 100)!')

while triedNumber != numberToGuess:
	
	triedNumber := awaitHumanText()
	
	if triedNumber < numberToGuess:
		displayNewMessage('Too low! Try again!')
	if triedNumber > numberToGuess:
		displayNewMessage('Too high! Try again!')
	if triedNumber = numberToGuess:
		displayNewMessage('Well Done!')
```

* [Online Playground](https://cl4cnam.github.io/try_FuncSug)

* [Tutorials](https://github.com/cl4cnam/funcSug/wiki/Tutorials)

* [GitHub repository](https://github.com/cl4cnam/funcSug)
