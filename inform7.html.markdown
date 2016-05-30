---
language: Inform7
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
filename: LearnInform.Inform
---
Inform 7 is a natural language based language created by Graham Nelson and Emily Short for writing text adventures, but also potentially usable for other text based applications, especially data backed ones.

```
"LearnInform" by Hyphz

[This is a comment.]

[Inform 7 is a language designed for building text adventures.
It can be used for other purposes too, although the default 
library builds a text adventure. Inform 7 is object oriented.]

[This creates a class by subclassing. "Value" is the universal subclass,
but "object" is the most basic that behaves like an OO object.]
A datablock is a kind of object. 

[Classes can have properties.]
A datablock can be broken. [This creates a boolean property.]
A datablock is usually not broken. [This sets its default value.]
A datablock can be big or small. [This creates an enumerated property.]
A datablock is usually small. [This sets its default value.]
A datablock has a number called the sequence number. [This creates a typed property.]
A datablock has some text called the name. ["Some text" means a string.]
A datablock has a datablock called the chain. [Declared classes become types.]

[This creates a global named instance.]
Block1 is a datablock.
The sequence number of Block1 is 1.
The name of Block1 is "Block One."

[Functions and procedures are defined as "phrases".]
To do the thing everyone does with their first program:
	say "Hello World.". [Full stop indicates the end, indent indicates the scope.]
	
To dump (the block - a datablock): [That's how we create a parameter.]
	say the sequence number of the block;
	say the name of the block;
	if the block is broken, say "(Broken)".
		
To toggle (the block - a datablock):
	if the block is broken: [Conditional.]
		now the block is not broken; [Updating a property.]
	else:
		now the block is broken.
		
[Multiple parameters.]
To fix (the broken block - a datablock) using (the repair block - a datablock):
	if the broken block is not broken, stop; [Comma for a non indented single command.]
	if the repair block is broken, stop;
	now the sequence number of the broken block is the sequence number of the repair block;
	now the broken block is not broken.

[Because of its text adventure origins, Inform 7 doesn't generally allow objects
to be created dynamically, although there's a language extension that enables it.]	
Block2 is a datablock. 
Block2 is broken.
The sequence number of Block2 is 2.
The name of Block2 is "Block two."

To demonstrate calling a phrase with two parameters:
	Let the second block be block2; [Local pointer variable.]
	fix the second block using Block1;
	say the sequence number of the second block. [1.]
	
[Lists.]	
To show how to use list types:
	let the list be a list of datablocks;
	add Block1 to the list;
	add Block2 to the list;
	say the list; ["Block1 and Block2"]
	[Membership.]
	if Block1 is listed in the list:
		say "Block1 is there.";
	[Loop.]
	repeat with the block running through the list:
		dump the block;  [1 Block One. 1 Block Two.]
		[Remember block two's sequence number was changed above.]
	let X be entry 2 of the list; [Counting starts at 1.]
	dump X; ["1 Block two."]
	remove X from the list;
	say the list. [Block1]
		
[Here's how we define a function and do arithmetic.]

To decide which number is the sum of all numbers up to (X - a number) (this is summing up):
	let the total so far be a number;
	repeat with the current number running from 1 to X:
		now the total so far is the total so far + the current number;
	decide on the total so far. [This is the return statement.]
	
[ We have higher order functions too. ]

To demonstrate a higher order function:
	say summing up applied to {1, 2, 3, 4}.

To decide which number is the result of applying (phrase - phrase A -> A) twice to (B - a value of kind A):
	let b1 be phrase applied to B;
	let b2 be phrase applied to b1;
	decide on b2.
	
To demonstrate defining a higher order function:
	let X be 5;
	say the result of applying summing up twice to X.

[ Rulebooks allow a number of functions which apply to the same type under different conditions to be stacked. ]

Datablock validation rules is a datablock based rulebook.

A datablock validation rule for a broken datablock: rule fails.
A datablock validation rule for a datablock (called the block): 
	dump the block;
	rule succeeds.
		
To demonstrate invoking a rulebook:
	follow datablock validation rules for Block1;
	follow datablock validation rules for Block2.
	
[ Objects can also have relations, which resemble those in a relational database. ]
A dog is a kind of thing.
Rover is a dog.
The kennel is a container. [This is a built in base class.]
Rover is in the kennel. [This creates an inbuilt relation called "containment".]

[We can create relations by declaring their type.]

Guide dog ownership relates one dog to one person. [One-to-one.]
Property ownership relates various things to one person. [Many-to-one.]
Friendship relates various people to various people.  [Many-to-many.]

[To actually use them we must assign verbs or prepositions to them.]

The verb to own means the property ownership relation.
The verb to be the guide dog of means the guide dog ownership relation.
The verb to be guided by means the reversed guide dog ownership relation. 
The verb to be friends with means the friendship relation.

Edward is a person. A person can be blind. Edward is blind.
Edward is guided by Rover.
Benny is a person. Edward is friends with Benny.

To demonstrate looking something up with a relation:
	repeat with the dog running through things that are the guide dog of Edward:
		say the dog;
	repeat with the friend running through things that are friends with Edward:
		say the friend.

[We can also define relations that exist procedurally.]

Helpfulness relates a person (called the helper) to a person (called the helpee) when the helpee is blind and the helper is not blind.
The verb to be helpful to means the helpfulness relation.
To demonstrate using a procedural relation:
	repeat with the helper running through people that are helpful to Edward:
		say the helper.
	

[ Interface to the text adventure harness to allow the above code to be run. ]
Tutorial room is a room. 
"A rather strange room full of buttons. Push them to run the exercises, or turn on the robot to run them all."
A button is a kind of thing. A button is fixed in place. 

The red button is a button in tutorial room. 
Instead of pushing the red button, do the thing everyone does with their first program.
The green button is a button in tutorial room. 
Instead of pushing the green button, demonstrate calling a phrase with two parameters.
The blue button is a button in tutorial room. 
Instead of pushing the blue button, show how to use list types.
The cyan button is a button in tutorial room.
Instead of pushing the cyan button, say the sum of all numbers up to 5.
The purple button is a button in tutorial room.
Instead of pushing the purple button, demonstrate a higher order function.
The black button is a button in tutorial room.
Instead of pushing the black button, demonstrate defining a higher order function.
The white button is a button in tutorial room.
Instead of pushing the white button, demonstrate invoking a rulebook.
The puce button is a button in tutorial room.
Instead of pushing the puce button, demonstrate looking something up with a relation.
The orange button is a button in tutorial room.
Instead of pushing the orange button, demonstrate using a procedural relation.

The robot is an object in tutorial room.
Instead of switching on the robot:
	say "The robot begins to frantically flail its arms about.";
	repeat with button running through buttons in the tutorial room:
		say "The robot randomly hits [the button].";
		try pushing button.
```

##Ready For More?

* [Inform 7](http://www.inform7.com/)
