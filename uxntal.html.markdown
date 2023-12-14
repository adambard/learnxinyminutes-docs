---
language: uxntal
contributors:
    - ["Devine Lu Linvega", "https://wiki.xxiivv.com"]
filename: learnuxn.tal
---

Uxntal is a stack-machine assembly language targeting the [Uxn virtual machine](https://wiki.xxiivv.com/site/uxn.html).

Stack machine programming might look at bit odd, as it uses a postfix notation, 
which means that operators are always found at the end of an operation. For 
instance, one would write 3 4 + instead of 3 + 4. 

The expression written (5 + 10) * 3 in conventional notation would be 
written 10 5 + 3 * in reverse Polish notation.

```forth
( This is a comment )

( All programming in Unxtal is done by manipulating the stack )

#12 ( push a byte )
#3456 ( push a short )

( Uxn has 32 opcodes, each opcode has 3 possible modes )

POP ( pop a byte )
POP2 ( pop a short )

( The modes are:
	[2] The short mode consumes two bytes from the stack.
	[k] The keep mode does not consume items from the stack.
	[r] The return mode makes the operator operate on the return-stack. )

#12 #34 ADD ( 46 )
#12 #34 ADDk ( 12  34  46 )

( The modes can be combined )

#1234 #5678 ADD2k ( 12  34  56  78  68  ac )

( The arithmetic/bitwise opcodes are:
	ADD SUB MUL DIV
	AND ORA EOR SFT )

( New opcodes can be created using macros )

%MOD2 { DIV2k MUL2 SUB2 }

#1234 #0421 MOD2 ( 01  b0 )

( ---------------------------------------------------------------------------- )

( A short is simply two bytes, each byte can be manipulated )

#1234 SWP ( 34  12 )
#1234 #5678 SWP2 ( 56  78  12  34 )
#1234 #5678 SWP ( 12  34  78  56 )

( Individual bytes of a short can be removed from the stack )

#1234 POP ( 12 )
#1234 NIP ( 34 )

( The stack opcodes are:
	POP DUP NIP SWP OVR ROT )

( ---------------------------------------------------------------------------- )

( To compare values on the stack with each other )

#12 #34 EQU ( 00 )
#12 #12 EQU ( 01 )

( Logic opcodes will put a flag with a value of either 00 or 01 )

#12 #34 LTH 
#78 #56 GTH 
	#0101 EQU2 ( 01 )

( The logic opcodes are:
	EQU NEQ GTH LTH )

( ---------------------------------------------------------------------------- )

( Uxn's accessible memory is as follows: 
	256 bytes of working stack 
	256 bytes of return stack
	65536 bytes of memory
	256 bytes of IO memory )

( The addressable memory is between 0000-ffff )

#12 #0200 STA ( stored 12 at 0200 in memory )
#3456 #0201 STA2 ( stored 3456 at 0201 in memory )
#0200 LDA2 ( 12  34 )

( The zero-page can be addressed with a single byte )

#1234 #80 STZ2 ( stored 12 at 0080, and 34 at 0081 )
#80 LDZ2 ( 12  34 )

( Devices are ways for Uxn to communicate with the outside world
	There is a maximum of 16 devices connected to Uxn at once
	Device bytes are called ports, the Console device uses the 10-1f ports
	The console's port 18 is called /write )

%EMIT { #18 DEO }

#31 EMIT ( print "1" to console )

( A label is equal to a position in the program )
@parent ( defines a label "parent" )
	&child ( defines a sublabel "parent/child" )

( Label positions can be pushed on stack )
;parent ( push the absolute position, 2 bytes )
,parent ( push the relative position, 1 byte )
.parent ( push the zero-page position, 1 byte )

( The memory opcodes are:
	LDZ STZ LDR STR
	LDA STA DEI DEO )

( ---------------------------------------------------------------------------- )

( Logic allows to create conditionals )

#12 #34 NEQ ,skip JCN
	#31 EMIT
	@skip

( Logic also allows to create for-loops )

#3a #30
@loop
	DUP EMIT ( print "123456789" to console )
	INC GTHk ,loop JCN
POP2

( Logic also allows to create while-loops )

;word
@while
	LDAk EMIT
	INC2 LDAk ,while JCN
POP2
BRK

@word "vermillion $1

( Subroutines can be jumped to with JSR, and returned from with JMP2r )

;word ,print-word JSR
BRK

@print-word ( word* -- )
	@while
		LDAk EMIT
		INC2 LDAk ,while JCN
	POP2
JMP2r

@word "cerulean

( The jump opcodes are: 
	JMP JCN JSR )
```

## Ready For More?

* [Uxntal Lessons](https://compudanzas.net/uxn_tutorial.html)
* [Uxntal Assembly](https://wiki.xxiivv.com/site/uxntal.html)
* [Uxntal Resources](https://github.com/hundredrabbits/awesome-uxn)
