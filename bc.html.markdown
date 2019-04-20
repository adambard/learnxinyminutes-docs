---
language: bc
contributors:
    - ["Btup"]
filename: learnlua.lua
---
```c
/*This is a multi-
line comment.*/
# This is also a (one-line) comment! (in GNU bc).
/*1. Variables and control structures*/
num = 45 /*All variables save only doubles, and you cannot save characters directly.*/
/*Blocks are denoted using the {} operators(similar to C):*/
while(num<50) { /*While loop. Execute a block of code until the condition is falsy.*/
    num += 1 /*equivalent to num=num+1. a = a op b is equivalent to a op= b.*/
}
/*And there are ++ and -- operators.*/
/*If clauses:*/
hour=read() /*Input a number*/
if(hour < 12) { /*Operators are exactly like C. <, >, <=, >=, ==, !=*/
    print "Good morning\n" /*"print" outputs strings or variables separated by commas.*/
} else if(hour == 12) {
    print "Hello\n"
} else {
    /*Variables are global by default.*/
    thisIsGlobal = 5
    /*Sorry, you cannot make a variable local.*/
}
/*Every variable is pre-set to 0.*/
foo=blankVariable /*foo is set to 0.*/
/*Like C, only 0 is falsy.*/
if(!foo){print "false\n"}
/*Unlike C, bc does not have the ?: operators. For example, this block of code will cause an error:
a=(foo)?1:0
However, you can simulate one:*/
a=(foo)&&(1)||(0) /*&& is and, || is or*/
/*For loops*/
num = 0
for(i = 1; i<=100; i++) {/*Similar to the C for loop.*/
    num += i
}
/*for (begin; condition; end) first executes begin, then executes a block of code inside a while
loop with condition, and always executes end at the end of the while loop.*/
/*2.Functions*/
define fac(n) { /*define a function using define*/
    if(n == 1 || n == 0) {
        return 1 /*return a value*/
    }
    return n * fac(n - 1) /*recursion is posssible*/
}
```
