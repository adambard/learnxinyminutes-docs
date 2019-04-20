---
language: bc
contributors:
    - ["Btup"]
filename: learnlua.lua
---
```c
/*This is a multi-
line comment.*/
# This is also a (one-line) comment! (in GNU bc)
/*1. Variables and control structures*/
num = 45 /*All variables save only doubles, and you cannot save characters directly.*/
/*Blocks are denoted using the {} operators(similar to C):*/
while(num<50) {
    num += 1 /*equivalent to num=num+1. a = a op b is equivalent to a op= b.*/
}
/*And there are ++ and -- operators.*/
```
