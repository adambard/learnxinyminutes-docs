---
language: self
contributors:
    - ["Russell Allen", "http://github.com/russellallen"]
filename: learnself-ro.self
lang: ro-ro
translators:
    - ["Serban Constantin", "https://github.com/fuzzmz"]
---

Self este un limbaj orientat pe obiecte de prototyping rapid ce ruleaza in
propriul VM JIT. Majoritatea dezvoltarii este facuta prin interactionarea cu 
obiecte live printr-un mediu vizual de dezvoltare numit *morphic* ce contine
browsere si debugger integrat.

Totul in Self este un obiect. Toate functiile sunt efectuate trimitand mesaje
catre obiecte. Obiectele in Self pot fi intelese ca sloturi de perechi
cheie-valoare.

# Construirea obiectelor

Parserul Self integrat poate construi obiecte, inclusiv obiecte metoda. 

```
"Acesta este un comentariu"

"Un string:"
'Un string cu caractere \'escaped\'.\n'

"Un integer de 30 de biti"
23

"Un float de 30 de biti"
3.2

"-20"
-14r16

"Un obiect ce intelege doar un mesaj, 'x' ce returneaza 20"
(|
  x = 20.
|)

"Un obiect ce intelege si 'x:' ce seteaza slotul x"
(|
  x <- 20.
|)

"Un obiect ce intelege metoda 'doubleX' ce 
dubleaza valoarea lui x si apoi returneaza obiectul"
(|
  x <- 20.
  doubleX = (x: x * 2. self)
|)

"Un obiect ce intelege toate mesajele pe care 
'traits point' le intelege". Parserul verifica 
'traits point' prin trimiterea mesajelor 
'traits' apoi 'point' catre un obiect cunoscut sub numele 
de 'lobby'. Verifica obiectul 'true' tot prin trimiterea 
mesajului 'true' catre lobby."
(|     parent* = traits point.
       x = 7.
       y <- 5.
       isNice = true.
|)
```

# Trimiterea mesajelor catre obiect

Mesajele pot fi fie unare, binare sau keyword. Precedenta este in acea ordine.
Spre deosebire de Smalltalk, precedenta mesajelor binare trebuie specificata,
si toate keywords dupa primul trebuie sa inceapa cu litera mare.
Mesajele sunt separate de destinatie prin spatii libere.

```
"mesaj unar, trimite 'printLine' catre obiectul '23' 
care printeaza stringul '23' pe stdout si returneaza obiectul destinatie (ex 23)"
23 printLine

"trimite mesajul '+' cu '7' catre '23', apoi mesajul '*' cu '8' catre rezultat"
(23 + 7) * 8 

"trimite 'power:' catre '2' cu '8' returneaza 256"
2 power: 8 

"sends 'keyOf:IfAbsent:' to 'hello' with arguments 'e' and '-1'. 
Returns 1, the index of 'e' in 'hello'."
'hello' keyOf: 'e' IfAbsent: -1 
```

# Blocks

Self defines flow control like Smalltalk and Ruby by way of blocks. Blocks are delayed computations of the form:

```
[|:x. localVar| x doSomething with: localVar]
```

Examples of the use of a block:

```
"returns 'HELLO'"
'hello' copyMutable mapBy: [|:c| c capitalize] 

"returns 'Nah'"
'hello' size > 5 ifTrue: ['Yay'] False: ['Nah'] 

"returns 'HaLLO'"
'hello' copyMutable mapBy: [|:c| 
   c = 'e' ifTrue: [c capitalize]
            False: ['a']]
```

Multiple expressions are separated by a period. ^ returns immediately.

```
"returns An 'E'! How icky!"
'hello' copyMutable mapBy: [|:c. tmp <- ''| 
   tmp: c capitalize.
   tmp = 'E' ifTrue: [^ 'An \'E\'! How icky!'].
   c capitalize
   ]
```

Blocks are performed by sending them the message 'value' and inherit (delegate to) their contexts:
```
"returns 0"
[|x|
    x: 15.
    "Repeatedly sends 'value' to the first block while the result of sending 'value' to the
     second block is the 'true' object"
    [x > 0] whileTrue: [x: x - 1]. 
    x
] value
```

# Methods

Methods are like blocks but they are not within a context but instead are stored as values of slots. Unlike Smalltalk, methods by default return their final value not 'self'.

```
"Here is an object with one assignable slot 'x' and a method 'reduceXTo: y'.
Sending the message 'reduceXTo: 10' to this object will put 
the object '10' in the 'x' slot and return the original object"
(| 
    x <- 50.
    reduceXTo: y = (
        [x > y] whileTrue: [x: x - 1]. 
        self)
|)
.
```

# Prototypes

Self has no classes. The way to get an object is to find a prototype and copy it.

```
| d |
d: dictionary copy.
d at: 'hello' Put: 23 + 8.
d at: 'goodbye' Put: 'No!.
"Prints No!"
( d at: 'goodbye' IfAbsent: 'Yes! ) printLine.
"Prints 31"
( d at: 'hello' IfAbsent: -1 ) printLine.
```

# Further information

The [Self handbook](http://handbook.selflanguage.org) has much more information, and nothing beats hand-on experience with Self by downloading it from the [homepage](http://www.selflanguage.org).
