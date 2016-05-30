---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Vojta Svoboda", "https://github.com/vojtasvoboda/"]
filename: learnbrainfuck-cz.bf
lang: cs-cz
---

Brainfuck (psaný bez kapitálek s vyjímkou začátku věty) je extrémně minimální 
Turingovsky kompletní (ekvivalentní) programovací jazyk a má pouze 8 příkazů.

Můžete si ho vyzkoušet přímo v prohlížeči s [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Jakýkoliv znak mimo "><+-.,[]" (bez uvozovek) je ignorován.

Brainfuck je reprezentován jako pole, které má 30.000 buněk s počátkem v nule 
a datovým ukazatelem na aktuální buňce.

Můžeme využít těchto osm příkazů:
+ : Přičte k aktuální buňce jedničku.
- : Odečte od aktuální buňky jedničku.
> : Posune datový ukazatel na další buňku, která je napravo.
< : Posune datový ukazatel na předchozí buňku, která je nalevo.
. : Vytiskne ASCII hodnotu aktuální buňky (například 65 = 'A').
, : Načte jeden znak do aktuální buňky.
[ : Pokud je hodnota aktuální buňky nulová, přeskočí na buňku odpovídající ] .
    Jinak skočí na další instrukci.
] : Pokud je hodnota aktuální buňky nulova, přeskočí na další instrukci.
    Jinak skočí zpět na instrukci odpovídající [ .

[ a ] tak tvoří 'while' smyčku a tyto symboly musí tak být v páru.

Pojďme se mrknout na některé brainfuck programy.

++++++ [ > ++++++++++ < - ] > +++++ .

Tento program vypíše písmeno 'A' (v ASCII je to číslo 65). Nejdříve navýší 
buňku #1 na hodnotu 6. Buňka #1 bude použita pro smyčku. Potom program vstoupí 
do smyčky ([) a sníží hodnotu buňky #1 o jedničku. Ve smyčce zvýší hodnotu 
buňky #2 desetkrát, vrátí ze zpět na buňku #1 a sníží její hodnotu o jedničku.
Toto se stane šestkrát (je potřeba šestkrát snížit hodnotu buňky #1, aby byla 
nulová a program přeskočil na konec cyklu označený znakem ].

Na konci smyčky, kdy jsme na buňce #1 (která má hodnotu 0), tak má buňka #2 
hodnotu 60. Přesuneme se na buňku #2 a pětkrát zvýšíme její hodnotu o jedničku 
na hodnotu 65. Na konci vypíšeme hodnotu buňky #2 - 65, což je v ASCII znak 'A' 
na terminálu.


, [ > + < - ] > .

Tento program přečte znak z uživatelského vstupu a zkopíruje ho do buňky #1.
Poté začne smyčka - přesun na buňku #2, zvýšení hodnoty buňky #2 o jedničku,
přesun zpět na buňku #1 a snížení její hodnoty o jedničku. Takto smyčka pokračuje
do té doby, než je buňka #1 nulová a buňka #2 nabyde původní hodnotu buňky #1. 
Protože jsme na buňce #1, přesuneme se na buňku #2 a vytiskneme její hodnotu 
v ASCII.

Je dobré vědět, že mezery jsou v programu uvedené pouze z důvodu čitelnosti. 
Program je možné klidně zapsat i takto:

,[>+<-]>.


Nyní se podívejte na tento program a zkuste zjistit co dělá: 

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Tento program vezme dvě čísla ze vstupu a vynásobí je.

Program nejdříve načte dvě vstupní hodnoty. Poté začíná smyčka řízená hodnotou
v buňce #1 - přesun na buňku #2 a start druhé vnořené smyčky, která je řízená 
hodnotou v buňce #2 a zvyšuje hodnotu v buňce #3. Nicméně je zde problém 
kdy na konci vnitřní smyčky je v buňce #2 nula a smyčka by tak znovu 
napokračovala. Vyřešíme to tak, že zvyšujeme o jedničku i buňku #4 a její 
hodnotu poté překopírujeme do buňky #2. Na konci programu je v buňce #3 
výsledek.
```

A to je brainbuck. Zase tak složitý není, co? Zkuste si nyní napsat nějaký
vlastní brainfuck program a nebo interpretr v jiném jazyce, což není zase
tak složité, ale pokud jste opravdový masochista, zkuste si naprogramovat
interpretr jazyka brainfuck v jazyce... brainfuck :)
