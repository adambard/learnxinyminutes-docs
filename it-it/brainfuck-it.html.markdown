---

language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Ivan Sala", "http://slavni.github.io/"]
lang: it-it

---

Brainfuck è un linguaggio di programmazione estremamente minimale,
ma è ingrado di rappresentare completamente una macchina di turnig,
e sfrutta solo 8 caratteri.
[Per saperne di più](http://it.wikipedia.org/wiki/Brainfuck)

```

Qualsiasi carattere che non sia "><+-.,[]" (escludendo gli apici) viene ignorato.
Branfuck è caratterizzato da un array (vettore) di 30,000 celle inizializzare a zero, e un puntatore che punta alla cella corrente.

Vi sono solo otto comando:
+ : Incrementa il valore della cella attuale di uno.
- : Decrementa il valore della cella attuale di uno.
> : Sposta il puntatore sulla cella seguente (prossima a destra).
< : Sposta il puntatore sulla cella precendete (precedente a sinistra).
. : Stampa il valore in ASCII della cella corrente. (es: 65 = 'A')
, : Legge un singolo carattere come input per la cella corrente.
[ : Se il valore della cella corrente è zero, conclude il ciclo 
    andando alla sua corrispondente ].
    Altrimenti, passa alla prossima istruzione.
] : Se il valore della cella corrente è zero, passa alla prossima istruzione.
    Altrimenti torna indetro fino alla [ corrispondente. 

[ e ] creano un loop (while). Ovviamente dovranno essere bilanciati.
Per ogni [ dovrà corrispondere una ]

Alcuni semplici esempi di programmi scritti in Brainfuck:

++++++ [ > ++++++++++ < - ] > +++++ .

Questo programma stampa in output la lettera 'A'. Priam incrementa
la cella #1 fino a 6, Quindi la cella #1 viene usata per crare un ciclo.
Poi, entra in un loop ([) e si sposta alla cella #2.
Incrementa la cella #2 10 volte, e torna alla cella #1, e la decrementa.
Questo avviene 6 volte (servono che la cella #1 venga decrementata 6 volte
per raggiungere lo 0. Quindi passa alla corrispondente ] e prosegue).

A questo punto, siamo sulla cella #1, che ha valore 0, 
la cella #2 ha valore 60 (6*10). Ci spostiamo sulla cella #2, incrementiamo
per 5 volte, e otteniamo il valore 65, quindi stampaimo il valore della cella
#2 (.).
65 è 'A' in ASCII, quindi alla fine viene stampata 'A'.


, [ > + < - ] > .

This program reads a character from the user input and copies the character into
cell #1. Then we start a loop. Move to cell #2, increment the value at cell #2,
move back to cell #1, and decrement the value at cell #1. This continues on
until cell #1 is 0, and cell #2 holds cell #1's old value. Because we're on
cell #1 at the end of the loop, move to cell #2, and then print out the value
in ASCII.

Also keep in mind that the spaces are purely for readability purposes. You
could just as easily write it as:

,[>+<-]>.

Try and figure out what this program does:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

This program takes two numbers for input, and multiplies them.

The gist is it first reads in two inputs. Then it starts the outer loop,
conditioned on cell #1. Then it moves to cell #2, and starts the inner
loop conditioned on cell #2, incrementing cell #3. However, there comes a
problem: At the end of the inner loop, cell #2 is zero. In that case,
inner loop won't work anymore since next time. To solve this problem,
we also increment cell #4, and then recopy cell #4 into cell #2.
Then cell #3 is the result.
```

E questo è brainfuck...Non è difficele, vero? 
Per divertimento adesso puoi scrivere i tuoi programmi in brainfuck,
oppure puoi scrivere un interprete brainfuck in un altro linguaggio.
L'interprete è abbastanza semplice da implementare, ma se sei veramente
masochista prova ad implementare un interprete brainfuck in...
brainfuck.

