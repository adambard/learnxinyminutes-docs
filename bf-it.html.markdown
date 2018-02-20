---
language: "Brainfuck"
filename: brainfuck.bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
    - ["Alexandru Cazacu", "https://github.com/alexandru-cazacu"]
lang: it-it
---

Brainfuck (maiuscolo solo all'inizio di una frase) è un linguaggio di
programmazione Turing-completo estremamente minimalistico composto da soli 8 comandi.

Potete provare brainfuck sul vostro browser tramite [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```bf
Ciascun carattere tranne "><+-.,[]" (escluse le virgolette) è ignorato.

Brainfuck è rappresentato da un array di 30,000 celle inizializzate a zero
e da un puntatore che punta alla cella corrente.

Ci sono otto comandi:
+ : Incrementa il valore nella cella corrente di uno.
- : Decrementa il valore nella cella corrente di uno.
> : Muove il puntatore alla cella successiva (cella a destra).
< : Muove il puntatore alla cella precedente (cella a sinistra).
. : Stampa il valore ASCII nella cella corrente (es. 65 = 'A').
, : Legge un singolo carattere in input nella cella corrente.
[ : Se il valore nella cella corrente è zero, salta al corrispondente ] .
    Altrimenti vai alla prossima istruzione.
] : Se il valore nella cella corrente è zero, vai alla prossima istruzione.
    Altrimenti salta al corrispondente [ .

[ and ] formano un ciclo while. Ovviamente devono essere in numero uguale.

Diamo un'occhiata ad alcuni programmi brainfuck basilari.

++++++ [ > ++++++++++ < - ] > +++++ .

Questo programma stampa la lettera 'A'. Prima, incrementa la cella #1 a 6.
La cella #1 sarà usata per il ciclo. Poi, entra nel ciclo ([) e si sposta
alla cella #2. Incrementa la cella #2 10 volte, ritorna indietro alla cella #1
e la decrementa. Questo ciclo avviene 6 volte (servo 6 decrementi affinchè
la cella #1 raggiunga 0, a quel punto salta al corrispondente ] e continua
l'esecuzione).

A questo punto siamo nella cella #1, che ha un valore di 0, mentre la
cella #2 ha un valore di 60. Ci muoviamo alla cella #2, la incrementiamo
5 volte, per arrivare a 65 e poi stampiamo il valore della cella #2.
65 equivale ad 'A' in ASCII, quindi 'A' viene stampato nel terminale.

, [ > + < - ] > .

Questo programma legge un carattere immesso dall'utente e lo copia nella
cella #1. Poi inizi un loop. Si muove nella cella #2, incrementa il valore
della cella #2, si muove alla cella #1 e decrementa il suo valore. Questo
ciclo continua finchè la cella #1 non vale 0, e la cella #2 contiene il vecchio
valore della cella #1. Siccome siamo nella cella #1 alla fine del ciclo,
ci muoviamo alla cella #2 e stampiamo il valore ASCII.

Tenete in mente che gli spazi sono puramente estetici. Si potrebbe benissimo
anche scrivere come:

,[>+<-]>.

Cercate di capire cosa fa questo programma:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Questo programma prende 2 numeri in input e li moltiplica.

Per prima cosa prende in input 2 numeri. Poi inizia il ciclo esterno,
condizionato dalla cella #1. Poi si muove nella cella #2 e inizia il ciclo
interno, condizionato dalla cella #2, incrementando la cella #3. Però c'è
un problema: alla fine del ciclo interno la cella #2 è zero. In questo caso
il ciclo interno non funzionerà pià fino alla volta seguente. Per risolvere
il problema, incrementiamo la cella #4 e poi ricopiamo #4 in #2. La cella #3
è il risultato.
```
E questo è brainfuck. Non è così difficile, vero? Per passare il tempo
potreste provare a scrivere un pogramma in brainfuck, oppure potete
scrivere un interprete brainfuck in un altro linguaggio. L'interprete
è relativamente semplice da implementare, ma se siete masochisti, potete
provare a scrivere un interprete brainfuck... in brainfuck.
And that's brainfuck. Not that hard, eh? For fun, you can write your own
brainfuck programs, or you can write a brainfuck interpreter in another
language. The interpreter is fairly simple to implement, but if you're
a masochist, try writing a brainfuck interpreter… in brainfuck.
