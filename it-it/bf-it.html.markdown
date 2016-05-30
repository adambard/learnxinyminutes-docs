---
language: bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Ivan Sala", "http://slavni96.github.io/"]
    - ["Christian Grasso", "http://chris54721.net"]
lang: it-it
---

Brainfuck è un linguaggio di programmazione
[Turing equivalente](https://it.wikipedia.org/wiki/Turing_equivalenza)
estremamente minimale, composto da solo 8 comandi.

Puoi provarlo nel tuo browser utilizzando
[brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```

Qualsiasi carattere diverso da "><+-.,[]" (escludendo gli apici)
viene ignorato.
Branfuck è caratterizzato da un array di 30,000 celle inizializzate a zero
e da un puntatore che punta alla cella corrente.

Vi sono otto comandi:
+ : Incrementa il valore della cella attuale di uno.
- : Decrementa il valore della cella attuale di uno.
> : Sposta il puntatore sulla cella seguente (sulla destra).
< : Sposta il puntatore sulla cella precendete (sulla sinistra).
. : Stampa il valore ASCII della cella corrente. (es. 65 = 'A')
, : Legge un singolo carattere come input e lo salva nella cella corrente.
[ : Se il valore della cella corrente è zero, prosegue fino alla ] corrispondente.
    Altrimenti, passa alla prossima istruzione.
] : Se il valore della cella corrente è zero, passa alla prossima istruzione.
    Altrimenti, torna indietro fino alla [ corrispondente.

[ e ] formano un ciclo while. Ovviamente dovranno essere bilanciati.
(Ad ogni [ dovrà corrispondere una ])

Ecco alcuni semplici esempi di programmi scritti in Brainfuck:

++++++ [ > ++++++++++ < - ] > +++++ .

Questo programma stampa in output la lettera 'A'. Prima di tutto, incrementa
la cella #1 fino al valore 6. La cella #1 verrà utilizzata per il ciclo.
Poi, entra nel ciclo ([) e si sposta alla cella #2. Incrementa la cella #2 10
volte, torna alla cella #1, e decrementa quest'ultima.
Il ciclo si ripete 6 volte (la cella #1 viene decrementata 6 volte prima di
raggiungere lo 0, quindi prosegue oltre la corrispondente ]).

A questo punto, siamo sulla cella #1, che ha valore 0, mentre la cella #2 ha
valore 60. Ci spostiamo sulla cella #2, la incrementiamo per 5 volte, ottenendo
il valore 65, quindi stampiamo il valore della cella #2.
Il valore 65 equivale ad 'A' in ASCII, per cui viene stampato 'A' nel terminale.


, [ > + < - ] > .

Questo programma legge un carattere come input dall'utente, quindi salva il
carattere nella cella #1. Dopodichè entra in un ciclo. Si sposta alla cella #2,
incrementa quest'ultima, torna alla cella #1, e decrementa quest'ultima.
Il ciclo continua fino a quando la cella #1 diventa 0, e quindi la cella #2
avrà il valore iniziale della cella #1. Infine, visto che ci troviamo sulla
cella #1 alla fine del ciclo, si sposta sulla cella #2 e stampa il valore in
ASCII.

Gli spazi nel codice sovrastante sono presenti solo a scopo di ottenere
una maggiore leggibilità. Lo stesso programma poteva essere scritto senza spazi:

,[>+<-]>.

Proviamo, adesso, a capire cosa fa invece questo programma:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Il programma legge 2 numeri come input dall'utente, e li moltiplica.

Innanzitutto, legge in input i due numeri. Poi entra nel ciclo più esterno
basandosi sulla cella #1. Quindi si sposta sulla cella #2, e inizia il ciclo
più interno basandosi sul valore della cella #2, incrementando la cella #3.
Arrivati a questo punto abbiamo un problema: alla fine del ciclo interno
la cella #2 avrà valore 0. Ciò impedirà di eseguire nuovamente il ciclo interno.
Per ovviare a questo problema, incrementiamo anche la cella #4, e copiamo il
valore di quest'ultima nella cella #2.
Il risultato sarà infine contenuto nella cella #3.
```

E questo è brainfuck. Non è così difficile, eh? Se vuoi, ora puoi scrivere per
divertimento altri programmi in brainfuck, oppure scrivere un interprete
brainfuck in un altro linguaggio. L'interprete è abbastanza semplice da
implementare, ma se sei veramente masochista, prova ad implementare un interprete brainfuck... in brainfuck.
