---

language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Ivan Sala", "http://slavni96.github.io/"]
lang: it-it

---

Brainfuck è un linguaggio di programmazione estremamente minimale,
ma è ingrado di rappresentare completamente una macchina di turnig,
e sfrutta solo 8 caratteri.
[Per saperne di più](http://it.wikipedia.org/wiki/Brainfuck)

```

Qualsiasi carattere che non sia "><+-.,[]" (escludendo gli apici) 
viene ignorato.
Branfuck è caratterizzato da un array (vettore) di 30,000 celle 
inizializzare a zero, e un puntatore che punta alla cella corrente.

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

Questo programma legge un carattere come input dall'utente, 
quindi salva il carattere dentro la cella #1.
In seguito, incominca a ciclare.
Si sposta alla cella #², e increementa il valore della cella (#2).
Quindi torna alla cella #1, e decrementa il valore della cella (#1).
Questo continua fino a quando la cella #²1 diventa 0, e quindi la cella #2
avrà il valore iniziale della cella #1.
Infine, visto che ci troviamo sulla cella #1 alla fine del ciclo, si sposta
sulla cella #2 e stampa il valore in ASCII.

Gli spazi nel codice sovrastante, sono presenti solo a scopo di ottenere
una maggiore leggibilità, si poteva anche scrivere senza:

,[>+<-]>.

Proviamo, adesso, a capire cosa fa invece questo programma:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Prende due numeri in input e quindi li moltiplica.

Prima prende in input i due numeri (,>,<), quindi inizia un cilclo
basandosi sulla cella #1.
Quindi si sposta sulla cella #2, e inizia un altro ciclo condizionato
dal valore della cella #2, incrementando la cella #3.
Arrivati a questo punto abbiamo un problema: alla fine del ciclo interno
la cella #2 ha valore 0. In questo caso, quando il ciclo esterno rifarà
partire il ciclo interno, non funzionerà più perchè la cella #2 ha valore 0.
Per ovviare a questo problema, oltre alla cella 3, incrementiamo anche la cella
#4, e alla fine di ogni ciclo interno copiala il valore della cella #4 
nella cella #2, in modo che il ciclo interno 
possa essere eseguito una altra volta.
Alla fine la cella #3 contiene il risultato.
```

E questo è brainfuck...Non è difficele, vero? 
Per divertimento adesso puoi scrivere i tuoi programmi in brainfuck,
oppure puoi scrivere un interprete brainfuck in un altro linguaggio.
L'interprete è abbastanza semplice da implementare, ma se sei veramente
masochista prova ad implementare un interprete brainfuck in...
brainfuck.
