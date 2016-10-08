---
language: bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Petru Dimitriu", "http://petru-dimitriu.github.io"]
filename: bf-ro.html
lang: ro-ro
---

Brainfuck (un nume propriu care nu primește majusculă inițială decât la începutul
propoziției) este un limbaj de programare Turing-comple extrem de minimalist cu
doar 8 instrucțiuni.

Puteți încerca brainfuck în navigatorul dumneavoastră cu [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Orice caracter in afara de "><+-.,[]" (fara ghilimele) este ignorat.

Brainfuck se reprezinta ca un vector de 30 000 de celule initializate cu zero
si un pointer de date care trimite spre celula curenta.

Exista opt comenzi:
+ : Incrementeaza valoarea celulei curente cu 1.
- : Decrementeaza valoarea celulei curente cu 1.
> : Muta pointerul de date la urmatoarea celula (o celula la dreapta).
< : Muta pointerul de date la celula precedenta (o celula la stanga).
. : Afiseaza valoarea caracterului ASCII din celul caurenta (ex. 65 = 'A').
, : Citeste un singur caracter si plaseaza valoarea lui in celula curenta.
[ : Daca valoarea in celula curenta este zero, sare la urmatorul caracter ] .
    Altfel, merge la urmatoarea instructiune.
] : Daca valoarea in celula curenta este zero, sare la urmatoarea
	instructiune.
    Altfel, se intoarce la instructiunea de dupa caracterul [ precedent .

[ and ] formeaza un ciclu. Evident, trebuie ca parantezarea sa fie corecta.

Sa privim cateva programe brainfuck simple.

++++++ [ > ++++++++++ < - ] > +++++ .

Acest program afiseaza litera 'A'. Mai intai, incrementeaza celula #1 pana
la valoarea 6. Celula #1 va fi folosita pentru ciclare. Apoi, intra in ciclu
([) si muta pointerul la celula #2. Incrementeaza celula #2 de 10 ori,
muta pointerul la celula #1 si decrementeaza celula #1. Acest ciclu parcurge
6 iteratii (este nevoie de 6 decrementari pentru ca celula #1 sa ajunga la 0),
iar dupa aceea se trece la caracterul ] corespunzator si se continua executia.

In acest moment, ne aflam in celula #1, care are valoarea 0, in timp ce celula
#2 are valoarea 60. Ne mutam pe celula #2, incrementam de 5 ori, pentru a
obtine valoarea 65, si apoi afisam valoarea celulei #2. 65 este codul ASCII
pentru 'A', deci se afiseaza 'A' in terminal.

, [ > + < - ] > .

Acest program citeste un caracter de la intrarea utilizator si copiaza caracterul
in celula #1. Apoi incepem un ciclu. Se muta pointerul in celula #2, se
incremneteaza valoarea de la celula #2, se muta inapoi la celula #1, se
decrementeaza valoarea de la celula #1. Aceasta continua pana cand celula #1 este
0 iar celula #2 retine vechea valoare a celulei #1. Deoarece ne aflam in celula
#1 la sfarsitul ciclului, ne mutam pe celula #2 si afisam simbolul corespunzator
in ASCII.

Aveti in vedere ca spatiile sunt doar pentru usurinta citirii. La fel de bine
programul ar fi putut fi scris astfel:

,[>+<-]>.

Incercati sa va dati seama ce face acest program:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Acest program citeste doua numere ca intrare si le inmulteste.

Pe scurt, programul citeste doua date de intrare, apoi incepe ciclul
mare, a carui conditie se afla in celula #1; apoi se muta la celula #2
si incepe un ciclu imbricat a carui conditie de reluare se afla in
celula #2, si care incrementeaza celula #3. Totusi aici intervine o
problema: La sfarsitul ciclului imbricat, celula #2 este zero. In
acest caz, celula ciclul imbricat nu va mai functiona data viitoare.
Pentru a rezolva aceasta problema, incrementam celula si #4, si
recopiem celula #4 in celula #2. In final, celula #3 este rezultatul.

```

Așadar acesta este limbajul brainfuck. Nu e atât de greu, nu? Pentru
amuzament, puteți să scrieți propriile dumneavoastră limbaje, sau puteți
scrie un interpretor pentru brainfuck într-un alt limbaj. Interpretorul
este destul de ușor de implementat, dar dacă sunteți masochist, încercați
să implementați un interpretor de brainfuck… în brainfuck.
