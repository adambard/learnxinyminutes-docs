---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Dragomir Ioan", "http://github.com/ianiD"]
lang: ro-ro
---

Brainfuck (nu e capitalizat decăt la începutul propoziției) este un limbaj de
programare extrem de minimalist Turing-complete ce consista doar din 8 instructiuni.

Poți încerca brainfuck în browser cu [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Orice caracter diferit de "><+-.,[]" (fără ghilimele) este ignorat.

Braifuck e reprezentat de o mulțime de 30.000 de celule inițializate zero
și un pointer către celula curentă.

Sunt 8 instructiuni:
+ : Incrementează valoarea celulei curente cu 1.
- : Decrementează valoarea celulei curente cu 1.
> : Mută pointer-ul cu o celulă la dreapta.
< : Mută pointer-ul cu o celulă la stânga.
. : Scrie valoarea ASCII a celulei curente (de ex. 65 = 'A')
, : Citește un caracter și îi introduce valoarea în celula curentă.
[ : Sare la ] corespondentă dacă valoarea celulei curente este zero,
    altfel nu face nimic.
] : Sare la [ corespondentă dacă valoarea celulei curente nu este zero,
    altfel nu face nimic.

[ și ] formează o buclă 'while'. Evident, trebuie să fie balansate.

Să ne uităm la niște programe brainfuck simple.

++++++ [ > ++++++++++ < - ] > +++++ .

Acest program afișează litera 'A'. Mai întâi incrementează celula #1 până
la 6. Aceasta va fi folosită pentru buclă. După asta, începe bucla ([) și
se trece la celula #2. Se incrementează celula #2 de 10 ori, apoi se
întoarce la celula #1, care este decrementată. Asta se întâmplă de 10 ori
(până când celula #1 ajunge la zero, moment în care se va sfârși bucla).

Acum suntem pe ceulua #1, care are valoarea 0, pe când celula #2 are
valoarea de 60. Se mută pointer-ul pe celula #2, incrementează de 5 ori,
pentru o valoare de 65, apoi se afișează valoarea celulei #2. 65 corespunde
caracterului 'A' în ASCII, deci este afișat 'A'.


, [ > + < - ] > .

Acest program citește un caracter în celula #1. Apoi începe o buclă. Se trece
la celula #2, apoi se incrementează valoarea acesteia, iar apoi se întoarce la
celula #1, care este decrementată. Aceasta continuă până când veloarea celulei
#1 devine zero, iar celula #2 are valoarea inițială a celulei #1. Deoarece
suntem la celula #1 la sfârșitul buclei, trecem la celula #2 și afișăm
caracterul corespunzător valorii acesteia.

Ar trebui să se știe că spațiile sunt numai pentru lozibilitate. Se poate scrie
pentru același rezultat și așa:

,[>+<-]>.

Încearcă să îți dai seama ce face acest program:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Acesta ia 2 numere ca și date de intrare și le înmulțește.

Inițial se citesc 2 numere. Apoi începe bucla exterioară, condiționată
cu celula #1. Se trece la celula #2, apoi se începe bucla interioară,
condiționată, la rândul ei, cu celula #2, în care se incrementează celula
#3. Însă există o problemă: la sfârșitul buclei interioare celula #2 e
nulă. În acest caz, bucla interioară nu va mai funcționa decât prima dată.
Pentru a rezolva asta, se incrementează și celula #4, care este apoi
copiată în celula #2. Rezultatul înmulțirii se află în celula #3.
```

Și cam acesta este brainfuck. Nu e chiar așa de greu, nu? De distracție,
încearcă să scrii un interpretor brainfuck în alt limbaj de programare.
Nu ar trebui să fie prea greu, dar dacă ești masochist(ă), încearcă să
faci un interpretor brainfuck... în brainfuck.
