---
language: brainfuck
filename: brainfuck-sv.bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Richard Lindberg", "https://github.com/Lidenburg"]
lang: sv-se
---

Brainfuck (ej versaliserat förutom vid ny mening) är ett extremt
minimalistiskt Turing-komplett programmeringsspråk med endast 8 kommandon.

Du kan testa brainfuck i din webbläsare via [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Alla karaktärer förutom "><+-.,[]" (inte inkluderat citattecken) ignoreras.

Brainfuck är representerat av ett fält med 30 000 celler initialiserade till
noll och en data pekare som pekar på den valda cellen.

Det finns åtta kommandon:
+ : Ökar värdet av den valda cellen med ett.
- : Minskar värdet av den valda cellen med ett.
> : Flyttar data pekaren till nästa cell (cellen till höger).
< : Flyttar data pekaren till förra cellen (cellen till vänster).
. : Skriver ut ASCII värdet av den valda cellen (t.ex. 65 = 'A').
, : Läser in en karaktär till den valda cellen.
[ : Om värdet vid den valda cellen är noll, hoppa till matchande ].
    Annars fortsätts till nästa instruktion.
] : Om värdet vid den valda cellen är noll, fortsätt till nästa instruktion.
    Annars, gå tillbaka till matchande ].

[ och ] formar en while loop.

Nedan är ett exempel på ett simpelt brainfuck program.

++++++ [ > ++++++++++ < - ] > +++++ .

Programmet skriver ut bokstaven 'A'. Först ökar den värdet av cell #1 till 6.
Cell #1 kommer att användas för att loopa. Sen börjar den loopen (vid '[') och
flyttar till cell #2. Den ökar värdet av cell #2 10 gånger, går tillbaka till
cell #1 och minskar den med 1. Den gör det här 6 gånger (så många iterationer
det tar för cell #1 att bli noll).

Nu är programmet på cell #1, vilket har ett värde av 0 och cell #2 har värdet 60.
Programmet flyttar pekaren till cell #2 och ökar värdet med 5, vilket leder till
att cell #2 har ett värde av 65 (vilket är bokstaven 'A' i ASCII), sedan skriver
den ut cell #2 och bokstaven 'A' skrivs ut till skärmen.


, [ > + < - ] > .

Det här programmet läser en karaktär från användaren och kopierar karaktären
till cell #1. Sedan startas en loop. Pekaren flyttas till cell #2, värder ökas
med ett, pekaren flyttas tillbaka till cell #1 och minskar värdet med ett.
Det här fortsätter tills cell #1 innehåller noll och cell #2 innehåller det
värde som cell #1 innehöll från början. Eftersom att programmet vid slutet av
loopen är på cell #1 flyttas pekaren till cell #2 och sedan skriver den ut
värdet av cell #2 i ASCII.

Värt att komma ihåg är att programmet ovan kan skrivas utan mellanslag också:

,[>+<-]>.


Försök och lista ut vad det här programmet gör:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Programmet tar två nummer som indata, och multiplicerar dem.

Kärnan av det är att den först läser in två tal/bokstäver. Sedan startar
den yttre loopen som beror på cell #1. Sedan går den vidare till cell #2 och
startar den innre loopen som beror på cell #2 och ökar cell #3. Men det uppstår
ett problem: Vid slutet av den innre loopen är cell #2 noll. Vilket betyder att
den inre loopen inte kommer att fungera tills nästa gång. För att lösa det här
problemet ökas också cell #4 som sedan kopieras till cell #2.
Sedan är resultatet i cell #3.
```

Och det är brainfuck. Inte så svårt va? För skojs skull kan du skriva dina egna
brainfuck program, eller skriva en egen brainfuck interpretator i ett annat
språk. interpretatorn är ganska simpel att implementera, men om man är en
masochist, testa att skriva en brainfuck interpretator… i brainfuck.
