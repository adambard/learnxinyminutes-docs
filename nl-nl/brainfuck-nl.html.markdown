---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Jelle Besseling", "https://github.com/Jell-E"]
    - ["Sam van Kampen", "http://tehsvk.net"]
lang: nl-nl
---

Brainfuck (schrijf je niet met een hoofdletter behalve aan het begin van een
zin) is een extreem
minimalistische Turing-complete programmeertaal met maar acht commando's.

```
Elk karakter behalve "><+-.,[]" (en de quotes) wordt genegeerd.

Brainfuck wordt gerepresenteerd door een array met 30.000 cellen die initieel
gevuld is met nullen en een pointer die wijst naar de huidige cel.

Dit zijn de acht commando's:
+ : Verhoog de huidige cel met 1.
- : Verminder de huidige cel met 1.
> : Beweeg de pointer naar de volgende cel (één naar rechts).
< : Beweeg de pointer naar de vorige cel (één naar links).
. : Print de huidige cel als een ASCII karakter (d.w.z. 65 = 'A').
, : Lees een karakter in de huidige cel.
[ : Als de huidige cel nul is ga dan naar de bijbehorende ] .
    Als het geen nul is, ga dan gewoon verder.
] : Als de huidige cel nul is ga dan gewoon verder.
    Als het geen nul is, ga dan terug naar de bijbehorende [ .

[ en ] maken een while loop. Ze moeten uiteraard wel gebalanceerd zijn

Laten we kijken naar een paar brainfuckprogramma's.

++++++ [ > ++++++++++ < - ] > +++++ .

Dit programma print het karakter 'A'. Eerst verhoogt het cel #1 tot 6.
Cel #1 wordt gebruikt om te loopen. Dan begint het de loop ([) en gaat
naar cel #2. Het verhoogt cel #2 tien keer, gaat terug naar cel #1, en
verlaagt cel #1. Deze loop gebeurt zes keer (na zes keer staat cel #1
weer op nul, waarna het doorgaat naar het einde van de loop (]) en
verder gaat).

De pointer staat nu weer op cel #1, deze heeft een waarde van 0, en cel #2
heeft een waarde van 60. > beweegt de pointer naar cel #2, daarna verhoogt
het de cel vijf keer, waardoor het een waarde van 65 bevat, en print dan
de waarde van cel #2. 65 is 'A' in ASCII, dus 'A' wordt geprint in de terminal.


, [ > + < - ] > .

Dit programma leest een karakter dat de gebruiker intoetst en kopieert dat
karakter naar cel #1. Dan start de loop. Ga naar cel #2, verhoog de waarde in
cel #2, ga terug naar cel #1, en verlaag de waarde in cel #1. Dit gaat door
totdat cel #1 nul is en cel #2 de oude waarde heeft van cel #1. Omdat we
op cel #1 staan verplaatst > de pointer één naar rechts en . print het
karakter in cel #2.

Houd wel in gedachten dat de spaties alleen zijn voor leesbaarheid, je kan het
bovenstaande programma net zo goed schrijven als:

,[>+<-]>.

Probeer maar eens te bedenken wat het volgende programma doet:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Dit programma neemt twee getallen als input, en vermenigvuldigt ze.

In het begin leest het twee karakters in cel #1 en #2. Dan start het de
buitenste loop, met als teller cel #1. Het beweegt naar cel #2, dan start het
de binnenste loop met als teller cel #2, daar verhoogd het cel #3. Maar
dan is er een probleem als cel #2 nul wordt aan het einde van de binnenste loop.
Om dit op te lossen wordt ook cel #4 verhoogd naar het oorspronkelijke getal
uit cel #2 en daarna wordt cel #4 weer gekopieerd naar cel #2.
Het resultaat komt in cel #3 te staan.
```

En dat is dan brainfuck. Niet heel moeilijk, toch? Je kan zelf voor de lol
brainfuckprogramma's gaan schrijven, of je kan een interpreter schrijven
voor brainfuck in een andere taal. Het is namelijk redelijk makkelijk om te
implementeren, aangezien brainfuck maar acht commando's heeft. En als je een
masochist bent, kan je ook nog proberen om brainfuck te implementeren… in
brainfuck.
