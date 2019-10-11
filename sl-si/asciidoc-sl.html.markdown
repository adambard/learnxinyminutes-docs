---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
translators:
    - ["Filip Štamcar", "https://github.com/filips123"]
lang: sl-si
filename: asciidoc-sl.md
---

AsciiDoc je označevalni jezik, ki je podoben Markdownu in ga je mogoče uporabiti za vse od knjig do spletnih dnevnikov. Jezik, ki ga je leta 2002 ustvaril Stuart Rackham, je preprost, vendar omogoča veliko prilagoditev.

## Glava dokumenta

Glave so neobvezne in ne smejo vsebovati praznih vrstic. Od vsebine jih mora ločiti vsaj ena prazna vrstica.

### Naslov

```
= Naslov dokumenta

Prvi stavek dokumenta.
```

### Naslov in avtor

```
= Naslov dokumenta
Ime Priimek <ime.priimek@learnxinyminutes.com>

Prvi stavek dokumenta.
```

### Naslov in več avtorjev

```

= Naslov dokumenta
Ime Priimek <ime.priimek@learnxinyminutes.com>; Janez Novak <janez.novak@pirate.com>

Prvi stavek dokumenta.
```

Vrstica za revizijo

```
= Naslov dokumenta V1
Janez Novak <janez.novak@pirate.com>
v1.0, 2016-01-13

Prvi stavek dokumenta.
```

## Odstavki

```
Za odstavke ne potrebujete nič posebnega.

Da jih ločite, dodajte prazno črto med odstavki.

Če želite ustvariti prazno vrstico, dodajte +
in ustvarili boste prelom vrstice!
```

## Oblikovanje besedila

```
_podčrtaj za pošvno_
*zvezdice za krepko*
*_kombinacije za zabavo_*
`krativec za monospace`
`*krepki monospace*`
```

## Naslovi razdelkov

```
= Stopnja 0 (samo za naslov dokumenta)

== Stopnja 1 <h2>

=== Stopnja 2 <h3>

==== Stopnja 3 <h4>

===== Stopnja 4 <h5>

```

## Seznami

Če želite ustvariti neoštevilčen seznam, uporabite zvezdice.

```
* foo
* bar
* baz
```

Če želite ustvaril oštevilčen seznam, uporabite pike.

```
. predmet 1
. predmet 2
. predmet 3
```

Seznami lahko do petkrat gnezdite tako, da dodate dodatne zvezdice ali pike.

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```

## Nadaljnje branje

Obstajata dve orodji za obdelavo AsciiDoc dokumentov:

1. [AsciiDoc](http://asciidoc.org/): izvirna implementacija v Pythonu je na voljo v glavnih distribucijah Linuxa. Stabilen in trenutno v vzdrževalnem načinu.
2. [Asciidoctor](http://asciidoctor.org/): alternativna Ruby implementacija, uporabno tudi iz Java in JavaScript. Z aktivnim razvojem si prizadeva razširiti sintakso AsciiDoc z novimi funkcijami in izhodnimi formati.

Naslednje povezave so povezane `Asciidoctor` implementacijo:

* [Markdown - AsciiDoc syntax comparison](http://asciidoctor.org/docs/user-manual/#comparison-by-example): primerjava skupnih elementov Markdowna in AsciiDoca.
* [Getting started](http://asciidoctor.org/docs/#get-started-with-asciidoctor): namestitev in navodila, ki omogočajo enostavne dokumente.
* [Asciidoctor User Manual](http://asciidoctor.org/docs/user-manual/): popolni priročnik z enim dokumentom s sklicevanjem na sintakso, primeri in oridji za upodabljanje.
