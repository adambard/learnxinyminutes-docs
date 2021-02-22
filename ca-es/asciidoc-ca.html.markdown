---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
translators:
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
lang: ca-es
filename: asciidoc-ca.md
---

AsciiDoc és un llenguatge de marques similar a Markdown i que pot ser usat per qualsevol ús, des de llibres fins a blogs.
Creat al 2002 per Stuart Rackham, és un llenguatge simple però permet un gran nivell de personalització.

Capçalera de document

La capçalera és opcional i no pot contenir línies buides. Ha d'estar separada del contingut per com a mínim una línia en blanc.

Només títol

```
= Títol del document

Primer contingut del document.
```

Títol i autor

```
= Títol del document
Nom Cognom(s) <nom.cognom@learnxinyminutes.com>

Inici d'aquest document.
```

Múltiples autors

```
= Títol del document
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Inici d'un document amb múltiples autors.
```

Línia de versió (requereix línia d'autor)

```
= Títol del document V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

Aquest article sobre patates fregides serà genial.
```

Paràgraf

```
No necessites res especial per un paràgraf.

Insereix una línia buida entre cada paràgraf per separar-los.

Per inserir un salt de línia, afegeix només un +
i ja ho tens!
```

Donant format al text

```
_guió baix per cursiva_
*asteriscs per negreta*
*_combina'ls i veuràs_*
`usa cometes invertides per monospace`
`*combina per negreta monospace*`
```

Títols de secció

```
= Nivell 0 (usa'l només pel títol del document)

== Nivell 1 <h2>

=== Nivell 2 <h3>

==== Nivell 3 <h4>

===== Nivell 4 <h5>
```

Llistes

Per crear una llista sense ordre usa asteriscs.

```
* foo
* bar
* baz
```

Per crear una llista numerada usa punts.

```
. element 1
. element 2
. element 3
```

Pots crear fins a 5 subnivells de llista afegint asteriscs o punts.

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

## Referències

Existeixen dues eines per processar documentació en AsciiDoc:

1. [AsciiDoc](http://asciidoc.org/): implementació original per Python, disponible a las principals distribucions Linux. Versió estable actualment en mode manteniment.
2. [Asciidoctor](http://asciidoctor.org/): implementació alternativa per Ruby, usable també des de Java i JavaScript. Implementació completa en evolució, té com a objectiu ampliar AsciiDoc amb noves funcionalitats i conversors de sortida.

Els següents enllaços pertanyen a `Asciidoctor` (documentació en anglès):

* [Comparació de sintaxi Markdown - AsciiDoc](http://asciidoctor.org/docs/user-manual/#comparison-by-example): comparativa d'elements comuns entre Markdown i AsciiDoc.
* [Primeres pases](http://asciidoctor.org/docs/#get-started-with-asciidoctor): manuals d'instal·lació i inici per convertir documents simples.
* [Manual d'usuari d'Asciidoctor](http://asciidoctor.org/docs/user-manual/): referència completa en un únic document, conté exemples, guies d'eines, etc.
