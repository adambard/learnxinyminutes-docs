---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
translators:
    - ["Ale46", "https://github.com/ale46"]
lang: it-it
filename: asciidoc-it.md
---

AsciiDoc è un linguaggio di markup simile a Markdown e può essere usato per qualsiasi cosa, dai libri ai blog. Creato nel 2002 da Stuart Rackman, questo linguaggio è semplice ma permette un buon numero di personalizzazioni.

Intestazione Documento

Le intestazioni sono opzionali e possono contenere linee vuote. Deve avere almeno una linea vuota rispetto al contenuto.

Solo titolo

```
= Titolo documento

Prima frase del documento.
```

Titolo ed Autore

```
= Titolo documento
Primo Ultimo <first.last@learnxinyminutes.com>

Inizio del documento
```

Autori multipli

```
= Titolo Documento
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Inizio di un documento con autori multipli.
```

Linea di revisione (richiede una linea autore)

```
= Titolo documento V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

Questo articolo sulle patatine sarà divertente.
```

Paragrafi

```
Non hai bisogno di nulla di speciale per i paragrafi.

Aggiungi una riga vuota tra i paragrafi per separarli.

Per creare una riga vuota aggiungi un +
e riceverai una interruzione di linea!
```

Formattazione Testo

```
_underscore crea corsivo_
*asterischi per il grassetto*
*_combinali per maggiore divertimento_*
`usa i ticks per indicare il monospazio`
`*spaziatura fissa in grassetto*`
```

Titoli di sezione

```
= Livello 0 (può essere utilizzato solo nell'intestazione del documento)

== Livello 1 <h2>

=== Livello 2 <h3>

==== Livello 3 <h4>

===== Livello 4 <h5>

```

Liste

Per creare un elenco puntato, utilizzare gli asterischi.

```
* foo
* bar
* baz
```

Per creare un elenco numerato usa i periodi.

```
. item 1
. item 2
. item 3
```

È possibile nidificare elenchi aggiungendo asterischi o periodi aggiuntivi fino a cinque volte.

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

## Ulteriori letture

Esistono due strumenti per elaborare i documenti AsciiDoc:

1. [AsciiDoc](http://asciidoc.org/): implementazione Python originale, disponibile nelle principali distribuzioni Linux. Stabile e attualmente in modalità di manutenzione.
2. [Asciidoctor](http://asciidoctor.org/): implementazione alternativa di Ruby, utilizzabile anche da Java e JavaScript. In fase di sviluppo attivo, mira ad estendere la sintassi AsciiDoc con nuove funzionalità e formati di output.

I seguenti collegamenti sono relativi all'implementazione di `Asciidoctor`:

* [Markdown - AsciiDoc comparazione sintassi](http://asciidoctor.org/docs/user-manual/#comparison-by-example): confronto affiancato di elementi di Markdown e AsciiDoc comuni.
* [Per iniziare](http://asciidoctor.org/docs/#get-started-with-asciidoctor): installazione e guide rapide per il rendering di documenti semplici.
* [Asciidoctor Manuale Utente](http://asciidoctor.org/docs/user-manual/): manuale completo con riferimento alla sintassi, esempi, strumenti di rendering, tra gli altri.
