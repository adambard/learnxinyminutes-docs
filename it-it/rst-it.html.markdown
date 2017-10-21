---
language: restructured text (RST)
filename: restructuredtext-it.rst
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Ale46", "https://github.com/Ale46"]
lang: it-it
---

RST è un formato di file formalmente creato dalla comunità Python per scrivere documentazione (e quindi fa parte di Docutils).

I file RST sono semplici file di testo con una sintassi leggera (in confronto all'HTML).

## Installazione

Per usare Restructured Text, sarà necessario installare [Python](http://www.python.org) ed il pacchetto `docutils`.

`docutils` può essere installato da riga di comando:

```bash
$ easy_install docutils
```

O se il tuo sistema ha `pip`, puoi usare anche lui:

```bash
$ pip install docutils
```


## Sintassi del file

Un semplice esempio della sintassi del file:

```
.. Le righe che iniziano con due punti sono comandi speciali. Ma se non è possibile trovare alcun comando, la riga viene considerata come un commento

===============================================================================
I titoli principali sono scritti utilizzando caratteri di uguale, sopra e sotto
===============================================================================

Si noti che devono esistere tanti caratteri di uguale quanti sono i caratteri del titolo.

Anche il titolo è sottolineato con caratteri di uguale
======================================================

Sottotitoli con i trattini
--------------------------

E sotto-sottotitoli con tildi
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Puoi inserire il testo in *corsivo* o in **grassetto**, puoi "contrassegnare" il testo come codice con un doppio apice ``: `` print () ``.

Le liste sono semplici come in Markdown:

- primo articolo
- Secondo elemento
     - Sottoelemento

o

* Primo elemento
* Secondo elemento
     * Sottoelemento

Le tabelle sono davvero facili da scrivere:

=========== ========
Stato       Capitale
=========== ========
Francia     Parigi
Giappone    Tokio
=========== ========

Le tabelle più complesse possono essere fatte facilmente (colonne e/o righe unite) ma ti suggerisco di leggere il documento completo per questo :)

Esistono diversi modi per creare collegamenti:

- Aggiungendo un underscore dopo una parola: Github_ e aggiungendo l'URL di destinazione dopo il testo (questo modo ha il vantaggio di non inserire URL non necessari all'interno del testo leggibile).
- Digitando un URL completo: https://github.com/ (verrà automaticamente convertito in un collegamento)
- Facendo un collegamento simile a Markdown: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```


## Come usarlo

RST viene fornito con docutils che dispone di `rst2html`, per esempio:

```bash
$ rst2html miofile.rst output.html
```

*Nota : In alcuni sistemi il comando potrebbe essere rst2html.py*

Ma ci sono applicazioni più complesse che utilizzano il formato RST:

- [Pelican](http://blog.getpelican.com/), un generatore di siti statici
- [Sphinx](http://sphinx-doc.org/), un generatore di documentazione
- e molti altri


## Letture

- [Riferimento ufficiale rapido](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
