---
language: restructured text (RST)
filename: restructuredtext-it.rst
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Ale46", "https://github.com/Ale46"]
    - ["Chris54721", "https://chris54721.net"]
lang: it-it
---

RST (Restructured Text) è un formato di file inizialmente creato dalla comunità Python
per la documentazione (per questo motivo appartiene a Docutils).

I file RST sono semplici file di testo con una sintassi leggera (in confronto all'HTML).

## Installazione

Per usare Restructured Text, sarà necessario installare [Python](http://www.python.org) ed il pacchetto `docutils`.

`docutils` può essere installato da riga di comando:

```bash
$ easy_install docutils
```

Oppure, se hai `pip` installato sul tuo sistema:

```bash
$ pip install docutils
```


## Sintassi del file

Ecco un semplice esempio della sintassi RST:

```
.. Le righe che iniziano con due punti sono comandi speciali. Ma se non è possibile trovare alcun comando, la riga viene considerata come un commento

===============================================================================
I titoli principali sono scritti utilizzando caratteri di uguale, sopra e sotto
===============================================================================

Si noti che devono esserci tanti caratteri di uguale quanti caratteri del titolo.

Anche i titoli normali usano caratteri di uguale, ma solo sotto
===============================================================

I sottotitoli usano i trattini
------------------------------

E i sotto-sottotitoli le tildi
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Puoi inserire il testo in *corsivo* o in **grassetto**, puoi "contrassegnare" il testo come codice con un doppio apice ``: `` print () ``.

Le liste sono semplici come in Markdown:

- primo articolo
- Secondo elemento
     - Sottoelemento

oppure

* Primo elemento
* Secondo elemento
     * Sottoelemento

Le tabelle sono molto semplici da inserire:

=========== ========
Stato       Capitale
=========== ========
Francia     Parigi
Giappone    Tokio
=========== ========

Anche le tabelle più complesse possono essere inserite facilmente (colonne e/o righe unite) ma ti suggerisco di leggere la documentazione completa per questo :)

Esistono diversi modi per creare collegamenti:

- Aggiungendo un underscore dopo una parola: Github_ e aggiungendo l'URL di destinazione dopo il testo (questo metodo ha il vantaggio di non inserire URL non necessari all'interno del testo leggibile).
- Digitando un URL completo: https://github.com/ (verrà automaticamente convertito in un collegamento)
- Utilizzando una sintassi simile a Markdown: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```

## Come usarlo

RST viene fornito con docutils, che dispone di `rst2html`, per esempio:

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
