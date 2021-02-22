---
language: restructured text (RST)
filename: restructuredtext-de.rst
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

RST ist ein Dateiformat, das von der Python Community entwickelt wurde,

um Dokumentation zu schreiben (und ist somit Teil von Docutils). 

RST-Dateien sind simple Textdateien mit einer leichtgewichtigen Syntax (im Vergleich zu HTML).


## Installation

Um Restructured Text zu vewenden musst du [Python](http://www.python.org)

installieren und das `docutils` Packet installieren. `docutils` kann mit dem folgenden 

Befehl auf der Kommandozeile installiert werden:

```bash
$ easy_install docutils
```

Wenn auf deinem System `pip` installiert kannst du es statdessen auch verwenden:

```bash
$ pip install docutils
```


## Dateisyntax

Ein einfaches Beispiel für die Dateisyntax:

```
.. Zeilen, die mit zwei Punkten starten sind spezielle Befehle. 

.. Wenn kein Befehl gefunden wird, wird die Zeile als Kommentar gewertet. 

============================================================================
Haupttitel werden mit Gleichheitszeichen darüber und darunter gekennzeichnet
============================================================================

Beachte das es genau so viele Gleichheitszeichen, wie Hauptitelzeichen
geben muss.

Titel werden auch mit Gleichheitszeichen unterstrichen
======================================================

Untertitel werden mit Strichen gekennzeichnet
---------------------------------------------

Text in *kursiv* oder in **fett**. Du kannst Text als Code "makieren", wenn
du doppelte Backquotes verwendest ``: ``print()``.

Listen sind so einfach wie in Markdown:

- Erstes Element
- Zweites Element
    - Unterelement

oder

* Erstes Element
* Zweites Element
    * Unterelement

Tabellen sind einfach zu schreiben:

=========== ==========
Land        Hauptstadt 
=========== ==========
Frankreich  Paris
Japan       Tokyo
=========== ========

Komplexere Tabellen (zusammengeführte Spalten und Zeilen) können einfach 
erstellt werden, aber ich empfehle dir dafür die komplette Dokumentation zu lesen :)

Es gibt mehrere Möglichkeiten um Links zu machen:

- Wenn man einen Unterstrich hinter einem Wort hinzufügt: Github_ Zusätzlich 
muss man die Zielurl nach dem Text hinzufügen. 
(Dies hat den Vorteil, dass man keine unnötigen Urls in lesbaren Text einfügt.
- Wenn man die vollständige Url eingibt : https://github.com/
(Dies wird automatisch in ein Link konvertiert.)
- Wenn man es mehr Markdown ähnlich eingibt: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```


## Wie man es verwendet

RST kommt mit docutils, dort hast du den Befehl `rst2html`, zum Beispiel:

```bash
$ rst2html myfile.rst output.html
```

*Anmerkung : Auf manchen Systemen könnte es rst2html.py sein*

Es gibt komplexere Anwendungen, die das RST Format verwenden: 

- [Pelican](http://blog.getpelican.com/), ein statischer Websitengenerator
- [Sphinx](http://sphinx-doc.org/), Ein Dokumentationsgenerator
- und viele Andere

## Zum Lesen

- [Offizielle Schnellreferenz](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
