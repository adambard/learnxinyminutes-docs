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
RST-Dateien sind simple Textdateien mit einer leichtgewichtigen Syntax (im
Vergleich zu HTML).

## Installation

Um Restructured Text zu verwenden, musst du [Python](http://www.python.org)
installieren und das `docutils` Paket installieren. `docutils` kann mit dem
folgenden Befehl auf der Kommandozeile installiert werden:

```bash
easy_install docutils
```

Ebenso kann die Installation mit `pip`

```bash
pip install docutils
```

initiiert werden.

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

Text in *kursiv* oder in **fett**. Du kannst Text als Code "markieren", wenn
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
erstellt werden, aber ich empfehle dir dafür die komplette Dokumentation zu
lesen :)

Es gibt mehrere Möglichkeiten um Links zu machen:

- Wenn man einen Unterstrich hinter einem Wort hinzufügt: GitHub_ Zusätzlich
muss man die Zielurl nach dem Text hinzufügen.
(Dies hat den Vorteil, dass man keine unnötigen Urls in lesbaren Text einfügt.
- Wenn man die vollständige Url eingibt: https://github.com/
(Dies wird automatisch in ein Link konvertiert.)
- Wenn man es mehr Markdown ähnlich eingibt: `GitHub <https://github.com/>`_ .

.. _GitHub https://github.com/
```

## Wie man es verwendet

Mit der Installation von [docutils](https://docutils.sourceforge.io/) bietet
sich beispielsweise die Umwandlung zu html (mehrere Standards stehen zur
Auswahl) an:

```bash
rst2html myfile.rst output.html
```

*Anmerkung: Auf manchen Systemen könnte es `rst2html.py` sein.*

Weitere Exporte bieten beispielsweise `rst2latex`, `rst2man`, `rst2odt`,
`rst2pdf` und `rst2xml`.

Es gibt komplexere Anwendungen, die das RST Format verwenden:

- [Pelican](http://blog.getpelican.com/), ein statischer Webseitengenerator
- [Sphinx](http://sphinx-doc.org/), ein Dokumentationsgenerator
- und viele Andere

## Zum Lesen

- [Offizielle Schnellreferenz](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
