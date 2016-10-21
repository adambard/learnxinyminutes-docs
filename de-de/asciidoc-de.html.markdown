---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
translators:
  - ["Dennis Keller", "https://github.com/denniskeller"]
filename: asciidoc-de.md
lang: de-de
---

AsciiDoc ist eine Auszeichnungssprache ähnlich zu Markdown. Sie kann für alles verwendet werden von Büchern zu Blogs. Erfunden wurde sie 2002 von Stuart Rackham. Die Sprache ist simpel aber sie ermöglicht eine große Anzahl an Anpassungen.

Kopfzeile des Dokuments

Kopfzeilen sind optional und dürfen keine Leerzeilen besitzen. Sie müssen mindestens eine Leerzeile vom Inhalt versetzt sein.

Nur Titel

```
= Dokumententitel

Erster Satz des Dokuments.
```

Titel und Autor

```
= Dokumententitel
Vorname Nachname <Vorname.Nachname@learnxinyminutes.com>

Start des Dokuments.
```

Mehrere Autoren

```
= Dokumententitel
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Starte ein Dokument mit mehreren Autoren.
```

Revisionszeile (benötigt eine Autorzeile)

```
= Dokumententitel V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

Dieser Artikel über Chips wird Spaß machen.
```

Absätze

```
Du musst nichts besonderes machen für Absätze.

Füge eine Leerzeile zwischen zwei Absätze, um sie zu trennen.

Um eine Leerzeile zu erhalten musst du ein +
ergänzen und du erhälst einen Umbruch!
```

Textformatierung

```
_Unterstriche erstellt Kursivschrift_
*Sternchen für Fett gedruckt*
*_Kombinieren für extra Spaß_*
`Benutze Ticks um Monospace zu signalisieren`
`*Fett gedruckter Monospace*`
```

Abteilungstitel

```
= Level 0 (sollte nur in der Kopfzeile verwendet werden)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>

====== Level 5 <h6>

======= Level 6  <h7>

```

Listen

Um eine Aufzählung zu erstellen verwendest du Sternchen.

```
* foo
* bar
* baz
```

Um eine nummerierte Liste zu erstellen verwendest du Punkte.

```
. item 1
. item 2
. item 3
```

Um Listen zu verschachteln musst du zusätzliche Sternchen und Punkte hinzufügen. Dies ist bis zu fünf Mal möglich.

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
