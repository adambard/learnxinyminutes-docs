---
language: haml
filename: learnhaml-de.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
  - ["Sol Bekic", "https://github.com/S0lll0s"]
lang: de-de
---

Haml ist eine Markup- und Templatingsprache, aufgesetzt auf Ruby, mit der HTML Dokumente einfach beschrieben werden können.

Haml vermindert Wiederholung und Fehleranfälligkeit, indem es Tags basierend auf der Markup-Struktur schließt und schachtelt.
Dadurch ergibt sich kurzes, präzises und logisches Markup.

Haml kann außerhalb eines Ruby-projekts verwendet werden. Mit dem installierten Haml gem kann man das Terminal benutzen um Haml zu HTML umzuwandeln:

$ haml input_file.haml output_file.html


```haml
/ -------------------------------------------
/ Einrückung
/ -------------------------------------------

/
  Einrückung ist ein wichtiges Element des Haml Syntax, deswegen ist es
  wichtig ein konsequentes Schema zu verwenden. Meistens werden zwei spaces
  verwendet, solange die Einrückungen das gleiche Schema verfolgen können
  aber auch andere Breiten und Tabs verwendet werden


/ -------------------------------------------
/ Kommentare
/ -------------------------------------------

/ Kommentare beginnen mit einem Slash

/
  Mehrzeilige Kommentare werden eingerückt und mit einem Slash
  eingeführt

-# Diese Zeile ist ein "stummes" Kommentar, es wird nicht mitgerendert


/ -------------------------------------------
/ HTML Elemente
/ -------------------------------------------

/ Tags werden durch ein Prozentzeichen und den Tagnamen erzeugt
%body
  %header
    %nav

/ Die Zeilen oben würden folgendes ergeben:
  <body>
    <header>
      <nav></nav>
    </header>
  </body>

/ Text kann direkt nach dem Tagnamen eingefügt werden:
%h1 Headline copy

/ Mehrzeilige Inhalte müssen stattdessen eingerückt werden:
%p 
  This is a lot of content that we could probably split onto two
  separate lines.

/ 
  HTML kann mit &= escaped werden. So werden HTML-sensitive Zeichen
  enkodiert. Zum Beispiel:

%p
  &= "Ja & Nein"

/ würde 'Ja &amp; Nein' ergeben

/ HTML kann mit != dekodiert werden:
%p
  != "so schreibt man ein Paragraph-Tag: <p></p>"

/ ...was 'This is how you write a paragraph tag <p></p>' ergeben würde

/ CSS Klassen können mit '.classname' an Tags angehängt werden:
%div.foo.bar

/ oder über einen Ruby Hash:
%div{:class => 'foo bar'}

/ Das div Tag wird standardmäßig verwendet, divs können also verkürzt werden:
.foo

/ andere Attribute können über den Hash angegeben werden:
%a{:href => '#', :class => 'bar', :title => 'Bar'}

/ Booleesche Attribute können mit 'true' gesetzt werden:
%input{:selected => true}

/ data-Attribute können in einem eigenen Hash im :data key angegeben werden:
%div{:data => {:attribute => 'foo'}}


/ -------------------------------------------
/ Verwendung von Ruby
/ -------------------------------------------

/ Mit dem = Zeichen können Ruby-werte evaluiert und als Tag-text verwendet werden:

%h1= book.name

%p
  = book.author
  = book.publisher


/ Code nach einem Bindestrich wird ausgeführt aber nicht gerendert:
- books = ['book 1', 'book 2', 'book 3']

/ So können zum Beispiel auch Blöcke verwendet werden:
- books.shuffle.each_with_index do |book, index|
  %h1= book

  if book do
    %p This is a book

/
  Auch hier werden wieder keine End-Tags benötigt!
  Diese ergeben sich aus der Einrückung.


/ -------------------------------------------
/ Inline Ruby / Ruby Interpolation
/ -------------------------------------------

/ Ruby variablen können mit #{} in Text interpoliert werden:
%p dein bestes Spiel ist #{best_game}


/ -------------------------------------------
/ Filter
/ -------------------------------------------

/
  Mit dem Doppelpinkt können Haml Filter benutzt werden.
  Zum Beispiel gibt es den :javascript Filter, mit dem inline JS
  geschrieben werden kann:

:javascript
  console.log('Dies ist ein <script>');

```

## Weitere Resourcen

- [What is HAML?](http://haml.info/) - Eine gute Einleitung auf der Haml homepage (englisch)
- [Official Docs](http://haml.info/docs/yardoc/file.REFERENCE.html) - Die offizielle Haml Referenz (englisch)
