---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators :
    - ["Frederik Ring", "https://github.com/m90"]
    - ["Philipp Fischbeck", "https://github.com/PFischbeck"]
filename: markdown-de.md
lang: de-de
---

Markdown wurde im Jahr 2004 von John Gruber kreiert. Ziel ist und war eine
Syntax, in der sich Dokumente leicht schreiben *und* lesen lassen. Außerdem
sollte Markdown sich leicht nach HTML (und in andere Formate) konvertieren
lassen.

```md
<!-- Markdown ist eine Obermenge von HTML - jede valide HTML-Datei ist also
automatisch valides Markdown - was heisst dass wir jedes HTML-Element (also auch
Kommentare) in Markdown benutzen können, ohne dass der Parser sie verändert.
Jedoch kann man innerhalb eines solchen HTML-Elements dann kein Markdown
mehr verwenden. -->

<!-- Es existieren unterschiedliche Markdown-Parser und -Dialekte, die sich in
manchen Punkten unterscheiden. Diese Einführung wird versuchen, zu erläutern,
welche Features überall verfügbar sind, und welche davon parser-spezifisch sind -->

<!-- Überschriften -->
<!-- HTML-Überschriften <h1> bis <h6> lassen sich einfach durch ein Voranstellen
der entsprechenden Anzahl an Hashes (#) auszeichnen -->
# Das ist eine <h1>
## Das ist eine <h2>
### Das ist eine <h3>
#### Das ist eine <h4>
##### Das ist eine <h5>
###### Das ist eine <h6>

<!-- Für die Elemente <h1> und <h2> gibt es in Markdown noch Sonderformen -->
Das ist eine h1
=============

Das ist eine h2
-------------

<!-- Einfaches Textstyling -->
<!-- Jeglicher Text lässt sich mit Markdown leicht als kursiv oder
auch als fett auszeichnen -->

*Dieser Text ist kursiv.*
_Genau wie dieser._

**Dieser Text ist fett.**
__Genau wie dieser.__

***Dieser Text ist beides***
**_Dieser auch!_**
*__Und dieser genau so!__*

<!-- In "GitHub Flavored Markdown", dem von GitHub verwendeten Dialekt / Parser,
gibt es auch noch durchgestrichenen Text: -->

~~Dieser Text wird durchgestrichen dargestellt.~~

<!-- Absätze sind eine oder mehrere zusammenhängende Zeilen Text, und werden
durch eine oder mehrere Leerzeilen voneinander abgesetzt. -->

Das ist ein Absatz. Ich kann immer noch nicht glauben, wie viel Spaß das macht !?!

Jetzt bin ich schon bei Absatz 2.
Hier ist dann immer noch Absatz 2!


Jetzt ist das dann Nummer drei!

<!-- Sollte man jemals ein <br />-Tag einfügen wollen, kann man einen Absatz
mit zwei oder mehr Leerzeichen beenden, und danach einen neuen Absatz beginnen. -->

Ich höre mit zwei Leerzeichen auf (markiere mich, und du siehst es).  

Über mir ist wohl ein <br />!

<!-- Zitate werden ganz einfach mit einem  > ausgezeichnet. -->

> Das ist ein Zitat. Du kannst Zeilenumbrüche
> entweder manuell hinzufügen und ein `>` vor jeder Zeile einfügen, oder du kannst deine Zeilen einfach immer länger und länger werden lassen, die Umbrüche werden dann automatisch erzeugt.
> Solange sie mit einem `>` beginnen, macht das keinen Unterschied.

> Auch möglich ist es, den Text
>> mehrstufig einzurücken.
> Nicht schlecht, oder?

<!-- Listen -->
<!-- <ul>s können mit Sternen, Pluszeichen oder Minuszeichen erzeugt werden -->

* Punkt auf der Liste
* Punkt auf der Liste
* Anderer Punkt auf der Liste

oder

+ Punkt auf der Liste
+ Punkt auf der Liste
+ Noch ein Punkt auf der Liste

oder

- Punkt auf der Liste
- Punkt auf der Liste
- Ein letzter Punkt auf der Liste

<!-- <ol>s werden mit einer Zahl gefolgt von einem Punkt erzeugt -->

1. Punkt eins
2. Punkt zwei
3. Punkt drei

<!-- Auch wenn es keine gute Idee sein mag: du müsstest die einzelnen Punkte
nicht mal korrekt numerieren -->

1. Punkt eins
1. Punkt zwei
1. Punkt drei
<!-- (Das sieht genau so aus wie das Beispiel eins weiter oben) -->

<!-- Man kann Listen auch verschachteln -->

1. Punkt eins
2. Punkt zwei
3. Punkt drei
    * Unterpunkt
    * Unterpunkt
4. Punkt vier

<!-- Code-Blöcke -->
<!-- Blöcke von Programmcode (also ein <code>-Element) kannst du auszeichnen,
indem du eine Zeile mit vier Leerzeichen oder einem Tabulator einrückst -->

    Das ist Quellcode
    Das hier auch

<!-- Der Code kann natürlich auch wiederum eingerückt sein -->

    my_array.each do |item|
        puts item
    end

<!-- Innerhalb normalen Texts kannst du Code mit Backticks \` auszeichnen -->

Hermann hatte nicht die leiseste Ahnung, was dieses `go_to()` bedeuten könnte!

<!-- In "GitHub Flavored Markdown" gibt es für Code nocheinmal eine
besondere Syntax -->

\`\`\`ruby <!-- in "echt" musst du die Backslashes entfernen: ```ruby ! -->
def foobar
    puts "Hallo Welt!"
end
\`\`\` <!-- hier auch keine Backslashes, nur ``` -->

<-- der obige Block muss nicht extra eingerückt werden, außerdem fügt GitHub
Syntax-Highlighting für die nach dem ``` angegebene Sprache hinzu -->

<!-- Horizontale Linie (<hr />) -->
<!-- Trenner lassen sich einfach mit drei (oder mehr) Sternen oder Bindestrichen
erzeugen (egal ob mit oder ohne Leerzeichen dazwischen)-->

***
---
- - -
****************

<!-- Hyperlinks -->
<!-- Eines der besten Features von Markdown ist das kinderleichte Erzeugen von
Hyperlinks: Einfach den Linktext in eckige Klammern [] setzen, gefolgt von
einer mit runden Klammern () umschlossenen URL. -->

[Klick mich!](http://test.de/)

<!-- Man kann dem Link auch noch ein title-Attribut geben -->

[Klick mich!](http://test.at/ "Link zu Test.at")

<!-- Relative Pfade funktionieren natürlich auch -->

[Zu meiner Musiksammlung](/music/).

<!-- URLs lassen sich auch über Referenzen festlegen -->

[Klick mich][link1], um mehr über mich herauszufinden!
[Hier kannst du auch mal draufklicken][foobar], wenn es dich interessiert.

[link1]: http://test.de/ "Wahnsinn!"
[foobar]: http://foobar.ch/ "Erstaunlich!"

<!-- Das title-Attribut wird entweder mit Anführungszeichen oder Klammern
umschlossen (oder gleich ganz weggelassen). Die Referenzen können an jeder
Stelle im gesamtem Dokument vorkommen, als ID kann alles verwendet werden, solange
es dokumentweit eindeutig ist. -->

<!-- Man kann den Linktext auch als implizite Referenz benutzen -->

[Das][] ist ein Link.

[das]: http://dasisteinlink.at/

<!-- Das ist aber eher unüblich. -->

<!-- Bilder -->
<!-- Bilder funktionieren genau wie Links, nur dass man noch ein Ausrufezeichen
voranstellt! -->

![Das ist das alt-Attribut für mein Bild](http://imgur.com/myimage.jpg "Hier noch ein title-Attribut")

<!-- Referenzen funktionieren auch hier genau wie erwartet -->

![Das ist das alt-Attribut][meinbild]

[meinbild]: relative/urls/gehen/auch.jpg "hier wäre noch Platz für einen title"

<!-- Bonusfeatures -->
<!-- Auto-Links -->

<http://testwebseite.de/> ist das selbe wie
[http://testwebseite.de/](http://testwebseite.de/)

<!-- Automatische Links für E-Mail-Addressen -->

<foo@bar.com>

<!-- Maskieren -->

Ich würde *diesen Teil gerne mit Sternen umschließen*, doch ohne dass er kursiv
wird. Also mache ich folgendes: \*Ich umschließe diesen Text mit Sternen\*!

<!-- Tabellen -->
<!-- Tabellen gibt es bis jetzt nur in "GitHub Flavored Markdown".
Zudem sind sie ziemlich mühselig, aber wenn du es wirklich wissen willst: -->

| Spalte1      | Spalte2  | Spalte3       |
| :----------- | :------: | ------------: |
| linksbündig  | mittig   | rechtsbündig  |
| blah         | blah     | blah          |

<!-- oder das selbe in grün: -->

Spalte1 | Spalte2 | Spalte3
:-- | :-: | --:
Ganz schön hässlich | vielleicht doch lieber | wieder aufhören

<!-- Das war's! -->

```

Mehr Informationen gibt es in [John Gruber's offiziellem Blog-Post](http://daringfireball.net/projects/markdown/syntax)
und bei Adam Pritchards [grandiosem Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
Infos zu GitHub Flavored Markdown [gibt es hier](https://help.github.com/articles/github-flavored-markdown).
