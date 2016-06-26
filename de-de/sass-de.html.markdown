---
language: sass
filename: learnsass-de.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
  - ["Kyle Mendes", "https://github.com/pink401k"]
translators:
    - ["Philipp Bochmann", "https://github.com/phbo85"]  
lang: de-de
---
Sass ist eine CSS-erweiternde Sprache, welche Features wie Variablen, Verschachtelung, Mixins und mehr hinzufügt.
Sass (und andere Präprozessoren wie [Less](http://lesscss.org/)) helfen Entwicklern dabei ihren Code wartbar und DRY (Don't Repeat Yourself - wiederhole dich nicht) zu schreiben.

Sass hat zwei verschiedene Syntax-Optionen. SCSS, mit der gleichen Syntax wie CSS aber mit den zusätzlichen Features von Sass. Oder Sass (die originale Syntax), welche Einrückung statt geschweiften Klammern und Semikolons benutzt.
Dieses Tutorial wurde mit SCSS geschrieben.

Wenn du bereits mit CSS3 vertraut bist, wirst du dir Sass relativ schnell aneignen. Es bietet keine neuen Styling-Eigenschaft, sondern Werkzeuge mit denen du dein CSS effizienter schreiben kannst und die Wartung viel einfacher machst.


```scss


//Einzeilige Kommentare werden entfernt, wenn Sass zu CSS kompiliert wird.

/* Mehrzeilige Kommentare bleiben bestehen. */



/* Variablen
============================== */



/* Du kannst einen CSS-Wert (wie eine Farbe) in einer Variable speichern.
Benutze das '$'-Zeichen um eine Variable zu erstellen. */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* Du kannst die Variablen überall in deinem Stylesheet verwenden.
Wenn du nun eine Farbe ändern willst, musst du das nur einmal tun. */

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* Das wird kompiliert zu: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Dies ist viel besser wartbar als die Farbe
an jeder Stelle im Stylesheet einzeln ändern zu müssen. */



/* Mixins
============================== */



/* Wenn du merkst, dass du den gleichen Code für mehr als ein
Element schreiben musst, kannst du ihn in einem mixin speichern.

Dazu benutzt du '@mixin' plus einem Namen für dein mixin. */

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Du kannst das mixin mit '@include' und dem Namen des mixin benutzen. */

div {
	@include center;
	background-color: $primary-color;
}

/* Das kompiliert zu: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* Du kannst Mixins benutzen, um shorthand Eigenschaften zu erstellen. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* Diese kannst du aufrufen und width und height als Parameter übergeben. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* Compiles to: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* Funktionen
============================== */



/* Sass bietet Funktionen, welche benutzt werden können um eine Reihe
   von Aufgaben zu bewältigen. Berücksichtige das Folgende: */

/* Funktionen können aufgerufen werden indem du ihren Namen benutzt
   und die benötigten Parameter übergibst. */
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25)
}

/* Kompiliert: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Du kannst auch deine eigenen Funktionen definieren. Funktionen ähneln
   Mixins. Wenn du zwischen Funktionen und Mixins auswählen musst, denke
   daran, dass Mixins am besten zur Generierung von CSS eignen, während
   Funktionen besser für Logik in deinem Sass Code genutzt werden. Die
   Beispiele mit in der Sektion "Mathematische Operatoren" sind ideale
   Kandidaten für wiederverwendbare Funktionen. */

/* Diese Funktion errechnet den Prozentwert aus target-size und parent-size
   und gibt diesen zurück. */

@function calculate-percentage($target-size, $parent-size) {
  @return $target-size / $parent-size * 100%;
}

$main-content: calculate-percentage(600px, 960px);

.main-content {
  width: $main-content;
}

.sidebar {
  width: calculate-percentage(300px, 960px);
}

/* Kompiliert: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* Extend (Vererbung)
============================== */



/* Extend ist ein Weg um Eigenschaften eines Selektoren mit einem anderem
   zu teilen. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Kompiliert: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}

/* Aufgrund der Art wie Sass die Klassen zusammen gruppiert, welche
   alle das gleiche Grund-Styling haben, ist Extend der Erstellung
   eines Mixins vorzuziehen. Wenn dies mit einem Mixin gemacht worden
   wäre, würden width, height und border für jedes Element dupliziert
   werden, welches das Mixin aufruft. Dies beeinflusst zwar nicht
   deinen Workflow, bläht aber die vom Sass-Compiler erzeugten Dateien
   unnötige auf. */



/* Nesting (Verschachtelung)
============================== */



/* Sass erlaubt es Selektoren in Selektoren zu verschachteln. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&' wird durch den übergeordneten Selektor ersetzt. */
/* Du kannst auch Pseudo-Klassen verschachteln. */
/* Denk daran, dass zu viel Verschachtelung deinen Code schlechter
   wartbar macht.
   Die Best Practices empfehlen nicht mehr als 3 Ebenen zu verschachteln.
   Zum Beispiel: */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* Kompiliert: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/* Partials und Imports
============================== */



/* Sass erlaubt dir das Erstellen partieller Dateien (partials).
   Das hilft dir modularisierten Sass Code zu schreiben.
   Partielle Dateien fangen mit einem '_' an, z.B. _reset.css.
   Partielle Dateien werden nicht zu CSS generiert. */

/* Schau dir folgendes CSS an, was wir in einer Datei namens _reset.css haben */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Mit @import kannst du in Sass partielle Dateien importieren.
   Dies unterscheidet sich vom traditionellen CSS @import Statement
   welches einen neuen HTTP Request macht, um die zu importierende Datei
   zu holen. Sass nimmt die importierte Datei und kombiniert sie mit
   dem kompilierten Code. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* Kompiliert: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* Platzhalter Selektoren
============================== */



/* Platzhalter sind nützlich, um ein CSS Statement zum Erweitern zu
   erstellen. Wenn du ein CSS Statement erstellst, welches du ausschließlich
   zur Verwendung mit @extend nutzen willst, kannst du das mit einem
   Platzhalter tun. Platzhalter fangen mit einem '%' statt einem '.'
   oder '#' an und erscheinen nicht im kompilierten CSS. */

%content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend %content-window;
  background-color: #0000ff;
}

/* Kompiliert: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* Mathematische Operationen
============================== */



/* Sass bietet die folgenden Operatoren: +, -, *, /, und %. Diese können
   nützlich sein, wenn du Werte direkt in Sass berechnen willst, anstatt
   vorher manuell errechnete Werte zu verwenden. Unten folgt ein Beispiel
   für ein einfaches zweispaltiges Design. */

$content-area: 960px;
$main-content: 600px;
$sidebar-content: 300px;

$main-size: $main-content / $content-area * 100%;
$sidebar-size: $sidebar-content / $content-area * 100%;
$gutter: 100% - ($main-size + $sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: $main-size;
}

.sidebar {
  width: $sidebar-size;
}

.gutter {
  width: $gutter;
}

/* Compiles to: */

body {
  width: 100%;
}

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}

```

## SASS oder Sass?
Hast du dich jemals gefragt, ob Sass ein Akronym ist oder nicht? Hast du wahrscheinlich nicht, aber ich sage es dir trotzdem. Der Name der Sprache ist ein Wort, "Sass", und kein Akronym.
Da die Leute durchgehend "SASS" geschrieben haben, hat der Ersteller der Sprache es scherzhaft "Syntactically Awesome StyleSheets" genannt.

## Sass üben
Wenn du mit Sass in deinem Browser spielen willst, schau dir [SassMeister](http://sassmeister.com/) an.
Du kannst beide Syntax-Optionen benutzen, gehe einfach in die Einstellungen und wähle entweder Sass oder SCSS.

## Kompatibilität
Sass kann in jedem Projekt verwendet werden, solange du ein Programm hast, um es in CSS zu kompilieren.
Du solltest verifizieren, dass das CSS, was du verwendest, mit deinen Ziel-Browsern kompatibel ist.

[QuirksMode CSS](http://www.quirksmode.org/css/) und [CanIUse](http://caniuse.com) sind gute Resourcen um die Kompatibilät zu überpüfen.


## Literaturhinweise
* [Offizielle Dokumentation](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) bietet Tutorials (Anfänger bis Fortgeschritten) und Artikel.
