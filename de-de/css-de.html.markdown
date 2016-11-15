---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
translators:
    - ["Kyr", "http://github.com/kyrami"]
lang: de-de
filename: learncss-de.css
---

In den frühen Tagen des Internets gab es keine visuellen Elemente, alles war nur reiner Text. Aber mit der Weiterentwicklung von Browsern wurden auch vollständig visuelle Webseiten zu einem Standard.
Durch Verwendung von CSS lässt sich eine strikte Trennung zwischen HTML-Code und Designelementen erreichen.

Kurzgefasst, CSS ermöglicht es, verschiedene HTML-Elemente innerhalb eines Dokuments auszuwählen und ihnen visuelle Eigenschaften zu geben.

CSS hat wie jede andere Sprache viele Versionen. Hier fokussieren wir uns auf CSS2.0, welche nicht die neueste, aber die am weitesten verbreitete und unterstützte Version ist.

**HINWEIS:** Weil die Ausgabe von CSS visuelle Eigenschaften sind, wirst du wahrscheinlich eine CSS-Sandbox wie [dabblet](http://dabblet.com/) benutzen müssen, um die Sprache richtig zu lernen.
In diesem Artikel wird am meisten auf generelle Hinweise und die Syntax geachtet.


```css
/* Kommentare werden in Sternchen-Schrägstrichkombinationen gepackt (genauso wie hier!) */

/* ####################
   ## SELEKTOREN
   ####################*/

/* Eigentlich ist das grundlegende CSS-Statement sehr simpel */
selektor { eigenschaft: wert; /* mehr eigenschaften...*/ }

/* Der Selektor wird dazu benutzt, ein Element auf der Seite auszuwählen.

Man kann aber auch alle Elemente auf einer Seite auswählen! */
* { color:red; } /* farbe:rot */

/*
Angenommen wir haben folgendes Element auf einer Seite:

<div class='eine-klasse klasse2' id='eineId' attr='wert' />
*/

/* kann man es so über seine Klasse auswählen */
.eine-klasse { }

/* oder über beide Klassen! */
.eine-klasse.klasse2 { }

/* oder über den Namen des Tags */
div { }

/* oder über seine Id */
#eineId { }

/* oder darüber, dass es ein Attribut hat! */
[attr] { font-size:smaller; }

/* oder auch darüber, dass das Attribut einen bestimmten Wert hat */
[attr='wert'] { font-size:smaller; }

/* beginnt mit dem übergebenen Wert */
[attr^='we'] { font-size:smaller; }

/* endet damit */
[attr$='rt'] { font-size:smaller; }

/* oder beinhaltet einen Teil davon */
[attr~='er'] { font-size:smaller; }


/* Noch wichtiger ist aber die Möglichkeit, all das miteinander kombinieren
zu können - man sollte hierbei nur mit der Leerzeichensetzung vorsichtig sein,
ein Leerzeichen macht es zu zwei verschiedenen Selektoren */

div.eine-klasse[attr$='rt'] { } /* so ist es richtig */

/* Man kann auch ein Element über seine Elternelemente auswählen */

/* > wählt ein direktes Kind aus */
div.ein-elternteil > .klassen-name {}

/* Mit einem Leerzeichen getrennt kann man alle Elternelemente ansprechen */
/* Das folgende heißt also, dass jedes Element mit der Klasse 'klassen-name'
und dem Elternteil IN JEDER TIEFE ausgewählt wird */
div.ein-elternteil .klassen-name {}

/* Achtung: das selbe ohne das Leerzeichen hat eine andere Bedeutung,
kannst du mir sagen, was? */
div.ein-elternteil.klassen-name {}

/* Man kann ein Element auch nach seinem direkten Nachbarelement
auswählen */
.ich-bin-vorher + .dieses-element { }

/* Oder über jedes Geschwisterelement davor */
.ich-kann-jeder-davor-sein ~ .dieses-element {}

/* Mit Pseudoklassen lassen sich Elemente anhand ihres momentanen Zustands
auf der Seite auswählen (anstatt über die Seitenstruktur) */

/* Zum Beispiel, wenn über ein Element mit dem Mauszeiger gefahren wird */
:hover {}

/* Oder einen bereits besuchten Link*/
:visited {}

/* Oder einen noch nicht besuchten Link*/
:link {}

/* Oder ein Eingabeelement, das zurzeit im Fokus steht */
:focus {}


/* ####################
   ## EIGENSCHAFTEN
   ####################*/

selector {

    /* Einheiten */
    width: 50%; /* in Prozent */
    font-size: 2em; /* mal der derzeitigen Schriftgröße */
    width: 200px; /* in Pixeln */
    font-size: 20pt; /* in Punkten */
    width: 5cm; /* in Zentimetern */
    width: 50mm; /* in Millimetern */
    width: 5in; /* in Zoll */

    /* Farben */
    background-color: #F6E  /* in kurzem Hex */
    background-color: #F262E2 /* in langem Hex */
    background-color: tomato /* kann auch eine benannte Farbe sein */
    background-color: rgb(255, 255, 255) /* in RGB */
    background-color: rgb(10%, 20%, 50%) /* in RGB Prozent */
    background-color: rgba(255, 0, 0, 0.3); /* in semi-transparentem RGB */

    /* Bilder */
    background-image: url(/pfad-zum-bild/image.jpg);

    /* Schriften */
    font-family: Arial;
    font-family: "Courier New"; /* wenn der Name ein Leerzeichen beinhält, kommt er in
    Anführungszeichen */
    font-family: "Courier New", Trebuchet, Arial; /* wird die erste Schriftart 
    nicht gefunden, wird die zweite benutzt, usw. */
}

```

## Benutzung

Speichere das CSS, das du benutzen willst, mit der Endung '.css'.

```xml
<!-- du musst die CSS-Datei im <head>-bereich der Seite einbinden -->
<link rel='stylesheet' type='text/css' href='filepath/filename.css' />

<!-- Einbindung funktioniert auch inline, wobei diese Methode nicht
empfohlen ist -->
<style>
   selector { property:value; }
</style>

<!-- Oder direkt auf einem Element (sollte aber vermieden werden) -->
<div style='property:value;'>
</div>

```

## Spezifität

Ein Element kann natürlich auch von mehr als einer Regel in einem Stylesheet
angesprochen werdenm und kann eine Eigenschaft auch öfters als einmal zugewiesen
bekommen. In diesen Fällen gibt es Regeln, die die Spezifität von Selektoren regeln.

Wir haben dieses CSS:

```css
/*A*/
p.klasse1[attr='wert']

/*B*/
p.klasse1 {}

/*C*/
p.klasse2 {}

/*D*/
p {}

/*E*/
p { property: wert !important; }

```

und das folgende Markup:

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>
```

Die Spezifität der Stile ist wie folgt:
(die Spezifität gilt nur für **einzelne Eigenschaften**, nicht für ganze Blöcke)

* `E` hat die größte Spezifität wegen des Schlüsselworts `!important`.
	man sollte diese Form aber vermeiden.
* `F` ist als nächstes dran, da es direkt an dem Element definiert ist.
* Dann folgt `A`, da es "spezifischer" als alle anderen ist.
	spezifischer = mehr Zuweisungen: 1 Tagname `p` +
	Klassenname `klasse1` + 1 Attribut `attr='value'`
* `C` kommt als nächstes, obwohl es genau so ist wie `B`,
	es erscheint aber später im Stylesheet.
* dann kommt `B`
* und als letztes `D`.

## Kompatibilität

Die meisten Features von CSS sind in allen Browsern verfügbar. Man sollte
jedoch immer darauf achten die benutzten Features auf Verfügbarkeit in den
vom Projekt unterstützten Browser zu überprüfen.

[QuirksMode CSS](http://www.quirksmode.org/css/) oder [Can I Use](http://caniuse.com/) sind zwei der besten Quellen dafür.

## Weiterlesen

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)

