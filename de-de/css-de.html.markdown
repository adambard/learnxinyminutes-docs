---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
translators:
    - ["Kyr", "http://github.com/kyrami"]
lang: de-de
filename: learncss-de.css
---

In den frühen Tagen des Internets gab es keine visuellen Elemente, alles war nur reiner Text. Aber mit der Weiterentwickliung von Browsern wurden auch vollständig visuelle Webseiten zu einem Standard.   
CSS ist die allgemeine Sprache, die dazu da ist, damit man den HTML-Code und die Designelemente von Webseiten (strikt) unterscheiden kann.

Kurzgefasst, CSS ermöglicht es, verschiedene HTML-Elemente anzuvisieren und ihnen stilistische Eigenschaften zu geben.

CSS hat wie jede andere Sprache viele Versionen. Hier fokussieren wir uns auf CSS2.0, welche nicht die neueste, aber die am weitesten verbreitete und unterstützte Version ist.

**NOTE:** Weil die Ausgabe von CSS visuelle Eigenschaften sind, wirst du wahrscheinlich eine CSS-Sandbox wie [dabblet](http://dabblet.com/) benutzen müssen, um die Sprache richtig zu lernen.
In diesem Artikel wird am meisten auf generelle Hinweise und die Syntax geachtet.


```css
/* kommentare werden in sternchen-schrägstrichkombinationen gepackt (genauso wie hier!) */

/* ####################
   ## SELEKTOREN
   ####################*/

/* Eigentlich ist die häufigste Anwendungsweise von CSS sehr simpel */
selektor { eigenschaft: wert; /* mehr eigenschaften...*/ }

/* der selektor wird dazu benutzt, ein element auf der seite anzuvisieren

Aber man kann auch alle Elemente auf einer Seite anvisieren! */
* { color:red; } /* farbe:rot */

/*
Wenn wir so ein Element auf einer Seite haben:

<div class='eine-klasse klasse2' id='eineId' attr='wert' />
*/

/* kann man es so bei seiner klasse anvisieren */
.eine-klasse { }

/*oder bei beiden klassen! */
.eine-klasse.klasse2 { }

/* oder beim namen des tags */
div { }

/* oder bei seiner id */
#eineId { }

/* oder daran, dass es ein Attribut hat! */
[attr] { font-size:smaller; }

/* oder daran, dass das attribut einen bestimmten wert hat*/
[attr='wert'] { font-size:smaller; }

/* beginnt mit einem wert*/
[attr^='wert'] { font-size:smaller; }

/* oder endet mit */
[attr$='rt'] { font-size:smaller; }

/* oder sogar nur beinhaltet */
[attr~='er'] { font-size:smaller; }


/* was aber noch wichtiger ist, ist dass man alle diese kombinieren
kann - man sollte nur mit der leerzeichensetzung vorsichtig sein, 
da es mit einem leerzeichen zwei verschiedene selektoren wären*/
div.eine-klasse[attr$='rt'] { } /* so ist es richtig */

/* man kann auch ein element daran festmachen, wie sich die übergeordneten
elemente verhalten!*/

/*es muss allerdings ein direktes kind sein */
div.ein-elternteil > .klassen-name {}

/* oder jeder seiner eltern in der struktur */
/* das folgende heißt also, dass jedes element mit der klasse 'klassen-name'
und dem elternteil IN JEDER TIEFE ausgewählt wird */
div.ein-elternteil .klassen-name {}

/* achtung: dasselbe ohne das leerzeichen hat eine andere bedeutung,
kannst du mir sagen, was? */
div.ein-elternteil.klassen-name {}

/* man kann auch ein element nach seinem direkten vorherigen zwilling
auswählen */
.ich-bin-vorher + .dieses-element { }

/* oder jeden zwilling davor */
.ich-kann-jeder-davor-sein ~ .dieses-element {}

/* es gibt ein paar pseudoklassen, die sich basierend auf dem
seitenverhalten, nämlich nicht auf der seitenstruktur auswählen
lassen können */

/* zum beispiel, wenn über ein element mit dem mauszeiger gefahren wird */
:hover {}

/* oder einen bereits besuchten link*/
:visited {}

/* oder einen noch nicht besuchten link*/
:link {}

/* oder ein eingabeelement, das zurzeit im fokus steht */
:focus {}


/* ####################
   ## EIGENSCHAFTEN
   ####################*/

selector {
    
    /* einheiten */
    width: 50%; /* in prozent */
    font-size: 2em; /* mal der derzeitigen schriftgröße */
    width: 200px; /* in pixeln */
    font-size: 20pt; /* in punkten */
    width: 5cm; /* in zentimetern */
    width: 50mm; /* in millimetern */
    width: 5in; /* in zoll */
    
    /* farben */
    background-color: #F6E  /* in kurzem hex */
    background-color: #F262E2 /* in langem hex */
    background-color: tomato /* kann auch eine genannte farbe sein */
    background-color: rgb(255, 255, 255) /* in rgb */
    background-color: rgb(10%, 20%, 50%) /* in rgb prozent */
    background-color: rgba(255, 0, 0, 0.3); /* in semi-transparentem rgb */
    
    /* bilder */
    background-image: url(/pfad-zum-bild/image.jpg);
    
    /* schriften */
    font-family: Arial;
    font-family: "Courier New"; /* wenn der name ein leerzeichen beinhält, kommt er in
    apostrophe */
    font-family: "Courier New", Trebuchet, Arial; /* wenn der erste nicht gefunden wird, wird
    der zweite benutzt, und so weiter */
}

```

## Benutzung

speichere das css, das du benutzen willst mit der endung '.css'.

```xml
<!-- du musst die css-datei im <head>-bereich der seite erwähnen -->
<link rel='stylesheet' type='text/css' href='filepath/filename.css' />

<!-- es geht allerdings auch direkt, wobei diese methode nicht
empfohlen ist -->
<style>
   selector { property:value; }
</style>

<!-- oder direkt am element (sollte aber gelassen werden) -->
<div style='property:value;'>
</div>

```

## Wichtigkeit

ein element kann von mehr als einem selektoren angezielt werden. 
und kann auch eine eigenschaft mehr als einmal zugewiesen bekommen.  
in diesen fällen gibt es regeln, die die wichtigkeit von selektoren einführen.

wie haben dieses CSS:

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

und das folgende markup:

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>
```

die wichtigkeit der stile ist wie folgt:  
(die wichtigkeit gilt nur für **eigenschaften**, nicht für ganze blöcke)

* `E` hat die größte wichtigkeit wegen dem schlüsselwort `!important`.  
	man sollte diese form aber vermeiden.
* `F` ist als nächstes, da es direkt an dem element definiert ist.
* `A` ist als nächstes, da es "spezifischer" als alle anderen ist.  
	spezifischer = mehr zuweisungen: 1 tagname `p` +   
	klassenname `klasse1` + 1 attribut `attr='value'`
* `C` ist als nächstes obwohl es genau so ist wie `B`  
	aber es erscheint als letztes.
* dann ist `B`
* und als letztes `D`.

## Kompabilität

die meisten features von CSS sind in allen browsern verfügbar.
man sollte jedoch immer darauf achten, wenn man etwas mit CSS
programmiert.

[QuirksMode CSS](http://www.quirksmode.org/css/) ist eine der besten quellen dafür.

## Weiterlesen

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)

