---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
translators:
    - ["Kyr", "http://github.com/kyrami"]
lang: de-de
---

In den frühen Tagen des Internets gab es keine visuellen Elemente, alles war nur reiner Text. Aber mit der Weiterentwickliung von Browsern wurden auch vollständig visuelle Webseiten zu einem Standard.   
CSS ist die allgemeine Sprache, die dazu da ist, damit man den HTML-Code und die Designelemente von Webseiten (strikt) unterscheiden kann.

Kurzgefasst, CSS ermöglicht es, verschiedene HTML-Elemente anzuvisieren und ihnen stilistische Eigenschaften zu geben.

CSS hat wie jede andere Sprache viele Versionen. Hier fokussieren wir uns auf CSS2.0, welche nicht die neueste, aber die am weitesten verbreitete und unterstützte Version ist.

**NOTE:** Weil die Ausgabe von CSS visuelle Eigenschaften sind, wirst du wahrscheinlich einen CSS-Playground wie [dabblet](http://dabblet.com/) benutzen müssen, um die Sprache richtig zu lernen.
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

/* man kann auch ein element daran festmachen, wie sich die übergeordneten elemente verhalten!*/

/*es muss allerdings ein direktes kind sein */
div.some-parent > .class-name {}

/* or any of it's parents in the tree */
/* the following basically means any element that has class "class-name"  
and is child of a div with class name "some-parent" IN ANY DEPTH */
div.some-parent .class-name {}

/* warning: the same selector wihout spaaace has another meaning.  
can you say what? */
div.some-parent.class-name {}

/* you also might choose to select an element based on it's direct  
previous sibling */
.i-am-before + .this-element { }

/*or any sibling before this */
.i-am-any-before ~ .this-element {}

/* There are some pseudo classes that allows you to select an element  
based on it's page behaviour (rather than page structure) */

/* for example for when an element is hovered */
:hover {}

/* or a visited link*/
:visited {}

/* or not visited link*/
:link {}

/* or an input element which is focused */
:focus {}


/* ####################
   ## PROPERTIES
   ####################*/

selector {
    
    /* Units */
    width: 50%; /* in percent */
    font-size: 2em; /* times current font-size */
    width: 200px; /* in pixels */
    font-size: 20pt; /* in points */
    width: 5cm; /* in centimeters */
    width: 50mm; /* in millimeters */
    width: 5in; /* in inches */
    
    /* Colors */
    background-color: #F6E  /* in short hex */
    background-color: #F262E2 /* in long hex format */
    background-color: tomato /* can be a named color */
    background-color: rgb(255, 255, 255) /* in rgb */
    background-color: rgb(10%, 20%, 50%) /* in rgb percent */
    background-color: rgba(255, 0, 0, 0.3); /* in semi-transparent rgb */
    
    /* Images */
    background-image: url(/path-to-image/image.jpg);
    
    /* Fonts */
    font-family: Arial;
    font-family: "Courier New"; /* if name has spaaace it appears in double-quote */
    font-family: "Courier New", Trebuchet, Arial; /* if first one was not found
    						 browser uses the second font, and so forth */
}

```

## Usage

Save any CSS you want in a file with extension `.css`.

```xml
<!-- you need to include the css file in your page's <head>: -->
<link rel='stylesheet' type='text/css' href='filepath/filename.css' />

<!-- you can also include some CSS inline in your markup. However it is highly  
recommended to avoid this. -->
<style>
   selector { property:value; }
</style>

<!-- or directly set CSS properties on the element. 
This has to be avoided as much as you can. -->
<div style='property:value;'>
</div>

```

## Precedence

As you noticed an element may be targetted by more than one selector. 
and may have a property set on it in more than one.  
In these cases, one of the rules takes precedence over others.

Given the following CSS:

```css
/*A*/
p.class1[attr='value']

/*B*/
p.class1 {}

/*C*/
p.class2 {}

/*D*/
p {}

/*E*/
p { property: value !important; }

```

and the following markup:

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>
```

The precedence of style is as followed:  
Remember, the precedence is for each **property**, not for the entire block.

* `E` has the highest precedence because of the keyword `!important`.  
	It is recommended to avoid this unless it is strictly necessary to use.
* `F` is next, because it is inline style.
* `A` is next, because it is more "specific" than anything else.  
	more specific = more specifiers. here 3 specifiers: 1 tagname `p` +   
	class name `class1` + 1 attribute `attr='value'`
* `C` is next. although it has the same specificness as `B`  
	but it appears last.
* Then is `B`
* and lastly is `D`.

## Compatibility

Most of the features in CSS2 (and gradually in CSS3) are compatible across  
all browsers and devices. But it's always vital to have in mind the compatiblity  
of what you use in CSS with your target browsers.

[QuirksMode CSS](http://www.quirksmode.org/css/) is one of the best sources for this.

## Further Reading

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)

