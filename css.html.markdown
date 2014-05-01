---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
filename: learncss.css
---

In early days of web there was no visual elements, just pure text. But with the  
further development of browser fully visual web pages also became common.   
CSS is the standard language that exists to keep the separation between   
the content (HTML) and the look-and-feel of web pages.

In short, what CSS does is to provide a syntax that enables you to target   
different elements on an HTML page and assign different visual properties to them.

Like any other language, CSS has many versions. Here we focus on CSS2.0   
which is not the most recent but the most widely supported and compatible version.

**NOTE:** Because the outcome of CSS is some visual effects, in order to 
learn it, you need try all different things in a 
CSS playground like [dabblet](http://dabblet.com/).
The main focus of this article is on the syntax and some general tips.


```css
/* comments appear inside slash-asterisk, just like this line! */

/* ####################
   ## SELECTORS
   ####################*/

/* Generally, the primary statement in CSS is very simple */
selector { property: value; /* more properties...*/ }

/* the selector is used to target an element on page.

You can target all elments on the page! */
* { color:red; }

/*
Given an element like this on the page:

<div class='some-class class2' id='someId' attr='value' />
*/

/* you can target it by its name */
.some-class { }

/*or by both classes! */
.some-class.class2 { }

/* or by its element name */
div { }

/* or its id */
#someId { }

/* or by the fact that it has an attribute! */
[attr] { font-size:smaller; }

/* or that the attribute has a specific value */
[attr='value'] { font-size:smaller; }

/* start with a value*/
[attr^='val'] { font-size:smaller; }

/* or ends with */
[attr$='ue'] { font-size:smaller; }

/* or even contains a value */
[attr~='lu'] { font-size:smaller; }


/* and more importantly you can combine these together -- there shouldn't be  
any spaaace between different parts because that makes it to have another  
meaning.*/
div.some-class[attr$='ue'] { }

/* you can also select an element based on its parent.*/

/*an element which is direct child of an element (selected the same way) */
div.some-parent > .class-name {}

/* or any of its parents in the tree */
/* the following basically means any element that has class "class-name"  
and is child of a div with class name "some-parent" IN ANY DEPTH */
div.some-parent .class-name {}

/* warning: the same selector wihout spaaace has another meaning.  
can you say what? */
div.some-parent.class-name {}

/* you also might choose to select an element based on its direct  
previous sibling */
.i-am-before + .this-element { }

/*or any sibling before this */
.i-am-any-before ~ .this-element {}

/* There are some pseudo classes that allows you to select an element  
based on its page behaviour (rather than page structure) */

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
    background-color: #F6E;  /* in short hex */
    background-color: #F262E2; /* in long hex format */
    background-color: tomato; /* can be a named color */
    background-color: rgb(255, 255, 255); /* in rgb */
    background-color: rgb(10%, 20%, 50%); /* in rgb percent */
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

