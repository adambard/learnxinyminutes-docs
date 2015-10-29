---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
filename: learncss.css
---

In the early days of the web there were no visual elements, just pure text. But with the  
further development of browsers, fully visual web pages also became common.   
CSS is the standard language that exists to keep the separation between   
the content (HTML) and the look-and-feel of web pages.

In short, what CSS does is to provide a syntax that enables you to target   
different elements on an HTML page and assign different visual properties to them.

Like any other languages, CSS has many versions. Here we focus on CSS2.0,   
which is not the most recent version, but is the most widely supported and compatible version.

**NOTE:** Because the outcome of CSS consists of visual effects, in order to 
learn it, you need try everything in a 
CSS playground like [dabblet](http://dabblet.com/).
The main focus of this article is on the syntax and some general tips.


```css
/* comments appear inside slash-asterisk, just like this line!
   there are no "one-line comments"; this is the only comment style */

/* ####################
   ## SELECTORS
   #################### */

/* Generally, the primary statement in CSS is very simple */
selector { property: value; /* more properties...*/ }

/* the selector is used to target an element on page.

You can target all elements on the page using asterisk! */
* { color:red; }

/*
Given an element like this on the page:

<div class='some-class class2' id='someId' attr='value' otherAttr='en-us foo bar' />
*/

/* you can target it by its name */
.some-class { }

/* or by both classes! */
.some-class.class2 { }

/* or by its element name */
div { }

/* or its id */
#someId { }

/* or by the fact that it has an attribute! */
[attr] { font-size:smaller; }

/* or that the attribute has a specific value */
[attr='value'] { font-size:smaller; }

/* start with a value (CSS3) */
[attr^='val'] { font-size:smaller; }

/* or ends with (CSS3) */
[attr$='ue'] { font-size:smaller; }

/* or select by one of the values from the whitespace separated list (CSS3) */
[otherAttr~='foo'] { font-size:smaller; }

/* or value can be exactly “value” or can begin with “value” immediately followed by “-” (U+002D) */
[otherAttr|='en'] { font-size:smaller; }


/* and more importantly you can combine these together -- there shouldn't be  
any space between different parts because that makes it to have another  
meaning. */
div.some-class[attr$='ue'] { }

/* you can also select an element based on its parent. */

/* an element which is direct child of an element (selected the same way) */
div.some-parent > .class-name {}

/* or any of its parents in the tree
   the following basically means any element that has class "class-name"  
   and is child of a div with class name "some-parent" IN ANY DEPTH */
div.some-parent .class-name {}

/* warning: the same selector without space has another meaning.  
   can you say what? */
div.some-parent.class-name {}

/* you also might choose to select an element based on its direct  
   previous sibling */
.i-am-before + .this-element { }

/* or any sibling before this */
.i-am-any-before ~ .this-element {}

/* There are some pseudo classes that allows you to select an element  
   based on its page behaviour (rather than page structure) */

/* for example for when an element is hovered */
selector:hover {}

/* or a visited link */
selected:visited {}

/* or not visited link */
selected:link {}

/* or an input element which is focused */
selected:focus {}


/* ####################
   ## PROPERTIES
   #################### */

selector {
    
    /* Units */
    width: 50%; /* in percent */
    font-size: 2em; /* times current font-size */
    width: 200px; /* in pixels */
    font-size: 20pt; /* in points */
    width: 5cm; /* in centimeters */
    min-width: 50mm; /* in millimeters */
    max-width: 5in; /* in inches. max-(width|height) */
    height: 0.2vh; /* times vertical height of browser viewport (CSS3) */
    width: 0.4vw; /* times horizontal width of browser viewport (CSS3) */
    min-height: 0.1vmin; /* the lesser of vertical, horizontal dimensions of browser viewport (CSS3) */
    max-width: 0.3vmax; /* same as above, except the greater of the dimensions (CSS3) */
    
    /* Colors */
    background-color: #F6E;  /* in short hex */
    background-color: #F262E2; /* in long hex format */
    background-color: tomato; /* can be a named color */
    background-color: rgb(255, 255, 255); /* in rgb */
    background-color: rgb(10%, 20%, 50%); /* in rgb percent */
    background-color: rgba(255, 0, 0, 0.3); /* in semi-transparent rgb (CSS3) */
    background-color: transparent; /* see thru */
    background-color: hsl(0, 100%, 50%); /* hsl format (CSS3). */
    background-color: hsla(0, 100%, 50%, 0.3); /* Similar to RGBA, specify opacity at end (CSS3) */

    
    /* Images */
    background-image: url(/path-to-image/image.jpg); /* quotes inside url() optional */
    
    /* Fonts */
    font-family: Arial;
    font-family: "Courier New"; /* if name has space it appears in single or double quotes */
    font-family: "Courier New", Trebuchet, Arial, sans-serif; /* if first one was not found
                             browser uses the second font, and so forth */
}

```

## Usage

Save any CSS you want in a file with extension `.css`.

```xml
<!-- you need to include the css file in your page's <head>: -->
<link rel='stylesheet' type='text/css' href='path/to/style.css' />

<!-- you can also include some CSS inline in your markup. However it is highly  
recommended to avoid this. -->
<style>
   a { color: purple; }
</style>

<!-- or directly set CSS properties on the element. 
This has to be avoided as much as you can. -->
<div style="border: 1px solid red;">
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
all browsers and devices. But it's always vital to have in mind the compatibility 
of what you use in CSS with your target browsers.

[QuirksMode CSS](http://www.quirksmode.org/css/) is one of the best sources for this.

To run a quick compatibility check, [Can I Use...](http://caniuse.com) is a great resource.

## Further Reading

* [Mozilla Developer Network's CSS documentation](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops' CSS Reference](http://tympanus.net/codrops/css_reference/)
* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SCSS](http://sass-lang.com/) and [LESS](http://lesscss.org/) for CSS pre-processing
