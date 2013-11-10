---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
---

In early days of web there was no visual elements, just pure text. But with the further development of browser fully visual web pages also became common. CSS is the standard language that exists to keep the separation between the content (HTML) and the look-and-feel of web pages.

In short, what CSS does is to provide a syntax that enables you to target different elements on an HTML page and assign different visual properties to them.

Like any other language, CSS has many versions. Here we focus on CSS2.0 which is not the most recent but the most widely supported and compatible version.

## Selectors

```css
/* let's stick to the tradition and show how comments are made first! */

/* Generally, the primary statement in CSS is very simple */
selector { property: value; /* more properties...*/ }

/* the selector is used to target an element on page.

You can target all elments on the page! */
* { color:red; }

/*
Given an element like this on the page:

<div class='some-class class2' id='someId' attr='value' />
*/

/* you can target it by it's class name */
.some-class { }

/*or by both classes! */
.some-class.class2 { }

/* or by it's tag name */
div { }

/* or it's id */
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


/* and more importantly you can combine these together -- there shouldn't be any space between different parts because that makes it to have another meaning.*/
div.some-class[attr$='ue'] { }

/* you can also select an element based on how it's parent is.*/

/*an element which is direct child of an element (selected the same way) */
div.some-parent > .class-name {}

/* or any of it's parents in the tree */
/* the following basically means any element that has class "class-name" and is child of a div with class name "some-parent" IN ANY DEPTH */
div.some-parent .class-name {}

/* warning: the same selector wihout space has another meaning. can you say what? */
div.some-parent.class-name {}

/* you also might choose to select an element based on it's direct previous sibling */
.i-am-before + .this-element { }

/*or any sibling before this */
.i-am-any-before ~ .this-element {}

/* There are some pseudo classes that allows you to select an element based on it's page behaviour (rather than page structure) */

/* for example for when an element is hovered */
:hover {}

/* or a visited link*/
:visited {}

/* or not visited link*/
:link {}

/* or an input element which is focused */
:focus {}

```

## Properties

```css
/*## Units
can be like :

- 50%: in percent
- 2em: two times the current font-size
- 20px: in pixels
- 20pt: in point
- 2cm: centimeter
- 20mm: millimeter
- 2in: inches

*/

/* ## Colors 

#F6E  -- can be in short hex
#F262E2 -- or in long hex format
red or tomato -- can be a named color
rgb(255, 255, 255) -- in rgb
rgb(10%, 20%, 50%) -- in rgb percent

*/

/* ## Font */
selector {
    font-style: italic; /* or normal */
    font-weight: bold; /* or normal, lighter or a number between 100 and 900*/
    font-size: 2em; /* see units */
    font-family: Verdana, Arial; /* if the first one is not supported the second one is taken.*/
}

/* you can set all in one declaration. */
selector { font: bold italic 12px Consolas; }

/*## Background */
selector {
    background-color: red; /* see colors */
    background-image: url(/path/cat.jpg);
    background-repeat: no-repaet; /* or repeat or repeat-x or repeat-y */
    background-attachment: scroll; /* or fixed */
    background-position: 10px 20px; /* or center, top, bottom, left, right */
}

/* again you can set all in one declaratio */
selector { background: red url(image.jpg) no-repeat center top fixed; }

/*## Box model 

All elements (other than inline elements like span) follow a box model
that consists of the following components.

                             ---------------
                                margin
                             ---------------
                                border
                             ---------------
                                padding
                             ---------------
| margin | border | padding | [width/height] | padding | border | margin
                             ---------------
                                padding
                             ---------------
                                border
                             ---------------
                                margin
                             ---------------

of these components all except margin add to the dimension of the element.

e.g. padding-left: 10px; width: 100px; border-left: 2px;
    => effective width of the element 112px (given all -right components are zero)

*/
selector {
    width: 100px;
    height: 100px;

    padding-top:10px;
    padding-bottom:10px;
    padding-left:10px;
    padding-right:10px;

    margin-top:10px;
    margin-bottom:10px;
    margin-left:10px;
    margin-right:10px;
    
    border-top:10px;
    border-bottom:10px;
    border-left:10px;
    border-right:10px;
}

/* again you can use shorthands 

for padding, margin and border the order is:
[top] [right] [bottom] [left]*/
selector {
    padding: 10px 8px 6px 5px;
    /* same for margin and border*/
}

/* a shorter one!
[top and bottom] [left and right] */
selector {
    padding: 6px 5px;
    /* same for margin and border*/
}

/* and even shorter!
[all sides] */
selector {
    padding: 6px;
    /* same for margin and border*/
}

/*## Positioning

elements are normally following the page flow based on where in the markup they are.

some tags like `div` or `p` are full-width and the rest are inline by default.

but all elements can have their layout changed*/
selelctor {
    display: inline; /* inline-block, block, table-cell, et.*/
}

/* elements can be absolutely positioned -- which means they won't follow the page flow and will be independently positioned on the screen. */
selector { position:absolute; left: 200px; top:10px; /* or right:10px; bottom:10px; */ }

/* in this case the elements top left will be alighned with the page body.

but if you want it to be relative to an earlier parent.*/
parent-selector { position:relative; }

/* if you want to have the same thing but moving with scroll: */
selector { position:fixed; left: 200px; top:10px; /* or right:10px; bottom:10px; */ }

/* when elements appear on the same absolute position. the latest one in the markup appears on top.

unless...*/
selector { z-index: 2; /* or any number higher than others' */ }

/* if you wish your element to follow the markup layout (not absolutely positioned) but floated to a side in it's parent:*/
selector { float: left; /* or right */ }

/*## Lists

you can also control how the lists appear on the screen:*/

selector {
    list-style-type: circle; /* disc | square | decimal | etc... */
    list-style-position: inside; /* or outside */
    list-style-image: url(path/image.jpg);
}

/*as always this can be shorthanded */

selector { list-tyle: disc inside url(...); }


```

## Usage

Save any CSS you want in a file with extension `.css`.

```markup
<!-- you need to include the css file in your page's <head>: -->
<link rel='stylesheet' type='text/css' href='filepath/filename.css' />

<!-- you can also include some CSS inline in your markup. However it is highly recommended to avoid this. -->
<style>
   selector { property:value; }
</style>

<!-- or directly set CSS properties on the element. This has to be avoided as much as you can. -->
<div style='property:value;'>
</div>

```

## Precedence

As you noticed an element may be targetted by more than one selector. and may have a property set on it in more than one. In these cases, one of the rules takes precedence over others.

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

and the following markdown:
```markdown
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>
```

The precedence of style is as followed:  
Note that the precedence is for each **property** in the style and not for the entire block.

* `E` has the highest precedence because of the keyword `!important`. It is recommended to avoid this unless it is strictly necessary to use.
* `F` is the next because it is inline style.
* `A` is the next because is more "specific" that anything else. more specific = more specifiers. here 3 specifiers: 1 tagname `p` +  class name `class1` + 1 attribute `attr='value'`
* `C` is the next. although it has the same specificness as `B` but it appears after that.
* Then is `B`
* and lastly is `D`.

## Compatibility

Most of the features in CSS2 (and gradually in CSS3) are compatible across all browsers and devices. But it's always vital to have in mind the compatiblity of what you use in CSS with your target browsers.

[QuirksMode CSS](http://www.quirksmode.org/css/) is one of the best sources for this.

## Further Reading

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)

