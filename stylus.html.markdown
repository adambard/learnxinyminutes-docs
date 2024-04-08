---
language: stylus
filename: learnStylus.styl
contributors:
  - ["Salomão Neto", "https://github.com/salomaosnff"]
  - ["Isaac Henrique", "https://github.com/Isaachi1"]
translators:
  - ["Divay Prakash", "https://github.com/divayprakash"]
---

Stylus is a dynamic stylesheet preprocessor language that is compiled into CSS. It aims to add functionality to CSS without breaking compatibility across web browsers.
It does this using variables, nesting, mixins, functions and more.

Stylus syntax is very flexible. You can use standard CSS syntax and leave the semicolon (;), colon (:) and even the ({) and (}) optional, making your code even more readable.

Stylus does not provide new style options, but gives functionality that lets you make your CSS much more dynamic.

```sass
/* Code style
==============================*/

/* Keys, semicolon, and colon are optional in Stylus. */

body {
  background: #000;
}

body {
  background: #000
}

body {
  background #000
}

body
  background #000

body
  background: #000;

body
  background: #000

// Single-line comments are removed when Stylus is compiled into CSS.

/* Multi-line comments are preserved. */


/* Selectors
==============================*/

/* Selecting elements within another element */
body {
  background: #000000;
  h1 {
    color: #FF0000;
  }
}

/* Or if you prefer... */
body
  background #000000
  h1
    color #FF0000


/* Getting parent element reference
==============================*/
a {
  color: #0088dd;
  &:hover {
    color: #DD8800;
  }
}


/* Variables
==============================*/


/*
  You can store a CSS value (such as the color) of a variable.
  Although it is optional, it is recommended to add $ before a variable name
  so you can distinguish a variable from another CSS value.
*/

$primary-color = #A3A4FF
$secondary-color = #51527F
$body-font = 'Roboto', sans-serif

/* You can use variables throughout your style sheet.
Now, if you want to change the color, you only have to make the change once. */

body
	background-color $primary-color
	color $secondary-color
	font-family $body-font

/* After compilation: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/ *
This is much easier to maintain than having to change color
each time it appears throughout your style sheet.
* /


/* Mixins
==============================*/

/* If you find that you are writing the same code for more than one
element, you may want to store that code in a mixin.

center()
  display block
	margin-left auto
	margin-right auto
	left 0
	right 0

/* Using the mixin */
body {
  center()
  background-color: $primary-color
}

/* After compilation: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}

/* You can use mixins to create a shorthand property. */

size($width, $height)
  width $width
  height $height

.rectangle
  size(100px, 60px)

.square
	size(40px, 40px)

/* You can use a mixin as a CSS property. */
circle($ratio)
  width $ratio * 2
  height $ratio * 2
  border-radius $ratio

.ball
  circle 25px


/* Interpolation
==============================*/

vendor(prop, args)
  -webkit-{prop} args
  -moz-{prop} args
  {prop} args

border-radius()
  vendor('border-radius', arguments)

box-shadow()
  vendor('box-shadow', arguments)

button
  border-radius 1px 2px / 3px 4px


/* Functions
==============================*/

/* Functions in Stylus allow you to perform a variety of tasks, such as recalling some data. */

body {
  background darken(#0088DD, 50%) // Dim color #0088DD by 50%
}

/* Creating your own function */
add(a, b)
  a + b

body
  padding add(10px, 5)


/* Conditions
==============================*/
compare(a, b)
  if a > b
    bigger
  else if a < b
    smaller
  else
    equal

compare(5, 2)   // => bigger
compare(1, 5)   // => smaller
compare(10, 10) // => equal


/* Iterations
==============================*/

/*
Repeat loop syntax for:
for <val-name> [, <key-name>] in <expression>
*/

for $item in (1..2) /* Repeat block 12 times */
  .col-{$item}
    width ($item / 12) * 100% /* Calculate row by column number */
```

Now that you know a little about this powerful CSS preprocessor, you're ready to create more dynamic style sheets. To learn more, visit the official stylus documentation at http://stylus-lang.com.
