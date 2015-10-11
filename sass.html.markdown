---
language: sass
filename: learnsass.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
---

Sass is a CSS extension language that adds features such as variables, nesting, mixins and more. 
Sass (and other preprocessors, such as [Less](http://lesscss.org/)) help developers to write maintainable and DRY (Don't Repeat Yourself) code.

Sass has two different syntax options to choose from. SCSS, which has the same syntax as CSS but with the added features of Sass. Or Sass (the original syntax), which uses indentation rather than curly braces and semicolons. 
This tutorial is written using SCSS.


```scss

	
//Single line comments are removed when Sass is compiled to CSS.

/*Multi line comments are preserved. */	
	
	
	
/*Variables
==============================*/
	
	

/* You can store a CSS value (such as a color) in a variable.
Use the '$' symbol to create a variable. */
	
$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;	

/* You can use the variables throughout your stylesheet. 
Now if you want to change a color, you only have to make the change once.*/	
	
body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* This would compile to: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* This is much more maintainable than having to change the color
each time it appears throughout your stylesheet. */
	


/*Mixins
==============================*/



/* If you find you are writing the same code for more than one
element, you might want to store that code in a mixin.

Use the '@mixin' directive, plus a name for your mixin.*/

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* You can use the mixin with '@include' and the mixin name. */

div {
	@include center;
	background-color: $primary-color;
}

/*Which would compile to: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* You can use mixins to create a shorthand property. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}
	
/*Which you can invoke by passing width and height arguments. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* This compiles to: */
.rectangle {
  width: 100px;
  height: 60px; 
}

.square {
  width: 40px;
  height: 40px; 
}




/*Extend (Inheritance)
==============================*/



/*Extend is a way to share the properties of one selector with another. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Compiles to: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F; 
}

.display-success {
  border-color: #22df56; 
}


	

/*Nesting
==============================*/



/*Sass allows you to nest selectors within selectors */

ul {
	list-style-type: none;
	margin-top: 2em;
	
	li {
		background-color: #FF0000;		
	}	
}

/* '&' will be replaced by the parent selector. */
/* You can also nest pseudo-classes. */
/* Keep in mind that over-nesting will make your code less maintainable.
For example: */

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

/* Compiles to: */

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



	
```	



## SASS or Sass?
Have you ever wondered whether Sass is an acronym or not? You probably haven't, but I'll tell you anyway. The name of the language is a word, "Sass", and not an acronym. 
Because people were constantly writing it as "SASS", the creator of the language jokingly called it "Syntactically Awesome StyleSheets". 


## Practice Sass
If you want to play with Sass in your browser, check out [SassMeister](http://sassmeister.com/).
You can use either syntax, just go into the settings and select either Sass or SCSS.

	
## Further reading
* [Official Documentation](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) provides tutorials (beginner-advanced) and articles.
