---
language: sass
contributors:
    - ["Sean Corrales", "https://github.com/droidenator"]
filename: learnsass.scss
---

Sass is a CSS pre-processor. It adds several features that plain
CSS lacks such as variables, mixins, basic math, and inheritance.
 
Initially, Sass was written using spacing and indention instead
of brackets and semi-colons; these files use the extension '.sass'.
Sass was later revised to use brackets and semi-colons and become
a superset of CSS3. This new version uses the extension ".scss". 
Using ".scss" means that any valid CSS3 file can be converted to 
Sass by simply changing the file extension to '.scss'.

If you're already familiar with CSS3, you'll be able to pick up Sass
relatively quickly. It does not provide any new styling options but rather
the tools to write your CSS more efficiently and make maintenance much
easier.

```sass
/* Like CSS, Sass uses slash-asterisk to denote comments. Slash-asterisk 
   comments can span multiple lines. These comments will appear 
   in your compiled CSS */

// Sass also supports single line comments that use double slashes. These 
// comments will not be rendered in your compiled CSS

/* ####################
   ## VARIABLES
   #################### */

/* Sass allows you to define variables that can be used throughout
   your stylesheets. Variables are defined by placing a '$' in front 
   of a string. Many users like to keep their variables in a single file */
$primary-color: #0000ff;
$headline-size: 24px;

/* Variables can be used in any CSS declaration. This allows you to change
   a single value in one place. */
a {
  color: $primary-color;
}

h1 {
  color: $primary-color;
  font-size: $headline-size;
}

/* Generated CSS result */
   
a { 
  color: #0000ff;
}

h1 {
  color: #0000ff;
  font-size: 24px;
}

/* ####################
   ## NESTING
   #################### */
   
/* Nesting allows you to easily group together statements and nest them
   in a way that indicates their hierarchy */
article {
  font-size: 14px;
  
  a {
    text-decoration: underline; 
  }
  
  ul {
    list-style-type: disc;
    
    li {
      text-indent: 3em;
    }
  }
  
  pre, img {
    display: inline-block;
    float: left;
  }
}

/* Generated CSS result */
article {
  font-size: 14px;
}

article a {
  text-decoration: underline;
}

article ul {
  list-style-type: disc;
}

article ul li {
  text-indent: 3em;
}

article pre, 
article img {
  display: inline-block;
  float: left;
}

/* It is recommended to not nest too deeply as this can cause issues with
   specificity and make your CSS harder to work with and maintain. Best 
   practices recommend going no more than 3 levels deep when nesting. */
   
/* ###############################
   ## REFERENCE PARENT SELECTORS
   ############################### */   
   
/* Reference parent selectors are used when you're nesting statements and want
   to reference the parent selector from within the nested statements. You can
   reference a parent using & */
   
a {
  text-decoration: none;
  color: #ff0000;
  
  &:hover {
    text-decoration: underline;  
  }
  
  body.noLinks & {
    display: none;
  }
}

/* Generated CSS result */

a {
  text-decoration: none;
  color: #ff0000;
}

a:hover {
  text-decoration: underline;
}

body.noLinks a {
  display: none;
}


/* ####################
   ## MIXINS
   #################### */
   
/* Mixins allow you to define reusable chunks of CSS. They can take one or more
   arguments to allow you to make reusable pieces of styling. Mixins very 
   helpful when dealing with vendor prefixes. */
   
@mixin form-button($color, $size, $border-radius) {
  color: $color;
  font-size: $size;
  border-radius: $border-radius;
}

/* Mixins are invoked within a CSS declaration. */

.user-form .submit {
  @include form-button(#0000ff, 16px, 4px);
}

/* Generated CSS result */

.user-form .submit {
  color: #0000ff;
  font-size: 16px;
  border-radius: 4px;
}

/* ####################
   ## FUNCTIONS
   #################### */
   
/* Sass provides functions that can be used to accomplish a variety of 
   tasks. Consider the following */

/* Functions can be invoked by using their name and passing in the 
   required arguments */
body {
  width: round(10.25px);    
}

.footer {
  background-color: fade_out(#000000, 0.25)
}

/* Generated CSS result */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}   
   
/* You may also define your own functions. Functions are very similar to 
   mixins. When trying to choose between a function or a mixin, remember
   that functions are best for returning values while mixins are best for
   generating CSS while functions are better for logic that might be used
   throughout your Sass code. The examples in the Math Operators' section 
   are ideal candidates for becoming a reusable function. */

/* This function will take a target size and the parent size and calculate 
   and return the percentage */
   
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

/* Generated CSS result */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

/* #####################
   ## EXTEND/INHERITANCE
   ##################### */
   
/* Sass allows you to extend an existing CSS statement. This makes it 
   very easy to write CSS that does not violate DRY principles. Any
   CSS statement can be extended */
   
.content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend .content-window;
  background-color: #0000ff;
}

.notification-window {
  @extend .content-window;
  background-color: #ff0000;
}

.settings-window {
  @extend .content-window;
  background-color: #ccc;
}

/* Generated CSS result */

.content-window,
.message-window,
.notification-window,
.settings-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}

.notification-window {
  background-color: #ff0000;
}

.settings-window {
  background-color: #ccc;
}

/* Extending a CSS statement is preferable to creating a mixin 
   because of the way it groups together the classes that all share
   the same base styling. If this was done with a mixin, the font-size,
   padding, color, and border-radius would be duplicated for each statement
   that called the mixin. While it won't affect your workflow, it will
   add unnecessary bloat to the files created by the Sass compiler. */
   
/* #########################
   ## PLACEHOLDER SELECTORS
   ######################### */   
   
/* Placeholders are useful when creating a CSS statement to extend. If you
   wanted to create a CSS statement that was exclusively used with @extend,
   you can do so using a placeholder. Placeholders begin with a '%' instead
   of '.' or '#'. Placeholders will not appear in the compiled CSS. */
   
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

/* Generated CSS result */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}
      
/* ####################
   ## MATH OPERATIONS
   #################### */      
   
/* Sass provides the following operators: +, -, *, /, and %. These can
   be useful for calculating values directly in your Sass files instead
   of using values that you've already calculated by hand. Below is an example
   of a setting up a simple two column design. */
   
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

/* Generated CSS result */

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

## Usage

Sass files must be compiled into CSS. You can use any number of commandline
tools to compile Sass into CSS. Many IDEs also offer Sass compilation, as well.

[Compass](http://compass-style.org/) is one of the more popular tools for Sass compilation.  

## Compatibility

Sass can be used in any project as long as you have something to compile it
into CSS. You'll want to verify that the CSS you're using is compatible
with your target browsers. 

[QuirksMode CSS](http://www.quirksmode.org/css/) and [CanIUse](http://caniuse.com) great resources for checking compatibility. 