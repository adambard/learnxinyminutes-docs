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

Sass files must be compiled into CSS. You can use any number of commandline
tools to compile Sass into CSS. Many IDEs also offer Sass compilation, as well.


```sass
/* Like CSS, Sass uses slash-asterisk to denote comments */

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

/* After compiling the Sass files into CSS, you'll have the following code
   in your generated CSS file */
   
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

/* The above will compile into the following CSS */
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
   specificity and make your CSS harder to work with and maintain. Best practices
   recommend going no more than 3 levels deep when nesting. */

/* ####################
   ## MIXINS
   #################### */
/* Mixins allow you to define reusable chunks of CSS. They can take one or more
   arguments to allow you to make reusable pieces of styling. */
@mixin form-button($color, $size, $border-radius) {
  color: $color;
  font-size: $size;
  border-radius: $border-radius;
}

/* Mixins are invoked within a CSS declaration. */
.user-form .submit {
  @include form-button(#0000ff, 16px, 4px);
  margin: 10px;
}

/* The above mixin will compile into the following css */
.user-form .submit {
  color: #0000ff;
  font-size: 16px;
  border-radius: 4px;
  margin: 10px;
}

/* ####################
   ## EXTEND/INHERITANCE
   #################### */
      
/* ####################
   ## MATH OPERATIONS
   #################### */      
   
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

To run a quick compatibility check, [CanIUse](http://caniuse.com) is a great resource.

## Further Reading

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
