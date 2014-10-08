---
language: haml
filename: learnhaml.html.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
---

Haml is a markup language predominantly used with Ruby that cleanly 
and simply describes the HTML of any web document without the use of
inline code.

It aims to reduce repetition in your markup by closing tags for you
based on the structure of the indents in your code. The result is
markup that is well-structured, DRY, logical, and easier to read.


```haml
/ -------------------------------------------
/ Comments
/ -------------------------------------------

/ This is what a comment looks like haml

/
  To write a multi line comment, indent your commented code to be
  wrapped by the forward slash

-# This is a silent comment, which means it wont be rendered into the doc at all


/ -------------------------------------------
/ Html elements
/ -------------------------------------------

/ To write your tags, use the percent sign followed by the name of the tag
%body
  %header
    %nav

/ Notice no closing tags. The above code would output

  <body>
    <header>
      <nav></nav>
    </header>
  </body>

/ Divs are the default elements so they can be written simply like this
.foo

/ To add content to a tag, nest it
%h1 Headline copy

/ 
  To output a ruby value as the contents of the tag, use an equals sign followed
  by the ruby code

%h1= author.name

/ Classes can be added to your tags either by chaining .classnames to the tag
%div.foo.bar

/ or as part of a ruby hash
%div{:class => 'foo bar'}

/ Attributes for any tag can be added in the hash
%a{:href => '#', :class => 'bar', :title => 'Bar'}

/ For boolean attributes assign the value 'true'
%input{:selected => true}

/ To write data-attributes, use the :data key with it's value as another hash
%div{:data => {:attribute => 'foo'}}


