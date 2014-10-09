---
language: haml
filename: learnhaml.html.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
---

Haml is a markup language predominantly used with Ruby that cleanly 
and simply describes the HTML of any web document without the use of
inline code. It is a popular alternative to using rails templating 
language (.erb) and allows you to embed ruby code into your markup.

It aims to reduce repetition in your markup by closing tags for you
based on the structure of the indents in your code. The result is
markup that is well-structured, DRY, logical, and easier to read.


```haml
/ -------------------------------------------
/ Comments
/ -------------------------------------------

/ This is what a comment looks like haml.

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

/ To add content to a tag, add the text directly after the declaration
%h1 Headline copy

/ To write multiline content, nest it instead
%p 
  This is a lot of content that we could probably split onto two
  separate lines.

/ You can escape html by using the ampersand and equals sign ( &= )
%p
  &= "Yes & yes"

/ which would output 'Yes &amp; yes'

/ You can unescape html by using the bang and equals sign ( != )
%p
  != "This is how you write a paragraph tag <p></p>"

/ which would output 'This is how you write a paragraph tag <p></p>'

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


/ -------------------------------------------
/ Inserting Ruby
/ -------------------------------------------

/ 
  To output a ruby value as the contents of a tag, use an equals sign followed
  by the ruby code

%h1= book.name

%p
  = book.author
  = book.publisher


/ To run some ruby code without rendering it to the html, use a hyphen instead
- books = ['book 1', 'book 2', 'book 3']

/ Allowing you to do all sorts of awesome, like ruby blocks
- books.shuffle.each_with_index do |book, index|
  %h1= book

  if book do
    %p This is a book

/
  Again, no need to add the closing tags to the block, even for the ruby.
  Indentation will take care of that for you.


/ -------------------------------------------
/ Inline Ruby / Ruby interpolation
/ -------------------------------------------

/ Include a ruby variable in a line of plain text using #{}
%p Your highest scoring game is #{best_game}


/ -------------------------------------------
/ Filters
/ -------------------------------------------

/
  Use the colon to define haml filters, one example of a filter you can 
  use is :javascript, which can be used for writing inline js

:javascript
  console.log('This is inline <script>');

```

## Additional resources

- [What is HAML?](http://haml.info/) - A good introduction that does a much better job of explaining the benefits of using HAML.
- [Official Docs](http://www.ruby-doc.org/core-2.1.1/) - If you'd like to go a little deeper.
