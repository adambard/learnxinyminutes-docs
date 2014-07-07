---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
filename: markdown.md
---

Markdown was created by John Gruber in 2004. It's meant to be an easy to read and write syntax which converts easily to HTML (and now many other formats as well).

Give me as much feedback as you want! / Feel free to fork and pull request!


```
<!-- Markdown is a superset of HTML, so any HTML file is valid Markdown, that
means we can use HTML elements in Markdown, such as the comment element, and
they won't be affected by a markdown parser. However, if you create an HTML
element in your markdown file, you cannot use markdown syntax within that
element's contents. -->

<!-- Markdown also varies in implementation from one parser to a next. This
guide will attempt to clarify when features are universal or when they are
specific to a certain parser. -->

<!-- Headers -->
<!-- You can create HTML elements <h1> through <h6> easily by prepending the
text you want to be in that element by a number of hashes (#) -->
# This is an <h1>
## This is an <h2>
### This is an <h3>
#### This is an <h4>
##### This is an <h5>
###### This is an <h6>

<!-- Markdown also provides us with two alternative ways of indicating h1 and h2 -->
This is an h1
=============

This is an h2
-------------

<!-- Simple text styles -->
<!-- Text can be easily styled as italic, bold, or strikethrough using markdown -->

*This text is in italics.*
_And so is this text._

**This text is in bold.**
__And so is this text.__

***This text is in both.***
**_As is this!_**
*__And this!__*

<!-- In Github Flavored Markdown, which is used to render markdown files on
Github, we also have: -->

~~This text is rendered with strikethrough.~~

<!-- Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines. -->

This is a paragraph. I'm typing in a paragraph isn't this fun?

Now I'm in paragraph 2.
I'm still in paragraph 2 too!


I'm in paragraph three!

<!-- Should you ever want to insert an HTML <br /> tag, you can end a paragraph
with two or more spaces and then begin a new paragraph. -->

I end with two spaces (highlight me to see them).  

There's a <br /> above me!

<!-- Block quotes are easy and done with the > character. -->

> This is a block quote. You can either
> manually wrap your lines and put a `>` before every line or you can let your lines get really long and wrap on their own.
> It doesn't make a difference so long as they start with a `>`.

> You can also use more than one level
>> of indentation?
> How neat is that?

<!-- Lists -->
<!-- Unordered lists can be made using asterisks, pluses, or hyphens -->

* Item
* Item
* Another item

or

+ Item
+ Item
+ One more item

or 

- Item
- Item
- One last item

<!-- Ordered lists are done with a number followed by a period -->

1. Item one
2. Item two
3. Item three

<!-- You don't even have to label the items correctly and markdown will still
render the numbers in order, but this may not be a good idea -->

1. Item one
1. Item two
1. Item three
<!-- (This renders the same as the above example) -->

<!-- You can also use sublists -->

1. Item one
2. Item two
3. Item three
    * Sub-item
    * Sub-item
4. Item four

<!-- Code blocks -->
<!-- You can indicate a code block (which uses the <code> element) by indenting
a line with four spaces or a tab -->

    This is code
    So is this

<!-- You can also re-tab (or add an additional four spaces) for indentation
inside your code -->

    my_array.each do |item|
        puts item
    end

<!-- Inline code can be created using the backtick character ` -->

John didn't even know what the `go_to()` function did!

<!-- In Github Flavored Markdown, you can use a special syntax for code -->

\`\`\`ruby <!-- except remove those backslashes when you do this, just ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- here too, no backslashes, just ``` -->

<-- The above text doesn't require indenting, plus Github will use syntax
highlighting of the language you specify after the ``` -->

<!-- Horizontal rule (<hr />) -->
<!-- Horizontal rules are easily added with three or more asterisks or hyphens,
with or without spaces. -->

***
---
- - - 
****************

<!-- Links -->
<!-- One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses () -->

[Click me!](http://test.com/)

<!-- You can also add a link title using quotes inside the parentheses -->

[Click me!](http://test.com/ "Link to Test.com")

<!-- Relative paths work too. -->

[Go to music](/music/).

<!-- Markdown also supports reference style links -->

[Click this link][link1] for more info about it!
[Also check out this link][foobar] if you want to.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!-- The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique. -->

<!-- There is also "implicit naming" which lets you use the link text as the id -->

[This][] is a link.

[this]: http://thisisalink.com/

<!-- But it's not that commonly used. -->

<!-- Images -->
<!-- Images are done the same way as links but with an exclamation point in front! -->

![This is hover-text (alt text) for my image](http://imgur.com/myimage.jpg "An optional title")

<!-- And reference style works as expected -->

![This is the hover-text.][myimage]

[myimage]: relative/urls/cool/image.jpg "if you need a title, it's here"

<!-- Miscellany -->
<!-- Auto-links -->

<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links for emails -->

<foo@bar.com>

<!-- Escaping characters -->

I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.

<!-- Tables -->
<!-- Tables are only available in Github Flavored Markdown and are slightly
cumbersome, but if you really want it: -->

| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah         | blah     | blah          |

<!-- or, for the same results -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop

<!-- The end! -->

```

For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
