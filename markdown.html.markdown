---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Jacob Ward", "http://github.com/JacobCWard/"]
filename: markdown.md
---


Markdown was created by John Gruber in 2004. It's meant to be an easy to read 
and write syntax which converts easily to HTML (and now many other formats as 
well).

Markdown also varies in implementation from one parser to a next. This
guide will attempt to clarify when features are universal or when they are
specific to a certain parser.

- [HTML Elements](#html-elements)
- [Headings](#headings)
- [Simple Text Styles](#simple-text-styles)
- [Paragraphs](#paragraphs)
- [Lists](#lists)
- [Code blocks](#code-blocks)
- [Horizontal rule](#horizontal-rule)
- [Links](#links)
- [Images](#images)
- [Miscellany](#miscellany)

## HTML Elements
Markdown is a superset of HTML, so any HTML file is valid Markdown.

```markdown
<!--This means we can use HTML elements in Markdown, such as the comment 
element, and they won't be affected by a markdown parser. However, if you 
create an HTML element in your markdown file, you cannot use markdown syntax 
within that element's contents.-->
```

## Headings

You can create HTML elements `<h1>` through `<h6>` easily by prepending the
text you want to be in that element by a number of hashes (#).

```markdown
# This is an <h1>
## This is an <h2>
### This is an <h3>
#### This is an <h4>
##### This is an <h5>
###### This is an <h6>
```
Markdown also provides us with two alternative ways of indicating h1 and h2.

```markdown
This is an h1
=============

This is an h2
-------------
```

## Simple text styles

Text can be easily styled as italic or bold using markdown.

```markdown
*This text is in italics.*
_And so is this text._

**This text is in bold.**
__And so is this text.__

***This text is in both.***
**_As is this!_**
*__And this!__*
```

In GitHub Flavored Markdown, which is used to render markdown files on
GitHub, we also have strikethrough:

```markdown
~~This text is rendered with strikethrough.~~
```
## Paragraphs

Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines.

```markdown
This is a paragraph. I'm typing in a paragraph isn't this fun?

Now I'm in paragraph 2.
I'm still in paragraph 2 too!


I'm in paragraph three!
```

Should you ever want to insert an HTML `<br />` tag, you can end a paragraph
with two or more spaces and then begin a new paragraph.

```markdown
I end with two spaces (highlight me to see them).

There's a <br /> above me!
```

Block quotes are easy and done with the > character.

```markdown
> This is a block quote. You can either
> manually wrap your lines and put a `>` before every line or you can let your lines get really long and wrap on their own.
> It doesn't make a difference so long as they start with a `>`.

> You can also use more than one level
>> of indentation?
> How neat is that?

```

## Lists
Unordered lists can be made using asterisks, pluses, or hyphens.

```markdown
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
```

Ordered lists are done with a number followed by a period.

```markdown
1. Item one
2. Item two
3. Item three
```

You don't even have to label the items correctly and Markdown will still
render the numbers in order, but this may not be a good idea.

```markdown
1. Item one
1. Item two
1. Item three
```
(This renders the same as the above example)

You can also use sublists

```markdown
1. Item one
2. Item two
3. Item three
    * Sub-item
    * Sub-item
4. Item four
```

There are even task lists. This creates HTML checkboxes.

```markdown
Boxes below without the 'x' are unchecked HTML checkboxes.
- [ ] First task to complete.
- [ ] Second task that needs done
This checkbox below will be a checked HTML checkbox.
- [x] This task has been completed
```

## Code blocks

You can indicate a code block (which uses the `<code>` element) by indenting
a line with four spaces or a tab.

```markdown
    This is code
    So is this
```

You can also re-tab (or add an additional four spaces) for indentation
inside your code

```markdown
    my_array.each do |item|
        puts item
    end
```

Inline code can be created using the backtick character `

```markdown
John didn't even know what the `go_to()` function did!
```

In GitHub Flavored Markdown, you can use a special syntax for code

<pre>
<code class="highlight">&#x60;&#x60;&#x60;ruby
def foobar
    puts "Hello world!"
end
&#x60;&#x60;&#x60;</code></pre>

The above text doesn't require indenting, plus GitHub will use syntax
highlighting of the language you specify after the \`\`\`

## Horizontal rule

Horizontal rules (`<hr/>`) are easily added with three or more asterisks or 
hyphens, with or without spaces.

```markdown
***
---
- - -
****************
```

## Links

One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses ()

```markdown
[Click me!](http://test.com/)
```
You can also add a link title using quotes inside the parentheses.

```markdown
[Click me!](http://test.com/ "Link to Test.com")
```
Relative paths work too.

```markdown
[Go to music](/music/).
```

Markdown also supports reference style links.

<pre><code class="highlight">&#x5b;<span class="nv">Click this link</span>][<span class="ss">link1</span>] for more info about it!
&#x5b;<span class="nv">Also check out this link</span>][<span class="ss">foobar</span>] if you want to.

&#x5b;<span class="nv">link1</span>]: <span class="sx">http://test.com/</span> <span class="nn">"Cool!"</span>
&#x5b;<span class="nv">foobar</span>]: <span class="sx">http://foobar.biz/</span> <span class="nn">"Alright!"</span></code></pre>

The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique.

There is also "implicit naming" which lets you use the link text as the id.

<pre><code class="highlight">&#x5b;<span class="nv">This</span>][] is a link.

&#x5b;<span class="nv">this</span>]: <span class="sx">http://thisisalink.com/</span></code></pre>

But it's not that commonly used.

## Images
Images are done the same way as links but with an exclamation point in front!

```markdown
![This is the alt-attribute for my image](http://imgur.com/myimage.jpg "An optional title")
```

And reference style works as expected.

<pre><code class="highlight">!&#x5b;<span class="nv">This is the alt-attribute.</span>][<span class="ss">myimage</span>]

&#x5b;<span class="nv">myimage</span>]: <span class="sx">relative/urls/cool/image.jpg</span> <span class="nn">"if you need a title, it's here"</span></code></pre>
## Miscellany
### Auto-links

```markdown
<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)
```

### Auto-links for emails

```markdown
<foo@bar.com>
```

### Escaping characters

```markdown
I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.
```

### Keyboard keys

In GitHub Flavored Markdown, you can use a `<kbd>` tag to represent keyboard 
keys.

```markdown
Your computer crashed? Try sending a
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```
### Tables

Tables are only available in GitHub Flavored Markdown and are slightly
cumbersome, but if you really want it:

```markdown
| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah         | blah     | blah          |
```
or, for the same results

```markdown
Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop
```

---
For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
