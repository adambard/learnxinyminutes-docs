---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Jacob Ward", "http://github.com/JacobCWard/"]
    - ["Tomáš Hartman", "https://github.com/tomas-hartman"]
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
- [Simple text styles](#simple-text-styles)
- [Paragraphs](#paragraphs)
- [Lists](#lists)
- [Code blocks](#code-blocks)
- [Horizontal rule](#horizontal-rule)
- [Links](#links)
  - [Table of contents](#table-of-contents)
- [Images](#images)
- [Miscellany](#miscellany)
  - [Auto-links](#auto-links)
  - [Auto-links for emails](#auto-links-for-emails)
  - [Escaping characters](#escaping-characters)
  - [Keyboard keys](#keyboard-keys)
  - [Tables](#tables)
- [Markdownlint](#markdownlint)
- [Further reading](#further-reading)

## HTML Elements

Markdown is a superset of HTML, so any HTML file is valid Markdown.

```md
<!--This means we can use HTML elements in Markdown, such as the comment
element, and they won't be affected by a markdown parser. However, if you
create an HTML element in your markdown file, you cannot use markdown syntax
within that element's contents.-->
```

## Headings

You can create HTML elements `<h1>` through `<h6>` easily by prepending the
text you want to be in that element by a number of hashes (#).

```md
# This is an <h1>
## This is an <h2>
### This is an <h3>
#### This is an <h4>
##### This is an <h5>
###### This is an <h6>
```

Markdown also provides us with two alternative ways of indicating h1 and h2.

```md
This is an h1
=============

This is an h2
-------------
```

## Simple text styles

Text can be easily styled as italic or bold using markdown.

```md
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

```md
~~This text is rendered with strikethrough.~~
```

## Paragraphs

Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines.

```md
This is a paragraph. I'm typing in a paragraph isn't this fun?

Now I'm in paragraph 2.
I'm still in paragraph 2 too!


I'm in paragraph three!
```

Should you ever want to insert an HTML `<br />` tag, you can end a paragraph
with two or more spaces and then begin a new paragraph.

```md
I end with two spaces (highlight me to see them).

There's a <br /> above me!
```

Block quotes are easy and done with the > character.

```md
> This is a block quote. You can either
> manually wrap your lines and put a `>` before every line or you can let your lines get really long and wrap on their own.
> It doesn't make a difference so long as they start with a `>`.

> You can also use more than one level
>> of indentation?
> How neat is that?

```

## Lists

Unordered lists can be made using asterisks, pluses, or hyphens.

```md
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

```md
1. Item one
2. Item two
3. Item three
```

You don't even have to label the items correctly and Markdown will still
render the numbers in order, but this may not be a good idea.

```md
1. Item one
1. Item two
1. Item three
```

(This renders the same as the example above.)

You can also use sublists.

```md
1. Item one
2. Item two
3. Item three
    * Sub-item
    * Sub-item
4. Item four
```

There are even task lists. This creates HTML checkboxes.

```md
Boxes below without the 'x' are unchecked HTML checkboxes.
- [ ] First task to complete.
- [ ] Second task that needs done
This checkbox below will be a checked HTML checkbox.
- [x] This task has been completed
```

## Code blocks

You can indicate a code block (which uses the `<code>` element) by indenting
a line with four spaces or a tab.

```md
    This is code
    So is this
```

You can also re-tab (or add an additional four spaces) for indentation
inside your code.

```md
    my_array.each do |item|
      puts item
    end
```

Inline code can be created using the backtick character `` ` ``.

```md
John didn't even know what the `go_to()` function did!
```

In GitHub Flavored Markdown, you can use a special syntax for code.

````md
```ruby
def foobar
  puts "Hello world!"
end
```
````

The above text doesn't require indenting, plus GitHub will use syntax
highlighting of the language you specify after the opening <code>```</code>.

## Horizontal rule

Horizontal rules (`<hr/>`) are easily added with three or more asterisks or
hyphens, with or without spaces.

```md
***
---
- - -
****************
```

## Links

One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses ()

```md
[Click me!](http://test.com/)
```

You can also add a link title using quotes inside the parentheses.

```md
[Click me!](http://test.com/ "Link to Test.com")
```

Relative paths work too.

```md
[Go to music](/music/).
```

Markdown also supports reference style links.

```md
[Click this link][link1] for more info about it!
[Also check out this link][foobar] if you want to.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"
```

The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique.

There is also "implicit naming" which lets you use the link text as the id.

```md
[This][] is a link.

[This]: http://thisisalink.com/
```

But it's not that commonly used.

### Table of contents

Some Markdown flavors even make use of the combination of lists, links and
headings in order to create tables of contents. In this case, heading titles in
lowercase are prepended with hash (`#`) and are used as link ids. Should the
heading have multiple words, they will be connected with a hyphen (`-`), that
also replaces some special characters. (Some other special characters are
omitted though.)

```md
- [Heading](#heading)
- [Another heading](#another-heading)
- [Chapter](#chapter)
  - [Subchapter <h3 />](#subchapter-h3-)
```

Nonetheless, this is a feature that might not be working in all Markdown
implementations the same way.

## Images

Images are done the same way as links but with an exclamation point in front!

```md
![This is the alt-attribute for my image](http://imgur.com/myimage.jpg "An optional title")
```

And reference style works as expected.

```md
![This is the alt-attribute.][myimage]

[myimage]: relative/urls/cool/image.jpg "if you need a title, it's here"
```

## Miscellany

### Auto-links

```md
<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)
```

### Auto-links for emails

```md
<foo@bar.com>
```

### Escaping characters

```md
I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.
```

### Keyboard keys

In GitHub Flavored Markdown, you can use a `<kbd>` tag to represent keyboard
keys.

```md
Your computer crashed? Try sending a
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```

### Tables

Tables are only available in GitHub Flavored Markdown and are slightly
cumbersome, but if you really want it:

```md
| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah         | blah     | blah          |
```

or, for the same results

```md
Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop
```

## Markdownlint

In order to simplify work with Markdown and to unify its coding style,
`Markdownlint` has been created. Available as a
[separate tool](https://github.com/markdownlint/markdownlint)
as well as a plugin for some IDEs, it can be used to ensure validity and
readability of Markdown.

---

## Further reading

For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).

If you want to learn more on some major Markdown flavors' features, see:

- [GitHub flavored Markdown](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
- [GitLab flavored Markdown](https://docs.gitlab.com/ee/user/markdown.html)
