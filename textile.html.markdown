---
language: textile
contributors:
    - ["Keith Miyake", "https://github.com/kaymmm"]
filename: learn-textile.textile
---


Textile is a lightweight markup language that uses a text formatting syntax to
convert plain text into structured HTML markup. The syntax is a shorthand
version of HTML that is designed to be easy to read and write. Textile is used
for writing articles, forum posts, readme documentation, and any other type of
written content published online.

- [Comments](#comments)
- [Paragraphs](#paragraphs)
- [Headings](#headings)
- [Simple Text Styles](#simple-text-styles)
- [Lists](#lists)
- [Code blocks](#code-blocks)
- [Horizontal rule](#horizontal-rule)
- [Links](#links)
- [Images](#images)
- [Footnotes and Endnotes](#footnotes-and-endnotes)
- [Tables](#tables)
- [Character Conversions](#character-conversions)
- [CSS](#css)
- [Spans and Divs](#spans-and-divs)
- [Additional Info](#additional-info)

## Comments

```
###. Comments begin with three (3) '#' signs followed by a full-stop period '.'.
Comments can span multiple lines until a blank line is reached.

###..
Multi-line comments (including blank lines) are indicated by three (3) '#'
signs followed by two (2) full-stop periods '..'.

This line is also part of the above comment.

The comment continues until the next block element is reached

p. This line is not commented

<!-- HTML comments are also…

respected -->
```

## Paragraphs

```
###. Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines. They can also be indicated explicitly with a 'p. '

This is a paragraph. I'm typing in a paragraph isn't this fun?

Now I'm in paragraph 2.
I'm still in paragraph 2 too!
Line breaks without blank spaces are equivalent to a <br /> in XHTML.

p. I'm an explicitly defined paragraph

 Lines starting with a blank space are not wrapped in <p>..</p> tags.

###. Paragraphs (and all block elements) can be aligned using shorthand:

p<. Left aligned paragraph (default).

p>. Right aligned paragraph.

p=. Centered paragraph.

p<>. Justified paragraph.

h3>. Right aligned <h3>


###. Paragraphs can be indented using a parentheses for each em
Indentation utilizes padding-[left/right] css styles.

p(. Left indent 1em.

p((. Left indent 2em.

p))). Right indent 3em.

h2). This is equivalent to <h2 style="padding-right: 1em;">..</h2>


###. Block quotes use the tag 'bq.'

bq. This is a block quote.

bq.:http://someurl.com You can include a citation URL immediately after the '.'

bq.. Multi-line blockquotes containing

blank lines are indicated using two periods

p. Multi-line blockquotes continue until a new block element is reached.

bq. You can add a footer to a blockquote using html:
<footer>citation text</footer>


###. Preformatted text blocks:

pre. This text is preformatted.  <= those two spaces will carry through.

pre.. This is a multi-line preformatted…

…text block that includes blank lines

p. End a multi-line preformatted text block with a new block element.
```

## Headings

You can create HTML elements `<h1>` through `<h6>` easily by prepending the
text you want to be in that element by 'h#.' where # is the level 1-6.
A blank line is required after headings.


```
h1. This is an <h1>

h2. This is an <h2>

h3. This is an <h3>

h4. This is an <h4>

h5. This is an <h5>

h6. This is an <h6>
```


## Simple text styles

```
###. Bold and strong text are indicated using asterisks:

*This is strong text*
**This is bold text**
This is [*B*]old text within a word.

*Strong* and **Bold** usually display the same in browsers
but they use different HTML markup, thus the distinction.

###. Italics and emphasized text are indicated using underscores.

_This is Emphasized text_
__This is Italics text__
This is It[_al_]ics within a word.

_Emphasized_ and __Italics__ text typically display the same in browsers,
but again, they use different HTML markup and thus the distinction.

###. Superscripts and Subscripts use carats and tildes:

Superscripts are 2 ^and^ to none, but subscripts are CO ~2~ L too.
Note the spaces around the superscripts and subscripts.

To avoid the spaces, add square brackets around them:
2[^and^] and CO[~2~]L

###. Insertions and deletions are indicated using -/+ symbols:
This is -deleted- text and this is +inserted+ text.

###. Citations are indicated using double '?':

??This is a cool citation??
```

## Lists

```
###. Unordered lists can be made using asterisks '*' to indicate levels:

* Item
** Sub-Item
* Another item
** Another sub-item
** Yet another sub-item
*** Three levels deep

###. Ordered lists are done with a pound sign '#':

# Item one
# Item two
## Item two-a
## Item two-b
# Item three
** Mixed unordered list within ordered list

###. Ordered lists can start above 1 and can continue after another block:

#5 Item 5
# Item 6

additional paragraph

#_ Item 7 continued from above
# Item 8

###. Definition lists are indicated with a dash and assignment:

- First item := first item definition
- Second := second def.
- Multi-line :=
Multi-line
definition =:
```

## Code blocks

```
Code blocks use the 'bc.' shorthand:

bc. This is code
    So is this

This is outside of the code block

bc.. This is a multi-line code block

Blank lines are included in the multi-line code block

p. End a multi-line code block with any block element

p. Indicate @inline code@ using the '@' symbol.
```

## Horizontal rule

Horizontal rules (`<hr/>`) are easily added with two hyphens

```
--
```

## Links

```
###. Link text is in quotes, followed by a colon and the URL:

"Link text":http://linkurl.com/ plain text.

"Titles go in parentheses at the end of the link text"(mytitle):http://url.com
###. produces <a href... title="mytitle">...</a>

###. Use square brackets when the link text or URL might be ambiguous:
["Textile on Wikipedia":http://en.wikipedia.org/wiki/Textile_(markup_language)]

###. Named links are useful if the same URL is referenced multiple times.
Multiple "references":txstyle to the "txstyle":txstyle website.

[txstyle]https://txstyle.org/
```

## Images

```
###. Images can be included by surrounding its URL with exclamation marks (!)
Alt text is included in parenthesis after the URL, and they can be linked too:

!http://imageurl.com!

!http://imageurl.com(image alt-text)!

!http://imageurl.com(alt-text)!:http://image-link-url.com
```

## Footnotes and Endnotes

```
A footnote is indicated with the reference id in square brackets.[1]

fn1. Footnote text with a "link":http://link.com.

A footnote without a link.[2!]

fn2. The corresponding unlinked footnote.

A footnote with a backlink from the footnote back to the text.[3]

fn3^. This footnote links back to the in-text citation.


Endnotes are automatically numbered[#first] and are indicated using square[#second]
brackets and a key value[#first]. They can also be unlinked[#unlinkednote!]

###. Give the endnotes text:

note#first. This is the first endnote text.

note#second. This is the second text.

note#unlinkednote. This one isn't linked from the text.

### Use the notelist block to place the list of notes in the text:
This list will start with #1. Can also use alpha or Greeks.
notelist:1. ###. start at 1 (then 2, 3, 4...)
notelist:c. ###. start at c (then d, e, f...)
notelist:α. ###. start at α (then β, γ, δ...)

###. The notelist syntax is as follows:

notelist.    Notes with backlinks to every citation made to them.
notelist+.   Notes with backlinks to every citation made to them,
               followed by the unreferenced notes.
notelist^.   Notes with one backlink to the first citation made to each note.
notelist^+.  Notes with one backlink to the first citation made to each note,
               followed by unreferenced notes.
notelist!.   Notes with no backlinks to the citations.
notelist!+.  Notes with no backlinks to the citations, followed by 
               unreferenced notes.
```

## Tables


```
###. Tables are simple to define using the pipe '|' symbol

| A | simple | table | row |
| And | another | table | row |
| With an | | empty | cell |

###. Headers are preceded by '|_.'
|_. First Header |_. Second Header |
| Content Cell | Content Cell |

###. The <thead> tag is added when |^. above and |-. below the heading are used.

|^.
|_. First Header |_. Second Header |
|-.
| Content Cell | Content Cell |
| Content Cell | Content Cell |

###. The <tfoot> tag is added when |~. above and |-. below the footer are used.

|~.
|\2=. A footer, centered & across two columns |
|-.
| Content Cell | Content Cell |
| Content Cell | Content Cell |

###. Attributes are be applied either to individual cells, rows, or to
the entire table. Cell attributes are placed within each cell:

|a|{color:red}. styled|cell|

###. Row attributes are placed at the beginning of a row,
followed by a dot and a space:

(rowclass). |a|classy|row|

###. Table attributes are specified by placing the special 'table.' block
modifier immediately before the table:

table(tableclass).
|a|classy|table|
|a|classy|table|

###. Spanning rows and columns:
A backslash \ is used for a column span:

|\2. spans two cols |
| col 1 | col 2 |

###. A forward slash / is used for a row span:

|/3. spans 3 rows | row a |
| row b |
| row c |

###. Vertical alignments within a table cell:

|^. top alignment|
|-. middle alignment|
|~. bottom alignment|

###. Horizontal alignments within a table cell

|:\1. |400|
|=. center alignment |
| no alignment |
|>. right alignment |
```

or, for the same results

```
Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop
```


## Character Conversions

### Registered, Trademark, Copyright Symbols

```
RegisteredTrademark(r), Trademark(tm), Copyright (c)
```

### Acronyms

```
###. Acronym definitions can be provided in parentheses:

EPA(Environmental Protection Agency) and CDC(Center for Disease Control)
```

### Angle Brackets and Ampersand

```
### Angled brackets < and > and ampersands & are automatically escaped:
< => &lt;
> => &gt;
& => &amp;
```

### Ellipses

```
p. Three consecutive periods are translated into ellipses...automatically
```

### Em and En dashes

```
###. En dashes (short) is a hyphen surrounded by spaces:

This line uses an en dash to separate Oct - Nov 2018.

###. Em dashes (long) are two hyphens with or without spaces:

This is an em dash--used to separate clauses.
But we can also use it with spaces -- which is a less-used convention.
That last hyphen between 'less' and 'used' is not converted between words.
```

## Fractions and other Math Symbols

```
One quarter: (1/4) => ¼
One half: (1/2) => ½
Three quarters: (3/4) => ¾
Degree: (o) => °
Plus/minus: (+/-) => ±
```

### Multiplication/Dimension

```
p. Numbers separated by the letter 'x' translate to the multiplication
or dimension symbol '×':
3 x 5 => 3 × 5
```

### Quotes and Apostrophes

```
###. Straight quotes and apostrophes are automatically converted to
their curly equivalents:

"these", 'these', and this'n are converted to their HTML entity equivalents.
Leave them straight using '==' around the text: =="straight quotes"==.
```

## CSS

```
p{color:blue}. CSS Styles are enclosed in curly braces '{}'
p(my-class). Classes are enclosed in parenthesis
p(#my-id). IDs are enclosed in parentheses and prefaced with a pound '#'.
```

## Spans and Divs

```
%spans% are enclosed in percent symbols
div. Divs are indicated by the 'div.' shorthand
```

---

## For More Info

* TxStyle Textile Documentation: [https://txstyle.org/](https://txstyle.org/)
* promptworks Textile Reference Manual: [https://www.promptworks.com/textile](https://www.promptworks.com/textile)
* Redmine Textile Formatting: [http://www.redmine.org/projects/redmine/wiki/RedmineTextFormattingTextile](http://www.redmine.org/projects/redmine/wiki/RedmineTextFormattingTextile)
