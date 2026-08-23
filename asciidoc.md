---
name: AsciiDoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
    - ["Mykolas Bamberg", "https://github.com/MykBamberg"]
filename: asciidoc.adoc
---

AsciiDoc is a markup language similar to Markdown and it can be used for anything from books to blogs. Created in 2002 by Stuart Rackham the language is simple but it allows for a great amount of customization.

**Document Header**

Headers are optional and can't contain blank lines. It must be offset from content by at least one blank line.

Title Only

```
= Document Title

First sentence of document.
```

Title and Author

```
= Document Title
First Last <first.last@learnxinyminutes.com>

Start of this document.
```

Multiple Authors

```
= Document Title
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Start of a doc with multiple authors.
```

Revision Line (requires an author line)

```
= Doc Title V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

This article about chips is going to be fun.
```

**Comments**

```
// AsciiDoc offers line comments, beginning with a double slash

////
and block comments enclosed
by four-slash delimiters
which can span multiple lines
////
```

**Built-in Document Attributes**

```
= Document Title
// Document attributes change different behaviors of the document
// These are commonly used to
:imagesdir: ./images
:iconsdir: ./icons
// set resource directories,
:toc:
// enable an automatic table of contents,
:notitle:
// hide the document title,
:sectnums:
// automatically number document sections,
:source-highlighter: pygments
// set a source code highlighter
```

**Paragraphs**

```
You don't need anything special for paragraphs.

Add a blank line between paragraphs to separate them.

To create a line break within a paragraph add a +
to the end of the line and you will receive a line break!
```

**Formatting Text**

```
_underscore creates italics_
*asterisks for bold*
*_combine for extra fun_*
`use ticks to signify monospace`
`*bolded monospace*`
"`double curved quotes`"
'`single curved quotes`'
```

**Section Titles**

```
= Level 0 (may only be used in document's header)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>
```

**Lists**

To create an unordered list use asterisks.

```
* foo
* bar
* baz
```

To create an ordered list use periods.

```
. item 1
. item 2
. item 3
```

You can nest lists by adding extra asterisks or periods up to five times.

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```

**Source Code Blocks**

```
[source,c]
----
#include <stdio.h>

int main(void) {
    printf("AsciiDoc source code blocks are rendered"
           "in a fixed-width font with syntax highlighting\n");
}
----
```

**Images**

```
image::image.png[]

.Figure caption
image::image.png["Alt text",128,128]
```

**Tables**

```
// The cols attribute specifies the number and relative width of each
// column. In this case there are two columns with the first having
// Twice the width of the latter.
[cols="2,1"]
|===
|column 1, row 1
|column 2, row 1

|column 1, row 2
|column 2, row 2
|===
```

**Hyperlinks**

```
https://learnxinyminutes.com/

<https://learnxinyminutes.com/>

https://learnxinyminutes.com/[]

https://learnxinyminutes.com/[Learn X in Y minutes]
```

**Text replacements**

Some character sequences are replaced by appropriate Unicode characters

```
* Copyright: (C)
* Registered: (R)
* Trademark: (TM)
* Em dash: --
* Ellipsis: ...
* Arrows: -> => <- <=
```

**Includes**

Include another file's content in the document

```
include::other_asciidoc_file.adoc[]
```

**Horizontal Rule**

```
'''
```

## Further Reading

**Converters**

1. [Asciidoctor](http://asciidoctor.org/): Actively developed reference implementation in Ruby
2. [AsciiDoc.py](http://asciidoc.org/): Legacy implementation supporting an older syntax

**Helpful Resources**

* [Markdown - AsciiDoc syntax comparison](http://asciidoctor.org/docs/user-manual/#comparison-by-example): side-by-side comparison of common Markdown and AsciiDoc elements.
* [Getting started](http://asciidoctor.org/docs/#get-started-with-asciidoctor): installation and quick start guides to render simple documents.
* [Asciidoctor User Manual](http://asciidoctor.org/docs/user-manual/): complete single-document manual with syntax reference, examples, rendering tools, amongst others.
* [AsciiDoc Syntax Quick Reference](https://docs.asciidoctor.org/asciidoc/latest/syntax-quick-reference/)
