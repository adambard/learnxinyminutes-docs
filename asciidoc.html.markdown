---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
filename: asciidoc.adoc
---

AsciiDoc is a markup language similar to Markdown and it can be used for anything from books to blogs. Created in 2002 by Stuart Rackham the language is simple but it allows for a great amount of customization.

Document Header

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

Paragraphs

```
You don't need anything special for paragraphs.

Add a blank line between paragraphs to separate them.

To create a line blank add a +
and you will receive a line break!
```

Formatting Text

```
_underscore creates italics_
*asterisks for bold*
*_combine for extra fun_*
`use ticks to signify monospace`
`*bolded monospace*`
```

Section Titles

```
= Level 0 (may only be used in document's header)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>
```

Lists

To create a bulleted list use asterisks.

```
* foo
* bar
* baz
```

To create a numbered list use periods.

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

## Further Reading

There are two tools to process AsciiDoc documents:

1. [AsciiDoc](http://asciidoc.org/): original Python implementation available in the main Linux distributions. Stable and currently in maintenance mode.
2. [Asciidoctor](http://asciidoctor.org/): alternative Ruby implementation, usable also from Java and JavaScript. Under active development, it aims to extend the AsciiDoc syntax with new features and output formats.

Following links are related to `Asciidoctor` implementation:

* [Markdown - AsciiDoc syntax comparison](http://asciidoctor.org/docs/user-manual/#comparison-by-example): side-by-side comparison of common Markdown and AsciiDoc elements.
* [Getting started](http://asciidoctor.org/docs/#get-started-with-asciidoctor): installation and quick start guides to render simple documents.
* [Asciidoctor User Manual](http://asciidoctor.org/docs/user-manual/): complete single-document manual with syntax reference, examples, rendering tools, amongst others.
