---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/:]
filename: asciidoc.md
---

AsciiDoc is a markup language similar to Markdown and it can be used for anything from books to blogs. Created in 2002 by Stuart Rackham the language is simple but it allows for a great amount of customization.

Document Header

Headers are optional and can't contain blank lines. It must be offset from content by at least one blank line.

Title Only

```asciidoc
= Document Title

First sentence of document.
```

Title and Author

```asciidoc
= Document Title
First Last <first.last@learnxinyminutes.com>

Start of this document.
```

Multiple Authors
```asciidoc
= Document Title
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Start of a doc with multiple authors.
```

Revision Line (requires an author line)
```asciidoc
= Doc Title V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

This article about chips is going to be fun.
```

Section Titles

```asciidoc
= Level 0 (may only be used in document's header)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>

====== Level 5 <h6>

======= Level 6 <h7>

```

