---
language: Texinfo
contributors:
    - ["Julien Lepiller", "https://github.com/roptat"]
filename: learntexinfo.texi
---

Texinfo is a documentation format you can use to create various types of
documents from the same source.  Its main usage is to create documentation
manuals and info pages for GNU projects.

Texinfo is a markup language that contains text and *@-commands* that specify
what the generator should do.

## Initial File

A simple example of a simple manual:

```
\input texinfo
@setfilename simple-document.info
@documentencoding UTF-8
@settitle simple-document
@c This is a comment
@c Replace simple-document above (twice) with the actual document title

@c Automake will take care of version.texi
@include version.texi

@copying
Copyright @copyright{} YEAR MY NAME

@c GFDL is common for GNU projects
@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.  A
copy of the license is included in the section entitled ``GNU Free
Documentation License''.
@end quotation
@end copying

@titlepage
@end titlepage

@c Now starts the actual content
@contents

@c The first node must always be Top
@node Top
@c And we give it a title
@top simple-document

This document quickly describes Texinfo features.

@c This is the ToC:
@menu
* Introduction::           A short summary of the chapter

@detailmenu
--- The Detailed Node Listing ---

Introduction

* Formatting::             How to format text nicely
* Links::                  Linking to other resources, pages, or manuals

@end detailmenu
@end menu

@node Introduction
@chapter Introduction

Each node must have the same name as the menu item that was defined in the ToC.

@node Formatting
@section Formatting
@c Add something to the content index, so people can get here when searching
@c for something else
@cindex bold text
@cindex titles

Similar to chapters, sections must have the same name and appear in the same order.

@subsection This is a subsection title
@subsubsection This is a sub-subsection title

Each block of text is a paragraph. You can use multiple lines for the paragraph
like so, only empty lines separate paragraphs.

Common formatting include @emph{emphasis}, @code{inline code}. Specific type of
text can be marked as well: @file{file.txt}, @option{--learn-fast},
@command{ls} or @var{variable}. You can escape the command character like
so: @@, and a newline with a single @@ at the end of the line.

You can add different types of blocks:

@example
Here is an example
@end example

@lisp
'(this is lisp code)
@end lisp

@itemize
@item An element in an unordered list
@item A second element in the same list
@end itemize

@enumerate
@item This list is similar
@item But ordered
@end enumerate

@quotation
A quotation block, by someone famous maybe
@end quotation

@table @asis
@item element title
element description

@item second element title
second element description. Note that the description part can span multiple
paragraphs, contain other blocks etc. This is usually used as a definition
list.

@code{@@asis} wraps the element title, and tells Texinfo to use them as-is.
@end table

@table @code
@item do-x
This item title is now wrapped in a code block, as in @code{@@code{do-x}}
@end table

@c content index can appear at any place in the document, not necessarily after
@c titles.
@cindex function definition
@deffn {Kind of Function} function_name @var{arg1} @var{arg2} @
  @var{arg3} @var{arg4} [@var{optional5}]
This text describes the function. Note how we could use multiple lines for the
function synopsis by escaping the line with a single @@.

This again can contain multiple paragraphs or blocks.
@end deffn

@node Links
@section Links

There are various types of links you can use. A simple link to a URL with
@uref{https://github.com} and optionally with it a title:
@uref{https://github.com, GitHub}. An email address @email{me@@me.me}.
A node in this document, @xref{Introduction}. Always use the exact node name
for that one. @code{xref} will include the text ``see'' before the link. To
insert something different, use @pxref{Introduction} (``See'') or
@xref{Introduction} (nothing is inserted). With an additional argument, you
can change the text of the link, @xref{Introduction, this introduction}.

It is possible to link to external manuals with these commands by adding
more arguments, as in @code{@@xref{Node name,,, manual-name, link text}},
@xref{Overview,,, texinfo, Texinfo's manual} for the complete reference
on Texinfo!

@bye
```

## How to Use It

With `automake`, all you need to do is to give it the path to your manual
in `Makefile.am`:

```
info_TEXINFOS= doc/simple-manual.texi
```

Then, get your info manual with `make doc/simple-manual.info` or in other formats,
e.g. HTML with `make doc/simple-manual.html`.

## Readings

- [Official manual](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/)
