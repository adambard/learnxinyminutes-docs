---
langauge: roff
contributors:
    - ["Jeff Parent", "https://github.com/jecxjo"]
filename: learnroff.ms
lang: en-us
---

```groff
.\" All comments start with a \"
.\" There are no multi-line comments. Comments can be inserted at the end of
.\" any line.

.\" roff is NOT a "What You See Is What You Get" word processing software like
.\" MS Word, or OpenOffice Writer.

.\" Every roff command starts with a new line and a period (.) or a single
.\" quote ('). Any line that doesn't start with a period or quote is considered
.\" normal text.

.\" roff documents start by deciding which macro package to use. There are
.\" many, all designed for different purposes and different types of documents.
.\" The most common one is the man macro package which most manpages are
.\" created from.

.\" The ms package is designed for creating for general purpose documents like
.\" papers, letters and books. It is the default package for groff. The me
.\" package is designed for creating technical documents. See `man 1 groff_me`
.\" or `man 1 groff_ms` for more details.

.\" To build this document into a pdf use the following command:
.\" groff -p -e -t -ms -Tpdf learnroff.ms > learnroff.pdf

.TL \" Sets the title of the document
LearnXinYMinutes - roff Edition

.AU \" Sets the author
Jeff Parent

.AI \" Sets the author's institution
ACME Corp

.AB \" Begins the abstract
roff is a typesetting system used to create documents using plain text and
outputting PDF, HTML, DVI and other formats with a simple Unix tool. It
supports basic typesettings as well as advanced features like tables, graphs
and equations.
.AE \" Ends the abstract

.tp  \" Triggers the title page

.NH 1 \" Triggers a heading, level 1 (e.g 1. Introduction)
Introduction
.LP \" Starts a new paragraph
Welcome to the wonderful typesetting world of roff.

.NH 1
A section
.LP
This is a section. Sections are used to divide a document into smaller parts.

.NH 2 \" Triggers a heading, level 2 (e.g. 1.1 Subsection)
A subsection
.LP
This is a subsection. There can be many subsections with varying levels of
depth.

.NH 2
Another subsection
.LP
More text can go here.

.NH S 2 9 2 
Some sections may need an explicit section number
.LP
However most cases you can rely on the automatic numbering. There is also
support for number/letter combinations.

.NH 1
Some notes about text
.LP
roff does a good job handling text. It will automatically wrap text if the line
gets too long and will also handle text
that has newlines
in it. If you
.br
needs
.br
to
.br
break
.br
you add a .br command to the source code.

.LP
Separate paragraphs by the .LP command. This will add a blank line and start
the text on the left most margin.

.PP
Using the .PP command a paragraph is started by indenting the first line. All
additional lines will be at the left most margin.

.QP
The .QP command is used to start a paragraph where all lines are indented on
both the left and right sides of the document. This is a good way to quote text
or specify some sort of significance to the text.

.XP
The opposite of .PP is the .XP macro. As you can see this paragraph indents all
lines except for the first one. This is knowing as an \*Qextended\*Q paragraph
or a hanging indent.

.sp 4 \" Adds 4 lines of space
Sometimes you need to add more space. That is what the .sp command is for.

.NH 1
Typography
.LP

roff supports many of the common typographic features all typesetting software
does. You can
.B bold
a word, or you can
.I italicize
a word. You can also
.BI "bold italicize"
a phrase or use
.CW "Common Width"
for showing commands. There is support for Super and Sub scripting as well and
it looks like this\*[{]Super Script\*[}] and this\*[<]Sub Script\*[>].
Unfortunately some typesetting is done with commands and others through escaped
characters.
.LP
Each set of macros has their own ways of handling typesetting features, roff
has a lot of low level commands so check out the documentation for more info.
The convention is that lowercase commands are low level built in operations
while uppercase commands are from the macro package.

.NH 1
Lists
.LP
Lists are easy to implement in roff. There are two types of lists, bulleted and
numeric.

.IP \[bu] 3 \" the \[bu] is a bullet character
Item 1
.IP \[bu] 3
Item 2
.RS \" Starts a new list inside a list
.IP \[bu] 3
Sub Item 1
.RE
.IP \[bu] 3
Item 3
.LP

.\" We can auto numerate the list or manually set the values
.nr step 0 1 \" Setting the step counter to 0, incrementing by 1

.IP \n+[step] 3 \" Increment the step counter by 1 and set the indentation to 3
Apple.
.IP \n+[step] 3
Bananna
.IP \n+[step] 3
Cherry
.IP "Words Not Numbers" 3
This is a list item without a number
.LP \" End of list, starting new paragraph

.NH 1
Math
.LP
roff was created for typesetting memos and technical documents. It was created
at Bell Labs as a word processor for all the technical stuff they were doing.
As such, we need to be able to add special symbols to our paper!
.LP
Math is often a big part of writing technical documents. roff has support for
this both as inline characters as well as equation figures. Unlike other
typesetting software, roff relies on independent preprocessors to handle things
like Math Equations. The .EQ macro starts an equation section and the .EN macro
ends it. See
.CW "man 1 eqn"
for details on how to use the eqn preprocessor.
.EQ
a sup 2 + b sup 2 = c sup 2
.EN
.LP
Greek letter can be written inline such as \(*x, \(*b, \(*g and \(*s. Within an
equation you can use these symbols as well.
.EQ
Area^of^a^Circle = pi r sup 2
.EN
Operators are an essential part of a mathematical document. Let's write an
equation to see how that is done:
.EQ
cos { 2 theta } = cos sup 2 { theta } - sin sup 2 { theta }
.EN
Fractions (Numerator-Denominators) can be written in this form:
.EQ
10 over 7
.EN
Summation and Integrals are written with the sum and int commands:
.EQ
sum from {i = 0} to 5 {f(i)}
.EN
.EQ
int from 0 to inf { e sup { -x } d x }
.EN

.NH 1
Tables
.LP 

Tables are created using the 
.CW tbl
preprocessor. To include a table use the .TS and .TE macros.

.TS
center box;
c | c | c
l | n | r.
Name	Age	Location
_
John Doe	30	New York
Jane Smith	25	Los Angeles
Sam Brown	40	Chicago
.TE

.NH 1
Diagrams
.LP

Diagrams are also supported, using the
.CW pic
preprocessor. To include a block diagram use the .PS and .PE macros.

.PS
box "Starting";
arrow;
circle "B1";
arrow;
ellipse "End";
.PE

.LP
There is also the ability to generate a graph.

.PS
pi = atan2(0, -1);
for i = 0 to 2 * pi by 0.1 do {
  "-" at (i/2, 0);
  "." at (i/2, sin(i)/2);
  ":" at (i/2, cos(i)/2);
}
.PE

.NH 1
Other Preprocessors
.LP
There are a few other preprocessors available to roff.
.CW chem
is used to create chemical structure diagrams.
.CW grn
is used for creating pictures.
.CW refer
is used for creating bibliographies.
```

## More on roff

* [The GNU Troff Manual](https://www.gnu.org/software/groff/manual/groff.html)
* [A Guide to Typesetting Mathematics using GNU eqn](https://lists.gnu.org/archive/html/groff/2013-10/pdfTyBN2VWR1c.pdf)
* [Making Pictures with GNU PIC](https://pikchr.org/home/uv/gpic.pdf)
* [man groff_me](https://linux.die.net/man/7/groff_me)
* [man groff_ms](https://linux.die.net/man/7/groff_ms)
