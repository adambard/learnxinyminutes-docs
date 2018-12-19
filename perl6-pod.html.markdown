---
language: Pod
name: Perl 6 Pod
contributors:
    - ["Luis F. Uceta", "https://uzluisf.gitlab.io/"]
filename: learnpod.pod6
---

Perl 6 Pod is an easy-to-use and purely descriptive mark-up language, 
with no presentational components. Besides its use for documenting
Perl 6 programs and modules, Pod can be utilized to write language
documentation, blogs, and other types of document composition as well.

Pod documents can be easily converted to HTML and many other formats
(e.g., Markdown, Latex, plain text, etc.) by using the corresponding
variant of the `Pod::To` modules (e.g. `<Pod::To::HTML>` for HTML conversion).

Note: This document can be also be found as a Pod document
[here](https://git.io/fpxKI).

- [General Info](#general-info)
- [Pod Basics](#pod-basics)
	- [Basic Text Formatting](#basic-text-formatting)
	- [Headings](#headings)
	- [Ordinary Paragraphs](#ordinary-paragraphs)
	- [Lists](#lists)
	- [Code Blocks](#code-blocks)
	- [Comments](#comments)
	- [Links](#links)
	- [Tables](#tables)
- [Block Structures](#block-structures)
	- [Abbreviated Blocks](#abbreviated-blocks)
	- [Delimited Blocks](#delimited-blocks)
	- [Paragraph Blocks](#paragraph-blocks)
- [Configuration Data](#configuration-data)
	- [Standard Configuration Options](#standard-configuration-options)
	- [Block Pre-configuration](#block-pre-configuration)
- [Semantic Blocks](#semantic-blocks)
- [Miscellaneous](#miscellaneous)
	- [Notes](#notes)
	- [Keyboard Input](#keyboard-input)
	- [Terminal Output](#terminal-output)
	- [Unicode](#unicode)
- [Rendering Pod](#rendering-pod)
- [Accessing Pod](#accessing-pod)

## General Info

Every Pod document has to begin with `=begin pod` and end with `=end pod`.
Everything between these two delimiters will be processed and used to
generate documentation.

```md
=begin pod

A very simple Perl 6 Pod document. All the other directives go here!

=end pod
```

Pod documents usually coexist with Perl 6 code. If by themselves, Pod files
often have the `.pod6` suffix. Moving forward, it's assumed that the
constructs are between the `=begin pod ... =end pod` directives.

## Pod Basics

### Basic Text Formatting

Text can be easily styled as bold, italic, underlined or verbatim (for code
formatting) using the formatting code: `B<>`, `I<>`, `U<>` and `C<>`. 

```md
B<This text is in Bold.>

I<This text is in Italics.>

U<This text is Underlined.>

The function C<sub sum { $^x + $^y}> is treated as verbatim.
```

There are more formatting codes (e.g., `L<>`, `T<>`, etc.) but they'll be
discussed later throughout the document. You'll recognize them because they're
just a single capital letter followed immediately by a set of single or double
angle brackets. The Unicode variant («») can also be used.

### Headings 

Headings are created by using the `=headN` directive where `N` is the
heading level.

```md
=head1 This is level 1
=head2 This is level 2
=head3 This is level 3
=head4 This is level 4
=head5 This is level 5
=head6 This is level 6
```

### Ordinary Paragraphs

Ordinary paragraphs consist of one or more adjacent lines of text, each of
which starts with a non-whitespace character. Any paragraph is terminated
by the first blank line or block directive.

```md
=head1 First level heading block

=head2 Paragraph 1

This is an ordinary paragraph. Its text will be squeezed and
short lines filled. It is terminated by the first blank line.

=head2 Paragraph 2

This is another ordinary paragraph albeit shorter.
```

Alternatively, the `=para` directive can be used to explicitly mark adjacent
lines of text as a paragraph.

```md
=head1 First level heading block

=head2 Paragraph 1

=para
This is an ordinary paragraph. Its text will be squeezed and
short lines filled. It is terminated by the first blank line.

=head2 Paragraph 2

=para
This is another ordinary paragraph albeit shorter.
```

### Lists

Unordered lists can be made using the `=item` directive.

```md
=item Item
=item Item
=item Another item
```

Sublists are achieved with items at each level specified using the `=item1`,
`=item2`, `=item3`, etc. directives.

```md
=item1 Item one
=item1 Item two
=item1 Item three
    =item2 Sub-item
    =item2 Sub-item
=item1 Item four
```

Definition lists that define terms or commands use the `=defn`. This is
equivalent to the `<dl>` element in HTML.

```md
=defn Beast of Bodmin
A large feline inhabiting Bodmin Moor.

=defn Morgawr
A sea serpent.

=defn Owlman
A giant owl-like creature.
```

### Code Blocks

A code block is created (which uses the `<code>` element) by starting each
line with one or more whitespace characters.

```md
    #`( this is comment )
    my $sum = -> $x, $y { $x + $y }
    say $sum(12, 5);
```

As shown in the [Basic Text Formatting](#basic-text-formatting) section,
inline code can be created using the `C<>` code.

```md
In Perl 6, there are several functions/methods to output text. Some of them
are C<print>, C<put> and C<say>.
```

### Comments

Although Pod blocks are ignored by the Perl 6 compiler, everything indentified
as a Pod block will be read and interpreted by Pod renderers. In order to
prevent Pod blocks from being rendered by any renderer, use the `=comment`
directive.

```md
=comment Add more here about the algorithm.

=comment Pod comments are great for documenting the documentation.
```

To create inline comments, use the `Z<>` code.

```md
Pod is awesome Z<Of course it is!>. And Perl 6 too!
```

Given that the Perl interpreter never executes embedded Pod blocks,
comment blocks can also be used as an alternative form of nestable block
comments in Perl 6.

### Links

Creating links in Pod is quite easy and is done by enclosing them in
a `L<>` code. The general format is `L<Label|Url>` with `Label`
being optional. 

```md
Perl 6 homepage is L<https://perl6.org>.
L<Click me!|http://link.org/>.
```

Relative paths work too.

```md
L<Go to music|/music/>.
```

Linking to a section in the same document works as well.

```md
L<Link to Headings|#Headings>
```

### Tables

The Pod 6 specifications are not completely handled properly yet and this
includes the handling of table. For simplicity's sake, only one way of
constructing tables is shown. To learn about good practices and see examples
of both good and bad tables, please visit
https://docs.perl6.org/language/tables.

```md
=begin table
Option      | Description     
============|================  
data        | path to data files.
engine      | engine to be used for processing templates.
ext         | extension to be used for dest files.
=end table
```

## Block Structures

As mentioned earlier, Pod documents are specified using directives, which are
used to delimit blocks of textual content and declare optional [configuration
information](#configuration-data). Every directive starts with an equals
sign (`=`) in the first column.

The content of a document is specified within one or more blocks. Every Pod
block may be declared in any of three equivalent forms: delimited style,
paragraph style, or abbreviated style.

Up to this point, we only used the abbreviated style for the block
types (e.g., `=head1`, `=para`, `=comment`, `=item`, etc).

### Abbreviated Blocks

Abbreviated blocks are introduced by an `=` sign in the first column, which
is followed immediately by the `typename` of the block and then the content.
The rest of the line is treated as block data, rather than as configuration.
The content terminates at the next Pod directive or the first blank line
(which is not part of the block data). The general syntax is

```md
=BLOCK_TYPE  BLOCK_DATA
```
For example:

```md
=head1 Top level heading
```

### Delimited Blocks

Delimited blocks are bounded by `=begin` and `=end` markers, both of which are
followed by a valid Perl 6 identifier, which is the `typename` of the block.
The general syntax is 

```md
=begin BLOCK_TYPE
BLOCK_DATA
=end BLOCK_TYPE
```

For example:

```md
=begin head1
Top level heading
=end head1
```

This type of blocks is useful for creating headings, list items, code blocks,
etc. with multiple paragraphs. For example, 

* a multiline item of a list

```md
=begin item
This is a paragraph in list item.

This is another paragraph in the same list item.
=end item
```

* a code block

```md
=begin code
#`(A recursive implementation of
a power function using multi subs.
)

multi pow( Real $base, 0 ) { 1 }

multi pow( Real $base, Int $exp where * >= 0) {
	$base * pow($base, $exp - 1)
}

multi pow( Real $base ) {
     pow($base, 2)
}

say pow(3, 0);   #=> 1
say pow(4.2, 2); #=> 17.64
say pow(6);      #=> 36
=end code
```

### Paragraph Blocks

Paragraph blocks are introduced by a `=for` marker and terminated by
the next Pod directive or the first blank line (which is not considered to
be part of the block's contents). The `=for` marker is followed by the
`typename` of the block. The general syntax is 

```md
=for BLOCK_TYPE
BLOCK DATA
```

For example:

```md
=for head1
Top level heading
```

## Configuration Data
 
Except for abbreviated blocks, both delimited blocks and paragraph
blocks can be supplied with configuration information about their
contents right after the `typename` of the block. Thus the following 
are more general syntaxes for these blocks:

* Delimited blocks

```md
=begin BLOCK_TYPE OPTIONAL_CONFIG_INFO
=                 ADDITIONAL_CONFIG_INFO
BLOCK_DATA
=end BLOCK_TYPE
```

* Paragraph blocks

```md
=for BLOCK_TYPE OPTIONAL_CONFIG_INFO
=               ADDITIONAL_CONFIG_INFO
BLOCK DATA
```

The configuration information is provided in a format akin to the
["colon pair"](https://docs.perl6.org/language/glossary#index-entry-Colon_Pair)
syntax in Perl 6. The following table is a simplified version of the 
different ways in which configuration info can supplied. Please go to 
https://docs.perl6.org/language/pod#Configuration_information for a more
thorough treatment of the subject.

| Value     | Specify with...             | Example                        |
| :-------- | :------                     | :------                        |
| List      | :key($elem1, $elem2, ...)   | :tags('Pod', 'Perl6')          |
| Hash      | :key{$key1 => $value1, ...} | :feeds{url => 'perl6.org'}     |
| Boolean   | :key/:key(True)             | :skip-test(True)               |
| Boolean   | :!key/:key(False)           | :!skip-test                    |
| String    | :key('string')              | :nonexec-reason('SyntaxError') |
| Int       | :key(2)                     | :post-number(6)                |


### Standard Configuration Options

Pod provides a small number of standard configuration options that can
be applied uniformly to built-in block types. Some of them are:

* `:numbered`

This option specifies that the block is to be numbered. The most common
use of this option is to create numbered headings and ordered lists, but it
can be applied to any block.

For example:

```md
=for head1 :numbered
The Problem
=for head1 :numbered
The Solution
=for head2 :numbered
Analysis
=for head3 :numbered
Overview
```

* `:allow`

The value of the `:allow` option must be a list of the (single-letter) names
of one or more formatting codes. Those codes will then remain active inside
the code block. The option is most often used on `=code` blocks to allow
mark-up within those otherwise verbatim blocks, though it can be used in any
block that contains verbatim text.

Given the following snippet:

```md
=begin code :allow('B', 'I')
B<sub> greet( $name ) {
    B<say> "Hello, $nameI<!>";
}
=end code
```

we get the following output:

<pre><strong>sub</strong> greet( $name ) {
    <strong>say</strong> &quot;Hello, $name<em>!</em>&quot;;
}
</pre>

This is highly dependent on the format output. For example, while this works
when Pod is converted to HTML, it might not work with Markdown.

### Block Pre-configuration

The `=config` directive allows you to prespecify standard configuration
information that is applied to every block of a particular type.
The general syntax for configuration directives is:

```md
=config BLOCK_TYPE  CONFIG OPTIONS
=                  ADDITIONAL_CONFIG_INFO
```

For example, to specify that every heading level 1 be numbered, bold
and underlined, you preconfigure the `=head1` as follows:

```md
=config head1 :formatted('B', 'U') :numbered
```

## Semantic Blocks

All uppercase block typenames are reserved for specifying standard
documentation, publishing, source components, or meta-information.
Some of them are:

```md
=NAME
=AUTHOR
=VERSION
=CREATED
=SYNOPSIS
=DESCRIPTION
=USAGE
```

Most of these blocks would typically be used in their full
delimited forms. For example,

```md
=NAME B<Doc::Magic>

=begin DESCRIPTION
This module helps you generate documentation automagically.
Not source code needed! Most of it is outsourced from a black hole.
=end DESCRIPTION

=begin SYNOPSIS
	use Doc::Magic;

 	my Doc::Magic $doc .= new();

    my $result = $doc.create-documentation($fh);
=end SYNOPSIS

=AUTHOR Authorius Docus
=VERSION 42
```

## Miscellaneous

### Notes

Notes are rendered as footnotes and created by enclosing a note in a
`N<>` code.

```md
In addition, the language is also multi-paradigmatic N<According to Wikipedia,
this means that it supports procedural, object-oriented, and functional
programming.>
```

### Keyboard Input

To flag text as keyboard input enclose it in a `K<>` code.

```md
Enter your name K<John Doe>
```

### Terminal Output

To flag text as terminal output enclose it in `T<>` code.

```md
Hello, T<John Doe>
```

### Unicode

To include Unicode code points or HTML5 character references in
a Pod document, enclose them in a `E<>` code.

```md
Perl 6 makes considerable use of the E<171> and E<187> characters.
Perl 6 makes considerable use of the E<laquo> and E<raquo> characters.
```

this is rendered as:

Perl 6 makes considerable use of the « and » characters.
Perl 6 makes considerable use of the « and » characters.

## Rendering Pod

To generate any output (i.e., Markdown, HTML, Text, etc.), you need to 
have the Perl 6 compiler installed. In addition, you must install 
a module (e.g., `Pod::To::Markdown`, `Pod::To::HTML`, `Pod::To::Text`, etc.)
that generates your desired output from Pod.

For instructions about installing Perl 6, 
[look here](https://perl6.org/downloads/).

Run the following command to generate a certain output 

```md
perl6 --doc=TARGET input.pod6 > output.html
```

with `TARGET` being `Markdown`, `HTML`, `Text`, etc. Thus to generate
Markdown from Pod, run this:

```md
perl6 --doc=Markdown input.pod6 > output.html
```

## Accessing Pod

In order to access Pod documentation from within a Perl 6 program, 
it is required to use the special `=` twigil (e.g., `$=pod`, `$=SYNOPSIS`,etc).

The `=` twigil provides the introspection over the Pod structure, 
providing a `Pod::Block` tree root from which it is possible to access
the whole structure of the Pod document.

If we place the following piece of Perl 6 code and the Pod documentation
in the section [Semantic blocks](#semantic-blocks) in the same file:

```md
my %used-directives;
for $=pod -> $pod-item {
    for $pod-item.contents -> $pod-block {
        next unless $pod-block ~~ Pod::Block::Named;
        %used-directives{$pod-block.name} = True;
    }
}

say %used-directives.keys.join("\n");
```

we get the following output:

```md
SYNOPSIS
NAME
VERSION
AUTHOR
DESCRIPTION
```

## Additional Information 

* https://docs.perl6.org/language/pod
* https://docs.perl6.org/language/tables
* https://design.perl6.org/S26.html

