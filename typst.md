---
name: Typst
filename: learntypst.typ
contributors:
    - ["uzanto", "https://uzanto.me"]
---

[Typst](https://typst.app/) is a markup-based typesetting system: you write
plain text and a compiler turns it into a PDF. Everyday formatting has
lightweight, Markdown-like syntax; everything else is a function call in a
small built-in scripting language. Compiling takes milliseconds, so the
preview updates while you type.

Use it in the [web app](https://typst.app/) or locally by installing the
[open-source compiler](https://github.com/typst/typst). The
[Tinymist](https://github.com/Myriad-Dreamin/tinymist) language server adds
completion and a live preview to most editors (such as VSCode, neovim, emacs).

````typst
// A line comment.
/* A block comment. */

// Compile with `typst compile learntypst.typ`, which writes learntypst.pdf,
// or keep `typst watch learntypst.typ` running to recompile on every save.

// There is no preamble: whatever you type is the document.
// Typst has three modes, and you can switch between them anywhere:
//   markup  the default. In code, [square brackets] switch back to it.
//   code    after a `#`, for one expression or a whole {block}.
//   math    between $dollar signs$.

////////////////////////////////////////////////////////////////////////////
// 1. Markup
////////////////////////////////////////////////////////////////////////////

= Markup
== A subheading
// The number of `=` signs is the heading level.

Lines that follow each other
form a single paragraph.

A blank line starts a new one. A backslash \
breaks the line.

*Strong*, _emphasis_, `raw text`, "smart quotes", and a URL such as
https://typst.app/ turns into a link.

// Shorthands: ~ no-break space, -- en dash, --- em dash, ... ellipsis.
See Fig.~1 on pages 3--5 --- or don't...

// A backslash escapes characters that would otherwise be syntax.
\* \_ \` \# \$ \@ \< \\ and any Unicode codepoint: \u{03A9}

- Bullet list
- Indent to nest
  - like this

+ Numbered list
+ An item holds whatever is indented under it, like this code block:
  ```rust
  fn main() { println!("Highlighted by language"); }
  ```

/ Term: A term list holds definitions.

////////////////////////////////////////////////////////////////////////////
// 2. Code mode: calling functions
////////////////////////////////////////////////////////////////////////////

= Functions

// Markup is only shorthand for function calls. `#` switches to code for
// one expression, then markup resumes.
#emph("Emphasis") is what _Emphasis_ stands for.

// Arguments are positional or `named: value`. Markup in [square brackets]
// is a value of type content. Content arguments may trail the parentheses.
#text(fill: red, size: 14pt, [Red text.])
#text(fill: red, size: 14pt)[The same.]
#underline[Empty parentheses can be dropped.]

// Whatever has no markup syntax is a plain function:
#highlight[marked], #strike[gone], #smallcaps[Small Caps], H#sub[2]O,
x#super[2], #link("https://typst.app/docs/")[a link with a label], a
footnote#footnote[Numbered and placed at the page bottom.] and
placeholder text: #lorem(8)

// #image("photo.jpg", width: 50%)  // PNG, JPEG, GIF, SVG, PDF or WebP

// An expression ends where it stops parsing; a `;` ends it by force.
// Expressions with operators need parentheses.
#let n = 3
The #n;rd time, then the #(n + 1)th.

////////////////////////////////////////////////////////////////////////////
// 3. Scripting
////////////////////////////////////////////////////////////////////////////

= Scripting

#let name = "Typst"                               // a variable
#let double(x) = 2 * x                            // a function
#let greet(who, mark: "!") = [Hello, #who#mark]   // named: has a default
#let add = (a, b) => a + b                        // an anonymous function

#name, #double(21), #add(1, 2)                    // Typst, 42, 3
#greet[you] #greet(mark: "?")[*world*]            // Hello, you! Hello, world?

// A {code block} holds several statements. Inside, no `#` is needed.
#{
  let nothing = none        // also: true, false, auto ("pick for me")
  let i = 42                // int; also 0xff and 0b101
  let f = 3.14              // float; also 1e-3
  let s = "a string\n"      // str: plain characters, no markup
  let c = [*content* with markup]
  let len = 2cm + 12pt      // length: pt mm cm in; 1em = the font size
  let ratio = 50%           // relative to the container
  let share = 1fr           // a fraction of the remaining space
  let angle = 90deg
  let color = rgb("#239dad")  // also: red, navy, luma(80%), ...
  [A #type(len) and a #type(color).]    // content in code is output
}

// Operators: + - * /  == != < <= > >=  and or not  in  = += -= *= /=
#(7 / 2), #calc.rem(7, 2), #calc.pow(2, 10)       // 3.5, 1, 1024
#("ab" + "c"), #("ab" * 2), #("b" in "abc")       // abc, abab, true
#(str(42) + "!"), #(int("7") + 1), #name.len()    // 42!, 8, 5

#let xs = (1, 2, 3)             // array; (1,) has one item, () is empty
#xs.at(0), #xs.last(), #xs.len(), #xs.slice(1)    // 1, 3, 3, (2, 3)
#xs.map(x => x * x).filter(x => x > 1).sum()      // 13
#(xs + (4, 5)).rev().map(str).join(", ")          // 5, 4, 3, 2, 1
#range(3), #xs.contains(2), #xs.zip(xs).first()   // (0, 1, 2), true, (1, 1)

#let d = (lang: "Typst", year: 2023)              // dictionary; (:) is empty
#d.lang, #d.at("year"), #d.keys(), #("age" in d), #d.at("age", default: 0)
// Typst, 2023, ("lang", "year"), false, 0

#let (first, ..rest) = xs       // destructuring: first = 1, rest = (2, 3)
#let (lang, year) = d

// if, for and while are expressions. A body is [content] or {code}.
#if year > 2020 [New.] else [Old.]
#for x in xs [#x squared is #(x * x). ]
#for (key, value) in d [/ #key: #value]

// The values of all statements in a block are joined into its result,
// so `return` is only needed to exit early.
#let countdown(n) = {
  while n > 0 {
    str(n) + "... "
    n -= 1
  }
  [*Liftoff!*]
}
#countdown(3)

#let total(..nums) = nums.pos().sum()   // `..` collects extra arguments
#total(1, 2, 3) = #total(..xs)          // and spreads an array into them
#let warn = text.with(fill: red)        // `.with` pre-fills arguments
#warn[Careful!]

// Functions are pure: they cannot change variables defined outside of
// them. Only the scope that owns a variable can do `n += 1`, `xs.push(4)`.

////////////////////////////////////////////////////////////////////////////
// 4. Math
////////////////////////////////////////////////////////////////////////////

= Math

Inline: $a^2 + b^2 = c^2$. Spaces inside the dollars make it a block:

$ x = (-b plus.minus sqrt(b^2 - 4 a c)) / (2 a) $

// A single letter is a variable. Several letters name a symbol or function,
// hence the spaces in `4 a c`. Quote words to get upright text.
$ "area" = pi r^2 $

// `_` and `^` attach, parentheses group them, `/` builds a fraction.
$ sum_(k=1)^n k = (n(n+1)) / 2, quad integral_0^oo e^(-x) dif x = 1 $

// Symbols have names, variants (after a dot) and often a shorthand.
$ alpha, Omega, oo, RR, arrow.r.long, x -> y, a != b, a <= b, x in NN $

// In markup, the same symbols are reached through `sym`.
Typst #sym.arrow.r PDF

// Functions are called without `#`. In `mat`, `;` separates rows.
$ vec(1, 2), mat(1, 2; 3, 4), abs(x), cal(A), bold(v), dot(x), hat(x) $
$ f(x) = cases(x^2 "if" x >= 0, -x "otherwise") $

// `\` breaks the line and `&` marks alignment points.
$ (a + b)^2 &= (a + b)(a + b) \
            &= a^2 + 2 a b + b^2 $

// `#` embeds code, e.g. a variable from above.
$ #n + 1 = #(n + 1) $

////////////////////////////////////////////////////////////////////////////
// 5. Styling: set and show rules
////////////////////////////////////////////////////////////////////////////

// A set rule changes the default arguments of a function, from here to the
// end of the enclosing block or file. Most styling is done this way.
// Page settings begin a new page, so they normally go at the top of a file.
#set page(paper: "a5", margin: (x: 2cm, y: 2.5cm), numbering: "1")
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set par(justify: true, leading: 0.6em)     // also: first-line-indent
#set heading(numbering: "1.1")      // earlier headings stay unnumbered
// More page options: columns: 2, flipped: true, header: [..], footer: [..]
// `typst fonts` lists the fonts you can use.

= Styling

// Markup is styled through its function: `+` items form an `enum`.
#[
  #set enum(numbering: "a)")
  + This list is lettered,
  + but only inside this block.
]

// A show-set rule applies a set rule only within the selected elements.
#show heading: set text(fill: navy)
#show heading.where(level: 1): set align(center)
#show figure.caption: set text(size: 9pt)

// A show rule with a function replaces the element with what it returns.
// `it` is the element; its fields are the arguments it was created with.
#show link: underline
#show heading.where(level: 2): it => emph(it.body) + [. ]

// Text and regular expressions can be selected as well.
#show "TODO": text(fill: red)[*TODO*]
#show regex("\d+%"): strong

== A run-in heading
https://typst.app/ is underlined now. TODO: be 100% sure.

////////////////////////////////////////////////////////////////////////////
// 6. Layout
////////////////////////////////////////////////////////////////////////////

= Layout

#align(center)[Centered.]
#align(right + bottom)[Alignments combine with `+`.]
#pagebreak()

Left #h(1fr) right      // h and v insert space; 1fr takes all that is left
#v(1cm)

// box is an inline container, block a block-level one.
A #box(stroke: 1pt, inset: 3pt)[box] stays in its line.
#block(fill: luma(230), inset: 8pt, radius: 4pt, width: 100%)[
  A block is set apart from the text around it.
]

// A grid arranges cells. Track sizes: auto, a length, a ratio or a fraction.
#grid(
  columns: (auto, 1fr, 2fr),
  gutter: 1em,
  [auto], [one share], [two shares],
  rect(width: 2cm), circle(radius: 5mm), line(length: 100%),
)

// place takes content out of the flow and positions it in its container,
// here the page.
#place(top + right, dy: -1cm, rotate(10deg)[_Draft_])

////////////////////////////////////////////////////////////////////////////
// 7. Figures, tables and references
////////////////////////////////////////////////////////////////////////////

= Figures and references <refs>      // a label attaches to what precedes it

#set math.equation(numbering: "(1)")
$ e^(i pi) + 1 = 0 $ <euler>

#figure(
  table(
    columns: 3,                      // or track sizes, as in a grid
    align: (left, center, right),
    table.header[Fruit][Stock][Price],
    [Apple], [3], [1.20],
    [Pear], table.cell(colspan: 2)[sold out],
  ),
  caption: [What the fruit stand offers.],
) <fruit>

// `@label` references a figure, or a numbered heading or equation.
@fruit, @euler and @refs are numbered automatically.

#outline()    // a table of contents, built from the headings

// With a bibliography (BibLaTeX .bib or Hayagriva .yml), `@key` cites.
// #bibliography("works.bib", style: "apa")

////////////////////////////////////////////////////////////////////////////
// 8. Templates, files and packages
////////////////////////////////////////////////////////////////////////////

= Templates

// A template is a function that takes a document and returns a styled one.
#let letter(sender: none, body) = {
  set text(font: "Libertinus Serif", style: "italic")
  align(right, sender)
  body
}

// `show: f` passes everything that follows in the block or file to `f`.
// This line usually sits at the top of a document.
#[
  #show: letter.with(sender: [Jane Doe])
  Dear reader, the rest of this block is the body.
]

// #import "letter.typ": letter       // bring definitions from a file
// #import "utils.typ"                // or the module: `utils.double(2)`
// #include "chapter-1.typ"           // insert the content of a file
// #import "@preview/cetz:0.4.2"      // a package from Typst Universe

////////////////////////////////////////////////////////////////////////////
// 9. Context
////////////////////////////////////////////////////////////////////////////

= Context

// Code runs before layout and does not know where its result ends up.
// `context` delays an expression until its surroundings are known.
#let where = context [#text.size text in section #counter(heading).display()]
#where \
#text(size: 8pt, where)     // one value, resolved separately in each place

// The result is opaque content, so do all the work inside the expression.
#context {
  let pages = counter(page).final().first()
  [Page #here().page() of #pages.]
}
````

## Further reading

* [Official tutorial](https://typst.app/docs/tutorial/)
* [Reference](https://typst.app/docs/reference/), starting with the
  [syntax cheat sheet](https://typst.app/docs/reference/syntax/)
* [Guide for LaTeX users](https://typst.app/docs/guides/guide-for-latex-users/)
* [Typst Universe](https://typst.app/universe/): packages and templates
* [Forum](https://forum.typst.app/)
