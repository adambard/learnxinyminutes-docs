---
name: Wikitext
contributors:
    - ["Yuxi Liu", "https://github.com/yuxi-liu-wired"]
filename: wikitext.md
---

A wiki is an online collaboratively edited hypertext publication, the most famous of which is Wikipedia. Wikitext is the markup language used by wikis. Its syntax is similar to a mix of Markdown and HTML.

## Syntax

`<!--- comments are hidden when reading, but visible when editing --->`

| wikitext | equivalent Markdown | effect |
| ---- | ---- | ---- |
| `''italics''` | `*italics*` | *italics* |
| `'''bold'''` | `**bold**` | **bold** |
| `'''''both'''''` | `***both***` | ***both*** |
| `<u>underlined</u>` | `<u>underlined</u>` | <u>underlined</u> |
| `<nowiki>do not render</nowiki>` | N/A | `do not render` |
| `<code>inline code snippet</code>` | \`inline code snippet\` | `inline code snippet` |
| `----` | `----` | horizontal linebreak |
| `<s>strikethrough</s>` | `~~strikethrough~~` | ~~strikethrough~~ |

Section headings are bracketed by `=`. They go from `= One equal sign =` to `====== Six equal signs ======`. They are equivalent to Markdown's hashtag headings, from `# One hashtag` to `###### Six hashtags`. Why six in both? I believe it's because HTML has six levels of headings, from `<h1>` to `<h6>`.

Note that the `= One equal sign =` heading actually corresponds to the title of the page, and so cannot actually be used within a page. Consequently, the least number of equal signs is `== Two equal signs ==`.

Subscripts and superscripts can be written as `x<sub>1</sub>` and `x<sup>1</sup>`. Alternatively they can be written by the `<math>` tag (see below). `<small>Small</small>` and `<big>big</big>` texts are rarely used.

```wikitext
Colons allow indentation
   :Each colon creates an indentation three characters wide.
      ::and they can be nested.
```

`*` Unnumbered lists start with `*`, and numbered lists start with `#`. <br>
&emsp; `**` Lists can be nested <br>
&emsp; &emsp; `***` for arbitrarily many levels.

The syntax for tables is [very complicated](https://en.wikipedia.org/wiki/Help:Table). The simplest of the [simple tables](https://en.wikipedia.org/wiki/Help:Basic_table_markup) is as follows:

```wikitext
{| class="wikitable"
|+
! column title A
! column title B
|-
| cell A1
| cell B1
|-
| cell A2
| cell B2
|-
| ...
| ...
|}
```

which renders to

| **column title A** | **column title B** |
|---|---|
| cell A1 | cell B1 |
| cell A2 | cell B2 |

Be warned that the newlines in a wikitext table are meaningful. Deleting a single newline above would completely change the shape of the rendered table.

You can insert images, audios, videos, or other forms of media by `[[File:Image.png|thumb|right|Image caption]]`. All media files must be hosted on [Wikimedia Commons](https://commons.wikimedia.org/wiki/Main_Page).

You can insert quotations either by HTML-like tag

```wikitext
<blockquote>
<p>Quotation text.</p>
<p>Name, source, reference</p>
</blockquote>
```

or [template](#templates)

```wikitext
{{Quote|text=Quotation text.|title=Title|author=Author|source=Location in the publication}}
```

A "[non-breaking space](https://en.wikipedia.org/wiki/Non-breaking_space)" is a whitespace that should not be separated by linebreaks, such as the whitespace in "400 km/h". This is written as `400&amp;nbsp;km/h`.

Extra whitespaces can be specified by `pad` tag. For example, `{{pad|4.0em}}` is a white space with length 4.0 [em-dashes](https://en.wikipedia.org/wiki/Dash#Em_dash).

Longer code blocks can be done by

```wikitext
<syntaxhighlight lang="cpp">
#include <iostream>
int m2 (int ax, char *p_ax) {
  std::cout <<"Hello World!";
  return 0;
}</syntaxhighlight>
```

which renders to

```cpp
#include <iostream>
int m2 (int ax, char *p_ax) {
  std::cout <<"Hello World!";
  return 0;
}
```

## Linking

Basic `[[linking]]` is done by double brackets.

The `|` symbol allows displaying a `[[Actual page title|different text]]`.

The `#` symbol allows linking to sections within a text, like `[[Frog#Locomotion]]` or `[[Frog#Locomotion|locomotion in frogs]]`.

If a word is interrupted by a link, it is "blended" into the link. For example, `[[copy edit]]ors` renders to [copy editors](https://en.wikipedia.org/wiki/copy_edit).

To suppress this behavior, use `<nowiki>`. For example, `[[micro-]]<nowiki />second` renders to [micro-](https://en.wikipedia.org/wiki/micro-)second.

There are three kinds of external linking. The third kind is preferred:

| wikitext | renders to |
|----|----|
| `https://www.wikipedia.org` | [https://www.wikipedia.org](https://www.wikipedia.org) |
| `[https://www.wikipedia.org]` | [[1]](https://www.wikipedia.org) |
| `[https://www.wikipedia.org Wikipedia]` | [Wikipedia](https://www.wikipedia.org) |

## Templates

Templates are macros for wikitext, and they look like `{{template name|attribute=value|...}}`. There are thousands of templates, but only a few are in common use.

The most (in)famous one is the \[citation needed\]`{{cn}}` template. Note that `{{cn}}` is synonymous with `{{citation needed}}`, as one template can have many names.

`{{reflist}}` is usually put at the ends of pages, to generate a list of references used in the page.

An `infobox` template is, as it says, a template for a box containing information. Usually, each page contains at most two infoboxes, one on top and one on bottom. For particularly detailed pages, there can be more than two.

The infobox on the top is usually used to compactly display tabular information. They are common for biographies, geographical locations, and such. For example, the top infobox for [Euler](https://en.wikipedia.org/wiki/Leonhard_Euler) is:

```wikitext
{{Infobox scientist
| name              = Leonhard Euler
| image             = Leonhard Euler.jpg
| caption           = Portrait by [[Jakob Emanuel Handmann]], 1753
| birth_date        = {{birth date|df=y|1707|4|15}}
| birth_place       = [[Basel]], [[Swiss&nbsp;Confederacy]]
| death_date        = {{nowrap|{{death date and age|df=y|1783|9|18|1707|4|15}}}} {{awrap|{{bracket|[[Adoption of the Gregorian calendar#Adoption in Eastern Europe|OS]]: 7 September 1783}}}}
...
}}
```

The infobox at the bottom is usually used to display a curated table of related links. For example, the bottom infobox for [Euler–Lagrange equation](https://en.wikipedia.org/wiki/Euler%E2%80%93Lagrange_equation) is just `{{Leonhard Euler}}`, which displays a box containing links to many of the things named after Euler.

`~~~~` is used to sign on talk pages, and expands to something like `Username (talk) 10:50, 12 June 2023 (UTC)`.

### Mathematics

`<math>` tag renders $\LaTeX$ inline like `$`, while `<math display=block>` renders it on a separate line like `$$`.

`<math>E = mc^2</math>` renders to $E = mc^2$.

`<math display=block></math>` renders to $$E = mc^2$$.

One can also include math using [HTML renders](https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Mathematics#Using_HTML) or even by [plain Unicode](https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode). These are less flexible but more compatible with older browsers. Further, parts of Wikipedia syntax themselves are incompatible with `<math>`, such as in section titles or some templates, forcing the use of HTML or Unicode in such cases.

Theorems and proofs can be boxed and named:

```wikitext
{{Math theorem
|name=Pythagorean theorem
|note=Pythagoras, 500s BC
|math_statement=Let <math>a, b, c</math> be the three side lengths of a right triangle, then
<math display=block>a^2 + b^2 = c^2</math>
}}

{{Math proof
|title=Proof by similar triangles
|proof=Drop a perpendicular from point C to side AB. Now argue by proportionality. <math>\blacksquare</math>
}}
```

## References

References are the backbone of Wikipedia `{{citation needed}}`. There are in general two ways to do citations.

| type | inline citation | expanded citation |
| ---- | ---- | ---- |
| purpose | Support specific claims. | Provide general reference work for the entire page. |
| location | Immediately after the supported claim. | In the `== References ==` section. |
| appearance | analytic continuation of of _f_.<sup>[\[6\]](#6)</sup> | Abramowitz, Milton; Stegun, Irene A., eds. (1972). ["Chapter 6"](http://www.math.sfu.ca/~cbm/aands/page_253.htm)... |
| syntax | `<ref>{{cite book\|...}}</ref>` | `{{cite book\|...}}` |

As expanded citations are just inline citations without the `<ref>` tag, we will describe just inline citations.

The most basic form is a plaintext citation, like `<ref>Author, Title, date, [url](https://example.com/), etc</ref>`.

One should generally use a templated citation, like `<ref>{{cite web|url=https://example.com/|title=Example|date=2001|access-date=2023}}</ref>`. There are three forms of citation templates: [`cite web`](https://en.wikipedia.org/wiki/Template:Cite_web), [`cite journal`](https://en.wikipedia.org/wiki/Template:Cite_journal), [`cite book`](https://en.wikipedia.org/wiki/Template:Cite_book).

A citation can be named as `<ref name="X">...</ref>`. It can then be invoked as `<ref name="X" />`. The instance `<ref name="X">...</ref>` can go before or after `<ref name="X" />`. Any ordering would render to the same page.

### Detailed information

Some parameters are required, meaning that every templated citation must fill those parameters. Some parameters are "grouped", meaning that if one parameter is filled, then all must be filled. For example, if one of `archive-url=`, `archive-date=` and `url-status=` were filled, then the other two must also be.

The following parameters are required in all citation templates:

* `title=` – Name of the cited item.

If the work is in a foreign language, then the following are strongly recommended:

* `language=` - THe 2-character ISO639-1 language code in which the work is in.
* `trans-title=` - Title translated to the local language.
* `script-title` - If the title is in a writing system that is not based on the Latin alphabet. Must be preceded by a 2-character ISO639-1 language code. For example, `script-title=ja:ディープラーニング`.

There are 5 major citation templates: `cite news`, `cite web`, `cite book`, `cite journal`, `cite conference`. For each of these, the following parameters are not necessary to be filled, but strongly recommended:

* `last=` / `first=` (plus `last2=`, `first2=`, etc) – author(s). If the author(s) has a Wikipedia page, then it can be specified via `author-link=`, `author-link2=`, etc. If one wishes to include the middle name, then it should be entered as part of the first name.
* `date=` – publication or last-update date, in the format of `YYYY`, `YYYY-MM`, or `YYYY-MM-DD`.

If the source is web-accessible currently or in the past, the following are strongly recommended:

* `url=` – direct link.
* `access-date=` – date you retrieved the `url`.
* `archive-url=`, `archive-date=`, `url-status=` – web-archive backup and status flag (`live`/`dead`).

Unique identifiers: Many publications, especially academic ones, have specialized unique identifiers. These should be filled if they exist. The most common ones are:

| name | stands for | commonly used by | example |
|----|----|----|----|
| `isbn=` | International Standard Book Number (either ISBN-10 or ISBN-13)  | Books published after 1970 | `isbn=9783161484100`  |
| `oclc=`  | OCLC Control Number  | Books appearing in WorldCat database | `oclc=9355469`  |
| `asin=`  | Amazon Standard Identification Number | Amazon's catalog. Should only be used when standard identifiers like ISBN are unavailable | `asin=B00005N5PF` |
| `doi=` | Digital Object Identifier | Academic publications | `doi=10.1038/news070508-7` |
| `citeseerx=` | CiteSeerX id | papers | Academic papers | `citeseerx=10.1.1.176.341` |
| `zbl=` | Zentralblatt MATH  | Mathematical publications   | `zbl=0472.53010`  |
| `pmc=` | PubMed Central | Biomedical papers | `pmc=345678`  |
| `pmid=`  | PubMed Identifier  | Biomedical papers  | `pmid=17322060` |
| `arxiv=` | arXiv  | Preprints in physics, mathematics, and computer science | `arxiv=0706.0001` |
| `biorxiv=` | bioRxiv  | Preprints in biology  | `biorxiv=10.1101/078733`  |

More examples are listed on the Wikipedia page [`Template:Cite book`](https://en.wikipedia.org/wiki/Template:Cite_book).

The following lists the differences for each of the 5 main citation templates from the common parameters above.

`{{cite news}}`: Suited to time-stamped, journalistic sources; usually includes an author and headline.

* `newspaper=` – names the periodical (e.g., *Time*).
* `pages =`, `location=` – If the news has been physically printed. Should not be filled for web-only news sources.

`{{cite web}}`:

* `website=` - name of the parent site.
* `publisher=` - the organisation behind the site, if different from `website=`. For example, citing <https://data.giss.nasa.gov/gistemp/news/> would have `website=NASA.gov | publisher=[[Goddard Institute for Space Studies]]`.

`{{cite book}}`:

* `isbn=` - either ISBN-10 or ISBN-13. Hyphens in the ISBN are optional. There is a Wikipedia bot that automatically adds the hyphens once in a while.
* `edition=` - when the publication has more than one edition. For example: `edition=2nd Revised`.
* `location=` - the city of publication.  
* `chapter=` (with optional `chapter-url=`) isolates a chapter in a multi-author volume.  
* `page=` or `pages=` - Location in the book. For example, `page=53` or `pages=50,53–55`. If a book is cited multiple times throughout the Wikipedia page, then it is recommended that you do not use `pages=`. Instead, use the [`{{Pg}}`](https://en.wikipedia.org/wiki/Template:Reference_page) macro. For example: `{{rp|pages=143,233–237}}`. Note that, for a range of pages, the dash is the en-dash, literally as `–` or `&ndash;`.

`{{cite journal}}`:

* `journal=` (Required) - Aliases: work, newspaper, magazine, periodical, website.
* `volume=` and `issue=` - If both are entered, then `issue=` is displayed in parentheses following `volume=`.
* `page=` or `pages=` - See previous list.

`{{cite conference}}`:

* `book-title=` (required) - the name of the proceedings or conference. For example:`book-title=American Astronomical Society 207th Meeting`.
* `publisher=` - the organisation releasing the proceedings. For example, `publisher=IEEE`.
* `location=` - the geographical location of the conference, usually the name of a city.
* `page=` or `pages=` - See previous list.

For `arXiv` preprints, there is a special macro `{{cite arxiv}}`, somewhat different from `{{cite journal}}`. It is of the form `{{cite arXiv |last= |first= |date= |title= |arxiv= |class=}}`.

## Typical Wikipedia page

```wikitext
{{Short description|One sentence summary of page}}

{{Infox box at the top
|infobox_data_1=...
|...
}}

[[File:Image of X.png|thumb|right|Image caption]]

The concept '''X''' is usually bolded. Now define the concept X. For non-specialist pages, this section should be written in plain language, with jargons defined in-line. Some [[link]]s would help.


== Introduction ==

Here one usually sets up the notation, overviews the history, and such. Details follow in the next sections.

Footnotes are numbered separately from inline references.{{NoteTag|note=Footnote text.}}

== Relation to Y ==
{{Main|Y}}
{{See also|Another page}}

Something about the relation between X and Y.

== See also ==
* [[Very relevant link]]
* [[Less relevant link]]

== External links ==
* [https://example.com/ External link one]: Summary of what is in the external link.

== Footnotes ==

<references group="note" />{{Notelist}}

== References ==
<!-- generates list of references from inline reference tags, with columns with a minimum width of 30 em-dashes. -->
{{Reflist|30em}}

<!-- extra, non-inlined references below -->
{{Refbegin|30em}}
* {{cite book|title=Book Title|date=2001|chapter=Chapter 1|...}}
* ...

== Further reading ==
* ...
* ...

{{Infox box at the bottom}}

[[Category:First category that the article belongs to]]
[[Category:First category that the article belongs to]]
[[Category:There is no limit to the number of categories allowed]]
```

## Further reading

* [Wikipedia's manual of style](https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style)
* [Wikitext cheatsheet](https://en.wikipedia.org/wiki/Help:Cheatsheet)
* [Wikitext, full reference](https://en.wikipedia.org/wiki/Help:Wikitext).
* [Tables, full reference](https://en.wikipedia.org/wiki/Help:Table#Simple_straightforward_tables)
