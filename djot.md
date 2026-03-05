---
name: Djot
contributors:
    - ["stag", "http://github.com/stag-enterprises/"]
filename: learndjot.dj
---

[Djot](https://djot.net/) is a lightweight markup language
derived from CommonMark.
It aims to be simple to parse, have little rules for edge cases, consistent,
hard-wrap friendly, and be expressive.
    
    Paragraphs are like Markdown.
    
    Start a new paragraph.
    
    # Headings like markdown
    
    ## 2nd level
    
    ### 3rd level
    
    Setext headings don't exist
    ========
    
    All block level elements must be surrounded by blank lines.
    
    # Heading
    this is still
    part of the heading
    
    But this is not.
    
    # Inline elements
    
    _emphasis (italics) _nest it!__
    _  no emphasis here!_
    *strong text (bold)*
    {=highlighted=}
    ^superscript^ ~subscript~
    {-deleted-}{+added+}
    
    > Block quotes like markdown
    > The space is required
    The > is optional after the first line
    
    # Links
    
    [Markdown style links](https://example.com/
    you-can-hard-wrap
    -the-links)
    [Reference style links][refname]
    {this-attribute="has been overridden"}
    [refname][] Omit the label like in Markdown
    
    [Implicitly link to headings][Inline elements]
    
    {this-attribute="will be on every use of this link"}
    [refname]: https://example.com
    
    Names are case sensitive
    
    ![images just like markdown](./pic.png)
    ![ref style also works!][pic]
    ![pic][]
    
    [pic]: ./pic.png
    
    Unlike markdown, https://this-is-not-a-link.com, but <https://this-is.com>
    
    # Verbatim
    
    Verbatim (code) elements start with *any* number of backticks,
    and end with the same amount.
    `This is code`, ``so is this``, ```and this```
    `` `literal backticks` ``
    Like in Markdown, the first and last space, if both present, are removed.
    
    Prefix with a $ for LaTeX mode: $`e=mc^2`
    Or for display mode: $$`a^2 + b^2 = c^2`
    
    If there are 3+ backticks on it's own line, it's a block code element
    
    `````language-specifier
    content
    ```
    `````
    
    If the language specifier starts with =,
    it is used to symbolize content of a different language.
    
    ```=html
    <p>raw html</p>
    ```
    
    # Lists
    
    1. This is a numbered list
    2. This is the second element
     indent is relative to the list marker, and only 1 space is needed
    
     On paragraph lines
    indentation can be omitted on continued lines.
    
    3. Sublists
    
     1. must have blank lines around, since they are block elements
    
    Bulleted list styles: + * -
    Numbered list styles: 1. 1) (1)
    Lettered list styles: a. a) (a) A. A) (A)
    Roman numbered list styles: i. i) (i) I. I) (I)
    
    - [ ] Here's a task list
    - [x] This one is completed
    * Don't mix styles, this is a new list
    
    5. This list starts at 5
    10. Though the next numbers are meaningless
    
    1. This is a loose list
    
    2. because of the blank lines.
    
    3. It will be rendered with extra space in between items
    
    : Definition list
    this is still part of the definition
    
     This is the definition. Note the indentation.
    
    # Tables
    
    | just one cell |
    
    Tables are basically the same as Markdown.
    
    | left aligned | center | right | default |
    |:-------------|:------:|------:|---------|
    | a            |   b    |     c | d       |
    
    Note that multiple header rows are supported.
    
    | a  |  b |
    |----|:--:|
    | 1  | 2  |
    |:---|---:|
    | 3  | 4  |
    
    Specify alignments without a header
    
    |:--|---:|
    | x | 2  |
    ^ Tables can be captioned
    
    # Attributes
    
    Inline code can have a format specifier, just like block code blocks:
    `content`{=format}
    Make spans like [this]{.myclass}.
    The attribute specifier (explained below) is required.
    
    _any inline element can have attributes_{#id .class attribute=value
    quoted="value" % comment %}{.stack .attribute .specifiers}
    
    The comment syntax only be used in attribute specifiers,
    but you can just use one standalone.
    {% my comment %}
    
    ::: single-class-name
    This produces a div
    :::
    
    Any block level element can have an attribute list by adding one before
    
    {#attribute .list}
    {.stack-them}
    {foo=bar
     must-be-indented="unlike inline"}
    # heading
    
    # Everything else
    
    There are smart quotes, like "this" or 'this'.
    Use brackets {"to"} override heuristics.
    Ellipsis: ..., en-dash: --, em-dash: ---.
    
    To escape special symbols, use a backslash. A straight quote: \"
    This is a non-breaking space: \ , and this is a hard line break\
    
    These are thematic breaks:
    ***
    ---
      * *  * *
     --------
    
    We have footnotes[^myfootnote]
    
    [^myfootnote]: source: me
     Add more content with indentation, like you'd expect,
    and omit the indentation in paragraphs, like you'd expect.
    
    In the case of overlap, elements have precedence by the first opener.
    _This *has_ literal* astrieks.
    [This link text has a literal astriek *](url)*
    *But this one [*](has literal brackets and no link)
    {_Use brackets to force opening and closing markers_}
    {*Use {_it_} {^with^} {~any~} {-symbol-}*}
    {_No closing marker, no emphasis
    
    :these: :are: :symbols:
    The spec defines none, but can be used for extensibility.
