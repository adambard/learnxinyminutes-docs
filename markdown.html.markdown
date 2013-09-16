---
category: tool
tool: markdown
contributors:
    - ["Michał Kupczyński", "https://github.com/ukoms"]
filename: markdown.md
---

Markdown is both - software tool for converting plain text formating to HTML
and text formatting syntax. This document is about syntax itself.

Main purpose of Markdown is to be as simple to use and readable as possible.

There are differences between classic Markdown and it's GitHub version -
GitHub Flavored Markdown. This document covers this differences when they
occure.

# BLOCKS

## Markdown

### Paragraphs
Making separate paragraphs require at least one empty line between two lines of text.

Below in markdown:

```markdown
Lorem ipsum dolor sit amet, consectetur adipiscing elit.

Suspendisse sagittis dolor ac lectus dictum blandit.

Aliquam blandit urna ut tristique gravida.
```

HTML equivalent:

```html
<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
<p>Suspendisse sagittis dolor ac lectus dictum blandit.</p>
<p>Aliquam blandit urna ut tristique gravida.</p>
```

### Line breaks
Classic Markdown does not follow "every line break is a &lt;br /&gt;" rule.
To insert line break it is required to type at least two spaces and then return.
Only to emphase the example spaces are marked as [SPACE] and return as [RETURN].

Markdown syntax:

```markdown
Lorem ipsum dolor sit amet, consectetur adipiscing elit.[SPACE][SPACE][RETURN]
Suspendisse sagittisdolor ac lectus dictum blandit.[SPACE][SPACE][RETURN]
Aliquam blandit urna ut tristique gravida.[SPACE][SPACE][RETURN]
```

HTML equivalent:

```html
Lorem ipsum dolor sit amet, consectetur adipiscing elit.<br />
Suspendisse sagittisdolor ac lectus dictum blandit.<br />
Aliquam blandit urna ut tristique gravida.<br />
```

## GitHub Flavored Markdown

# Newlines
Unlike classic Markdown GitHub Flavored Markdown treats every return
as line break.
Only to emphase the example return is marked as [RETURN]

GitHub Flavored Markdown syntax:

```markdown
Lorem ipsum dolor sit amet, consectetur adipiscing elit.[RETURN]
Suspendisse sagittisdolor ac lectus dictum blandit.[RETURN]
Aliquam blandit urna ut tristique gravida.[RETURN]
```

HTML equivalent:

```html
<p>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.<br />
    Suspendisse sagittisdolor ac lectus dictum blandit.<br />
    Aliquam blandit urna ut tristique gravida.<br />
</p>
```

# HEADERS

### First level headers

Markdown syntax:

```markdown
First title
===========

# Second title

# Third title #
```

HTML equivalent:

```html
<h1>First title</h>
<h1>Second title</h1>
<h1>Third title</h1>
```

### Second level headers

Markdown syntax:

```markdown
First subtitle
--------------

## Second subtitle

## Third subtitle ##

## Fourth subtitle #
```

HTML equivalent:

```html
<h2>First subtitle</h2>
<h2>Second subtitle</h2>
<h2>Third subtitle</h2>
<h2>Fourth subtitle</h2>
```

### Third, fourth, fifth and sixth level headers

Markdown syntax:

```markdown
### Third section ###
### Third section #

#### Forth section ####
#### Forth section #

##### Fifth section #####
##### Fifth section #

###### Sixth section ######
###### Sixth section #
```

HTML equivalent:

```html
<h3>Third section</h3>
<h3>Third section</h3>

<h4>Fourth section</h4>
<h4>Fourth section</h4>

<h5>Fifth section</h5>
<h5>Fifth section</h5>

<h6>Sixth section</h6>
<h6>Sixth section</h6>
```

# BLOCKQUOTES

### E-mail like blockquoting style

Markdown syntax:

```markdown
> Lorem ipsum dolor sit amet, consectetur adipiscing elit.
> Suspendisse sagittisdolor ac lectus dictum blandit.
> Aliquam blandit urna ut tristique gravida.
```

HTML equivalent:

```html
<blockquote>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Suspendisse sagittisdolor ac lectus dictum blandit.
    Aliquam blandit urna ut tristique gravida.
</blockquote>
```

### Lazy blockquoting style

Markdown syntax:

```markdown
> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse
sagittisdolor ac lectus dictum blandit.

> Aliquam blandit urna ut tristique gravida.
```

HTML equivalent:

```html
<blockquote>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse
sagittisdolor ac lectus dictum blandit.

    Aliquam blandit urna ut tristique gravida.
</blockquote>
```

### Nested blockquotes

Markdown syntax:

```markdown
> Lorem ipsum dolor sit amet, consectetur adipiscing elit.
> > Suspendisse sagittisdolor ac lectus dictum blandit.
> Suspendisse sagittisdolor ac lectus dictum blandit.
```

HTML equivalent:

```html
<blockquote>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    <blockquote>
        Suspendisse sagittisdolor ac lectus dictum blandit.
    </blockquote>
    Suspendisse sagittisdolor ac lectus dictum blandit.
</blockquote>
```

### Markdown in blockquotes

Markdown syntax:

```markdown
> # Lorem ipsum dolor sit amet, consectetur adipiscing elit. #
> ### Suspendisse sagittisdolor ac lectus dictum blandit. ###
> ###### Aliquam blandit urna ut tristique gravida. ######
```

HTML equivalent:

```html
<blockquote>
    <h1>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</h1>
    <h3>Suspendisse sagittisdolor ac lectus dictum blandit.</h3>
    <h6>Aliquam blandit urna ut tristique gravida.</h6>
</blockquote>
```

# LISTS

## Markdown

### Unordered lists

Markdown syntax:

```markdown
* lorem
* ipsum
* dolor


+ lorem
+ ipsum
+ dolor

- lorem
- ipsum
- dolor
```

HTML equivalent:

```html
<ul>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ul>

<ul>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ul>

<ul>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ul>
```

### Ordered lists

Markdown syntax:

```markdown
1. lorem
2. ipsum
3. dolor

1. lorem
1. ipsum
1. dolor

3. lorem
1. ipsum
7. dolor
```

HTML equivalent:

```html
<ol>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ol>

<ol>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ol>

<ol>
    <li>lorem</li>
    <li>ipsum</li>
    <li>dolor</li>
</ol>
```

## GitHub Flavored Markdown

### Tasks lists

GitHub Flavored Markdown syntax:

```markdown
- [ ] first task
- [ ] second task
- [x] third task
```

HTML equivalent:

```html
<input type="checkbox" />first task
<input type="checkbox" />second task
<input type="checkbox" checked /> third task
```

# CODE FRAGMENTS

## Markdown

### Blocks of code
Text need to be intended by at least 4 spaces or 1 tab.
Only to emphase in example spaces are marked as [SPACE] and tab as [TAB].

Markdown syntax:

```markdown
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
[SPACE][SPACE][SPACE][SPACE]Suspendisse sagittisdolor ac lectus dictum blandit.
Aliquam blandit urna ut tristique gravida.

Lorem ipsum dolor sit amet, consectetur adipiscing elit.
[TAB]Suspendisse sagittisdolor ac lectus dictum blandit.
Aliquam blandit urna ut tristique gravida.
```

HTML equivalent:

```html
<p>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
</p>
<pre>
    <code>
        Suspendisse sagittisdolor ac lectus dictum blandit.
    </code>
</pre>
<p>
    Aliquam blandit urna ut tristique gravida.
</p>

<p>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
</p>
<pre>
    <code>
        Suspendisse sagittisdolor ac lectus dictum blandit.
    </code>
</pre>
<p>
    Aliquam blandit urna ut tristique gravida.
</p>
```

### Inline code

Markdown syntax:

```markdown
` Lorem ipsum dolor sit amet, consectetur adipiscing `print ('elit.')`
```

HTML equivalent:

```html
<p>
    Lorem ipsum dolor sit amet, consectetur adipiscing <code>print ('elit.')</code>
</p>
```

## GitHub Flavored Markdown

### Simple code block
Use three semicolons with return (empty line) before them.
Only to empase in example return is marked as [RETURN]

GitHub Flavored Markdown syntax:

```markdown
[RETURN]
```
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
```
```

HTML equivalent:

```html
<pre>
    <code>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    </code>
</pre>
```

### Syntax highligting
[LANGUAGE] is one of the languages that GitHub detects. Valid keywords
are to be found in
https://github.com/github/linguist/blob/master/lib/linguist/languages.yml

GitHub Flavored Markdown syntax:

```markdown
[RETURN]
```[LANGUAGE]
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
```
```

HTML equivalent:

```html
<pre>
    <code>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    </code>
</pre>
```

# HORIZONTAL RULES

Markdown syntax:

```markdown

***
___

---
```

HTML equivalent:

```html
<hr />
<hr />
<hr />
```

# TEXT DECORATIONS

## Markdown

### Emphasis

Markdown syntax:

```markdown
*Lorem ipsum dolor sit amet, consectetur adipiscing elit.*
_Suspendisse sagittisdolor ac lectus dictum blandit._
```

HTML equivalent:

```html
<em>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</em>
<em>Suspendisse sagittisdolor ac lectus dictum blandit.</em>
```

### Strong

Markdown syntax:

```markdown
**Lorem ipsum dolor sit amet, consectetur adipiscing elit.**
__Suspendisse sagittisdolor ac lectus dictum blandit.__
```

HTML equivalent:

```html
<strong>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</strong>
<strong>Suspendisse sagittisdolor ac lectus dictum blandit.</strong>
```

## GitHub Flavored Markdown

### Strikethrough

GitHub Flavored Markdown syntax:

```markdown
~~Lorem ipsum dolor sit amet, consectetur adipiscing elit.~~
```

HTML equivalent:

```html
<del>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</del>
```

# LINKS

## Markdown

### Automatic links

Markdown syntax:

```markdown
<http://www.github.com>
```

HTML equvialent:

```html
<a href="http://www.github.com">http://www.github.com</a>
```

# MISCELLANEOUS

## Markdown

### Backslash escaping

Markdown syntax:

```markdown
\\
\`
\*
\_
\#
\+
\-
\.
\!
\{
\}
\[
\]
\(
\)
```

HTML equivalent:

```html
<p>
    \`*_#+-.!{}[]()
</p>
```

## GitHub Flavored Markdown

### Multiple underscores

GitHub Flavored Markdown syntax:

```markdown
_Lorem_ipsum_dolor_sit_amet,_consectetur_adipiscing_elit._
```

HTML equivalent:

```html
<p>
    _Lorem_ipsum_dolor_sit_amet,_consectetur_adipiscing_elit._
</p>
```
