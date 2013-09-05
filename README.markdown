# [Learn X in Y minutes](http://learnxinyminutes.com)

Whirlwind tours of (several, hopefully many someday) popular and
ought-to-be-more-popular programming languages, presented as valid,
commented code and explained as they go.

## We need YOU!...

... to write more inline code tutorials. Just grab an existing file from
this repo and copy the formatting (don't worry, it's all very simple).
Make a new file, send a pull request, and if it passes muster I'll get it up pronto.
Remember to fill in the "contributors" fields so you get credited
properly!

## Contributing

All contributions welcome, from the tiniest typo to a brand new article. Translations
in all languages are welcome (or, for that matter, original articles in any language).
Send a pull request or open an issue any time of day or night.

**Please tag your issues pull requests with [language/lang-code] at the beginning**
**(e.g. [python/en] for english python).** This will help everyone pick out things they
care about.

### Style Guidelines

* **Keep lines under 80 chars**
* **Prefer example to exposition**
* **Eschew surplusage**
* **Use utf-8**

Long version:

* Try to keep **line length in code blocks to 80 characters or fewer**, or they'll overflow
  and look odd.

* Try to use as few words as possible. Code examples are preferred over exposition in all cases.

* We welcome newcomers, but the target audience for this site is programmers with some experience.
  So, try to avoid explaining basic concepts except for those specific to the language in question,
  to keep articles succinct and scannable. We all know how to use google here.

* For translations (or english articles with non-ASCII characters), please make sure your file is
  utf-8 encoded, and try to leave out the byte-order-mark at the start of the file. (`:set nobomb` in vim)

### Header configuration

The actual site uses Middleman to generate HTML files from these markdown ones. Middleman, or at least
the custom scripts underpinning the site, required that some key information be defined in the header.

The following fields are necessary for english articles about programming languages:

* **language** The *programming language* in question
* **contributors** A list of [author, url] lists to credit

Other fields:

* **filename**: The filename for this article's code. It will be fetched, mashed together, and made downloadable.
  For non-english articles, *filename* should have a language-specific suffix.
* **lang**: For translations, the human language this article is in. For categorization, mostly.

Here's an example header for an esperanto translation of Ruby:

```yaml
---
language: ruby
filename: learnruby-epo.ruby
contributors:
    - ["Doktor Esperanto", "http://example.com/"]
    - ["Someone else", "http://someoneelseswebsite.com/"]
lang: ep-ep
---
```

## License

Contributors retain copyright to their work, and can request removal at any time.
By uploading a doc here, you agree to publish your work under the default
[Creative Commons Attribution-ShareAlike 3.0 Unported](http://creativecommons.org/licenses/by-sa/3.0/deed.en_US)
licensing included on each doc page.

Anything not covered by the above -- basically, this README -- you can use
as you wish, I guess.
