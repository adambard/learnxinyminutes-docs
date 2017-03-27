### General Guidelines

Please tag your issues and pull requests with [language/lang-code] at the
beginning (e.g. [python/en] for English Python). This will help everyone pick
out things they care about.

We're happy for any contribution in any form, but if you're making more than one major change
(i.e. translations for two different languages) it would be super cool of you to make a
separate pull request for each one so that someone can review them more effectively and/or
individually.

If you're making a significant change to the article, e.g. adding a whole new
section or doing a rewrite, add your name to the top of the page under the
contributors section. We do not have strict policies on what counts as
"significant," but please understand that cleanup patches or minor bug fixes do
not count.

### Small Fixes

When opening issues and pull requests that are (or would be) only a few lines
(usually < 5), we strongly encourage you to indicate as such in the title. We
tend to prioritize small changes since we can get through more of them quickly.

### Translations

We encourage you to @-mention specific collaborators for translation-related
pull requests of the following languages:

* @geoffliu: Chinese (simplified and traditional), Italian
* @oire: Russian
* @vendethiel: French

For all other (non-english) languages, we leave the pull request open for a few weeks for the
public to comment before merging. We thank you for your patience.

### Style Guidelines

* **Keep lines under 80 chars**
* **Prefer example to exposition**
* **Eschew surplusage**
* **Use UTF-8**

Long version:

* Try to keep **line length in code blocks to 80 characters or fewer**, or they'll overflow
  and look odd.

* Try to use as few words as possible. Code examples are preferred over exposition in all cases.

* We welcome newcomers, but the target audience for this site is programmers with some experience.
  So, try to avoid explaining basic concepts except for those specific to the language in question,
  to keep articles succinct and scannable. We all know how to use Google here.

* For translations (or English articles with non-ASCII characters), please make sure your file is
  UTF-8 encoded, and try to leave out the byte-order-mark at the start of the file. (`:set nobomb` in Vim)

### Header configuration

The actual site uses Middleman to generate HTML files from these Markdown ones. Middleman, or at least
the custom scripts underpinning the site, required that some key information be defined in the header.

The following fields are necessary for English articles about programming languages:

* **language** The *programming language* in question
* **contributors** A list of [author, URL] lists to credit

Other fields:

* **filename**: The filename for this article's code. It will be fetched, mashed together, and made downloadable.
  For non-English articles, *filename* should have a language-specific suffix.
* **lang**: For translations, the human language this article is in. For categorization, mostly.

Here's an example header for an Esperanto translation of Ruby:

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

Here's a quick checklist for your YAML frontmatter:

* [ ] Article is categorized correctly
* [ ] Has `lang` keyword (except english)
* [ ] `filename` has a suffix, for non-english articles (e.g. `filename: myfile-cn.md` instead of `filename: myfile.md`)
* [ ] Formatting of `contributors` and `translators` sections is correct (check for quotes and commas)
