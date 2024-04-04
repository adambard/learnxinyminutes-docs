# Contributing

All contributions are welcome, from the tiniest typo to a brand new article.
Translations in all languages are welcome (or, for that matter, original
articles in any language). Send a pull request or open an issue any time of day
or night.

**Please prepend the tag `[language/lang-code]` to your issues and pull
requests.** For example, `[python/en]` for English Python. This will help
everyone pick out things they care about.

We're happy for any contribution in any form, but if you're making more than one
major change (i.e. translations for two different languages) it would be super
cool of you to make a separate pull request for each one so that someone can
review them more effectively and/or individually.

## Style Guidelines

* **Keep lines under 80 chars**
   * Try to keep **line length in code blocks to 80 characters or fewer**.
   * Otherwise, the text will overflow and look odd.
   * This and other potential pitfalls to format the content consistently are
     identified by the freely available
     [markdownlint](https://github.com/markdownlint/markdownlint).
* **Prefer example to exposition**
   * Try to use as few words as possible.
   * Code examples are preferred over exposition in all cases.
* **Eschew surplusage**
   * We welcome newcomers, but the target audience for this site is programmers
     with some experience.
   * Try to avoid explaining basic concepts except for those specific to the
     language in question.
   * Keep articles succinct and scannable. We all know how to use Google here.
* **Use UTF-8**
   * For translations (or EN articles with non-ASCII characters) please ensure
     your file is UTF-8 encoded.
   * Leave out the byte-order-mark (BOM) at the start of the file (in Vim, use
     `:set nobomb`).
   * You can check if the file contains a BOM on Linux/Unix systems by running
    `file language.html.markdown`  You will see this if it uses a BOM:
    `UTF-8 Unicode (with BOM) text`.

### Header configuration

The actual site uses Middleman to generate HTML files from these Markdown ones.
Middleman, or at least the custom scripts underpinning the site, requires that
some key information be defined in the header.

The following fields are necessary for English articles about programming
languages:

* **language** The *programming language* in question
* **contributors** A list of [author, URL] lists to credit

Other fields:

* **category**: The category of the article. So far, can be one of *language*,
  *tool* or *Algorithms & Data Structures*. Defaults to *language* if omitted.
* **filename**: The filename for this article's code. It will be fetched, mashed
  together, and made downloadable.
   * For non-English articles, *filename* should have a language-specific
     suffix.
* **lang**: For translations, the human language this article is in. For
  categorization, mostly.

Here's an example header for an Esperanto translation of Ruby:

```yaml
*--
language: ruby
filename: learnruby-epo.ruby
contributors:
    - ["Doktor Esperanto", "http://example.com/"]
    - ["Someone else", "http://someoneelseswebsite.com/"]
lang: ep-ep
*--
```

### Syntax highlighter

[Rouge](https://github.com/rouge-ruby/rouge/wiki/List-of-supported-languages-and-lexers) is used for syntax highlighting.

### Should I add myself as a contributor?

If you want to add yourself to contributors, keep in mind that contributors get
equal billing, and the first contributor usually wrote the whole article. Please
use your judgment when deciding if your contribution constitutes a substantial
addition or not.

## Building the site locally

You can build the site locally to test your changes. Follow the steps below.

* Install Ruby language runtime and RubyGems. See
  [here](https://middlemanapp.com/basics/install/)
  for more details.
* Clone or zip download the
  [learnxinyminutes-site](https://github.com/adambard/learnxinyminutes-site)
  repository.
   * `git clone https://github.com/adambard/learnxinyminutes-site`
* Install Middleman and other required dependencies using Bundler.
   * `cd learnxinyminutes-site/`
   * `bundle install`
* Get the source in place
   * Copy the contents of your clone of the fork of learnxinyminutes-docs repo
     into the `source/docs` folder. There shouldn't be a `learnxinyminutes-docs`
     folder inside the `docs` folder, it should just contain all the repo
     contents.
   * Checkout your fork of the learnxinyminutes-docs repo as `source/docs`.
      * `cd source/docs/`
      * `git clone https://github.com/YOUR-USERNAME/learnxinyminutes-docs ./source/docs/`
* Build the site or run a development server to test your changes (NOTE: run
  these commands at `learnxinyminutes-site/`).
   * Build - `bundle exec middleman build`
   * Dev server - `bundle exec middleman --force-polling --verbose`
