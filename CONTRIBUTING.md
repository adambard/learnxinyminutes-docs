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
   * Try to keep line length in code blocks to 80 characters or fewer.
   * Otherwise, the text will overflow and look odd.
   * This and other potential pitfalls to format the content consistently are
     identified by [markdownlint](https://github.com/markdownlint/markdownlint).
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

The actual site generates HTML files from these Markdown ones.
The markdown files can contain extra metadata before the actual markdown,
called frontmatter.

The following fields are necessary for English articles about programming
languages:

* **name** The human-readable name of the programming language
* **contributors** A list of [author, URL] lists to credit

Other fields:

* **category**: The category of the article. So far, can be one of *language*,
  *tool* or *Algorithms & Data Structures*. Defaults to *language* if omitted.
* **filename**: The filename for this article's code. It will be fetched, mashed
  together, and made downloadable.

Non-English articles inherit frontmatter values from the English article (if it exists)
but you can overwrite them.

Here's an example header for Ruby:

```yaml
*--
name: Ruby
filename: learnruby.rb
contributors:
    - ["Doktor Esperanto", "http://example.com/"]
    - ["Someone else", "http://someoneelseswebsite.com/"]
*--
```

### Syntax highlighter

[Pygments](https://pygments.org/languages/) is used for syntax highlighting.

### Should I add myself as a contributor?

If you want to add yourself to contributors, keep in mind that contributors get
equal billing, and the first contributor usually wrote the whole article. Please
use your judgment when deciding if your contribution constitutes a substantial
addition or not.

## Building the site locally

Install Python. On macOS this can be done with [Homebrew](https://brew.sh/).

```sh
brew install python
```

Then clone two repos, install dependencies and run.

```sh
# Clone website
git clone https://github.com/adambard/learnxinyminutes-site
# Clone docs (this repo) nested in website
git clone https://github.com/<YOUR-USERNAME>/learnxinyminutes-docs ./learnxinyminutes-site/source/docs/

# Install dependencies
cd learnxinyminutes-site
pip install -r requirements.txt

# Run
python build.py
cd build
python -m http.server

# open http://localhost:8000/ in your browser of choice
```
