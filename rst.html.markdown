---
language: restructured text
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
filename: restructuredtext.rst
---

RST is file format formely created by Python community to write documentation (and so, is part of Docutils).

RST files are simple text files with lightweight syntaxe (comparing to HTML).


## Installation

To use Restructured Text, you will have to install [Python](http://www.python.org) and the `docutils` package.

`docutils` can be installed using the commandline:

```bash
$ easy_install docutils
```

If your system have `pip`, you can use it too:

```bash
$ pip install docutils
```


## File syntaxe

A simple example of the file syntax:

```rst
.. Line with two dotes are special commands. But if no command can be found, the line is considered as a comment

=========================================================
Main titles are written using equals signs over and under
=========================================================

Note that theire must be as many equals signs as title characters.

Title are underlined with equals signs too
==========================================

Subtitles with dashes
---------------------

And sub-subtitles with tilde
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can  put text in *italic* or in **bold**, you can "mark" text as code with double backquote ``: ``print()``.

Lists are as simple as markdown:

- First item
- Second item
    - Sub item

or

* First item
* Second item
    * Sub item

Tables are really easy to write:

=========== ========
Country     Capital
=========== ========
France      Paris
Japan       Tokyo
=========== ========

More complexe tabless can be done easily (merged columns and/or rows) but I suggest you to read the complete doc for this :)

Their is multiple ways to make links:

- By adding an underscore after a word : Github_ and by adding the target after the text (this have the advantage to not insert un-necessary URL inside the readed text).
- By typing a full comprehensible URL : https://github.com/ (will be automatically converted in link)
- By making a more "markdown" link: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```


## How to use it

RST comes with docutils in which you have `rst2html` for exemple:

```bash
$ rst2html myfile.rst output.html
```

*Note : On some systems the command could be rst2html.py*

But their is more complexe applications that uses RST file format:

- [Pelican](http://blog.getpelican.com/), a static site generator
- [Sphinx](http://sphinx-doc.org/), a documentation generator
- and many others


## Readings

- [Official quick reference](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
