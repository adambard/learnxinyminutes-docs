---
language: restructured text
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
filename: restructuredtext.rst
---

RST is a file format formely created by Python community to write documentation (and so, is part of Docutils).

RST files are simple text files with lightweight syntax (comparing to HTML).


## Installation

To use Restructured Text, you will have to install [Python](http://www.python.org) and the `docutils` package.

`docutils` can be installed using the commandline:

```bash
$ easy_install docutils
```

If your system has `pip`, you can use it too:

```bash
$ pip install docutils
```


## File syntax

A simple example of the file syntax:

```rst
.. Lines starting with two dots are special commands. But if no command can be found, the line is considered as a comment

=========================================================
Main titles are written using equals signs over and under
=========================================================

Note that there must be as many equals signs as title characters.

Title are underlined with equals signs too
==========================================

Subtitles with dashes
---------------------

And sub-subtitles with tildes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can  put text in *italic* or in **bold**, you can "mark" text as code with double backquote ``: ``print()``.

Lists are as simple as in Markdown:

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

More complex tabless can be done easily (merged columns and/or rows) but I suggest you to read the complete doc for this :)

There are multiple ways to make links:

- By adding an underscore after a word : Github_ and by adding the target URL after the text (this way has the advantage to not insert unnecessary URLs inside readable text).
- By typing a full comprehensible URL : https://github.com/ (will be automatically converted to a link)
- By making a more Markdown-like link: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```


## How to Use It

RST comes with docutils where you have `rst2html`, for example:

```bash
$ rst2html myfile.rst output.html
```

*Note : On some systems the command could be rst2html.py*

But there are more complex applications that use the RST format:

- [Pelican](http://blog.getpelican.com/), a static site generator
- [Sphinx](http://sphinx-doc.org/), a documentation generator
- and many others


## Readings

- [Official quick reference](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
