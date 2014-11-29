---
tool: unix
filename: terminal.html.markdown
contributors:
    - ["Leo Rudberg", "https://github.com/LOZORD"]
    - ["Betsy Lorton", "https://github.com/schbetsy"]
lang: en-us
---

Here's a tutorial on how to use your terminal (bash, zsh, etc.).
This tutorial assumes you're using a Unix machine (Mac or Linux), and not Windows.

There is a tutorial on using [bash](http://learnxinyminutes.com/docs/bash/), but it focuses more on using bash for scripting and more advanced topics.

This tutorial will cover just the basics for navigation and other important tools.

### pwd
Usage:

```bash
$ pwd
```

This command will print your current (working) directory's path. For this tutorial, you can think of directories and files as the same thing.

Every path originates from root, identified by `/`. If you are the admin for your machine, you will have access to modify things in root.

You will also have a user path, identified by `~`. This is where your Desktop, Downloads, Music, and other common folders can be found.

`~` is just an alias to something like `/Users/<yourname>`.

All other path names are "relative" -- they can be expanded to something originating at root.

If you ever get lost, use `pwd` as a way to navigate your system!

### ls

Usage:

```bash
$ ls
```

This command lists all of the things (files, folders, links, etc.) that reside in your current directory.

You can pass flags to `ls` to make it do different things:

* `ls -l`: Output the results in a long format. This is a more organized way of looking at your current directory's contents.
* `ls -t`: Sort the contents my last-modified date. Most recent changes come first.
* `ls -R`: Recursively "`ls`" this folder and all of its sub-folders.

Flags can be combined. For example, see what `ls -lt` does.

### cd

Usage:

```bash
$ cd <path>
```

This command will change your current directory to the folder with whatever path (relative or absolute).

---> Start more work here.

### man

### cat

### head

### open

### clear

### cp

### mv

### rm

### grep

### find

### diff

### ssh

### scp

### sudo
