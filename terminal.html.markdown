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

### man

Usage:

```bash
$ man <command>
```

This command will present the manual page for the corresponding command, if it exists.
To naviate the man page, you can use arrow keys. Use `/` to searching.
`q` quits the man page and returns you back to your shell.

All of the commands here have a man page!

### cat

Usage:

```bash
$ cat <filename>
```

There are more advanced uses for `cat`, but it is mainly used for printing a
file to the terminal. Some shells come with `tac` that prints a file in reverse!

### clear

Usage:

```bash
$ clear
```

Use `clear` to get a fresh terminal screen.

### cp

Usage:

```bash
$ cp <source(s)> <destination>
```

`cp` __copies__ files from the given source(s) to the given destination.
Note, these are separate, so editing one version won't effect the other.

* `cp -r sourceDir dest`: will recursively copy the contents of directory
  `sourceDir` to the destination `dest`

### mv

```bash
$ mv <source(s)> <destination>
```

`mv` is similar to `cp`, although it basically __moves__ source(s) to destination.
One of `mv`'s frequent uses is _renaming_ a file.

### rm

```bash
$ rm <files>
```

`rm` is used to __remove__ things. Be careful! You __cannot__ undo `rm` commands!

* `rm -f`: Delete first, ask questions never. I.e. __force__ the deletion.
* `rm -r`: Recursively remove. This is helpful when deleting directories.

### grep

```bash
$ grep <pattern> <dir>
```

`grep` is one of the most powerful tools you can use in development. It searches
`dir` for any occurance of `pattern`. This is basically your terminal's what to
find strings in a file! If you know how to use regular expressions, your
`grep`ping will be much more powerful, too.

* `grep <pattern> <dir> -r`: Search recursively (look in `dir`'s subfolders).
* `grep <pattern> <dir> -n`: Give line numbers for occurences.
* `grep <pattern> <dir> -I`: Ignore binary files. This helps is avoiding data
  you probably don't want to look at.

### diff

```bash
$ diff <ver1> <ver2>
```

`diff` allows you to compare two different files.

* `diff -y <ver1> <ver2>`: Compare files side by side.

### ssh

```bash
$ ssh my_username@some.external.server.computer
```

`ssh` allows you to log into another computer remotely. To leave the remote
session, just enter `exit`.

### scp

```bash
$ scp <src> <dest>
```

`scp` is a way to copy files between computers. It behaves similarly to regular
`cp`.

### sudo

```bash
$ sudo <command>
```

`sudo` allows you to run a command as the superuser, if you have the privilege.
For example, if something isn't installing, try running the install command
using sudo.

* `sudo su`: Changes your privilege level to `superuser`.
