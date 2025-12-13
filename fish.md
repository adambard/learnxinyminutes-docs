---
name: fish
contributors:
    - ["MySurmise", "https://github.com/MySurmise"]
    - ["Geo Maciolek", "https://github.com/GeoffMaciolek"]
filename: learn.fish
---

Fish (**f**riendly **i**nteractive **sh**ell) is the name of an exotic shell. That is a shell with a syntax that is derived from neither the Bourne-Shell nor the C-Shell.

The advantage of fish is that many features that you want in a modern shell come out-of-the-box, so you don't have to install additional software like zsh and oh-my-zsh.

Examples of these features are autosuggestions, 24-bit colors, Man Page Completions (meaning fish automatically parses your man pages and suggests additional options for your commands) or the ability to make options through a web page (when a GUI is installed).

It was released in February 2005.

- [Read more](https://fishshell.com/docs/current/language.html)
- [Installation guide](https://github.com/fish-shell/fish-shell#getting-fish)


## Guide

Be sure you have the newest fish shell. This guide is guaranteed to work with v4.2.0 (older/newer versions should probably work too).
To test, type:

```
> fish -v
```

To start the fish shell, type:

```
> fish
```

to exit, type:

```
> exit
```

or press <kbd>Ctrl + D</kbd>

Now, right out of the gate, there's one annoying thing in fish. It's the welcome message. Who needs that, right? When your shell is started, just type:

```
> set -U fish_greeting ""
```

If you want to execute a single command written in bash, without switching to that shell, you can type:

```
> bash -c 'echo "fish is better than bash"'
```

In fish, you can use single or double quotes. Note that they have different semantics and cannot be used interchangeably in some cases (more on that later).
The escape character is a `\`

You can change your configuration of fish either by editing the config file

```
> vim ~/.config/fish/config.fish
```

or by opening the aforementioned web settings:

```
> fish_config
```

Adding something to your fish PATH Variable is easy:

```
> fish_add_path ~/cowsay
```

Can you do that with bash, huh? No, you always have to look it up... It's just that easy!

But there's more. Most fish-specific commands start, you guessed it, with 'fish'. Just type in `fish` and press <kbd>TAB</kbd>. And there you have one of the many cool features of fish: The autocompletion that **just works.**
Now you can navigate the list of completions with <kbd>TAB</kbd>, <kbd>Shift + TAB</kbd> and your Arrow-Keys <kbd>←</kbd><kbd>↑</kbd><kbd>→</kbd><kbd>↓</kbd>. Pressing <kbd>Ctrl + s</kbd> will open up a fuzzy-search menu that you can use to filter the list.
If you would rather drop into the search menu in one shot, press <kbd>Shift + TAB</kbd> instead of <kbd>TAB</kbd>.

To get help, contact your local psychiatrist or type `man`. That will bring up the manual for that command, for example:

```
> man set
```

Note that there are many man pages with identical names that are specific to your current shell.
For example, running `man set` from bash will give you the man page about bash's `set`.

If you finally tried fish, you can see something other in fish that's really cool. Everything has cool colors, if you type in something wrong, it is red, without even executing, if you put something in quotes, you see where it ends and why that quote doesn't work, because there's another quotation mark in the quote at position 26.

fish has even more cool things, like wildcards.
For example, type

```
> ls *.fish
```

That will list all fish files in your current directory.

You can have multiple wildcards per command or even a recursive wildcard, `**`, which basically means it includes files and directories, that fit.
For example, the following command would return (in your case):

```
> ls ~/images/**.jpg

~/images/nudes/pewdiepie.jpg
~/images/nudes/peppa.jpg
~/images/screenshots/2020-42-69.jpg
~/images/omegalul.jpg
```

Of course, you can also pipe the output of a command to another command

```
> echo sick egg, nadia. no u do really goofy shit.   | grep [udense]
```

write to a file:

```
> echo This\ is\ text > file.txt
```

(noticed the escape character?)
Append to a file:

```
>echo This\ is\ a\ line >> file.txt
>echo This\ is\ a\ second\ line >> file.txt
```

For Autocompletion, just always press <kbd>TAB</kbd>. You will be surprised how many things fish knows.

Like many other shells, fish records your command history and stores it in a dedicated file (e.g. `~/.local/share/fish/fish_history`).
Pressing <kbd>Ctrl + r</kbd> opens up fuzzy-search menu with your history.
Use Up/Down arrow keys to select previous/next history entry. If you press the arrow key with some text
entered on the command line, fish will suggest history entries containing that text. For example,
typing `grep --` and pressing <kbd>↑</kbd> will match `ls ~/dir | grep --ignore-case` and will not
show irrelevant entries.
If you need to enter a sensitive command, prefix it with one or more spaces. This instructs fish not to record it.

And, of course, about the magic autosuggestions: fish suggests commands as you type, based on command history, completions, and valid file paths. As you start typing commands, you will see a suggestion appear after the cursor, in a muted gray color. To accept it, press <kbd>→</kbd> or <kbd>Ctrl + f</kbd>.

To use variables, just type `$VAR`, like in bash.

```
> echo "My home is $HOME"
My home is /home/myuser
```

Here comes a difference between single and double quotes. If you use a variable in single quotes, it will not substitute it.

```
> echo 'My home is $HOME'
My home is $HOME
```

More on variables later.

To execute two commands, separate them with `;`

```
> echo Lol; echo this is fun
```

The status code of the last command is stored in `$status`

You can use `&&` for two commands that depend on each other.

```
> set var lol && echo $var
```

`&&` can also be used to write multiple commands across multiple lines:

```
> set var lol &&    # press Enter here
      echo $var
```

If you rather want to split a *single* command across multiple lines, use a backslash

```
whoami \    # press Enter here
    --version
```

You can also use `and`  which executes if the previous command was successful,
`or` which executes if the previous command was not successful, and `not`
which inverts the exit status of a command.

For example:

```
> if not echo It's very late I should not waste my time with this
      echo Nobody heard you
  end
```

(You can of course do all of that in the shell)

---
Now let's start with the scripting part of fish.

As with every shell, you can not only execute commands in the shell, but also as files, saved as a  `.fish` file.
(You can also execute `.sh` files with fish syntax, but I always use `.fish` for fish-syntax scripts to distinguish them from bash script files)

```fish
# This is a comment in fish.
#
# If you execute a file without specifying an interpreter,
# meaning the software that runs your script, you need to tell the shell,
# where that interpreter is.
# For fish you just add the following comment as the first line in your script:

#!/bin/fish

# When executing via e.g. fish /path/to/script.fish
# you don't need that, because you specified fish as an interpreter

# Let's start with variables.
# for use inside a program, you can use the syntax
set name 'My Variable'

# Use...
set -x name value
# to eXport, or
set -e name
# to Erase

# A variable set with a space doesn't get sent as two arguments, but as one, as
# you would expect it.
set turtlefolder 'Turtle Folder'
mkdir $turtlefolder

# This will create one folder, as expected, not two, like in bash...
# Who would even want that? tHiS iS a fEaTurE, nOt a bUg...

# You can even have lists as variables. This actually makes sense, because if
# you want to have a variable that would create two folders, you just give mkdir a
# list of your foldernames.

# you can then count the entries in that list with:
count $PATH

# Not only is everything awesome, but in fish, everything is also a list.
# So $PWD for example is a list of length 1.
# To make a list, just give the set command multiple arguments:
set list entry1 entry2 entry3

# that way you can also append something to an existing variable:
set PATH $PATH ~/cowsay/

# But, as previously mentioned, we also have a simpler way to do that specifically in fish.
# As with every Array/List, you can access it with
$listvar[2]

# there's also ranges with
$listvar[1..5]

# and you can use negative numbers like
$listvar[-1]
# e.g to access the last element.

# You can also do fancy cartesian products when you combine two list variables:
set a 1 2 3
set 1 a b c
echo $a$1
# will output : 1a 2a 3a 1b 2b 3b 1c 2c 3c

# Of course, if you separate them, it will see them as two separate arguments
# and echo them one after the other. THAT is expected behavior @bash.

# There are also other useful things, like command substitutions.
# For example, when you want to output the returns of two commands in one line,
# in bash you you would do that with
# echo "`ls` is in $PWD"
# or
# echo "$(ls) is in $PWD"

# If you ask me, that's unnecessary. I always type in the wrong apostrophe.
# Why not just use two parenthesis, like in fish?
echo (ls) is in $PWD

# Yep, that easy. And thanks to fish's highlighting, you can instantly see
# if you typed it in correctly.

# And, as you would expect, if you ask me, your commands don't work in quotes.
# I mean why bash? Ok I'll stop now. But in fish, just do:
echo (ls)" is in $PWD"
# or
set myvar "The file"(ls -a)" is in the directory $PWD"
# will make a List with the string and all files. Try it out. Isn't that cool?

# And to separate these variables in separate arguments, just put a space between them:

set myvar "The files" (ls -a) " are in the directory $PWD"

# Passing the output of a command to another command that only accepts files can
# be done using process substitution.
# In bash, you use <(command):
# diff <(ls dir1) <(ls dir2)

# fish uses (command | psub) for that:
diff (ls dir1 | psub) (ls dir2 | psub)

# There's also if, else if, else
if grep -q fish /etc/shells
    echo Found fish
else if grep -q bash /etc/shells
    echo Found bash
else
    echo Got nothing
end

# A little weird is that you compare stuff with one = sign, of course because
# we don't need it to set variables, but still... and the keyword "test":
if test $var = "test"
    echo yes
else
    echo no
end

# Of course, there's also switch case with
switch $OS
case Linux
    echo "you're good"
case Windows
    echo "install Gentoo"
case Arch
    echo "I use arch btw"
case '*'
    echo "what OS is $OS, please?"
end


# Functions in fish get their arguments through the $argv variable.
# The syntax is following:

function print
    echo $argv
end

# There are also events, like the "fish_exit"-event (What may that be, hmm?).

# You can use them by adding them to the function definition:

function on_exit --on-event fish_exit
    echo fish is now exiting
end

# find events with the command
functions --handlers


# You can use the functions command to learn more about, well, functions.
# For example you can print the source code of every function:
functions cd
functions print
# or get the names of all functions:
functions

# There are while loops, of course:
while test $var = lol
    echo lol
end

# for loops (with wildcards, they are even cooler):
for image in *.jpg
    echo $image
end

# There's an equivalent to the range(0, 5) in Python, so you can also do
# the standard for loops with numbers:

set files (ls)
for number in (seq 10)
    echo "$files[$number] is file number $number"
end

# Cool!

# The bashrc equivalent is not fishrc, but the previously mentioned config.fish
# file in ~/.config/fish/
# To add a function to fish, though, you should create a simple .fish file in
# that directory.
# Don't just paste that function in the config.fish. That's ugly.

# If you have more, just add it, but those are the most important basics.
```
