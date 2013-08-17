---

language: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
filename: LearnBash.sh

---

Bash is a name of the unix shell, which was also distributed as the shell for the GNU operating system and as default shell on Linux and Mac OS X.
Nearly all examples below can be a part of a shell script or executed directly in the shell.

[Read more here.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/sh
# First line of the script is shebang which tells the system how to execute the script: http://en.wikipedia.org/wiki/Shebang_(Unix)
# As you already figured, comments start with #. Shebang is also a comment.

# Simple hello world example:
echo Hello, world!

# Each command starts on a new line, or after semicolon:
echo 'This is the first line'; echo 'This is the second line'

# Declaring a variable looks like this:
VARIABLE="Some string"

# But not like this:
VARIABLE = "Some string" # Bash will decide that VARIABLE is a command he must execute and give an error because it couldn't be found.

# Using the variable:
echo $VARIABLE
echo "$VARIABLE"
# When you use the variable itself — assign it, export it, or else — you write it's name without $. If you want to use variable's value, you should use $.

# Reading a value from input:
echo "What's your name?"
read NAME # Note that we didn't need to declare new variable
echo Hello, $NAME!

# We have the usual if structure:
if true
then
	echo "This is expected"
else
	echo "And is was not"
fi

# Expressions are denoted with the following format:
echo $(( 10 + 5 ))

```