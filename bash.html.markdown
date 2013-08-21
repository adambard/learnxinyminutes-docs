---

category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
filename: LearnBash.sh

---

Bash is a name of the unix shell, which was also distributed as the shell for the GNU operating system and as default shell on Linux and Mac OS X.
Nearly all examples below can be a part of a shell script or executed directly in the shell.

[Read more here.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/sh
# First line of the script is shebang which tells the system how to execute
# the script: http://en.wikipedia.org/wiki/Shebang_(Unix)
# As you already figured, comments start with #. Shebang is also a comment.

# Simple hello world example:
echo Hello, world!

# Each command starts on a new line, or after semicolon:
echo 'This is the first line'; echo 'This is the second line'

# Declaring a variable looks like this:
VARIABLE="Some string"

# But not like this:
VARIABLE = "Some string"
# Bash will decide that VARIABLE is a command it must execute and give an error
# because it couldn't be found.

# Using the variable:
echo $VARIABLE
echo "$VARIABLE"
# When you use the variable itself — assign it, export it, or else — you write
# its name without $. If you want to use variable's value, you should use $.

# Reading a value from input:
echo "What's your name?"
read NAME # Note that we didn't need to declare new variable
echo Hello, $NAME!

# We have the usual if structure:
if true
then
    echo "This is expected"
else
    echo "And this is not"
fi

# Expressions are denoted with the following format:
echo $(( 10 + 5 ))

# Unlike other programming languages, bash is a shell — so it works in a context
# of current directory. You can list files and directories in the current
# directories with ls command:
ls

# These commands have options that control their execution:
ls -l # Lists every file and directory on a separate line

# Results of the previous command can be passed to the next command as input.
# grep command filters the input with provided patterns. That's how we can list
# txt files in the current directory:
ls -l | grep "\.txt"

# Commands can be substitued within other commands using $( ):
# The following command displays the number of files and directories in the
# current directory.
echo "There are $(ls | wc -l) items here."

# Bash uses a case statement that works similarily to switch in Java and C++:
case "$VARIABLE"
in
    #List patterns for the conditions you want to meet
    0) echo "There is a zero."
    1) echo "There is a one."
    *) echo "It is not null."
esac

#For loops iterate for as many arguments given:
#The contents of var $VARIABLE is printed three times.
for $VARIABLE in x y z
do
    echo "$VARIABLE"
done

# Whenever a command executes it will return an exit status.
# Generally, 0 denotes success and any other exit status signals
# that the command failed. This concept is very similar to boolean true and false.
# The $? variable automatically contatins the exit status of the last command:
echo "Hello, World!"
Hello, World

echo $?
0
# Success!

# The test command uses exit statuses similarily to boolean expressions (true/false).
# Test commands are very useful for checking arguments the user
# inputs to ensure that the schell script program works correctly.
# The test command syntax is simply [ -options file ]. The spaces are MANDATORY.

if [ -f $VARIABLE ]
then
    echo "This is a file."
else
    echo "This is not a file."
fi

# This simply checks if a given variable is a file (-f), and informs the user appropiately.
# There are many options for test, so feel free to search them online or through man.
```
