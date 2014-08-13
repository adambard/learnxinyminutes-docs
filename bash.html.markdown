---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
filename: LearnBash.sh
---

Bash is a name of the unix shell, which was also distributed as the shell for the GNU operating system and as default shell on Linux and Mac OS X.
Nearly all examples below can be a part of a shell script or executed directly in the shell.

[Read more here.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# First line of the script is shebang which tells the system how to execute
# the script: http://en.wikipedia.org/wiki/Shebang_(Unix)
# As you already figured, comments start with #. Shebang is also a comment.

# Simple hello world example:
echo Hello world!

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
echo '$VARIABLE'
# When you use the variable itself — assign it, export it, or else — you write
# its name without $. If you want to use variable's value, you should use $.
# Note that ' (single quote) won't expand the variables!

# String substitution in variables
echo ${VARIABLE/Some/A}
# This will substitute the first occurance of "Some" with "A"

# Substring from a variable
echo ${VARIABLE:0:7}
# This will return only the first 7 characters of the value

# Default value for variable
echo ${FOO:-"DefaultValueIfFOOIsMissingOrEmpty"}
# This works for null (FOO=), empty string (FOO=""), zero (FOO=0) returns 0

# Builtin variables:
# There are some useful builtin variables, like
echo "Last program return value: $?"
echo "Script's PID: $$"
echo "Number of arguments: $#"
echo "Scripts arguments: $@"
echo "Scripts arguments seperated in different variables: $1 $2..."

# Reading a value from input:
echo "What's your name?"
read NAME # Note that we didn't need to declare a new variable
echo Hello, $NAME!

# We have the usual if structure:
# use 'man test' for more info about conditionals
if [ $NAME -ne $USER ]
then
    echo "Your name is your username"
else
    echo "Your name isn't your username"
fi

# There is also conditional execution
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# To use && and || with if statements, you need multiple pairs of square brackets:
if [ $NAME == "Steve" ] && [ $AGE -eq 15 ]
then
    echo "This will run if $NAME is Steve AND $AGE is 15."
fi

if [ $NAME == "Daniya" ] || [ $NAME == "Zach" ]
then
    echo "This will run if $NAME is Daniya OR Zach."
fi

# Expressions are denoted with the following format:
echo $(( 10 + 5 ))

# Unlike other programming languages, bash is a shell — so it works in a context
# of current directory. You can list files and directories in the current
# directory with the ls command:
ls

# These commands have options that control their execution:
ls -l # Lists every file and directory on a separate line

# Results of the previous command can be passed to the next command as input.
# grep command filters the input with provided patterns. That's how we can list
# .txt files in the current directory:
ls -l | grep "\.txt"

# You can also redirect a command, input and error output.
python2 hello.py < "input.in"
python2 hello.py > "output.out"
python2 hello.py 2> "error.err"
# The output error will overwrite the file if it exists, if you want to
# concatenate them, use ">>" instead.

# Commands can be substituted within other commands using $( ):
# The following command displays the number of files and directories in the
# current directory.
echo "There are $(ls | wc -l) items here."

# The same can be done using backticks `` but they can't be nested - the preferred way
# is to use $( ).
echo "There are `ls | wc -l` items here."

# Bash uses a case statement that works similarly to switch in Java and C++:
case "$VARIABLE" in 
    #List patterns for the conditions you want to meet
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# for loops iterate for as many arguments given:
# The contents of $VARIABLE is printed three times.
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# They can also be used to act on files..
# This will run the command 'cat' on file1 and file2
for VARIABLE in file1 file2
do
    cat "$VARIABLE"
done

# ..or the output from a command
# This will cat the output from ls.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# while loop:
while [ true ]
do
    echo "loop body here..."
    break
done

# You can also define functions
# Definition:
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    return 0
}

# or simply
bar ()
{
    echo "Another way to declare functions!"
    return 0
}

# Calling your function
foo "My name is" $NAME

# There are a lot of useful commands you should learn:
# prints last 10 lines of file.txt
tail -n 10 file.txt
# prints first 10 lines of file.txt
head -n 10 file.txt
# sort file.txt's lines
sort file.txt
# report or omit repeated lines, with -d it reports them
uniq -d file.txt
# prints only the first column before the ',' character
cut -d ',' -f 1 file.txt
```
