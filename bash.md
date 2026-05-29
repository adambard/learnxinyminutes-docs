---
name: Bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
    - ["Jonathan Wang", "https://github.com/Jonathansw"]
    - ["Leo Rudberg", "https://github.com/LOZORD"]
    - ["Betsy Lorton", "https://github.com/schbetsy"]
    - ["John Detter", "https://github.com/jdetter"]
    - ["Harry Mumford-Turner", "https://github.com/harrymt"]
    - ["Martin Nicholson", "https://github.com/mn113"]
    - ["Mark Grimwood", "https://github.com/MarkGrimwood"]
    - ["Emily Grace Seville", "https://github.com/EmilySeville7cfg"]
    - ["Ilyas B.", "https://github.com/Ily83"]
filename: LearnBash.sh
translators:
    - ["Dimitri Kokkonis", "https://github.com/kokkonisd"]
---

Bash is a name of the unix shell, which was also distributed as the shell
for the GNU operating system and as the default shell on most Linux distros.
This tutorial was tested on Bash v5.3.
Nearly all examples below can be a part of a shell script
or executed directly in the shell.

```bash
#!/usr/bin/env bash
# First line of the script is the shebang which tells the system how to execute
# the script: https://en.wikipedia.org/wiki/Shebang_(Unix)
# As you already figured, comments start with #. Shebang is also a comment.

# Simple hello world example:
echo "Hello world!" # => Hello world!

# Each command starts on a new line, or after a semicolon:
echo "This is the first command"; echo "This is the second command"
# => This is the first command
# => This is the second command

# Declaring a variable looks like this:
variable="Some string"

# But not like this:
variable = "Some string" # => returns error "variable: command not found"
# Bash will decide that `variable` is a command it must execute and give an error
# because it can't be found.

# Nor like this:
variable= "Some string" # => returns error: "Some string: command not found"
# Bash will decide that "Some string" is a command it must execute and give an
# error because it can't be found. In this case the "variable=" part is seen
# as a variable assignment valid only for the scope of the "Some string"
# command.

# Using the variable:
echo "$variable" # => Some string
echo '$variable' # => $variable
# When you use a variable itself, assign it, export it, or else, you write
# its name without $. If you want to use the variable's value, you should use $.
# Note that ' (single quote) won't expand variables!
# You can write variables without surrounding double quotes but it's not
# recommended due to how Bash handles variables with spaces in them.

# Parameter expansion ${...}:
echo "${variable}" # => Some string
# This is a simple usage of parameter expansion such as two examples above.
# Parameter expansion gets a value from a variable.
# It "expands" or prints the value.
# During the expansion time the value or parameter can be modified.
# Below are other modifications that add onto this expansion.

# String substitution in variables:
echo "${variable/Some/A}" # => A string
# This will substitute the first occurrence of "Some" with "A".
# Prepend a / before the search string to substitute every instance of it.
variable="Some string. Some character"
echo "${variable//Some/A}" # => A string. A character
# This will substitute every occurrence of "Some" with "A".

# Substring from a variable:
length=7
echo "${variable:0:length}" # => Some st
# This will return only the first 7 characters of the value
echo "${variable: -5}" # => tring
# This will return the last 5 characters (note the space before -5).
# The space before minus is mandatory here.

# String length:
echo "${#variable}" # => 11

# Indirect expansion:
other_variable="variable"
echo ${!other_variable} # => Some string
# This will expand the value of `other_variable`.

# The default value for variable:
echo "${foo:-"DefaultValueIfFooIsMissingOrEmpty"}"
# => DefaultValueIfFooIsMissingOrEmpty
# This works for null (foo=) and empty string (foo=""); zero (foo=0) returns 0.
# Note that it only returns default value and doesn't change variable value.

# Declare an array with 6 elements:
array=(one two three four five six)
# Print the first element:
echo "${array[0]}" # => "one"
# Print all elements:
echo "${array[@]}" # => "one two three four five six"
# Print the number of elements:
echo "${#array[@]}" # => "6"
# Print the number of characters in third element
echo "${#array[2]}" # => "5"
# Print 2 elements starting from fourth:
echo "${array[@]:3:2}" # => "four five"
# Print all elements each of them on new line.
for item in "${array[@]}"; do
    echo "$item"
done

# Built-in variables:
# There are some useful built-in variables, like:
echo "Last program's return value: $?"
echo "Script's PID: $$"
echo "Number of arguments passed to script: $#"
echo "All arguments passed to script: $@"
echo "Script's arguments separated into different variables: $1 $2..."
echo "Current script file: $BASH_SOURCE"
echo "Current function name: $FUNCNAME"
echo "Current line number: $LINENO"
echo "A random number: $RANDOM"             # random int between 0–32767
echo "Seconds since shell started: $SECONDS"
echo "Unix epoch time: $EPOCHSECONDS"       # Bash 5+

# $IFS, Internal Field Separator, controls word splitting (default: space/tab/newline)
IFS=',' read -ra fields <<< "a,b,c"
echo "${fields[1]}" # => b

# $PIPESTATUS, array of exit codes from the last pipeline
false | true | false
echo "${PIPESTATUS[@]}" # => 1 0 1

# Brace Expansion {...}
# used to generate arbitrary strings:
echo {1..10} # => 1 2 3 4 5 6 7 8 9 10
echo {a..z} # => a b c d e f g h i j k l m n o p q r s t u v w x y z
# This will output the range from the start value to the end value.
# Note that you can't use variables here:
from=1
to=10
echo {$from..$to} # => {$from..$to}

# Now that we know how to echo and use variables,
# let's learn some of the other basics of Bash!

# Our current directory is available through the command `pwd`.
# `pwd` stands for "print working directory".
# We can also use the built-in variable `$PWD`.
# Observe that the following are equivalent:
echo "I'm in $(pwd)" # execs `pwd` and interpolates output
echo "I'm in $PWD" # interpolates the variable

# If you get too much output in your terminal, or from a script, the command
# `clear` clears your screen:
clear
# Ctrl-L also works for clearing output.

# Reading a value from input:
echo "What's your name?"
read name
# Note that we didn't need to declare a new variable.
echo "Hello, $name!"

# We have the usual if structure.
# Condition is true if the value of $name is not equal to the current user's login username:
if [[ "$name" != "$USER" ]]; then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
fi

# To use && and || with if statements, you need multiple pairs of square brackets:
read age
if [[ "$name" == "Steve" ]] && [[ "$age" -eq 15 ]]; then
    echo "This will run if $name is Steve AND $age is 15."
fi

if [[ "$name" == "Daniya" ]] || [[ "$name" == "Zach" ]]; then
    echo "This will run if $name is Daniya OR Zach."
fi

# To check if a string is empty or not set use -z and -n to check if it is NOT empty
if [[ -z "$name" ]]; then
    echo "Name is unset"
fi

# There are other comparison operators for numbers listed below:
# -ne - not equal
# -lt - less than
# -gt - greater than
# -le - less than or equal to
# -ge - greater than or equal to

# There is also the `=~` operator, which tests a string against the Regex pattern:
email=me@example.com
if [[ "$email" =~ [a-z]+@[a-z]{2,}\.(com|net|org) ]]
then
    echo "Valid email!"
fi

# There is also conditional execution
echo "Always executed" || echo "Only executed if first command fails"
# => Always executed
echo "Always executed" && echo "Only executed if first command does NOT fail"
# => Always executed
# => Only executed if first command does NOT fail

# A single ampersand & after a command runs it in the background. A background command's
# output is printed to the terminal, but it cannot read from the input.
sleep 30 &
# List background jobs
jobs # => [1]+  Running                 sleep 30 &
# Bring the background job to the foreground
fg
# Ctrl-C to kill the process, or Ctrl-Z to pause it
# Resume a background process after it has been paused with Ctrl-Z
bg
# Kill job number 2
kill %2
# %1, %2, etc. can be used for fg and bg as well

# Redefine command `ping` as alias to send only 5 packets
alias ping='ping -c 5'
# Escape the alias and use command with this name instead
\ping localhost #\ping 192.168.1.1 
# Print all aliases
alias -p

# Expressions are denoted with the following format:
echo $(( 10 + 5 )) # => 15

# Unlike other programming languages, bash is a shell so it works in the context
# of a current directory. You can list files and directories in the current
# directory with the ls command:
ls # Lists the files and subdirectories contained in the current directory

# This command has options that control its execution:
ls -l # Lists every file and directory on a separate line
ls -t # Sorts the directory contents by last-modified date (descending)
ls -R # Recursively `ls` this directory and all of its subdirectories

# Results (stdout) of the previous command can be passed as input (stdin) to the next command
# using a pipe |. Commands chained in this way are called a "pipeline", and are run concurrently.
# The `grep` command filters the input with provided patterns.
# That's how we can list .txt files in the current directory:
ls -l | grep "\.txt"

# Use `cat` to print files to stdout:
cat file.txt

# We can also read the file using `cat`:
Contents=$(cat file.txt)
# "\n" prints a new line character
# "-e" to interpret the newline escape characters as escape characters
echo -e "START OF FILE\n$Contents\nEND OF FILE"
# => START OF FILE
# => [contents of file.txt]
# => END OF FILE

# Use `cp` to copy files or directories from one place to another.
# `cp` creates NEW versions of the sources,
# so editing the copy won't affect the original (and vice versa).
# Note that it will overwrite the destination if it already exists.
cp srcFile.txt clone.txt
cp -r srcDirectory/ dst/ # recursively copy

# Look into `rsync` or `sftp` if you plan on exchanging files between computers.
# `rsync` is the modern standard for fast, resumable, delta-synced transfers.
# `sftp` is more interactive (like an FTP session over SSH).
# Note: `scp` still works but is deprecated by OpenSSH in favor of `sftp`.

# Use `mv` to move files or directories from one place to another.
# `mv` is similar to `cp`, but it deletes the source.
# `mv` is also useful for renaming files!
mv s0urc3.txt dst.txt # sorry, l33t hackers...

# Since bash works in the context of a current directory, you might want to
# run your command in some other directory. We have cd for changing location:
cd ~    # change to home directory
cd      # also goes to home directory
cd ..   # go up one directory
        # (^^say, from /home/username/Downloads to /home/username)
cd /home/username/Documents   # change to specified directory
cd ~/Documents/..    # now in home directory (if ~/Documents exists)
cd -    # change to last directory
# => /home/username/Documents

# Use subshells to work across directories
(echo "First, I'm here: $PWD") && (cd someDir; echo "Then, I'm here: $PWD")
pwd # still in first directory

# Use `mkdir` to create new directories.
mkdir myNewDir
# The `-p` flag causes new intermediate directories to be created as necessary.
mkdir -p myNewDir/with/intermediate/directories
# if the intermediate directories didn't already exist, running the above
# command without the `-p` flag would return an error

# You can redirect command input and output (stdin, stdout, and stderr)
# using "redirection operators". Unlike a pipe, which passes output to a command,
# a redirection operator has a command's input come from a file or stream, or
# sends its output to a file or stream.

# Read from stdin until ^EOF$ and overwrite hello.py with the lines
# between "EOF" (which are called a "here document"):
cat > hello.py << EOF
#!/usr/bin/env python
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF
# Variables will be expanded if the first "EOF" is not quoted

# Use <<- to strip leading tabs (useful inside indented functions/if blocks):
if true; then
	cat <<- EOF
	This text can be indented with tabs.
	The leading tabs will be stripped from the output.
	EOF
fi

# Run the hello.py Python script with various stdin, stdout, and
# stderr redirections:
python hello.py < "input.in" # pass input.in as input to the script

python hello.py > "output.out" # redirect output from the script to output.out

python hello.py 2> "error.err" # redirect error output to error.err

python hello.py > "output-and-error.log" 2>&1
# redirect both output and errors to output-and-error.log
# &1 means file descriptor 1 (stdout), so 2>&1 redirects stderr (2) to the current
# destination of stdout (1), which has been redirected to output-and-error.log.

python hello.py > /dev/null 2>&1
# redirect all output and errors to the black hole, /dev/null, i.e., no output

# The output error will overwrite the file if it exists,
# if you want to append instead, use ">>":
python hello.py >> "output.out" 2>> "error.err"

# Overwrite output.out, append to error.err, and count lines:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Run a command and print its file descriptor (e.g. /dev/fd/123)
# see: man fd
echo <(echo "#helloworld")

# Overwrite output.out with "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Cleanup temporary files verbosely (add '-i' for interactive)
# WARNING: `rm` commands cannot be undone
rm -v output.out error.err output-and-error.log
rm -r tempDir/ # recursively delete
# You can install the `trash-cli` Python package to have `trash`
# which puts files in the system trash and doesn't delete them directly
# see https://pypi.org/project/trash-cli/ if you want to be careful

# Commands can be substituted within other commands using $( ):
# The following command displays the number of files and directories in the
# current directory.
echo "There are $(ls | wc -l) items here."

# The same can be done using backticks `` but they can't be nested -
# the preferred way is to use $( ).
echo "There are `ls | wc -l` items here."

# Bash uses a `case` statement that works similarly to switch in Java and C++:
case "$Variable" in
    # List patterns for the conditions you want to meet
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;  # match everything
esac

# `for` loops iterate for as many arguments given:
# The contents of $Variable is printed three times.
for Variable in {1..3}
do
    echo "$Variable"
done
# => 1
# => 2
# => 3


# Or write it the "traditional for loop" way:
for ((a=1; a <= 3; a++))
do
    echo $a
done
# => 1
# => 2
# => 3

# They can also be used to act on files..
# This will run the command `cat` on file1 and file2
for Variable in file1 file2
do
    cat "$Variable"
done

# ..or the output from a command
# This will `cat` the output from `ls`.
for Output in $(ls)
do
    cat "$Output"
done

# Bash can also accept patterns, like this to `cat`
# all the Markdown files in current directory
for Output in ./*.markdown
do
    cat "$Output"
done

# while loop:
while [ true ]
do
    echo "loop body here..."
    break
done
# => loop body here...

count=0
while [[ "count" -lt 3 ]]; do
    echo "count is $count"
    ((count++))
done
# => count is 0
# => count is 1
# => count is 2

# same with "until loop", runs while the condition is FALSE (opposite of while):
count=0
until [[ "$count" -ge 3 ]]; do
    echo "count is $count"
    (( count++ ))
done
# => count is 0
# => count is 1
# => count is 2

# You can also define functions
# Definition:
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    returnValue=0    # Variable values can be returned
    return $returnValue
}
# Call the function `foo` with two arguments, arg1 and arg2:
foo arg1 arg2
# => Arguments work just like script arguments: arg1 arg2
# => And: arg1 arg2...
# => This is a function
# Return values can be obtained with $?
resultValue=$?
# More than 9 arguments are also possible by using braces, e.g. ${10}, ${11}, ...

# or simply
bar ()
{
    echo "Another way to declare functions!"
    return 0
}
# Call the function `bar` with no arguments:
bar # => Another way to declare functions!

# Calling your function
foo "My name is" $Name

# `local` makes a variable scoped to a function (otherwise it's global):
greet() {
    local greeting="Hi"
    echo "$greeting, $1!"
}
greet "World"  # => Hi, World!
echo "$greeting" # => (empty, variable was local to the function)

# `readonly` prevents a variable from being changed:
readonly PI=3.14159
# PI=3 # => error: PI: readonly variable
# note: Avoid using readonly in the first place if the value might need to change.

# `declare` lets you set variable attributes:
declare -i num=42       # integer, arithmetic is automatic
num+=10
echo "$num"             # => 52  (not "4210")

declare -l lower="HELLO"
echo "$lower"           # => hello  (forced lowercase)

declare -u upper="hello"
echo "$upper"           # => HELLO  (forced uppercase)

declare -r constant="can't change me"  # same as readonly

# Namerefs (Bash 4.3+), a variable that references another variable by name:
declare -n ref=variable
echo "$ref"  # => Some string (the value of $variable)
ref="New value"
echo "$variable"  # => New value (the original variable was changed)

# mapfile / readarray, read lines into an indexed array:
mapfile -t lines < file.txt
echo "${lines[0]}"   # first line
echo "${#lines[@]}"  # total number of lines
# -t strips trailing newlines from each element

# There are a lot of useful commands you should learn:
# prints last 10 lines of file.txt
tail -n 10 file.txt

# prints first 10 lines of file.txt
head -n 10 file.txt

# print file.txt's lines in sorted order
sort file.txt

# report or omit repeated lines, with -d it reports them
uniq -d file.txt

# prints only the first column before the ',' character
cut -d ',' -f 1 file.txt

# replaces every occurrence of 'okay' with 'great' in file.txt
# (regex compatible)
sed -i 's/okay/great/g' file.txt
# be aware that this -i flag means that file.txt will be changed
# -i or --in-place erase the input file (use --in-place=.backup to keep a back-up)

# print to stdout all lines of file.txt which match some regex
# The example prints lines which begin with "foo" and end in "bar"
grep "^foo.*bar$" file.txt

# pass the option "-c" to instead print the number of lines matching the regex
grep -c "^foo.*bar$" file.txt

# Other useful options are:
grep -r "^foo.*bar$" someDir/ # recursively `grep`
grep -n "^foo.*bar$" file.txt # give line numbers
grep -rI "^foo.*bar$" someDir/ # recursively `grep`, but ignore binary files

# perform the same initial search, but filter out the lines containing "baz"
grep "^foo.*bar$" file.txt | grep -v "baz"

# if you literally want to search for the string,
# and not the regex, use `grep -F` (or the deprecated `fgrep`)
grep -F "foobar" file.txt

# The `trap` command allows you to execute a command whenever your script
# receives a signal. Here, `trap` will execute `rm` if it receives any of the
# three listed signals.
trap "rm $TEMP_FILE; exit" SIGHUP SIGINT SIGTERM

# `sudo` is used to perform commands as the superuser
# usually it will ask interactively the password of superuser
NAME1=$(whoami)
NAME2=$(sudo whoami)
echo "Was $NAME1, then became more powerful $NAME2"

# Read Bash shell built-ins documentation with the bash `help` built-in:
help
help help
help for
help return
help source
help .

# Read Bash manpage documentation with `man`
apropos bash
man 1 bash
man bash

# Read info documentation with `info` (`?` for help)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Read bash info documentation:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash


# SAFER SCRIPTING WITH SET OPTIONS


# Exit immediately if a command fails
set -e

# Treat unset variables as errors
set -u

# If any command in a pipeline fails, the whole pipeline fails
# (without this, only the last command's exit code matters)
set -o pipefail

# You'll often see all three combined at the top of robust scripts:
set -euo pipefail

# To undo any of them, use +
set +e  # disable exit-on-error

# Print each command before executing it, useful for debugging
set -x
echo "this will be printed with a + prefix before running"
set +x  # turn it off again


# FILE TEST OPERATORS

file="example.txt"

if [[ -e "$file" ]]; then echo "exists"; fi
if [[ -f "$file" ]]; then echo "is a regular file"; fi
if [[ -d "$file" ]]; then echo "is a directory"; fi
if [[ -r "$file" ]]; then echo "is readable"; fi
if [[ -w "$file" ]]; then echo "is writable"; fi
if [[ -x "$file" ]]; then echo "is executable"; fi
if [[ -s "$file" ]]; then echo "is non-empty"; fi
if [[ -L "$file" ]]; then echo "is a symbolic link"; fi

# Compare two files
if [[ "file1.txt" -nt "file2.txt" ]]; then echo "file1 is newer"; fi
if [[ "file1.txt" -ot "file2.txt" ]]; then echo "file1 is older"; fi


# ADVANCED STRING MANIPULATION

var="Hello, World!"

# Uppercase / lowercase (Bash 4+)
echo "${var^^}"   # => HELLO, WORLD!
echo "${var,,}"   # => hello, world!
echo "${var^}"    # => Hello, World! (capitalize first letter only)

# Remove prefix (shortest match)
path="/usr/local/bin/script.sh"
echo "${path#/usr/}"         # => local/bin/script.sh

# Remove prefix (longest match)
echo "${path##*/}"           # => script.sh  (like `basename`)

# Remove suffix (shortest match)
echo "${path%.sh}"           # => /usr/local/bin/script

# Remove suffix (longest match)
echo "${path%%/*}"           # => (empty, longest prefix from the left)

# Practical example, get directory from a path (like `dirname`):
echo "${path%/*}"            # => /usr/local/bin

# Replace first occurrence
echo "${var/World/Bash}"     # => Hello, Bash!

# Replace all occurrences
sentence="one two one three one"
echo "${sentence//one/1}"    # => 1 two 1 three 1

# Slice: ${var:offset:length}
echo "${var:7:5}"            # => World

# String length
echo "${#var}"               # => 13

# HERE STRINGS

# Feed a string directly to a command's stdin with <<<
# (a lighter alternative to echo "..." | command)
grep "World" <<< "Hello, World!"
# => Hello, World!

# Useful with read to parse a string into variables
read -r first second <<< "foo bar"
echo "$first"   # => foo
echo "$second"  # => bar

# ASSOCIATIVE ARRAYS (dictionaries)

declare -A person

person["name"]="Alice"
person["age"]=30
person["city"]="Zurich"

echo "${person["name"]}"   # => Alice

# Iterate over keys
for key in "${!person[@]}"; do
    echo "$key = ${person[$key]}"
done

# Check if a key exists
if [[ -v person["name"] ]]; then
    echo "name key exists"
fi

# Delete a key
unset 'person["city"]'

# All keys / all values
echo "${!person[@]}"  # => name age
echo "${person[@]}"   # => Alice 30

# ARITHMETIC

a=5
b=3

echo $(( a + b ))   # => 8
echo $(( a ** b ))  # => 125  (exponentiation)
echo $(( a % b ))   # => 2    (modulo)

# Use as a conditional, (( )) returns true if non-zero
if (( a > b )); then
    echo "$a is greater"
fi

# Increment / decrement
(( a++ ))
(( b -= 2 ))

# `let` is an older alternative
let result=a*b
echo "$result"

# Bash only does integers. For floating point, use bc:
echo "scale=4; 10 / 3" | bc   # => 3.3333
echo "scale=2; sqrt(2)" | bc -l  # => 1.41

# PRINTF (more reliable than echo)

# echo has inconsistent behavior across systems (-e, -n flags vary).
# printf is portable and predictable.

printf "Hello, %s!\n" "World"               # => Hello, World!
printf "Pi is approximately %.4f\n" 3.14159  # => Pi is approximately 3.1416
printf "%05d\n" 42                           # => 00042  (zero-padded)
printf "%-10s | %s\n" "left" "right"         # left-aligned column

# printf does not add a newline by default, you control it with \n


# ENVIRONMENT VARIABLES

# export makes a variable available to child processes
export MY_VAR="hello"

# View all environment variables
env
printenv

# View a single one
printenv HOME

# Unset a variable
unset MY_VAR

# Temporarily set an env variable for a single command only
MY_VAR="temp" some_command   # MY_VAR is not changed in the current shell



# SOURCE / DOT OPERATOR


# `source` (or `.`) runs a script in the CURRENT shell, changes affect your session.
# Unlike `bash script.sh` which runs in a subshell and changes are lost.
source ~/.bashrc
. ~/.bashrc  # identical, dot is the POSIX version

# Common use: load shared functions or config from another file
source ./lib/helpers.sh

# PARSING ARGUMENTS WITH GETOPTS

# getopts is the standard built-in for handling flags like -v, -f filename
# A colon after a letter means it expects an argument

usage() {
    echo "Usage: $0 [-v] [-f filename]"
    exit 1
}

verbose=false
filename=""

while getopts ":vf:" opt; do
    case $opt in
        v) verbose=true ;;
        f) filename="$OPTARG" ;;
        :) echo "Option -$OPTARG requires an argument."; usage ;;
        \?) echo "Invalid option: -$OPTARG"; usage ;;
    esac
done

# Shift past the processed options so $1, $2... refer to remaining args
shift $(( OPTIND - 1 ))

$verbose && echo "Verbose mode on"
[[ -n "$filename" ]] && echo "File: $filename"

# SELECT, INTERACTIVE MENUS

echo "Pick a shell:"
select shell in bash zsh fish quit; do
    case $shell in
        quit) break ;;
        "") echo "Invalid choice" ;;
        *) echo "You picked $shell"; break ;;
    esac
done

# PROCESS SUBSTITUTION

# <(command) treats the output of a command as if it were a file.
# Useful when a command requires a filename, not stdin.

# Compare output of two commands without creating temp files:
diff <(ls dir1/) <(ls dir2/)

# Loop over command output without a subshell (variables persist after the loop)
while IFS= read -r line; do
    echo "Got: $line"
done < <(ls -1)

# ROBUST ERROR HANDLING WITH TRAP

# Clean up temp files no matter how the script exits
TEMP_FILE=$(mktemp)

cleanup() {
    echo "Cleaning up..."
    rm -f "$TEMP_FILE"
}

# ERR fires on any error, EXIT fires when the script ends (for any reason)
trap cleanup EXIT
trap 'echo "Error on line $LINENO"' ERR

# Trap specific signals
trap 'echo "Interrupted!"; exit 1' SIGINT SIGTERM

# SHELL OPTIONS WITH SHOPT

# Enable case-insensitive globbing
shopt -s nocaseglob

# Make ** match files recursively (Bash 4+)
shopt -s globstar
for file in **/*.sh; do
    echo "$file"
done

# nullglob: if a glob matches nothing, expand to nothing instead of the literal pattern
shopt -s nullglob
for f in ./*.nonexistent; do
    echo "$f"  # this loop body never runs if there are no matches
done
shopt -u nullglob  # turn it off again

# extglob: enables extended pattern matching operators
shopt -s extglob
# ?(pattern) , zero or one match
# *(pattern) , zero or more matches
# +(pattern) , one or more matches
# @(pattern) , exactly one match
# !(pattern) , anything that does NOT match
files=(report_2023.txt report_2024.txt notes.txt)
for f in "${files[@]}"; do
    case "$f" in
        !(report_*)) echo "Not a report: $f" ;;
    esac
done

# EXEC

# exec replaces the current shell process with a new command.
# No new process is forked, the shell IS the command from that point on.
# Anything after exec in the script won't run.

# Common use: redirect all script output to a log file from within the script itself
exec > script.log 2>&1
echo "This goes to script.log, not the terminal"

# Or replace the shell with another program entirely
# exec /bin/sh   # uncomment to actually do this


# COPROCESSES

# coproc runs a command in the background with two-way pipes connected to it.
# You can write to its stdin and read from its stdout.
coproc myproc { while read -r line; do echo "echo: $line"; done; }

echo "hello" >&"${myproc[1]}"    # write to the coproc's stdin
read -r reply <&"${myproc[0]}"   # read from the coproc's stdout
echo "$reply"                    # => echo: hello

# Kill the coproc when done
kill "$myproc_PID" 2>/dev/null

# XARGS

# xargs builds and runs commands from stdin, great for piping to commands
# that don't accept stdin directly

# Delete all .tmp files found by find
find . -name "*.tmp" | xargs rm -f

# -I {} lets you place the argument wherever you want in the command
find . -name "*.txt" | xargs -I {} cp {} /backup/

# Run at most 4 parallel jobs with -P
find . -name "*.log" | xargs -P 4 -I {} gzip {}

# Safely handle filenames with spaces using null delimiter
find . -name "*.txt" -print0 | xargs -0 rm -f

# AWK, FIELD PROCESSING

# awk processes text line by line, splitting each into fields ($1, $2, ...)
# Default field separator is whitespace

echo "Alice 30 New York" | awk '{ print $1, "is", $2, "years old" }'
# => Alice is 30 years old

# Use a custom delimiter with -F
echo "name,age,city" | awk -F',' '{ print $3 }'
# => city

# Print lines where the second field is greater than 25
awk -F',' '$2 > 25 { print $1 }' people.csv

# Sum a column
awk -F',' '{ sum += $2 } END { print "Total:", sum }' data.csv

# Print total line count (like wc -l)
awk 'END { print NR }' file.txt


# SED, STREAM EDITING (beyond the basics)

# Substitute with flags:
sed 's/foo/bar/'        file.txt   # first occurrence per line
sed 's/foo/bar/g'       file.txt   # all occurrences
sed 's/foo/bar/2'       file.txt   # second occurrence only
sed 's/foo/bar/gi'      file.txt   # case-insensitive, all occurrences

# Delete lines matching a pattern
sed '/^#/d' file.txt    # remove comment lines

# Print only matching lines (like grep)
sed -n '/pattern/p' file.txt

# Edit in place and keep a backup
sed -i.bak 's/old/new/g' file.txt  # original saved as file.txt.bak

# Address ranges, act only on lines 5 to 10
sed '5,10s/foo/bar/g' file.txt

# Insert a line before/after a match
sed '/pattern/i\New line before' file.txt
sed '/pattern/a\New line after'  file.txt

# MISC SEFUL PATTERNS

# Assign a default only if variable is unset or empty (and persist it)
: "${NAME:=DefaultName}"
echo "$NAME"  # => DefaultName if NAME was previously unset

# Exit with an error message if a required variable is unset
: "${REQUIRED_VAR:?'REQUIRED_VAR must be set'}"

# Check if a command exists before using it
if command -v jq &>/dev/null; then
    echo "jq is installed"
fi

# Time how long a command takes
time sleep 1

# Quickly make a backup copy of a file using brace expansion
cp config.yaml{,.bak}   # expands to: cp config.yaml config.yaml.bak

# Create a whole project directory tree in one line
mkdir -p project/{src,tests,docs,scripts}

# Read a file line by line safely
# (IFS= preserves whitespace; || handles missing trailing newline)
while IFS= read -r line || [[ -n "$line" ]]; do
    echo "Line: $line"
done < file.txt

# Fail loudly if a command errors, with a message, without relying on set -e
some_command || { echo "some_command failed"; exit 1; }

# Capture a multiline string into a variable with a here-doc
# (Quoting 'EOF' prevents variable expansion inside the block)
read -r -d '' BLOCK << 'EOF'
This is
a multiline
string
EOF
echo "$BLOCK"

# THE WAIT BUILT-IN

# `wait` pauses until background jobs finish
sleep 2 &
pid=$!
wait "$pid"   # wait for a specific PID
echo "Process $pid finished with exit code $?"

# wait -n (Bash 4.3+), wait for ANY one background job to finish
sleep 1 &
sleep 3 &
wait -n
echo "First job done"


# BASH 5.3+ FEATURES

# 1. Forkless command substitution, runs in the current shell, no subshell fork.
#    ${ command; } captures stdout like $(), but without forking:
result=${ echo "no fork"; }
echo "$result"  # => no fork

#    ${| command; } runs in the current shell and expects the result in REPLY:
${| REPLY="computed value"; }
echo "$REPLY"   # => computed value
# This is much faster in tight loops since it avoids fork+pipe overhead.

# 2. GLOBSORT, control how pathname expansion results are sorted.
#    Options: name, size, blocks, mtime, atime, ctime, numeric, none
#    Prefix with - for descending order.
GLOBSORT=mtime           # sort globs by modification time (ascending)
ls *.txt
GLOBSORT=-size            # sort by size, largest first
GLOBSORT=none             # no sorting (filesystem order)

# 3. BASH_MONOSECONDS, monotonic clock, not affected by system time changes.
#    Useful for measuring elapsed time reliably.
start=$BASH_MONOSECONDS
sleep 1
elapsed=$(( BASH_MONOSECONDS - start ))
echo "Elapsed: ${elapsed}s"

# 4. EPOCHSECONDS and EPOCHREALTIME (Bash 5.0+, but worth knowing).
echo "$EPOCHSECONDS"     # => 1747500000  (Unix timestamp, integer)
echo "$EPOCHREALTIME"    # => 1747500000.123456  (microsecond precision)

# 5. source -p PATH, specify where to look for the sourced file
#    instead of using $PATH.
source -p /opt/scripts helper.sh

# 6. read -E, enables Readline tab completion during interactive read.
# read -E -p "Enter a filename: " fname

# 7. New loadable builtins:
#    kv      , generate associative arrays from key-value data
#    strptime, convert date strings to Unix timestamps
#    fltexpr , floating-point arithmetic (like `let` but for floats)
```

For more, see the [Bash documentation](https://www.gnu.org/software/bash/manual/bashref.html).
