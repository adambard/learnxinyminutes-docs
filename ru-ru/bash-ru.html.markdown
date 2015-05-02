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
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
filename: LearnBash.sh
---

Bash это командная оболочка unix (unix shell), которая распространяется как оболочка для операционной системы GNU и используется в качестве оболочки по умолчанию для Linux и Mac OS X.
Почти все нижеприведенные примеры могут могут быть частью shell-скриптов или быть исполнены напрямую в shell.

[Подробнее.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Первая строка скрипта - это shebang, который сообщает системе как испольнять
# этот скрипт: http://en.wikipedia.org/wiki/Shebang_(Unix)
# Как вы уже поняли, комментарии начинаются с #. Shebang - это тоже коммантарий.

# Простой пример hello world:
echo Hello world!

# Отдельные команды начинаются с новой строки или разделяются точкой с запятой:
echo 'This is the first line'; echo 'This is the second line'

# Вот так объявляется пемеренная:
VARIABLE="Some string"

# Но не так:
VARIABLE = "Some string"
# Bash решит что VARIABLE - это команда, которую он должен исполнить,
# и выдаст ошибку, потому что не сможет найти ее.

# И не так:
VARIABLE= 'Some string'
# Тут Bash решит что 'Some string' - это команда, которую он должен исполнить,
# и выдаст ошибку, потому что не сможет найти ее (здесь 'VARIABLE=' выглядит
# как присвоение значения переменной, но только в контексте исполнения
# команды 'Some string').

# Использование переменой:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Когда вы используете переменную — присвоение, экспорт и т.д — вы пищете её
# имя без $. А для получения значения переменной, используйте $.
# Заметте что ' (одинарные кавычки) не раскрывают переменные в них.

# Подстановка строк в переменных
echo ${VARIABLE/Some/A}
# Это выражение заменит первую встреченную подстроку "Some" на "A"

# Подстановка из переменной
LENGTH=7
echo ${VARIABLE:0:LENGTH}
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
    echo "Your name isn't your username"
else
    echo "Your name is your username"
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

# You can redirect command input and output (stdin, stdout, and stderr).
# Read from stdin until ^EOF$ and overwrite hello.py with the lines
# between "EOF":
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Run hello.py with various stdin, stdout, and stderr redirections:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# The output error will overwrite the file if it exists,
# if you want to append instead, use ">>":
python hello.py >> "output.out" 2>> "error.err"

# Overwrite output.txt, append to error.err, and count lines:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Run a command and print its file descriptor (e.g. /dev/fd/123)
# see: man fd
echo <(echo "#helloworld")

# Overwrite output.txt with "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Cleanup temporary files verbosely (add '-i' for interactive)
rm -v output.out error.err output-and-error.log

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

# Or write it the "traditional for loop" way:
for ((a=1; a <= 3; a++))
do
    echo $a
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
# replaces every occurrence of 'okay' with 'great' in file.txt, (regex compatible)
sed -i 's/okay/great/g' file.txt
# print to stdout all lines of file.txt which match some regex
# The example prints lines which begin with "foo" and end in "bar"
grep "^foo.*bar$" file.txt
# pass the option "-c" to instead print the number of lines matching the regex
grep -c "^foo.*bar$" file.txt
# if you literally want to search for the string,
# and not the regex, use fgrep (or grep -F)
fgrep "^foo.*bar$" file.txt 


# Read Bash shell builtins documentation with the bash 'help' builtin:
help
help help
help for
help return
help source
help .

# Read Bash manpage documentation with man
apropos bash
man 1 bash
man bash

# Read info documentation with info (? for help)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Read bash info documentation:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
