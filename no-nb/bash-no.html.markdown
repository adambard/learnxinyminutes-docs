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
filename: LearnBash-no.sh
translators:
    - ["Andreas Lindahl Flåten", "https://github.com/anlif"]
lang: no-nb
---
Bash er navnet på unix skallet, som også var distribuert som skallet for GNU 
operativsystemet og som standard skall på de fleste Linux distribusjoner og 
Mac OS X.

[Les mer her.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Den første linjen i et bash skript starter med `#!' (shebang) 
# etterfulgt av stien til bash http://en.wikipedia.org/wiki/Shebang_(Unix)
# Kommentarer starter med #.

# Enkelt hello world eksempel:
echo Hello world!

# Hver kommando starter på en ny linje, eller etter et semikolon:
echo 'Dette er den første linjen'; echo 'Dette er en andre linjen'

# Deklarering av en variabel ser slik ut:
VARIABLE="En tekststreng"

# Men ikke slik:
VARIABLE = "En tekststreng"
# Bash vil tolke dette som at VARIABLE er en kommando den skal kjøre 
# og gi en feilmelding dersom kommandoen ikke finnes

# Bruk av den nydeklarerte variabelen:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Når du bruker variabelen - setter verdien, eksporterer den, og linkende -
# skriver du navnet dens uten $. Hvis du vil bruke variabelens verdi, 
# skriver du $ før variabelnavnet.

# Strenginnhold i en variabel kan erstattes på følgende måte:
echo ${VARIABLE/tull/ball}
# Dette vil erstatte første forekomst av `tull' med `ball'

# Substreng (delstreng) av en variabel:
echo ${VARIABLE:0:7}
# Dette vil returnere de første 7 tegnene i en strengvariabel

# Å angi en standardverdi dersom en variabel er udeklarert gjøres slik:
echo ${FOO:-"StandardVerdiDersomFOOErTom"}
# Dette fungerer for null (FOO=), tom streng (FOO="") og tallet null (FOO=0)

# Det finnes en rekke hendige innebygde variable, eksempel: 
echo "Siste programs returnerte verdi: $?"
echo "Skript's PID: $$"
echo "Antall argumenter: $#"
echo "Alle argumenter til skriptet: $@"
echo "Argumenter til skriptet i egne variable: $1 $2..."

# Lesing av input:
echo "Hva heter du?"
read NAME # variabelen NAME blir automatisk deklarert av `read' kommandoen
echo Hei, $NAME!

# if setninger ser slik ut:
# se 'man test' for mer informasjon om betingelser
if [ $NAME -ne $USER ]
then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
fi

# Det finnes også betinget eksekvering
echo "Kjøres alltid" || echo "Kjøres kun dersom første kommando feilet"
echo "Kjøres alltid" && echo "Kjøres kun dersom første kommando IKKE feilet"

# For å bruke && (logisk OG) og || (logisk ELLER) sammen med if setninger, 
# trenger man par av firkantklammer [] på hver side av et logisk uttrykk:
if [ $NAME == "Steve" ] && [ $AGE -eq 15 ]
then
    echo "Dette kjører dersom $NAME er Steve OG $AGE er lik 15."
fi

if [ $NAME == "Daniya" ] || [ $NAME == "Zach" ]
then
    echo "Dette kjører dersom $NAME er Daniya ELLER Zach."
fi

# Matematiske uttrykk skrives slik:
echo $(( 10 + 5 ))

# Ulikt de fleste programmeringsspråk, så er bash et skall - det medfører at en 
# kommando i et skript kjører i en bestemt mappe i filsystemet. Du kan skrive
# ut innholdet i nåværende mappe med ls kommandoen:
ls

# Kommandoen har parametre som kontrollerer hvordan kommandoen utføres:
ls -l # Skriv hver fil og mappe på sin egen linje

# Resultatet av forrige kommando kan bli sendt til neste kommando som input.
# grep kommandoen filtrerer input ved hjelp av et regulært uttrykk.
# Ved å bruke grep kan vi skrive ut kun .txt filer på følgende måte:
ls -l | grep "\.txt" # lær mer om grep ved å skrive 'man grep'

# You can redirect command input and output (stdin, stdout, and stderr).
# Read from stdin until ^EOF$ and overwrite hello.py with the lines
# between "EOF":

# Input og output fra filer kan dirigeres (stdin, stdout og stderr).
# "cat" kommandoen uten argumenter skriver fra stdin til stdout.
# I det følgende eksempelet overskrives filen hello.py med linjene mellom EOF.
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Kjør hello.py with ulike stdin, stdout, and stderr omdirigeringer:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# ">" operatoren overskriver filen dersom den finnes.
# Hvis du heller vil legge til på slutten av en eksisterende fil, bruk ">>"
python hello.py >> "output.out" 2>> "error.err"

# Overskriv output.txt, legg til error.err, og tell antall linjer med "wc":
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
