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
# Den første linjen i et bash skript starter med '#!' (shebang) 
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
# Når du bruker variabelen, for eksempel setter verdien eller eksporterer den, 
# skriver du navnet dens uten $. Hvis du vil bruke variabelens verdi, 
# skriver du $ før variabelnavnet.

# Strenginnhold i en variabel kan erstattes på følgende måte:
echo ${VARIABLE/tull/ball}
# Dette vil erstatte første forekomst av 'tull' med 'ball'

# Substreng av en variabel:
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
read NAME # variabelen NAME blir automatisk deklarert av 'read' kommandoen
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

# Input og output fra filer kan dirigeres (stdin, stdout og stderr).
# 'cat' kommandoen uten argumenter skriver fra stdin til stdout.
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

# Kjør hello.py (et python skript) 
# med ulike stdin, stdout, and stderr omdirigeringer:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# '>' operatoren overskriver filen dersom den finnes.
# Hvis du heller vil legge til på slutten av en eksisterende fil, bruk '>>'
python hello.py >> "output.out" 2>> "error.err"

# Overskriv output.txt, legg til error.err, og tell antall linjer med 'wc':
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Run a command and print its file descriptor (e.g. /dev/fd/123)
# Kjør en kommando og print tilhørende 'file descriptor'
# se 'man fd'
echo <(echo "#helloworld")

# Ulike måter å overskrive output.out med '#helloworld':
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Slett noen filer med økt verbositet '-v', legg til '-i' for interaktiv modus
rm -v output.out error.err output-and-error.log

# Kommandoer kan kjøres i deklarasjonen av andre kommandoer ved å bruke $( ):
# Følgende kommando skriver antall filer og mapper i nåværende mappe
echo "There are $(ls | wc -l) items here."

# Det samme kan gjøres med backticks `` men de kan ikke være nøstede,
# det anbefales å bruke $( ) slik som i forrige eksempel.
echo "There are `ls | wc -l` items here."

# Bash har en 'case' setning som fungerer omtrent som en 'switch' i Java/C:
case "$VARIABLE" in 
    # Skriv ønskede match med tilhørende kommandoer
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# for løkker kan iterere over en mengde argumenter:
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# Eller vi kan skrive en for løkke omtrent slik det kan gjøres i Java/C:
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Man kan også iterere over resultatet av en annen kommando.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# while løkke, se if setninger:
while [ true ]
do
    echo "loop body here..."
    break
done

# Man kan også definere funksjoner.
# Definisjon:
function foo ()
{
    echo "Argumenter fungerer akkurat som skript argumenter: $@"
    echo "Og: $1 $2..."
    echo "Dette er en funksjon"
    return 0
}

# eller bare:
bar ()
{
    echo "En annen måte å deklarere en funksjon."
    return 0
}

# Å kalle en funksjon:
foo "Mitt navn er" $NAME

# Det er mange nyttige kommandoer du bør lære deg:
# "tail" skriver ut slutten av en fil, i dette tilfellet de siste 10 linjene
tail -n 10 file.txt
# skriv ut de første 10 linjene av file.txt
head -n 10 file.txt
# sorter linjene i file.txt ("man sort")
sort file.txt
# skriv ut eller fjern repeterte linjer, med -d skrives de ut
uniq -d file.txt
# skriver kun den første kolonnen før ',' tegnet
cut -d ',' -f 1 file.txt
# erstatter hvert tilfelle av 'bjarne' med 'alfa' i file.txt,
# sed støtter regulære uttrykk ("man sed").
sed -i 's/bjarne/alfa/g' file.txt
# skriv til stdout alle linjer i file.txt som matches av et regulært uttrykk
# eksempelet skriver ut alle linjer som begynner med "foo" og slutter med "bar"
grep "^foo.*bar$" file.txt
# skriv "-c" hvis du heller vil vite antall linjer som matcher
grep -c "^foo.*bar$" file.txt
# hvis du vil matche en bestemt streng, og ikke et regulært uttrykk
# bruker du enten "fgrep" eller ekvivalenten "grep -f"
fgrep "^foo.*bar$" file.txt 


# Les Bash sin egen dokumentasjon om innebygde konstruksjoner:
help
help help
help for
help return
help source
help .

# Les Bash sin "manpage":
apropos bash
man 1 bash
man bash

# Les "info" dokumentasjon:
apropos info | grep '^info.*('
man info
info info
info 5 info

# Les bash sin info dokumentasjon:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
