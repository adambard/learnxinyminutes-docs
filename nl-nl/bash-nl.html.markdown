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
translators:
    - ["Jeroen Deviaene", "https://www.github.com/jerodev"]
lang: nl-nl
filename: LearnBash-nl.sh
---

Bash is de naam van de unix shell, deze wordt gebruikt voor het GNU operating system en is de standaard shell op Linux en macOS.
Bijna alle voorbeelden hieronder kunnen deel uitmaken van een shell script of kunnen uitgevoerd worden in de shell.

[Lees er meer over hier.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# De eerste lijn in het schript is shebang, deze laat het systeem weten hoe
# het script uitgevoerd moet worden: http://en.wikipedia.org/wiki/Shebang_(Unix)
# Zoals je kan zien wordt # gebruikt om een commentaar lijn te starten.

# Een simpel hello world voorbeeld:
echo Hello world!

# Elk commando start op een nieuwe lijn, of achter een puntkomma (;):
echo 'Dit is de eerste lijn'; echo 'Dit is de tweede lijn'

# Een variabele declareren gebeurt op volgende manier:
Variabele="Een string"

# Maar niet op deze manier:
Variabele = "Een string"
# Bash ziet variabelen als een commando en zal een error geven omdat dit commando
# niet bestaat.

# Of op deze manier:
Variabele= 'Een string'
# Bash zal 'Een string' zien als een commando en een error geven omdat het niet
# gevonden kan worden.

# Variabelen gebruiken:
echo $Variabele
echo "$Variabele"
echo '$Variabele'
# Wanneer je een variable wil toekennen, exporteren of nog anders gebruik je 
# de naam zonder '$'. Als je de waarde van de variabele wilt, gebruik je een 
# '$' voor de naam.

# Strings vervangen in variables
echo ${Variabele/Een/De}
# Dit zal 'Een' vervangen door 'De' in de string

# Substring
Length=7
echo ${Variabele:0:Length}
# Dit zal de eerste 7 tekens van de string weergeven.

# Standaard waarde voor variabele
echo ${Foo:-"StandaardwaardeAlsFooLeegIsOfNietBestaat"}
# Dit werkt voor null en lege strings (Foo=""). Dit werkt niet voor 0 (Foo=0).
# Merk op dat dit enkel de waarde retourneerd en de variable niet aanpast.


# Ingebouwde variabelen:
# Er zijn enkele zeer handige ingebouwde variabelen, zoals:
echo "Return waarde van laatste programma: $?"
echo "PID van dit script: $$"
echo "Aantal argumenten voor dit script: $#"
echo "Alle argumenten voor dit script: $@"
echo "Argumenten voor dit script in apparte variabelen: $1 $2..."

# Een waarde lezen via input:
echo "Wat is uw naam?"
read Naam # Merk op dat we geen variabele gedeclareerd hebben
echo Hallo, $Naam!

# We hebben ook logische if structuren
# Gebruik 'man test' voor meer informatie over condities.
if [ $Naam -ne $USER ]
then
    echo "Uw naam is niet gelijk aan de gebruikersnaam"
else
    echo "Uw naam is de gebruikersnaam"
fi

# MERK OP: als $Naam leeg is ziet bash het bovenstaande als volgt:
if [ -ne $USER ]
# dit is ongeldige syntax
# Dus de beter manier om dit te schrijven is
if [ "$Naam" -ne $USER ] ...
# Als naam nu leeg is, ziet bash nu nog steeds
if [ "" -ne $USER ] ...
# Dit werkt wel zoals het hoort

# Er is ook conditionele executie
echo "Altijd uitvoeren" || echo "Enkel uitvoeren als vorige command mislukt"
echo "Altijd uitvoeren" && echo "Enkel uitvoeren als vorige command NIET mislukt"

# Om && en || te gebruiken in if structuren moeten vierkante haken gebruikt worden:
if [ "$Naam" == "Steve" ] && [ "$Leeftijd" -eq 15 ]
then
    echo "Dit wordt uitgevoerd als $Naam Steve is en $Leeftijd 15 is."
fi

# Expressies worden gemaakt met volgende syntax:
echo $(( 10 + 5 ))

# Bash werkt steeds in de context van een huidige map in het bestandssysteem.
# Bestanden en mappen in de huidige map kunnen weergegeven worden met het ls
# commando.
ls

# Commandos hebben opties die de uitvoer beinvloeden
ls -l # Lijst elk bestand en map op een nieuwe lijn.

# Resultaten van een vorig commando kunnen doorgegeven worden aan een volgend 
# commando als input.
# Het grep commando filter de input met een bepaald patroon. Op deze manier kunnen
# we alle .txt bestanden weergeven in de huidige map.
ls -l | grep "\.txt"

# Commando's kunnen gekoppeld worden met andere commando's door gebruik te maken van
# $( ):
# Het volgende commando geeft het aantal bestanden weer in de huidige map
echo "Er zijn hier $(ls | wc -l) bestanden."

# Het zelfde kan gedaan worden met `, maar die kunnen niet genest worden. De methode
# bij voorkeur is om $( ) te gebruiken.
echo "Er zijn hier `ls | wc -l` bestanden."

# Bash heeft een case statement dat werkt zoals in Java en C++
case "$Variabele" in
    0) echo "Er is een 0";;
    1) echo "Er is een 1";;
    *) echo "Er is iets";;
esac

# For lussen itereren over de gegeven argumenten
# De waarde van $Variabele wordt hier drie keer afgeprint
for Variable in {1..3}
do
    echo "$Variabele"
done

# Of schrijf een traditionele for loop op deze manier
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Lussen kunnen ook gebruikt worden met bestanden
# Deze lus voert het cat commando uit op file1 en file2
for Variable in file1 file2
do
    cat "$Variable"
done

# Of met het output commando
for Output in $(ls)
do
    cat "$Output"
done

# while lus:
while [ true ]
do
    echo "body van de lus..."
    break
done

# Je kan ook functies aanmaken
# Defenitie:
function foo ()
{
    echo "Alle argumenten: $@"
    echo "Apparte argumenten: $1 $2..."
    echo "Dit is een functie"
    return 0
}

# Of simpeler
bar ()
{
    echo "Dit is een andere manier om functies te maken."
    return 0
}

# Functies oproepen
foo "Mijn naam is" $Naam

# Enkele zeer handige commando's die je moet kennen
# print de laatste 10 lijnen van file.txt
tail -n 10 file.txt
# print de eerste 10 lijnen van file.txt
head -n 10 file.txt
# Sorteer de lijnen in file.txt
sort file.txt
# Vind dubbele lijnen in file.txt
uniq -d file.txt
# Print de eerste kolom voor het ',' karakter
cut -d ',' -f 1 file.txt
# Vervang elke 'okay' met 'great' in file.txt (werkt ook met regex)
sed -i 's/okay/great/g' file.txt
# Print alle lijnen die voldoen aan de regex naar stdout
grep "^foo.*bar$" file.txt


# Gebruik de ingebouwde help functies door het help commando te gebruiken:
help
help help
help for
help return
help source
help .

# Lees de bash documentatie met het man commando:
apropos bash
man 1 bash
man bash

# Lees bash info documentatie:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
