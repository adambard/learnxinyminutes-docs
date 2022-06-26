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
    - ["Juraj Kostolanský", "http://www.kostolansky.sk"]
lang: sk-sk
filename: LearnBash-sk.sh
---

Bash je pomenovanie pre unix shell (príkazový interpreter), ktorý bol
tiež distribuovaný ako shell pre GNU operačné systémy a ako predvolený
shell pre Linux a macOS.
Takmer všetky príklady uvedené nižšie môžu byť súčasťou shell skriptu alebo
vykonané priamo v shelli.

[Viac informácií tu.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Prvý riadok skriptu je tzv. shebang, ktorý systému povie ako skript vykonať:
# http://en.wikipedia.org/wiki/Shebang_(Unix)
# Komentáre začínajú znakom #. Shebang je tiež komentár.

# Jednoduchý príklad:
echo Ahoj svet!

# Každý príkaz začína na novom riadku alebo za bodkočiarkou:
echo 'Toto je prvý riadok'; echo 'Toto je druhý riadok'

# Deklarácia premenných vyzerá takto:
Premenna="Nejaky retazec"

# Ale nie takto:
Premenna = "Nejaky retazec"
# Bash si bude myslieť, že Premenna je príkaz, ktorý musí vykonať.
# Výsledkom bude chyba, pretože taký príkaz nenájde.

# Alebo takto:
Premenna= 'Nejaky retazec'
# Bash zistí, že 'Nejaky retazec' je príkaz, ktorý musí vykonať.
# Výsledkom je opäť chyba, lebo taký príkaz neexistuje.

# Používanie premenných:
echo $Premenna
echo "$Premenna"
echo '$Premenna'
# Keď je premenná použitá samostatne - priradenie, exportovanie a pod. - jej
# meno sa píše bez znaku $. Keď sa používa hodnota premennej, pred názov sa
# dáva znak $. Pozor však pri použití ' (apostrof), ktorý nenahradí premennú
# hodnotou!

# Nahradenie reťazca v premennej
echo ${Premenna/Nieco/A}
# Toto nahradí prvý výskyt reťazca "Nieco" za "A"

# Podreťazec z premennej
Dlzka=7
echo ${Premenna:0:Dlzka}
# Toto vráti iba prvých 7 znakov z hodnoty premennej

# Predvolená hodnota premennej
echo ${Foo:-"PredvolenaHodnotaAkFooChybaAleboJePrazdna"}
# Toto funguje pre null (Foo=) a prázdny reťazec (Foo="");
# nula (Foo=0) vráti 0. Všimni si, že toto iba vráti predvolenú hodnotu,
# ale nezmení hodnotu premennej.

# Štandardné premenné:
# Existujú aj užitočné "vstavané" premenné, ako
echo "Hodnota vrátená posledným programom: $?"
echo "PID skriptu: $$"
echo "Počet argumentov: $#"
echo "Argumeny skriptu: $@"
echo "Argumeny skriptu oddelené do rôznych premenných: $1 $2..."

# Čítanie hodnoty zo vstupu:
echo "Ako sa voláš?"
read Meno # Premenná nemusí byť deklarovaná skôr
echo Ahoj, $Meno!

# Klasická if štruktúra:
# použi 'man test' Pre viac informácií o podmienkach
if [ $Meno -ne $USER ]
then
    echo "Meno nie je tvoje používateľské meno"
else
    echo "Meno je tvoje používateľské meno"
fi

# Existuje aj podmienené vykonanie
echo "Vykonané vždy" || echo "Vykonané iba ak prvý príkaz zlyhá"
echo "Vykonané vždy" && echo "Vykonané iba ak prvý príkaz uspeje"

# Pre použitie && a || s if-podmienkou je potrebné použiť zátvorky:
if [ $Meno == "Steve" ] && [ $Vek -eq 15 ]
then
    echo "Toto sa spustí ak $Meno je Steve a $Vek je 15."
fi

if [ $Meno == "Daniya" ] || [ $Meno == "Zach" ]
then
    echo "Toto sa spustí ak $Meno je Daniya alebo Zach."
fi

# Pre výrazy sa používa nasledovný formát:
echo $(( 10 + 5 ))

# Na rozdiel od programovacích jazykov shell pracuje v kontexte aktuálneho
# adresára. Môžeš si prehliadať súbory a adresáre v aktuálnom adresári pomocou
# príkazu ls:
ls

# Tieto príkazy majú aj argumenty pre úpravu ich správania:
ls -l # Vypíše zoznam súborov a priečinkov, každý na samostatnom riadku

# Výsledok predchádzajúceho príkazu môže byť využitý priamo ako vstup pre
# ďalší príkaz.
# Príkaz grep filtruje vstupvyužitím poskytnutého vzoru. Takto môžeme vypísať
# iba .txt súbory:
ls -l | grep "\.txt"

# Vstup a výstup príkazu (stdin, stdout, stderr) môžu byť presmerované.
# Toto číta stdin až po ^EOF$ a prepíše hello.py riadkami medzi "EOF":
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Spustí hello.py s rôznymi presmerovaniami pre stdin, stdout a stderr:
python hello.py < "vstup.in"
python hello.py > "vystup.out"
python hello.py 2> "chyby.err"
python hello.py > "vystup-a-chyby.log" 2>&1
python hello.py > /dev/null 2>&1
# Chybový výstup prepíše uvedený súbor, ak už existuje.
# Ak chceš výstup pridať za existujúci obsah, použi ">>":
python hello.py >> "vystup.out" 2>> "chyby.err"

# Prepíše vystup.out, pripojí k chyby.err a spočíta riadky:
info bash 'Basic Shell Features' 'Redirections' > vystup.out 2>> chyby.err
wc -l vystup.out chyby.err

# Spustí príkaz a vypíše deskriptor súboru (napr. /dev/fd/123)
# pozri: man fd
echo <(echo "#ahojsvet")

# Prepíše vystup.out s "#ahojsvet":
cat > vystup.out <(echo "#ahojsvet")
echo "#ahojsvet" > vystup.out
echo "#ahojsvet" | cat > vystup.out
echo "#ahojsvet" | tee vystup.out >/dev/null

# Potichu odstráni dočasné súbory (pridaj '-i' pre interaktivitu)
rm -v vystup.out chyby.err vystup-a-chyby.log

# Príkazy môžu byť nahradené v iných príkazoch použitím $( ):
# Nasledujúci príkaz vypíše počet súborov a adresárov v aktuálnom adresári
echo "Je tu $(ls | wc -l) súborov a priečinkov."

# To isté sa dá spraviť pomocou spätného apostrofu ``, tie však nemôžu byť
# vhniezdené - preferovaný spôsob je preto $( ).
echo "Je tu `ls | wc -l` súborov a priečinkov."

# Bash používa case, ktorý funguje podobne ako switch v Jave a C++:
case "$Premenna" in
    #Zoznam vzorov pre podmienky
    0) echo "Je to nula.";;
    1) echo "Je to jednotka.";;
    *) echo "Nie je to null.";;
esac

# for-cyklus iteruje cez všetky argumenty:
# Obsah premennej $Premenna sa vypíše trikrát.
for Premenna in {1..3}
do
    echo "$Premenna"
done

# Alebo "tradičným" spôsobom:
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Môžu sa použiť aj na súbory..
# Toto spustí príkaz 'cat' na subor1 a subor2
for Premenna in subor1 subor2
do
    cat "$Premenna"
done

# ..alebo na výstup príkazu.
# Toto použije príkaz cat na výstup z ls.
for Vystup in $(ls)
do
    cat "$Vystup"
done

# while-cykklus:
while [ true ]
do
    echo "telo cyklu..."
    break
done

# Môžeš tiež definovať funkice
# Definícia:
function foo ()
{
    echo "Argumenty fungujú rovnako ako pri skriptoch: $@"
    echo "A: $1 $2..."
    echo "Toto je funkcia"
    return 0
}

# alebo jednoducho
bar ()
{
    echo "Iný spôsob definície funkcií"
    return 0
}

# Volanie funkcie
foo "Moje meno je" $Meno

# Existuje veľa užitočných príkazov, ktoré sa oplatí naučiť:
# vypíše posledných 10 riadkov zo subor.txt
tail -n 10 subor.txt
# vypíše prvých 10 riadkov zo subor.txt
head -n 10 subor.txt
# zotriedi riadky zo subor.txt
sort subor.txt
# vypíše alebo vynechá opakované riadky, použitím -d ich vypíše
uniq -d subor.txt
# vypíše iba prvý stĺpecpred znakom ','
cut -d ',' -f 1 subor.txt
# nahradí každý výskyt 'oukej' za 'super' v subor.txt (možnosť použiť regex)
sed -i 's/oukej/super/g' subor.txt
# vypíše všetky riadky zo subor.txt ktoré vyhovujú regexu
# ukážka vypíše riadky ktoré začínajú s "foo" a končia s "bar"
grep "^foo.*bar$" subor.txt
# pre výpis počtu riadkov vyhovujúcich regexu slúži "-c"
grep -c "^foo.*bar$" subor.txt
# pre vyhľadávanie reťazca bez regexu slúži fgrep (alebo grep -F)
fgrep "^foo.*bar$" subor.txt


# Prečítaj si dokumentáciu k Bash shellu použitím príkazu 'help':
help
help help
help for
help return
help source
help .

# Prečítaj si Bash manpage dokumentáciu príkazom 'man'
apropos bash
man 1 bash
man bash

# Prečítaj si info dokumentáciu pomocou 'info' (? pre help)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Prečítaj si bash info dokumentáciu:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
