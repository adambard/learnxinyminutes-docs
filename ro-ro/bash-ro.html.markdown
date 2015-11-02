---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
translators:
    - ["Adrian Bordinc", "https://github.com/ellimist"]
lang: ro-ro
filename: LearnBash-ro.sh
---

Bash este numele shell-ului unix, care a fost de asemenea distribuit drept shell pentru sistemul de operare GNU si ca shell implicit pentru Linux si Mac OS X.
Aproape toate exemplele de mai jos pot fi parte dintr-un script sau pot fi executate direct in linia de comanda.

[Citeste mai multe:](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Prima linie din script se numeste "shebang" 
# care spune systemului cum sa execute scriptul
# http://en.wikipedia.org/wiki/Shebang_(Unix)
# Dupa cum te-ai prins deja, comentariile incep cu #. 
# Shebang este de asemenea un comentariu.

# Exemplu simplu de hello world:
echo Hello world!

# Fiecare comanda incepe pe o linie noua, sau dupa punct si virgula ;
echo 'Prima linie'; echo 'A doua linie'

# Declararea unei variabile se face astfel:
VARIABLE="Niste text"

# DAR nu asa:
VARIABLE = "Niste text"
# Bash va crede ca VARIABLE este o comanda care trebuie executata si va
# returna o eroare pentru ca nu va putea fi gasita.

# Folosind variabila:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Atunci cand folosesti variabila, o atribui, o exporti sau altfel, 
# numele ei se scrie fara $.
# Daca vrei sa folosesti valoarea variabilei, atunci trebuie sa folosesti $.
# Atentie la faptul ca ' (apostrof) nu va inlocui variabla cu valoarea ei.

# Inlocuirea de caractere in variabile
echo ${VARIABLE/Some/A}
# Asta va inlocui prima aparitie a "Some" cu "A" in variabila de mai sus.

# Substring dintr-o variabila
echo ${VARIABLE:0:7}
# Asta va returna numai primele 7 caractere din variabila.

# Valoarea implicita a unei variabile:
echo ${FOO:-"ValoareaImplicitaDacaFOOLipsesteSauEGoala"}
# Asta functioneaza pentru null (FOO=),
# sir de caractere gol (FOO=""), zero (FOO=0) returneaza 0

# Variabile pre-existente
echo "Ulima valoare returnata de ultimul program rulat: $?"
echo "ID-ul procesului (PID) care ruleaza scriptul: $$"
echo "Numarul de argumente: $#"
echo "Argumentele scriptului: $@"
echo "Argumentele scriptului separate in variabile: $1 $2..."

# Citind o valoare din consola
echo "Care e numele tau?"
read NAME # Observa faptul ca nu a trebuit sa declaram o variabila noua
echo Salut, $NAME!

# Avem obisnuita instructiune "if"
# Foloseste "man test" pentru mai multe informatii 
# despre instructinea conditionala
if [ $NAME -ne $USER ]
then
    echo "Numele tau este username-ul tau"
else
    echo "Numele tau nu este username-ul tau"
fi

# Este de asemenea si executarea conditionala de comenzi
echo "Intotdeauna executat" || echo "Executat daca prima instructiune esueaza"
echo "Intotdeauna executat" && echo "Executat daca prima instructiune NU esueaza"

# Expresiile apar in urmatorul format
echo $(( 10 + 5 ))

# Spre deosebire de alte limbaje de programare bash este un shell - asa ca 
# functioneaza in contextul directorului curent. Poti vedea fisiere si directoare
# din directorul curent folosind comanda "ls":
ls

# Aceste comenzi au optiuni care la controleaza executia
ls -l # Listeaza fiecare fisier si director pe o linie separata

# Rezultatele comenzii anterioare pot fi 
# trimise urmatoarei comenzi drept argument
# Comanda grep filtreaza argumentele trimise cu sabloane. 
# Astfel putem vedea fiserele .txt din directorul curent.
ls -l | grep "\.txt"

# De asemenea poti redirectiona o comanda, input si error output
python2 hello.py < "input.in"
python2 hello.py > "output.out"
python2 hello.py 2> "error.err"
# Output-ul va suprascrie fisierul daca acesta exista.
# Daca vrei sa fie concatenate poti folosi ">>" 

# Comenzile pot fi inlocuite in interiorul altor comenzi folosind $( ):
# Urmatoarea comanda afiseaza numarul de fisiere 
# si directoare din directorul curent
echo "Sunt $(ls | wc -l) fisiere aici."

# Acelasi lucru se poate obtine folosind apostrf-ul inversat ``,
# dar nu pot fi folosite unele in interiorul celorlalte asa ca modalitatea 
# preferata este de a folosi $( )
echo "Sunt `ls | wc -l` fisiere aici."

# Bash foloseste o instructiune 'case' care functioneaza 
# in mod similar cu instructiunea switch din Java si C++
case "$VARIABLE" in 
    0) echo "Este un zero.";;
    1) echo "Este un unu.";;
    *) echo "Nu este null";;
esac

# Instructiunea for parcurge toate elementele trimise:
# Continutul variabilei $VARIABLE este printat de 3 ori
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# while loop:
while [true]
do
    echo "in interiorul iteratiei aici..."
    break
done

# De asemenea poti defini functii
# Definitie:
function foo ()
{
    echo "Argumentele functioneaza ca si argumentele scriptului: $@"
    echo "Si: $1 $2..."
    echo "Asta este o functie"
    return 0
}

# sau mai simplu
bar ()
{
    echo "Alta metoda de a declara o functie"
    return 0
}

# Invocarea unei functii
foo "Numele meu este: " $NAME

# Sunt o multime de comenzi utile pe care ar trebui sa le inveti:
tail -n 10 file.txt
# printeaza ultimele 10 linii din fisierul file.txt
head -n 10 file.txt
# printeaza primele 10 linii din fisierul file.txt
sort file.txt
# sorteaza liniile din file.txt
uniq -d file.txt
# raporteaza sau omite liniile care se repeta, cu -d le raporteaza
cut -d ',' -f 1 file.txt
# printeaza doar prima coloana inainte de caracterul ","
```
