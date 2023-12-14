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

Bash este numele shell-ului UNIX, care a fost de asemenea distribuit drept shell pentru sistemul de operare GNU și ca shell implicit pentru Linux si macOS.
Aproape toate exemplele de mai jos pot fi parte dintr-un script sau pot fi executate direct in linia de comanda.

[Citește mai multe:](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Prima linie din script se numeste "shebang" 
# care spune sistemului cum să execute scriptul
# http://en.wikipedia.org/wiki/Shebang_(Unix)
# După cum te-ai prins deja, comentariile încep cu #. 
# Shebang este de asemenea un comentariu.

# Exemplu simplu de hello world:
echo Hello world!

# Fiecare comandă începe pe o linie nouă, sau după punct și virgula ;
echo 'Prima linie'; echo 'A doua linie'

# Declararea unei variabile se face astfel:
VARIABLE="Niște text"

# DAR nu așa:
VARIABLE = "Niste text"
# Bash va crede că VARIABLE este o comandă care trebuie executată și va
# returna o eroare pentru că nu va putea fi găsita.

# Folosind variabila:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Atunci când folosesti variabila, o atribui, o exporți sau altfel, 
# numele ei se scrie fără $.
# Daca vrei sa folosesti valoarea variabilei, atunci trebuie să folosești $.
# Atentie la faptul că ' (apostrof) nu va inlocui variabla cu valoarea ei.

# Inlocuirea de caractere în variabile
echo ${VARIABLE/Niște/Un}
# Asta va înlocui prima apariție a "Niște" cu "Un" în variabila de mai sus.

# Substring dintr-o variabilă
echo ${VARIABLE:0:7}
# Asta va returna numai primele 7 caractere din variabila.

# Valoarea implicita a unei variabile:
echo ${FOO:-"ValoareaImplicitaDacaFOOLipseșteSauEGoală"}
# Asta functionează pentru null (FOO=),
# sir de caractere gol (FOO=""), zero (FOO=0) returnează 0

# Variabile pre-existente
echo "Ulima valoare returnată de ultimul program rulat: $?"
echo "ID-ul procesului (PID) care rulează scriptul: $$"
echo "Numărul de argumente: $#"
echo "Argumentele scriptului: $@"
echo "Argumentele scriptului separate în variabile: $1 $2..."

# Citind o valoare din consolă
echo "Care e numele tău?"
read NAME # Observă faptul că nu a trebuit să declarăm o variabilă nouă
echo Salut, $NAME!

# Avem obisnuita instructiune "if"
# Folosește "man test" pentru mai multe informații 
# despre instrucținea conditionala
if [ $NAME -ne $USER ]
then
    echo "Numele tău este username-ul tău"
else
    echo "Numele tău nu este username-ul tău"
fi

# Există, de asemenea, și executarea conditională de comenzi
echo "Întotdeauna executat" || echo "Executat dacă prima instrucțiune eșuează"
echo "Întotdeauna executat" && echo "Executat dacă prima instrucțiune NU esuează"

# Expresiile apar în urmatorul format
echo $(( 10 + 5 ))

# Spre deosebire de alte limbaje de programare, bash este un shell - așa că 
# funcționează in contextul directorului curent. Poți vedea fișiere și directoare
# din directorul curent folosind comanda "ls":
ls

# Aceste comenzi au optiuni care le controlează execuțiă
ls -l # Listează fiecare fișier și director pe o linie separată

# Rezultatele comenzii anterioare pot fi 
# trimise următoarei comenzi drept argument
# Comanda grep filtrează argumentele trimise cu sabloane. 
# Astfel putem vedea fiserele .txt din directorul curent.
ls -l | grep "\.txt"

# De asemenea, poți redirecționa date de intrare spre sau erori/date de ieșire
# dinspre o comandă
python2 hello.py < "intrare.in"
python2 hello.py > "ieșire.out"
python2 hello.py 2> "erori.err"
# Output-ul va suprascrie fișierul dacă acesta există.
# Daca vrei să fie concatenate datele poți folosi ">>" în loc de ">"

# Comenzile pot fi înlocuite în interiorul altor comenzi folosind $( ):
# Urmatoarea comandă afișează numărul de fișiere 
# și directoare din directorul curent
echo "Sunt $(ls | wc -l) fișiere aici."

# Același lucru se poate obține folosind apostroful inversat ``,
# dar nu pot fi folosite limbricate, așa ca modalitatea 
# preferată este de a folosi $( )
echo "Sunt `ls | wc -l` fișiere aici."

# Bash folosește o instrucțiune 'case' care funcționeaza 
# în mod similar cu instructiunea switch din Java si C++
case "$VARIABLE" in 
    0) echo "Este un zero.";;
    1) echo "Este un unu.";;
    *) echo "Nu este null";;
esac

# Instrucțiunea 'for' parcurge toate elementele trimise:
# Conținutul variabilei $VARIABLE este printat de 3 ori
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# Buclă while:
while [true]
do
    echo "în interiorul iterației aici..."
    break
done

# De asemenea poți defini funcții
# Definiție:
function foo ()
{
    echo "Argumentele funcționeaza ca și argumentele scriptului: $@"
    echo "Si: $1 $2..."
    echo "Asta este o funcție"
    return 0
}

# sau mai simplu:
bar ()
{
    echo "Altă metodă de a declara o funcție"
    return 0
}

# Invocarea unei funcții:
foo "Numele meu este: " $NAME

# Sunt o multime de comenzi utile pe care ar trebui să le inveți:
tail -n 10 file.txt
# afișează ultimele 10 linii din fișierul file.txt

head -n 10 file.txt
# afișează primele 10 linii din fișierul file.txt

sort file.txt
# sortează liniile din file.txt

uniq -d file.txt
# raporteaza sau omite liniile care se repetă. Cu -d le raporteaza

cut -d ',' -f 1 file.txt
# printează doar prima coloană inainte de caracterul ","
```
