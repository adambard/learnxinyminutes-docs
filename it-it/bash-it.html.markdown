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
filename: LearnBash.sh
translators:
    - ["Robert Margelli", "http://github.com/sinkswim/"]
lang: it-it
---

Bash è il nome della shell di unix, la quale è stata distribuita anche come shell del sistema oprativo GNU e la shell di default su Linux e Mac OS X.
Quasi tutti gli esempi sottostanti possono fare parte di uno shell script o eseguiti direttamente nella shell.

[Per saperne di piu'.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# La prima riga dello script è lo shebang il quale dice al sistema come eseguire
# lo script: http://it.wikipedia.org/wiki/Shabang
# Come avrai già immaginato, i commenti iniziano con #. Lo shebang stesso è un commento.

# Semplice esempio ciao mondo:
echo Ciao mondo!

# Ogni comando inizia su una nuova riga, o dopo un punto e virgola:
echo 'Questa è la prima riga'; echo 'Questa è la seconda riga'

# Per dichiarare una variabile:
VARIABILE="Una stringa"

# Ma non così:
VARIABILE = "Una stringa"
# Bash stabilirà che VARIABILE è un comando da eseguire e darà un errore
# perchè non esiste.

# Usare la variabile:
echo $VARIABILE
echo "$VARIABILE"
echo '$VARIABILE'
# Quando usi la variabile stessa - assegnala, esportala, oppure — scrivi
# il suo nome senza $. Se vuoi usare il valore della variabile, devi usare $.
# Nota che ' (singolo apice) non espande le variabili!

# Sostituzione di stringhe nelle variabili
echo ${VARIABILE/Una/A}
# Questo sostituirà la prima occorrenza di "Una" con "La"

# Sottostringa di una variabile
echo ${VARIABILE:0:7}
# Questo ritornerà solamente i primi 7 caratteri

# Valore di default per la variabile
echo ${FOO:-"ValoreDiDefaultSeFOOMancaOÈ Vuoto"}
# Questo funziona per null (FOO=), stringa vuota (FOO=""), zero (FOO=0) ritorna 0

# Variabili builtin:
# Ci sono delle variabili builtin molto utili, come
echo "Valore di ritorno dell'ultimo programma eseguito: $?"
echo "PID dello script: $$"
echo "Numero di argomenti: $#"
echo "Argomenti dello script: $@"
echo "Argomenti dello script separati in variabili distinte: $1 $2..."

# Leggere un valore di input:
echo "Come ti chiami?"
read NOME # Nota che non abbiamo dovuto dichiarare una nuova variabile
echo Ciao, $NOME!

# Classica struttura if:
# usa 'man test' per maggiori informazioni sulle condizionali
if [ $NOME -ne $USER ]
then
    echo "Il tuo nome non è lo username"
else
    echo "Il tuo nome è lo username"
fi

# C'è anche l'esecuzione condizionale
echo "Sempre eseguito" || echo "Eseguito solo se la prima condizione fallisce"
echo "Sempre eseguito" && echo "Eseguito solo se la prima condizione NON fallisce"

# Per usare && e || con l'if, c'è bisogno di piu' paia di parentesi quadre:
if [ $NOME == "Steve" ] && [ $ETA -eq 15 ]
then
    echo "Questo verrà eseguito se $NOME è Steve E $ETA è 15."
fi

if [ $NOME == "Daniya" ] || [ $NOME == "Zach" ]
then
    echo "Questo verrà eseguito se $NAME è Daniya O Zach."
fi

# Le espressioni sono nel seguente formato:
echo $(( 10 + 5 ))

# A differenza di altri linguaggi di programmazione, bash è una shell - quindi lavora nel contesto
# della cartella corrente. Puoi elencare i file e le cartelle nella cartella
# corrente con il comando ls:
ls

# Questi comandi hanno opzioni che controllano la loro esecuzione:
ls -l # Elenca tutti i file e le cartelle su una riga separata

# I risultati del comando precedente possono essere passati al comando successivo come input.
# Il comando grep filtra l'input con il pattern passato. Ecco come possiamo elencare i
# file .txt nella cartella corrente:
ls -l | grep "\.txt"

# Puoi redirezionare l'input e l'output del comando (stdin, stdout, e stderr).
# Leggi da stdin finchè ^EOF$ e sovrascrivi hello.py con le righe
# comprese tra "EOF":
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Esegui hello.py con diverse redirezioni stdin, stdout, e stderr:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# Lo output error sovrascriverà il file se esiste,
# se invece vuoi appendere usa ">>":
python hello.py >> "output.out" 2>> "error.err"

# Sovrascrivi output.txt, appendi a error.err, e conta le righe:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Esegui un comando e stampa il suo file descriptor (esempio: /dev/fd/123)
# vedi: man fd
echo <(echo "#ciaomondo")

# Sovrascrivi output.txt con "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Pulisci i file temporanei verbosamente (aggiungi '-i' per la modalità interattiva)
rm -v output.out error.err output-and-error.log

# I comandi possono essere sostituiti con altri comandi usando $( ):
# Il comando seguente mostra il numero di file e cartelle nella
# cartella corrente.
echo "Ci sono $(ls | wc -l) oggetti qui."

# Lo stesso puo' essere usato usando backticks `` ma non possono essere innestati - il modo migliore
# è usando $( ).
echo "Ci sono `ls | wc -l` oggetti qui."

# Bash utilizza uno statemente case che funziona in maniera simile allo switch in Java e C++:
case "$VARIABILE" in 
    #Lista di pattern per le condizioni che vuoi soddisfare
    0) echo "C'è uno zero.";;
    1) echo "C'è un uno.";;
    *) echo "Non è null.";;
esac

# I cicli for iterano per ogni argomento fornito:
# I contenuti di $VARIABILE sono stampati tre volte.
for VARIABILE in {1..3}
do
    echo "$VARIABILE"
done

# O scrivilo con il "ciclo for tradizionale":
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Possono essere usati anche per agire su file..
# Questo eseguirà il comando 'cat' su file1 e file2
for VARIABILE in file1 file2
do
    cat "$VARIABILE"
done

# ..o dall'output di un comando
# Questo eseguirà cat sull'output di ls.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# while loop:
while [ true ]
do
    echo "corpo del loop..."
    break
done

# Puoi anche definire funzioni
# Definizione:
function foo ()
{
    echo "Gli argomenti funzionano come gli argomenti dello script: $@"
    echo "E: $1 $2..."
    echo "Questa è una funzione"
    return 0
}

# o semplicemente
bar ()
{
    echo "Un altro modo per dichiarare funzioni!"
    return 0
}

# Per chiamare la funzione
foo "Il mio nome è" $NOME

# Ci sono un sacco di comandi utili che dovresti imparare:
# stampa le ultime 10 righe di file.txt
tail -n 10 file.txt
# stampa le prime 10 righe di file.txt
head -n 10 file.txt
# ordina le righe di file.txt
sort file.txt
# riporta o ometti le righe ripetute, con -d le riporta
uniq -d file.txt
# stampa solamente la prima colonna prima del carattere ','
cut -d ',' -f 1 file.txt
# sostituisce ogni occorrenza di 'okay' con 'great' in file.txt (compatible con le regex)
sed -i 's/okay/great/g' file.txt
# stampa su stdout tutte le righe di file.txt che soddisfano una certa regex
# L'esempio stampa le righe che iniziano con "foo" e che finiscono con "bar"
grep "^foo.*bar$" file.txt
# passa l'opzione "-c" per stampare invece il numero delle righe che soddisfano la regex
grep -c "^foo.*bar$" file.txt
# se vuoi letteralmente cercare la stringa,
# e non la regex, usa fgrep (o grep -F)
fgrep "^foo.*bar$" file.txt 


# Leggi la documentazione dei builtin di bash con il builtin 'help' di bash:
help
help help
help for
help return
help source
help .

# Leggi la manpage di bash con man
apropos bash
man 1 bash
man bash

# Leggi la documentazione con info (? per help)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Leggi la documentazione di bash:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
