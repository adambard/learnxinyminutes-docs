---
category: tool
tool: fish
contributors:
    - ["MySurmise", "https://github.com/MySurmise"]
    - ["Geo Maciolek", "https://github.com/GeoffMaciolek"]
translators:
    - ["Mario Stabile", "https://github.com/mariostabile1"]
filename: learnfish-it.fish
lang: it-it
---

Fish (**f**riendly **i**nteractive **sh**ell) è il nome di una shell esotica. Si tratta di una shell la cui sintassi non deriva né dalla Bourne-Shell né dalla C-Shell.

Il vantaggio di fish è che molte caratteristiche che si desiderano in una shell moderna sono implementate out-of-the-box (già pronte), quindi non è necessario installare software aggiuntivi come zsh e oh-my-zsh.

Esempi di queste caratteristiche sono i suggerimenti automatici, i colori a 24 bit, il completamento delle pagine man (cioè fish analizza automaticamente le pagine man e suggerisce opzioni aggiuntive per i comandi) o la possibilità di modificare le impostazioni attraverso una pagina web (quando è installata un'interfaccia grafica).

È stato rilasciato nel Febbraio 2005.

- [Leggi di più](https://fishshell.com/docs/current/language.html)
- [Guida all'installazione](https://github.com/fish-shell/fish-shell#getting-fish)

## Guida

Verifica di avere l'ultima versione di fish shell, Questa guida è stata fatta con la 3.3.0. Per farlo, scrivi:

```
> fish -v
```

Per avviare la shell di fish, scrivi:

```
> fish
```

per uscire, scrivi:

```
> exit
```

o premi <kbd>Ctrl + D</kbd>

Ora, sin dall'inizio, c'è una cosa fastidiosa in fish. Il messaggio di benvenuto. A chi importa, giusto? Quando si avvia la shell, basta scrivere:

```
> set -U fish_greeting ""
```

Se volessi eseguire un singolo comando in bash, senza passare a quella shell, puoi scrivere: 

```
> bash -c 'echo "Questo testo sarà stampato sul terminale"'
```

Su fish, si possono usare sia le singole che le doppie virgolette.
Il carattere di escape è uno `\` (slash)

Puoi cambiare le configurazioni di fish modificando il file di config

```
> vim ~/.config/fish/config.fish
```

o aprendo l'interfaccia web:

```
> fish_config
```

Aggiungere qualcosa alla variabile PATH di fish è semplice:

```
> fish_add_path ~/cowsay
```

Questo puoi farlo con bash, eh? No, devi sempre cercarlo... Così è facile!

Ma c'è di più. La maggior parte dei comandi specifici per fish iniziano, hai indovinato, con 'fish'. Basta scrivere `fish` e premere <kbd>TAB</kbd>. Ed ecco una delle tante funzioni interessanti di fish: L'autocompletamento che **funziona.**
Ora puoi navigare con <kbd>TAB</kbd>, <kbd>Shift + TAB</kbd> e le frecce <kbd>←</kbd><kbd>↑</kbd><kbd>→</kbd><kbd>↓</kbd>.

Per avere aiuto, chiama il tuo psichiatra di fiducia oppure scrivi `man`. Ti mostrerà il manuale per quel comando, per esempio:  

```
> man set
```

Se finalmente hai provato fish, potrai vedere in questa shell qualcosa di diverso, il che è molto figo. Ogni cosa ha colori fantastici, se scrivi qualcosa di sbagliato viene segnato in rosso, senza nemmeno eseguirlo!, se si mette qualcosa tra virgolette, si vede dove finisce e perchè quella citazione non funziona.

fish ha varie altre cose interessanti, come le wildcards (o carattere jolly).
Per esempio, scrivi:

```
> ls *.fish
```

Questo elencherà ogni file .fish nella directory corrente.

Puoi avere multiple wildcards per comando o anche una wildcard ricorsiva, `**`, il che significa che includerà tutti i sotto-file e le sotto-directory presenti nella directory corrente.
Per esempio, il seguente comando restituirà (in questo caso):

```
> ls ~/images/**.jpg

~/images/nudini/pewdiepie.jpg
~/images/nudini/peppa.jpg
~/images/screenshots/2020-42-69.jpg
~/images/omegalul.jpg
```

Ovviamente, puoi anche inviare l'output di un comando ad un'altro con una pipe

```
>echo . Qui ci sarà un testo | grep [udense]
```

scrivere su un file:

```
>echo Questo\ é\ un\ testo > file.txt
```

(notato il carattere di escape?)
Aggiungere a un file:

```
>echo Questa\ è\ una\ riga >> file.txt
>echo Questa\ è\ un'altra\ riga >> file.txt
```

Per l'autompletamento, basta premere sempre <kbd>TAB</kbd>. Ti sorprenderà quante cose conosce fish

Per usare le variabili, basta scrivere `$VAR`, come in bash.

```
> echo "La mia home è $HOME"
La mia home è /home/mioutente
```

Qui arriva la differenza tra le virgolette singole e doppie. Se usi una variabile dentro singole virgolette, non verrà sostituita.

```
> echo 'La mia home è $HOME'
La mia home è $HOME
```

Più info sulle variabili successivamente.

Per eseguire due comandi, separali con `;`

```
> echo Lol; echo questo è divertente
```

Lo status dell'ultimo comando eseguito è contenuto in `$status`

Puoi usare && per due comandi che dipendolo l'uno dall'altro.

```
> set var lol && echo $var
```

Puoi anche usare `and`  che esegue solo se il comando precedente ha avuto successo, `or` che esegue solo se il comando precedente fallisce, e `not`
che inverte lo stato di output del comando.

Per esempio:

```
> if not echo Testo testo, bla bla bla
      echo Altro testo, bla bla  
  end
```

(Ovviamente tutto questo lo puoi fare nella shell)

---

Adesso cominciamo con la parte di scripting di fish.

Come per ogni shell, puoi non solo eseguire comandi nella shell, ma anche da file, salvati come file `.fish`.
(Puoi anche eseguire file `.sh` con la sintassi di fish, ma io uso sempre l'estensione `.fish` per la sintassi di fish per distinguerla dai file in bash)

```fish
# Questo è un commento in fish.
# 
# Se esegui un file senza specificare un interprete, 
# cioè il programma che fa girare il tuo script, è necessario per dire alla shell,
# dove l'interprete è posizionato. 
# In fish basta aggiungere questo commento nella prima linea del tuo script:

#!/bin/fish

# Quando esegui lo script tramite, per esempio, fish /path/to/script.fish
# non è necessario, perchè hai specificato fish come interprete

# Cominciamo con le variabili.
# per l'uso interno a un programma, puoi usare la sintassi
set nome 'La mia variabile'

# Utilizzo...
set -x nome valore
# per esportare, o
set -e nome
# per Eliminare

# una variabile settata con uno spazio non viene inviata come due argomenti, ma come uno solo, come ci si aspetterebbe.
set directoryBella 'Directory Bella'
mkdir $directoryBella

# Questo creerà una sola directory, come ci si aspetterebbe, non due come in bash...
# chi vorrebbe una roba del genere? è UnA FeAtUrE n0n Un BuG...

# puoi anche avere liste in forma di variabili. Questo ha senso, perchè se volessi una variabile che crei due directory, basta dare a mkdir un elenco di nomi di directory.

# puoi anche contare le istanze nella lista con:
count $PATH

# Non solo è tutto fantastico, ma in fish, tutto è anche una lista. 
# $PWD per esempio è una lista di lunghezza 1. 
# Per fare una lista, basta dare al comando set più argomenti:
set list argomento1 argomento2 argomento3

# in questo modo puoi anche inserire qualcosa in una variabile pre-esistente:
set PATH $PATH ~/cowsay/

# Ma, come precedentemente menzionato, abbiamo anche un'altro modo più semplice per farlo, specialmente in fish.
# Come per ogni Array/Lista, puoi anche accedergli con
$listavar[2]

# ci sono anche gli intervalli
$listavar[1..5]

# puoi anche usare numeri negativi 
$listavar[-1]
# accesso all'ultimo elemento.

# Quando combini due liste di variabili puoi anche usare un bel prodotto cartesiano::
set a 1 2 3
set 1 a b c
echo $a$1
# Restituirà : 1a 2a 3a 1b 2b 3b 1c 2c 3c

# Naturalmente, se li si separa, saranno visti come due argomenti separati e li stamperà uno dopo l'altro. QUESTO è il comportamento che ci si aspetta da @bash.

# Ci sono anche altre cose utili, come la sostituzione di comandi. Per esempio, quando vuoi che ti sia restituito l'output di due comandi in una sola riga. In bash lo faresti in questo modo
echo "`ls` è in $PWD" 
# oppure
echo "$(ls) è in $PWD" 

# secondo me, non è necessario. Scrivo sempre l'apostrofo sbagliato. Perchè non usare semplicemente le parentesi, come in fish?
echo (ls) è in $PWD

# Yep, è facile. E grazie all'highlighting di fish lo puoi vedere istantaneamente, se lo scrivi correttamente.

# E, come ci si aspetterebbe, a mio avviso, i comandi non funzionano tra virgolette. Voglio dire, perchè bash? Ok adesso la smetto. Ma in fish, basta fare:
echo (ls)" è in $PWD"
# oppure
set miavar "Il file"(ls -a)" è nella directory $PWD"
# creerà una lista con la stringa e tutti i file. Prova. Non è una figata?

# E per semparare le variabili in diversi argomenti, basta mettere uno spazio:

set miavar "I file" (ls -a) " sono nella directory $PWD"

# Ci sono anche if, else if, else
if grep fish /etc/shells
    echo Trovato fish
else if grep bash /etc/shells
    echo Trovato bash
else
    echo Non ho trovato niente
end

# Risulta un pò strano confrontare due cose con un solo segno =, ovviamente perchè non en abbiamo bisogno per settare le variabili, ma comunque... e la parola chiave "test":
if test $var = "test"
    echo si 
else 
    echo no
end

# Naturalmente, ci sono anche gli switch case
switch $OS
case Linux
    echo "Sei un grande"
case Windows
    echo "Potresti provare fish su WSL"
case MacOS
    echo "Su MacOS c'è fish!"
case '*'
    echo "quale OS è $OS, per favore?"
end


# le funzioni in fish prendono gli argomenti attraverso la variabile $argv. La sintassi è la seguente:

function stampa
    echo $argv
end

# Ci sono anche gli eventi, come l'evento "fish_exit" (Cosa farà mai, hmm?).

# Puoi usarli aggiungendoli alle definizioni di funzione:

function in_uscita --on-event fish_exit
    echo fish si sta chiuendo
end

# trova gli eventi con il comando
functions --handlers


# Puoi usare il comando functions per approfondire, beh, le funzioni. 
# Per esempio puoi far stampare il codice sorgente di ogni funzione:
functions cd
functions print
# oppure ottenere il nome di ogni funzione:
functions

# Ci sono i cicli while, ovviamente
while test $var = lol
    echo lol
end

# Cicli for (con le wildcards, sono ancora meglio):
for immagine in *.jpg
    echo $immagine
end

# c'è un equivalente di range(0, 5) in Python, quindi puoi anche fare il classico ciclo for con i numeri:

set files (ls)
for numeri in (seq 10)
    echo "$files[$numeri] è il file numero $numeri"
end

# Bello!

# L'equivalente di bashrc non è fishrc, ma il già citato file config.fish in ~/.config/fish/
# Per aggiungere una funzione a fish, però, occorre creare un semplice file .fish in quella directory. Non incollare la funzione nel file config.fish. È brutto. 
# Se avete altro da dire, aggiugete pure, ma queste sono le basi più importanti.
```
