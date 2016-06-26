---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
translators:
    - ["Christian Grasso", "http://chris54721.net"]
filename: LearnGit-it.txt
lang: it-it
---

Git è un sistema di
[controllo versione distribuito](https://it.wikipedia.org/wiki/Controllo_versione_distribuito)
e di gestione del codice sorgente.

Git esegue una serie di _snapshot_ per salvare lo stato di un progetto, così
facendo può fornirti la possibilità di gestire il tuo codice e di salvarne lo
stato assegnando delle versioni.

## Basi del controllo versione

### Cos'è il controllo versione?

Il controllo versione (_Version Control_ o _Versioning_) è un sistema che
registra le modifiche apportate a uno o più file nel tempo.

### Controllo versione centralizzato e distribuito

* Il controllo versione centralizzato si concentra sulla sincronizzazione, il
  monitoraggio e il backup dei file.
* Il controllo versione distribuito si concentra sulla condivisione delle
  modifiche. Ogni modifica ha un identificatore univoco.
* I sistemi distribuiti non hanno una struttura definita. Si potrebbe creare
  ad esempio un sistema centralizzato simile a SVN utilizzando Git.

[Ulteriori informazioni](http://git-scm.com/book/it/v1/Per-Iniziare-Il-Controllo-di-Versione)

### Perchè usare Git?

* Consente di lavorare offline.
* Collaborare con altre persone è semplice!
* Utilizzare i branch (rami di sviluppo) è semplice!
* Git è veloce.
* Git è flessibile.

## Architettura di Git

### Repository

Un insieme di file, cartelle, registrazioni della cronologia e versioni.
Immaginalo come una struttura dati del codice, con la caratteristica che ogni
"elemento" del codice ti fornisce accesso alla sua cronologia delle revisioni,
insieme ad altre cose.

Un repository comprende la cartella .git e il working tree.

### Cartella .git (componente del repository)

La cartella .git contiene tutte le configurazioni, i log, i rami e altro.
[Lista dettagliata](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Working Tree (componente del repository)

Si tratta semplicemente delle cartelle e dei file presenti nel repository.
Spesso viene indicato come "directory di lavoro" ("working directory").

### Index (componente della cartella .git)

L'Index è l'area di staging di Git. Si tratta di un livello che separa il
working tree dal repository. Ciò fornisce agli sviluppatori più controllo su
cosa viene inviato al repository.

### Commit

Un commit è uno snapshot di una serie di modifiche apportate al working tree.
Ad esempio, se hai aggiunto 5 file e ne hai rimossi 2, ciò sarà registrato in
un commit. Il commit può essere pushato (inviato) o meno ad altri repository.

### Branch (ramo)

Un branch (ramo) è essenzialmente un puntatore all'ultimo commit che hai
effettuato. Effettuando altri commit, il puntatore verrà automaticamente
aggiornato per puntare all'ultimo commit.

### Tag

Un tag è un contrassegno applicato a un punto specifico nella cronologia dei
commit. Di solito i tag vengono utilizzati per contrassegnare le versioni
rilasciate (v1.0, v1.1, etc.).

### HEAD e head (componenti della cartella .git)

HEAD (in maiuscolo) è un puntatore che punta al branch corrente. Un repository
può avere solo 1 puntatore HEAD *attivo*.

head (in minuscolo) è un puntatore che può puntare a qualsiasi commit. Un
repository può avere un numero qualsiasi di puntatori head.

### Stadi di Git
* _Modified_ - Un file è stato modificato, ma non è ancora stato effettuato
  un commit per registrare le modifiche nel database di Git
* _Staged_ - Un file modificato è stato contrassegnato per essere incluso nel
  prossimo commit
* _Committed_ - È stato effettuato un commit e le modifiche sono state
  registrate nel database di Git

## Comandi

### init

Crea un repository Git vuoto. Le impostazioni e le informazioni del repository
sono salvate nella cartella ".git".

```bash
$ git init
```

### config

Utilizzato per configurare le impostazioni, sia specifiche del repository, sia
a livello globale. Le impostazioni globali sono salvate in `~/.gitconfig`.

```bash
$ git config --global user.email "email@example.com"
$ git config --global user.name "Nome utente"
```

[Ulteriori informazioni su git config](http://git-scm.com/docs/git-config)

### help

Fornisce una documentazione molto dettagliata di ogni comando.

```bash
# Mostra i comandi più comuni
$ git help

# Mostra tutti i comandi disponibili
$ git help -a

# Documentazione di un comando specifico
# git help <nome_comando>
$ git help add
$ git help commit
$ git help init
# oppure git <nome_comando> --help
$ git add --help
$ git commit --help
$ git init --help
```

### Ignorare file

Per impedire intenzionalmente che file privati o temporanei vengano inviati
al repository Git.

```bash
$ echo "temp/" >> .gitignore
$ echo "privato.txt" >> .gitignore
```


### status

Mostra le differenza tra lo stato attuale del working tree e l'attuale commit
HEAD.

```bash
$ git status
```

### add

Aggiunge file alla staging area, ovvero li contrassegna per essere inclusi nel
prossimo commit. Ricorda di aggiungere i nuovi file, altrimenti non saranno
inclusi nei commit!

```bash
# Aggiunge un file nella directory attuale
$ git add HelloWorld.java

# Aggiunge un file in una sottocartella
$ git add /path/to/file/HelloWorld.c

# Il comando supporta le espressioni regolari
$ git add ./*.java

# Aggiunge tutti i file non ancora contrassegnati
$ git add --all
```

Questo comando contrassegna soltanto i file, senza effettuare un commit.

### branch

Utilizzato per gestire i branch (rami). Puoi visualizzare, modificare, creare o
eliminare branch utilizzando questo comando.

```bash
# Visualizza i branch e i remote
$ git branch -a

# Crea un nuovo branch
$ git branch nuovoBranch

# Elimina un branch
$ git branch -d nomeBranch

# Rinomina un branch
$ git branch -m nomeBranch nuovoNomeBranch

# Permette di modificare la descrizione di un branch
$ git branch nomeBranch --edit-description
```

### tag

Utilizzato per gestire i tag.

```bash
# Visualizza i tag esistenti
$ git tag
# Crea un nuovo tag
# L'opzione -m consente di specificare una descrizione per il tag.
# Se l'opzione -m non viene aggiunta, Git aprirà un editor per consentire
# l'inserimento del messaggio.
$ git tag -a v2.0 -m 'Versione 2.0'
# Mostra informazioni relative a un tag
# Include informazioni sul creatore del tag, la data di creazione, e il
# messaggio assegnato al tag oltre alle informazioni sul commit.
$ git show v2.0
```

### checkout

Consente di cambiare branch o ripristinare i file a una revisione specifica.
Tutti i file nel working tree vengono aggiornati per corrispondere alla versione
presente nel branch o nel commit specificato.

```bash
# Effettua il checkout di un repository - il branch predefinito è 'master'
$ git checkout
# Effettua il checkout di un branch specifico
$ git checkout nomeBranch
# Crea un nuovo branch e ne effettua il checkout
# Equivalente a "git branch <nomeBranch>; git checkout <nomeBranch>"
$ git checkout -b nuovoBranch
```

### clone

Clona, o copia, un repository esistente in una nuova directory. Inoltre,
aggiunge dei branch _remote-tracking_, utilizzati per monitorare i branch
remoti corrispondenti a quelli locali, e consentendo così di inviare le
modifiche al repository remoto.

```bash
# Clona learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
# Clona solo l'ultima revisione di un repository
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git
# Clona solo un branch specifico
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Effettua uno _snapshot_ dello stato attuale del working tree e registra le
modifiche in un nuovo commit. Il commit contiene, oltre alle modifiche apportate,
anche l'autore e una descrizione.

```bash
# Crea un nuovo commit con un messaggio
$ git commit -m "Aggiunta la funzione multiplyNumbers() in HelloWorld.c"

# Aggiunge (git add) automaticamente i file modificati o eliminati (ESCLUSI
# i nuovi file) e quindi effettua il commit
$ git commit -a -m "Modificato foo.php e rimosso bar.php"

# Modifica l'ultimo commit (il comando elimina il commit precedente e lo
# sostituisce con uno nuovo)
$ git commit --amend -m "Messaggio corretto"
```

### diff

Mostra la differenza tra un file nel working tree e la sua versione nell'index,
in un branch o ad un commit specifico.

```bash
# Mostra la differenza tra il working tree e l'index
$ git diff

# Mostra la differenza tra l'index e il commit più recente
$ git diff --cached

# Mostra la differenza tra il working tree e un commit specifico
$ git diff <commit>

# Mostra la differenza tra due commit
$ git diff <commit1> <commit2>
```

### grep

Consente di effettuare una ricerca veloce nel repository.

```bash
# Cerca "variableName" nei file Java
$ git grep 'variableName' -- '*.java'

# Cerca una riga contenente "arrayListName" E "add" oppure "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Impostazioni relative a `git grep`:

```bash
# Mostra il numero delle righe
$ git config --global grep.lineNumber true

# Rende i risultati più leggibili
$ git config --global alias.g "grep --break --heading --line-number"
```

### log

Mostra la cronologia dei commit inviati al repository.

```bash
# Mostra tutti i commit
$ git log

# Mostra ogni commit su una sola riga
$ git log --oneline

# Mostra solo i commit legati ai merge
$ git log --merges
```

### merge

Effettua un "merge", ovvero unisce le modifiche di un branch in quello attuale.

```bash
# Unisce il branch specificato a quello attuale
$ git merge nomeBranch

# Genera un commit in ogni caso dopo aver eseguito il merge
$ git merge --no-ff nomeBranch
```

### mv

Rinomina o sposta un file.

```bash
# Rinomina un file
$ git mv HelloWorld.c HelloNewWorld.c

# Sposta un file
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Forza l'esecuzione del comando
# Se un file "nuovoNomeFile" esiste già nella directory, verrà sovrascritto
$ git mv -f nomeFile nuovoNomeFile
```

### pull

Aggiorna il repository effettuando il merge delle nuove modifiche.

```bash
# Aggiorna il branch attuale dal remote "origin"
$ git pull

# Di default, git pull aggiorna il branch attuale effettuando il merge
# delle nuove modifiche presenti nel branch remote-tracking corrispondente
$ git pull

# Aggiorna le modifiche dal branch remoto, quindi effettua il rebase dei commit
# nel branch locale
# Equivalente a: "git pull <remote> <branch>; git rebase <branch>"
$ git pull origin master --rebase
```

### push

Invia ed effettua il merge delle modifiche da un branch locale ad uno remoto.

```bash
# Invia ed effettua il merge delle modifiche dal branch "master"
# al remote "origin".
# git push <remote> <branch>
$ git push origin master

# Di default, git push invia ed effettua il merge delle modifiche
# dal branch attuale al branch remote-tracking corrispondente
$ git push

# Per collegare il branch attuale ad uno remoto, basta aggiungere l'opzione -u
$ git push -u origin master
```

### stash

Salva lo stato attuale del working tree in una lista di modifiche non ancora
inviate al repository con un commit che possono essere applicate nuovamente
in seguito.

Questo comando può essere utile se, ad esempio, mentre stai effettuando delle
modifiche non ancora completate, hai bisogno di aggiornare il repository locale
con `git pull`. Poichè non hai ancora effettuato il commit di tutte le modifiche,
non sarà possibile effettuare il pull. Tuttavia, puoi utilizzare `git stash` per
salvare temporaneamente le modifiche e applicarle in seguito.

```bash
$ git stash
```

Ora puoi effettuare il pull:

```bash
$ git pull
```

A questo punto, come già suggerito dall'output del comando `git stash`, puoi
applicare le modifiche:

```bash
$ git stash apply
```

Infine puoi controllare che tutto sia andato bene:

```bash
$ git status
```

Puoi visualizzare gli accantonamenti che hai effettuato finora utilizzando:

```bash
$ git stash list
```

### rebase (attenzione)

Applica le modifiche effettuate su un branch su un altro branch.
*Non effettuare il rebase di commit che hai già inviato a un repository pubblico!*

```bash
# Effettua il rebase di experimentBranch in master
$ git rebase master experimentBranch
```

[Ulteriori informazioni](https://git-scm.com/book/it/v1/Diramazioni-in-Git-Rifondazione)

### reset (attenzione)

Effettua il reset del commit HEAD attuale ad uno stato specifico.
Questo comando consente di annullare `merge`, `pull`, `commit`, `add` e altro.
Tuttavia, può essere pericoloso se non si sa cosa si sta facendo.

```bash
# Effettua il reset della staging area (annullando le aggiunte e le rimozioni
# di file dal repository, senza modificare il working tree)
$ git reset

# Effettua il reset completo della staging area, ovvero annulla qualsiasi
# modifica al repository eliminando definitivamente anche tutte le modifiche
# ai file non inviate e ripristinando il working tree
$ git reset --hard

# Effettua il reset del branch attuale al commit specificato (lasciando il
# working tree intatto)
$ git reset 31f2bb1

# Effettua il reset completo del branch attuale al commit specificato,
# eliminando qualsiasi modifica non inviata
$ git reset --hard 31f2bb1
```

### rm

Consente di rimuovere un file dal working tree e dal repository.
Per eliminare un file solo dal working tree ma non dal repository, è invece
necessario utilizzare `/bin/rm`.

```bash
# Elimina un file nella directory attuale
$ git rm HelloWorld.c

# Elimina un file da una sottocartella
$ git rm /pather/to/the/file/HelloWorld.c
```
