---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["kultprok", "http://www.kulturproktologie.de"]
lang: de-de
---

Git ist eine verteilte Versions- und Quellcodeverwaltung. 

Es nimmt Schnappschüsse der Projekte auf, um mit diesen Schnappschüssen verschiedene Versionen unterscheiden und den Quellcode verwalten zu können.

Anmerkung des Übersetzers: Einige englische Begriffe wie *Repository*, *Commit* oder *Head* sind idiomatische Bestandteile im Umgang mit Git. Sie wurden nicht übersetzt.

## Konzepte der Versionsverwaltung

### Was ist Versionsverwaltung?

Eine Versionsverwaltung erfasst die Änderungen einer Datei oder eines Verzeichnisses im Verlauf der Zeit.

### Vergleich zwischen Zentraler und verteilter Versionverwaltung

* Zentrale Versionsverwaltung konzentriert sich auf das Synchronisieren, Verfolgen und Sichern von Dateien.
* Verteilte Versionsverwaltung konzentriert sich auf das Teilen der Änderungen. Jede Änderung hat eine eindeutige ID.
* Verteilte Systeme haben keine vorbestimmte Struktur. Ein SVN-ähnliches, zentrales System wäre mit Git ebenso umsetzbar.

[Weiterführende Informationen](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Warum Git?

* Ist offline einsetzbar.
* Einfache Kollaboration!
* Branching ist einfach!
* Branching ist schnell!
* Merging ist einfach!
* Git ist schnell.
* Git ist flexibel.

## Die Architektur von Git


### Repository (Repo)

Ein Satz von Dateien, Verzeichnissen, Historieneinträgen, Commits und Heads. Stell es dir wie eine Quellcode-Datenstruktur vor, unter anderem mit der Eigenschaft, dass alle *Elemente* dir Zugriff auf die Revisionshistorie geben.

Ein Repository besteht in Git aus dem .git-Verzeichnis und dem Arbeitsverzeichnis.

### .git-Verzeichnis (Teil des Repositorys)

Das .git-Verzeichnis enthält alle Einstellungen, Logs, Branches, den HEAD und mehr.
[Ausführliche Übersicht](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Arbeitsverzeichnis (Teil des Repositorys)

Dies sind die Verzeichnisse und Dateien in deinem Repository, also z.B. dein Programmcode.

### Index (Teil des .git-Verzeichnisses)

Der Index ist die Staging-Area von Git. Es ist im Grunde eine Ebene, die Arbeitsverzeichnis vom Repository trennt. Sie gibt Entwicklern mehr Einfluss darüber, was ins Git-Repository eingeht.

### Commit

Ein Commit ist ein Schnappschuss von Änderungen in deinem Arbeitsverzeichnis. Wenn du zum Beispiel 5 Dateien hinzugefügt und 2 andere entfernt hast, werden diese Änderungen im Commit (Schnappschuss) enthalten sein. Dieser Commit kann dann in andere Repositories gepusht werden. Oder nicht!

### Branch

Ein Branch, ein Ast oder Zweig, ist im Kern ein Pointer auf den letzten Commit, den du gemacht hast. Während des Commits wird der Pointer automatisch auf diesen Stand gebracht und zeigt dann auf den neuen letzten Commit.

### HEAD und head (Teil des .git-Verzeichnisses)

HEAD ist ein Pointer auf den aktuellen Branch. Ein Repository hat nur einen *aktiven* HEAD. 

Ein *head* ist ein Pointer, der auf einen beliebigen Commit zeigt.  Ein Repository kann eine beliebige Zahl von *heads* enthalten.

### Konzeptionelle Hintergründe

* [Git For Computer Scientists](http://eagain.net/articles/git-for-computer-scientists/)
* [Git For Designers](http://hoth.entp.com/output/git_for_designers.html)


## Befehle


### init

Erstelle ein leeres Git-Repository im aktuellen Verzeichnis. Die Einstellungen, gespeicherte Informationen und mehr zu diesem Git-Repository werden in einem Verzeichnis namens *.git* angelegt.

```bash
$ git init
```

### config

Hiermit werden Einstellungen vorgenommen. Dies kann das Repository, das System selbst oder globale Einstellungen betreffen.

```bash
# Grundlegende Config-Variablen ausgeben und setzen
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Mehr über git config](http://git-scm.com/docs/git-config)

### help

Schnellzugriff auf extrem detaillierte Anleitungen zu allen Befehlen. Oder als Erinnerung zu semantischen Eigenheiten.

```bash
# Übersicht gängiger Befehle
$ git help

# Übersicht aller verfügbaren Befehle
$ git help -a

# Befehlspezifische Hilfe - Bedienungsanleitung
# git help <gesuchter_befehl>
$ git help add
$ git help commit
$ git help init
```

### status

Zeigt die Unterschiede zwischen Index (im Grunde dein Arbeitsverzeichnis/-repository) und dem aktuellen HEAD an.


```bash
# Zeigt den Branch, nicht-verfolgte Dateien, Änderungen und andere Unterschiede an
$ git status

# Anderes Wissenswertes über git status anzeigen
$ git help status
```

### add

Hinzufügen von Dateien zum Arbeitsverzeichnis/-repository. Wenn du neue Dateien nicht mit *git add* zum Arbeitsverzeichnis hinzufügst, werden sie nicht in den Commit aufgenommen!

```bash
# Fügt eine Datei deinem aktuellen Arbeitsverzeichnis hinzu
$ git add HelloWorld.java

# Fügt eine Datei aus einem verschachtelten Verzeichnis hinzu
$ git add /path/to/file/HelloWorld.c

# Reguläre Ausdrücke werden unterstützt!
$ git add ./*.java
```

### branch

Verwalte alle Branches. Du kannst sie mit diesem Befehl ansehen, bearbeiten, neue erzeugen oder löschen.

```bash
# Liste alle bestehenden Branches und Remotes auf
$ git branch -a

# Erstelle einen neuen Branch
$ git branch myNewBranch

# Lösche einen Branch
$ git branch -d myBranch

# Benenne einen Branch um
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# Ändere die Beschreibung eines Branchs
$ git branch myBranchName --edit-description
```

### checkout

Bringt alle Dateien im Arbeitsverzeichnis auf den Stand des Index oder des angegebenen Branches.

```bash
# Ein Repo auschecken - wenn nicht anders angegeben ist das der master
$ git checkout
# Eine Datei auschecken - sie befindet sich dann auf dem aktuellen Stand im Repository
$ git checkout /path/to/file
# Einen bestimmten Branch auschecken
$ git checkout branchName
# Erstelle einen neuen Branch und wechsle zu ihm. Wie: "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Ein bestehendes Repository in ein neues Verzeichnis klonen oder kopieren. Es fügt außerdem für jedes geklonte Repository remote-tracking Branches hinzu. Du kannst auf diese Remote-Branches pushen.

```bash
# Klone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
```

### commit

Speichert die aktuellen Inhalte des Index in einen neuen *Commit*. Dieser Commit enthält alle Änderungen und eine vom Benutzer erstellte Beschreibung der Änderungen.

```bash
# Commit mit Beschreibung erstellen.
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

# Alle veränderten oder gelöschten Dateien außer neue Dateien werden gestaged und dann wird ein Commit erstellt.
$ git commit -a -m "Modified foo.php and removed bar.php"

# Ändert den letzten Commit (der letzte Commit wird mit einem neuen Commit ersetzt)
$ git commit --amend -m "Correct message"
```

### diff

Zeigt die Unterschiede zwischen Dateien vom Arbeitsverzeichnis, dem Index und Commits an.

```bash
# Unterschiede zwischen deinem Arbeitsverzeichnis und dem Index anzeigen
$ git diff

# Unterschiede zwischen dem Index und dem aktuellsten Commit anzeigen
$ git diff --cached

# Unterschiede zwischen deinem Arbeitsverzeichnis und dem aktuellsten Commit anzeigen
$ git diff HEAD

# Unterschiede zwischen dem Index und dem aktuellsten Commit (betrifft nur Dateien im Index)
$ git diff --staged
```

### grep

Schnell ein Repository durchsuchen.

Optionale Einstellungen:

```bash
# Vielen Dank an Travis Jeffery für die Hinweise.
# Zeilennummerierung in grep-Suchergebnissen
$ git config --global grep.lineNumber true

# Suchergebnisse lesbarer gestalten, auch Gruppierungen sind möglich
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Suche nach "variableName" in allen java-Dateien
$ git grep 'variableName' -- '*.java'

# Suche nach eine Zeile, die "arrayListName" und  "add" oder "remove" enthält
$ git grep -e 'arrayListName' --and \( -e add -e remove \) 
```

Google ist dein Freund; für mehr Beispiele:
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Zeige Commits für das Repository an.

```bash
# Zeige alle Commits
$ git log

# Zeige die Anzahl n an Commits
$ git log -n 10

# Zeige nur Merges an
$ git log --merges
```

### merge

*Merge*, also verschmelze, alle Änderungen von externen Commits in den aktuellen Branch.

```bash
# Merge den angegebenen Branch in den aktuellen.
$ git merge branchName

# Erstelle immer einen Merge-Commit.
$ git merge --no-ff branchName
```

### mv

Eine Datei umbenennen oder verschieben.	

```bash
# Umbenennen
$ git mv HelloWorld.c HelloNewWorld.c

# Verschieben
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Umbenennung oder Verschieben erzwingen
# "existingFile" besteht schon im Verzeichnis, wird überschrieben mit "myFile"
$ git mv -f myFile existingFile
```

### pull

Führe einen Pull (zieht alle Daten eines Repositories) aus und führt einen Merge mit einem anderen Branch durch.

```bash
# Update deines lokalen Repos, indem ein Merge der neuen Änderungen
# von den remote-liegenden "origin"- und "master"-Branches durchgeführt wird.
# git pull <remote> <branch>
# git pull => impliziter Verweis auf origin und master
$ git pull origin master

# Führt einen Merge von Änderungen eines remote-Branch und ein Rebase
# des Branch-Commits im lokalen Repo durch. Wie: pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Führe einen Push, ein Hochladen, und einen Merge von Änderungen eines remote-Branch mit einem Branch aus.

```bash
# Führe Push und Merge von Änderungen des lokalen Repo zu einem
# remote-Branch namens "origin" und dem "master"-Branch aus.
# git push <remote> <branch>
# git push => impliziter Verweis auf => git push origin master
$ git push origin master
```

### rebase (mit Vorsicht einsetzen) 

Nimm alle Änderungen, die in einem Branch durch Commits vorgenommen wurden, und übertrage sie auf einen anderen Branch. Achtung: Führe keinen Rebase von Commits durch, die auf ein öffentliches Repo gepusht wurden!

```bash
# Rebase "experimentBranch" in den "master"-Branch
# git rebase <basisbranch> <themenbranch>
$ git rebase master experimentBranch
```

[Weiterführende Informationen](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (mit Vorsicht einsetzen)

Setze den aktuellen HEAD auf den angegebenen Zustand zurück. So können Merges, Pulls, Commits, Hinzufügungen und andere Änderungen rückgängig gemacht werden. Es ist ein hervorragender Befehl, aber auch sehr gefährlich, wenn du nicht weißt, was du tust.

```bash
# Setze die Staging-Area zurück, um dem letzten Commit zu entsprechen (das Verzeichnis bleibt unberührt)
$ git reset

# Setze die Staging-Area zurück, um dem letzten Commit zu entsprechen und überschreibe das Arbeitsverzeichnis
$ git reset --hard

# Bewegt die Spitze des Branches zu dem angegebenen Commit (das Verzeichnis bleibt unberührt)
# Alle Änderungen bleiben im Verzeichnis erhalten
$ git reset 31f2bb1

# Bewegt die Spitze des Branches zurück zu dem angegebenen Commit
# und gleicht die Arbeitsverzeichnisse ab (löscht nicht vom Commit erfasste Änderungen und alle Commits,
# die dem angegebenen Commit folgen).
$ git reset --hard 31f2bb1
```

### rm

Das Gegenteil von *git add*. *git rm* löscht Dateien vom Arbeitsverzeichnis.

```bash
# Entferne HelloWorld.c
$ git rm HelloWorld.c

# Entferne eine Datei aus einem verschachtelten Verzeichnis
$ git rm /pather/to/the/file/HelloWorld.c
```

## Weiterführende Informationen

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [gitflow - Ein Modell um mit Branches zu arbeiten](http://nvie.com/posts/a-successful-git-branching-model/)
