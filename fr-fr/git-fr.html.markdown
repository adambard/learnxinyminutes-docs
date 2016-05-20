---
category: tool
tool: git
filename: LearnGit.txt
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
translators:
    - ["Mr.Bricodage", "https://github.com/MrBricodage"]
lang: fr-fr

---

Git est un syst�me de contr�le distribu� et un syst�me de gestion de code source.

Git accompli cela via une s�rie d'instantan�s de votre projet, et il fonctionne
avec ces instantan�s pour vous permettre de versionner et de g�rer votre code source.

## Concepts de versionnage

### Qu'est ce qu'un controle de version?

Le controle de version est un syst�me qui enregistre les modifications apport�es � 
un fichier au cours du temps.

### Versionnage Centralis� VS Versionnage Distribu�

* Le controle de version centralis� se concentre sur la synchronisation, le suivi et la sauvegarde des fichiers.
* Le controle de version distribu� se concentre sur le partage des modifications. Chaque modification
a un identifiant unique.
* Les syst�mes distribu�s n'ont pas de structure d�finie. Avec Git, vous pouvez facilement avoir un syst�me organis� comme SVN.

[Information Compl�mentaire](http://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)

### Pourquoi utiliser Git?

* Peut fonctionner offline.
* La collaboration avec d'autres participants est facile!
* La gestion des branches est facile!
* La fusion est facile!
* Git est rapide.
* Git est flexible.

## Architecture Git


### D�pot (Repository)

Un ensemble de fichiers, r�pertoires, historique, instantan�s (commits) et pointeurs (HEADS).
Il faut l'imaginer comme une strucutre de donn�es pour le code source, avec les attributs
pour chaque �l�ment qui permet d'acc�der � l'historique des r�visions, parmi d'autres choses.

Un d�pot Git est compos� d'un r�pertoire .git et d'une arborescence de travail.

### R�pertoire .git (pr�sent � la racine du d�pot)

Le r�pertoire .git contient toutes les configurations, logs, branches, HEAD, et bien plus encore.
[Liste D�taill�e](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Arborescence de travail (pr�sent � la racine du d�pot)

L'arborescence de travail repr�sente basiquement les r�pertoires et fichiers dans le d�pot. 
Cet �l�ment vous est souvent pr�sent� comme votre dossier de travail.

### Index (pr�sent dans le r�pertoire .git)

L'index est la zone de travail dans git. C'est basiquement la couche qui separe le dossier de
travail du d�pot Git. Cela donne aux d�veloppeurs plus de latitude sur ce qui est envoy� vers le d�pot.

### Commit (instantan�)

Un commit Git est un instantan� des modifications sur l'arborescence de travail.
Par exemple, si vous avez ajout� 5 fichiers, et supprim� 2 autres fichiers, ces changements
seront contenu dans un commit. Ce commit peut ensuite �tre pouss� vers un d�pot, ou non!

### Branche

Une branche est un pointeur vers le dernier commit r�alis�. 

### HEAD et head (pr�sent dans le r�pertoire .git)

HEAD est un pointeur qui pointe vers la branche courante. Un d�pot Git ne poss�de qu'un HEAD actif.
head est un pointeur qui pointe vers n'importe quel commit. Un d�pot peut avoir une multitude de heads.

### Etapes de Git
* Modifi� - Des changement ont �t� r�alis�s sur un fichier. 
* Staged  - Marque un fichier modifi� comme �tant retenu pour �tre inclus dans le prochain commit
* Committed - Les fichiers ont �t� envoy�s vers la base de donn�es Git

### Ressources conceptuelles

* [Git pour les informaticiens](http://eagain.net/articles/git-for-computer-scientists/)
* [Git pours les designers](http://hoth.entp.com/output/git_for_designers.html)


## Commandes


### init

Cration d'un d�pot Git vide. Les param�tres du d�pot, les informations enregistr�es
et d'autres informations sont stock�es dans un r�pertoire nomm� ".git".

```bash
$ git init
```

### config

Pour d�finir la configuration. 
To configure settings. Que ce soit pour le d�pot, le syst�me lui-m�me, ou une configuration
globale (le fichier de configuration globale est `~/.gitconfig` ).


```bash
# Affiche & Enregistre quelques variable basique de configuration (Globale)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Plus d'information � propos de git config.](http://git-scm.com/docs/git-config)

### help

Pour vous fournir un acc�s rapide � un guide extr�mement d�taill� pour chaque commande. 
Ou juste pour vous proposer un rappel rapide de quelques commandes.

```bash
# Lister les commandes rapidement accessibles 
$ git help

# Lister toutes les commandes accessibles 
$ git help -a

# aide sp�cifique � une commande - manuel utilisateur
# git help <nom_de_la_commande>
$ git help add
$ git help commit
$ git help init
# or git <nom_de_la_commande> --help
$ git add --help
$ git commit --help
$ git init --help
```

### fichiers ignor�s

Pour ignorer volontairement des fichiers et/ou des r�pertoires. 
Typiquement utilis�s pour des fichiers priv�s ou temporaires qui seraient 
autrement partag�s avec la communaut� dans le d�pot.
```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```


### status

Pour lister les �carts entre le fichier "index" (espace de travail) et le commit HEAD.

```bash
# Va afficher la branche, les fichiers "untracked", les changements et autres diff�rences
$ git status

# Pour connaitre les autres options propos�es par git status
$ git help status
```

### add

Pour ajouter des fichiers � la liste des fichiers � comparer. Si les nouveaux fichiers
pr�sents dans l'espace de travail ne sont pas ajout�s, il ne seront pas inclus dans les commits!

```bash
# ajoute un fichier de l'espace de travail
$ git add HelloWorld.java

# ajoute un fichier d'un r�pertoire sp�cifique
$ git add /chemin/vers/le/fichier/HelloWorld.c

# Utilisation d'une expression r�guli�re
$ git add ./*.java
```

Cette commande ajoute le fichier � la liste des fichiers observ�s, aucun commit vers
le d�pot n'est r�alis�.

### branch

Gestion des branches. Vous pouvez voir, �diter, cr�er ou effacer des branches avec cette commande.

```bash
# Liste des branches existantes
$ git branch -a

# creation d'une nouvelle branche
$ git branch maNouvelleBranche

# suppression d'une branche
$ git branch -d maBranche

# renommer une branche
# git branch -m <ancien_nom> <nouveau_nom>
$ git branch -m myBranchName myNewBranchName

# Editer la description d'une branche
$ git branch myBranchName --edit-description
```

### checkout

Mise � jour de tous les fichiers dans l'espace de travail pour �tre coh�rent avec la version cibl�e
dans l'index, ou l'arborescence sp�cifi�e.

```bash
# bascule d'un d�pot - par d�faut vers la branche master
$ git checkout
# Bascule vers un branche sp�cifique
$ git checkout branchName
# Cr�ation d'une nouvelle branche et bascule vers celle-ci
# equivalent � "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Clonage, ou copie, d'un d�pot existant dans un nouveau r�pertoire. Cette commande ajoute
�galement le suivi � distance des branches pour chaque branche du d�pot clon�, ce qui
vous permet de pousser les modifications vers la branche distante.

```bash
# Clonage de learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
# clonage superficiel - clonage plus rapide qui r�cup�re seulement le dernier commit 
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git
# clonage d'une branche sp�cifique
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Stocke le contenu actuel de l'index dans un nouveau "commit". Ce commit contient
les changement r�alis�s et un message est cr�� par l'utilisateur.

```bash
# commit avec un message
$ git commit -m "Ajout de la fonction multiplyNumbers() dans HelloWorld.c"

# prendre en compte automatiquement les fichiers modifi�s ou supprim�s dans le 'staged', 
mais pas les nouveaux fichiers, puis commiter
$ git commit -a -m "foo.php modifi� et bar.php supprim�"

# modification du commit pr�c�dent (efface le dernier commit et le remplace)
$ git commit --amend -m "message corrig�"
```

### diff

Affiche les diff�rences entre un fichier dans l'espace de travail, l'index et les commits

```bash
# Affiche les diff�rences entre l'espace de travail et l'index 
$ git diff

# Affiche les diff�rences entre l'index et le commit le plus r�cent
$ git diff --cached

# Affiche les diff�rences entre l'espace de travail et le commit le plus r�cent
$ git diff HEAD
```

### grep

Permet de rechercher rapidement un d�pot.

Configurations optionnelles :

```bash
# Merci � Travis Jeffery pour ces lignes
# Active l'affichage des num�ros de lignes dans les r�sultats du grep
$ git config --global grep.lineNumber true

# Rend les r�sultats de la recherche plus lisible, avec regroupement
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Recherche de "variableName" dans tous les fichiers java
$ git grep 'variableName' -- '*.java'

# Recherche d'un ligne qui contient "arrayListName" et, "add" ou "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Google is your friend; pour plus d'exemples
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Affiche la liste des commits pour le d�pot

```bash
# Affiche tous les commits
$ git log

# Affiche seulement le message du commit et sa r�f�rence
$ git log --oneline

# Affiche les messages avec merge uniquement
$ git log --merges
```

### merge

Fusionne les changements de commits ext�rieurs dans la branche courante.

```bash
# Merge une branche sp�cifique dans la branche courante.
$ git merge branchName

# Toujours g�n�rer un commit de merge lors de la fusion
$ git merge --no-ff branchName
```

### mv

Renommer ou d�placer un fichier

```bash
# Renommer un fichier
$ git mv HelloWorld.c HelloNewWorld.c

# D�placer un fichier
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Forcer le renommage ou le d�placement
# "existingFile" existe d�j� dans le r�pertoire, va �tre �cras�
$ git mv -f myFile existingFile
```

### pull

R�cup�ration d'un d�pot et fusion des donn�es avec une autre branche.

```bash
# Mise � jour du d�pot local, en fusionnant les nouveaux changements
# Depuis le d�pot "origin", branche "master".
# git pull <remote> <branch>
# git pull => implicitement par d�faut => git pull origin master
$ git pull origin master

# Fusion des changements de la branche distante "branch" et rebase
# branch est commit� dans le d�pot local, comme : "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Pousse et fusionne les modifications d'une branche vers une branche d'un d�pot distant.

```bash
# Pousse et fusionne les modifications d'un d�pot local vers un d�pot 
# distant nomm� "origin" dans la branche "master".
# git push <remote> <branch>
# git push => implicitement par d�faut => git push origin master
$ git push origin master

# Pour lier la branche locale avec la branche distante, ajouter l'option -u :
$ git push -u origin master
# Maintenant, � chaque push depuis la branche locale, utiliser le raccourci :
$ git push
```

### stash

stash met de c�t� les modifications non finalis�es et les stocke temporairement dans un coin.
Ces modifications peuvent �tre r�appliqu�es � tout moment.

Disons que vous avez r�alis� un travail dans votre d�pot git, mais vous voulez t�l�charger
depuis un d�pot distant. Puisque vous avez des modifications non commit�es sur certains fichiers,
vous n'�tes pas capable d'ex�cuter 'git pull'. A la place, vous pouvez ex�cuter 'git stash' 
pour sauvegarder vos modifications.

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Maintenant, vous pouvez mettre � jour depuis le d�pot distant

```bash
git pull
```
`...changes apply...`

Now check that everything is OK

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```

Vous pouvez visualiser quelles modifications vous avez stash� avec la commande `git stash list`.
Les modifications sont stock�es dans une pile Last-In-First-Out, votre modification la plus
r�cente sera en haut de la liste.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

Maintenant, r�-appliquons les modifications en les resortant de la pile.

```bash
$ git stash pop
# On branch master
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#
#      modified:   index.html
#      modified:   lib/simplegit.rb
#
```

`git stash apply` r�alise la m�me chose

Maintenant, vous pouvez retourner travailler sur ce sujet!

[Lecture compl�mentaire.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (attention)

Prend tous les changements committ�s sur une branche, et les rejoue sur une autre branche.
* Ne pas rebaser les commits qui ont �t� pouss�s sur un d�pot public*.

```bash
# Rebase experimentBranch sur la branche master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Lecture compl�mentaire.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (attention)

R�initialise le HEAD courant � l'�tat sp�cifi�. Cela permet d'annuler des merges,
pulls, commits, adds, et autres. C'est une bonne commande mais �galement dangereuse
si vous ne savez pas ce que vous faites.

```bash
# R�initialise la liste des fichiers pris en compte pour le prochain commit (staging area) 
pour �tre coh�rent avec le dernier commit, sans modification de l'espace de travail
$ git reset

# R�initialise la liste des fichiers pris en compte pour le prochain commit (staging area) 
pour �tre coh�rent avec le dernier commit, en �crasant l'espace de travail
# Reset the staging area, to match the latest commit, and overwrite working dir
$ git reset --hard

# D�place la branche courante vers le commit sp�cifi� (sans modification du r�pertoire)
# toutes les modifications existent toujours dans le r�pertoire.
$ git reset 31f2bb1

# D�place la branche courante vers le commit sp�cifi� 
# et rend coh�rent le r�pertoire de travail (efface les modifications non commit�es
# et tous les commits existants apr�s le commit sp�cifi�).
$ git reset --hard 31f2bb1
```

### rm

L'oppos� de git add, git rm supprime les fichiers de l'arborescence de travail courante.

```bash
# suppression de  HelloWorld.c
$ git rm HelloWorld.c

# Supprime un fichier d'un r�pertoire sp�cifique
$ git rm /chemin/vers/le/fichier/HelloWorld.c
```

## Plus d'information

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [Udemy Git Tutorial: A Comprehensive Guide](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Git Immersion - A Guided tour that walks through the fundamentals of git](http://gitimmersion.com/)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - the simple guide](http://rogerdudler.github.io/git-guide/index.html)

* [Pro Git](http://www.git-scm.com/book/en/v2)
