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

Git est un système de contrôle distribué et un système de gestion de code source.

Git accompli cela via une série d'instantanés de votre projet, et il fonctionne
avec ces instantanés pour vous permettre de versionner et de gérer votre code source.

## Concepts de versionnage

### Qu'est ce qu'un controle de version?

Le controle de version est un système qui enregistre les modifications apportées à 
un fichier au cours du temps.

### Versionnage Centralisé VS Versionnage Distribué

* Le controle de version centralisé se concentre sur la synchronisation, le suivi et la sauvegarde des fichiers.
* Le controle de version distribué se concentre sur le partage des modifications. Chaque modification
a un identifiant unique.
* Les systèmes distribués n'ont pas de structure définie. Avec Git, vous pouvez facilement avoir un système organisé comme SVN.

[Information Complémentaire](http://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)

### Pourquoi utiliser Git?

* Peut fonctionner offline.
* La collaboration avec d'autres participants est facile!
* La gestion des branches est facile!
* La fusion est facile!
* Git est rapide.
* Git est flexible.

## Architecture Git


### Dépot (Repository)

Un ensemble de fichiers, répertoires, historique, instantanés (commits) et pointeurs (HEADS).
Il faut l'imaginer comme une strucutre de données pour le code source, avec les attributs
pour chaque élément qui permet d'accéder à l'historique des révisions, parmi d'autres choses.

Un dépot Git est composé d'un répertoire .git et d'une arborescence de travail.

### Répertoire .git (présent à la racine du dépot)

Le répertoire .git contient toutes les configurations, logs, branches, HEAD, et bien plus encore.
[Liste Détaillée](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Arborescence de travail (présent à la racine du dépot)

L'arborescence de travail représente basiquement les répertoires et fichiers dans le dépot. 
Cet élément vous est souvent présenté comme votre dossier de travail.

### Index (présent dans le répertoire .git)

L'index est la zone de travail dans git. C'est basiquement la couche qui separe le dossier de
travail du dépot Git. Cela donne aux développeurs plus de latitude sur ce qui est envoyé vers le dépot.

### Commit (instantané)

Un commit Git est un instantané des modifications sur l'arborescence de travail.
Par exemple, si vous avez ajouté 5 fichiers, et supprimé 2 autres fichiers, ces changements
seront contenu dans un commit. Ce commit peut ensuite être poussé vers un dépot, ou non!

### Branche

Une branche est un pointeur vers le dernier commit réalisé. 

### HEAD et head (présent dans le répertoire .git)

HEAD est un pointeur qui pointe vers la branche courante. Un dépot Git ne possède qu'un HEAD actif.
head est un pointeur qui pointe vers n'importe quel commit. Un dépot peut avoir une multitude de heads.

### Etapes de Git
* Modifié - Des changement ont été réalisés sur un fichier. 
* Staged  - Marque un fichier modifié comme étant retenu pour être inclus dans le prochain commit
* Committed - Les fichiers ont été envoyés vers la base de données Git

### Ressources conceptuelles

* [Git pour les informaticiens](http://eagain.net/articles/git-for-computer-scientists/)
* [Git pours les designers](http://hoth.entp.com/output/git_for_designers.html)


## Commandes


### init

Cration d'un dépot Git vide. Les paramètres du dépot, les informations enregistrées
et d'autres informations sont stockées dans un répertoire nommé ".git".

```bash
$ git init
```

### config

Pour définir la configuration. 
To configure settings. Que ce soit pour le dépot, le système lui-même, ou une configuration
globale (le fichier de configuration globale est `~/.gitconfig` ).


```bash
# Affiche & Enregistre quelques variable basique de configuration (Globale)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Plus d'information à propos de git config.](http://git-scm.com/docs/git-config)

### help

Pour vous fournir un accès rapide à un guide extrèmement détaillé pour chaque commande. 
Ou juste pour vous proposer un rappel rapide de quelques commandes.

```bash
# Lister les commandes rapidement accessibles 
$ git help

# Lister toutes les commandes accessibles 
$ git help -a

# aide spécifique à une commande - manuel utilisateur
# git help <nom_de_la_commande>
$ git help add
$ git help commit
$ git help init
# or git <nom_de_la_commande> --help
$ git add --help
$ git commit --help
$ git init --help
```

### fichiers ignorés

Pour ignorer volontairement des fichiers et/ou des répertoires. 
Typiquement utilisés pour des fichiers privés ou temporaires qui seraient 
autrement partagés avec la communauté dans le dépot.
```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```


### status

Pour lister les écarts entre le fichier "index" (espace de travail) et le commit HEAD.

```bash
# Va afficher la branche, les fichiers "untracked", les changements et autres différences
$ git status

# Pour connaitre les autres options proposées par git status
$ git help status
```

### add

Pour ajouter des fichiers à la liste des fichiers à comparer. Si les nouveaux fichiers
présents dans l'espace de travail ne sont pas ajoutés, il ne seront pas inclus dans les commits!

```bash
# ajoute un fichier de l'espace de travail
$ git add HelloWorld.java

# ajoute un fichier d'un répertoire spécifique
$ git add /chemin/vers/le/fichier/HelloWorld.c

# Utilisation d'une expression régulière
$ git add ./*.java
```

Cette commande ajoute le fichier à la liste des fichiers observés, aucun commit vers
le dépot n'est réalisé.

### branch

Gestion des branches. Vous pouvez voir, éditer, créer ou effacer des branches avec cette commande.

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

Mise à jour de tous les fichiers dans l'espace de travail pour être cohérent avec la version ciblée
dans l'index, ou l'arborescence spécifiée.

```bash
# bascule d'un dépot - par défaut vers la branche master
$ git checkout
# Bascule vers un branche spécifique
$ git checkout branchName
# Création d'une nouvelle branche et bascule vers celle-ci
# equivalent à "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Clonage, ou copie, d'un dépot existant dans un nouveau répertoire. Cette commande ajoute
également le suivi à distance des branches pour chaque branche du dépot cloné, ce qui
vous permet de pousser les modifications vers la branche distante.

```bash
# Clonage de learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
# clonage superficiel - clonage plus rapide qui récupère seulement le dernier commit 
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git
# clonage d'une branche spécifique
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Stocke le contenu actuel de l'index dans un nouveau "commit". Ce commit contient
les changement réalisés et un message est créé par l'utilisateur.

```bash
# commit avec un message
$ git commit -m "Ajout de la fonction multiplyNumbers() dans HelloWorld.c"

# prendre en compte automatiquement les fichiers modifiés ou supprimés dans le 'staged', 
mais pas les nouveaux fichiers, puis commiter
$ git commit -a -m "foo.php modifié et bar.php supprimé"

# modification du commit précédent (efface le dernier commit et le remplace)
$ git commit --amend -m "message corrigé"
```

### diff

Affiche les différences entre un fichier dans l'espace de travail, l'index et les commits

```bash
# Affiche les différences entre l'espace de travail et l'index 
$ git diff

# Affiche les différences entre l'index et le commit le plus récent
$ git diff --cached

# Affiche les différences entre l'espace de travail et le commit le plus récent
$ git diff HEAD
```

### grep

Permet de rechercher rapidement un dépot.

Configurations optionnelles :

```bash
# Merci à Travis Jeffery pour ces lignes
# Active l'affichage des numéros de lignes dans les résultats du grep
$ git config --global grep.lineNumber true

# Rend les résultats de la recherche plus lisible, avec regroupement
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

Affiche la liste des commits pour le dépot

```bash
# Affiche tous les commits
$ git log

# Affiche seulement le message du commit et sa référence
$ git log --oneline

# Affiche les messages avec merge uniquement
$ git log --merges
```

### merge

Fusionne les changements de commits extérieurs dans la branche courante.

```bash
# Merge une branche spécifique dans la branche courante.
$ git merge branchName

# Toujours générer un commit de merge lors de la fusion
$ git merge --no-ff branchName
```

### mv

Renommer ou déplacer un fichier

```bash
# Renommer un fichier
$ git mv HelloWorld.c HelloNewWorld.c

# Déplacer un fichier
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Forcer le renommage ou le déplacement
# "existingFile" existe déjà dans le répertoire, va être écrasé
$ git mv -f myFile existingFile
```

### pull

Récupération d'un dépot et fusion des données avec une autre branche.

```bash
# Mise à jour du dépot local, en fusionnant les nouveaux changements
# Depuis le dépot "origin", branche "master".
# git pull <remote> <branch>
# git pull => implicitement par défaut => git pull origin master
$ git pull origin master

# Fusion des changements de la branche distante "branch" et rebase
# branch est commité dans le dépot local, comme : "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Pousse et fusionne les modifications d'une branche vers une branche d'un dépot distant.

```bash
# Pousse et fusionne les modifications d'un dépot local vers un dépot 
# distant nommé "origin" dans la branche "master".
# git push <remote> <branch>
# git push => implicitement par défaut => git push origin master
$ git push origin master

# Pour lier la branche locale avec la branche distante, ajouter l'option -u :
$ git push -u origin master
# Maintenant, à chaque push depuis la branche locale, utiliser le raccourci :
$ git push
```

### stash

stash met de côté les modifications non finalisées et les stocke temporairement dans un coin.
Ces modifications peuvent être réappliquées à tout moment.

Disons que vous avez réalisé un travail dans votre dépot git, mais vous voulez télécharger
depuis un dépot distant. Puisque vous avez des modifications non commitées sur certains fichiers,
vous n'êtes pas capable d'exécuter 'git pull'. A la place, vous pouvez exécuter 'git stash' 
pour sauvegarder vos modifications.

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Maintenant, vous pouvez mettre à jour depuis le dépot distant

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

Vous pouvez visualiser quelles modifications vous avez stashé avec la commande `git stash list`.
Les modifications sont stockées dans une pile Last-In-First-Out, votre modification la plus
récente sera en haut de la liste.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

Maintenant, ré-appliquons les modifications en les resortant de la pile.

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

`git stash apply` réalise la même chose

Maintenant, vous pouvez retourner travailler sur ce sujet!

[Lecture complémentaire.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (attention)

Prend tous les changements committés sur une branche, et les rejoue sur une autre branche.
* Ne pas rebaser les commits qui ont été poussés sur un dépot public*.

```bash
# Rebase experimentBranch sur la branche master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Lecture complémentaire.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (attention)

Réinitialise le HEAD courant à l'état spécifié. Cela permet d'annuler des merges,
pulls, commits, adds, et autres. C'est une bonne commande mais également dangereuse
si vous ne savez pas ce que vous faites.

```bash
# Réinitialise la liste des fichiers pris en compte pour le prochain commit (staging area) 
pour être cohérent avec le dernier commit, sans modification de l'espace de travail
$ git reset

# Réinitialise la liste des fichiers pris en compte pour le prochain commit (staging area) 
pour être cohérent avec le dernier commit, en écrasant l'espace de travail
# Reset the staging area, to match the latest commit, and overwrite working dir
$ git reset --hard

# Déplace la branche courante vers le commit spécifié (sans modification du répertoire)
# toutes les modifications existent toujours dans le répertoire.
$ git reset 31f2bb1

# Déplace la branche courante vers le commit spécifié 
# et rend cohérent le répertoire de travail (efface les modifications non commitées
# et tous les commits existants après le commit spécifié).
$ git reset --hard 31f2bb1
```

### rm

L'opposé de git add, git rm supprime les fichiers de l'arborescence de travail courante.

```bash
# suppression de  HelloWorld.c
$ git rm HelloWorld.c

# Supprime un fichier d'un répertoire spécifique
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
