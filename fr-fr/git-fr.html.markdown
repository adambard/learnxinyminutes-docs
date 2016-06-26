---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
translators:
    - ["Xuan-thi Nguyen", "http://github.com/mellenguyen"]
filename: LearnGit-fr.txt
lang: fr-fr
---

Git est un logiciel de contrôle de versions distribué et un système de gestion
du code source.

Il effectue sa tâche via des séries d'instantanés (snapshots) du projet, et
travaille avec ces instantanés afin de fournir les fonctionnalités de gestion
de version et de code source.

## Concepts du versionnage

### Qu'est ce que le contrôle de version ?

Le contrôle de version est un système qui enregistre les changements faits sur
un ou plusieurs fichiers au fil du temps.

### Versionnage centralisé VS Versionnage distribué

* Le contrôle de version centralisé se concentre sur la synchronisation, le
suivi et la sauvegarde des fichiers.
* Le contrôle de version distribué se focalise sur l'échange des changements.
Chaque changement a un identifiant unique.
* Les systèmes distribués n'ont pas de structure définie. Vous pouvez aisément
avoir un système centralisé de type SVN, avec Git.

[Informations additionnelles](http://git-scm.com/book/fr/v1/D%C3%A9marrage-rapide-%C3%80-propos-de-la-gestion-de-version)

### Pourquoi utiliser Git ?

* Fonctionne hors ligne.
* Travailler avec les autres devient facile !
* Ramifier le travail (créer des branches différentes) est facile !
* Fusionner le travail est facile !
* Git est rapide.
* Git est flexible.

## Architecture Git


### Dépôt ("repository")

Un ensemble de fichiers, dossiers, historiques de modifications, commits
(validations de changements) et de heads (état courant, "tête").
Représentez-vous ceci comme une structure de données de code source, avec la
particularité que chaque "élement" vous donne, entre autres, accès à son
historique des révisions.

Un dépôt Git comprend un répertoire .git et "l'arbre de travail" (working tree).

### Répertoire .git (composant du dépôt)

Le répertoire .git contient toutes les configurations, logs (journaux),
branches, HEAD et plus.
[Liste détaillée (EN)](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Arbre de travail (composant du dépôt)

Il s'agit de l'ensemble des répertoires et fichiers de votre dépôt. Il est
souvent qualifié de répertoire de travail ("working directory").

### Index (composant du répertoire .git)

L'index est la zone de transit ("staging area") dans Git. Il s'agit d'une couche
séparant votre arbre de travail de votre dépôt Git. Ceci donne aux développeurs
plus de pouvoir sur ce qu'ils envoient au dépôt.

### Commit

Un "commit" (validation de changements) est un instantané d'un ensemble de
modifications de votre arbre de travail. Par exemple, si vous avez rajouté 5
fichiers et enlevé 2 autres, ces changements seront contenus dans un commit
(ou "snapshot", instantané). Ce commit peut ensuite être poussé ("pushed") dans
d'autres dépôts, ou non !

### Branches

Une branche consiste essentiellement en un pointeur vers le dernier commit que
vous avez fait. Au fur et à mesure de vos commits, ce pointeur se mettra
automatiquement à jour pour pointer vers le dernier commit.

### Etiquette ("tag")

Une étiquette est une marque sur un point spécifique de l'historique.
Typiquement, on utilise cette fonctionnalité pour marquer les états de
publication (v1.0, et ainsi de suite).

### HEAD and head (composant du répertoire .git)

HEAD est un pointeur pointant vers la branche courante. Un dépôt ne peut avoir
qu'un seul HEAD *actif*.
head est un pointeur pouvant pointer sur n'importe quel commit. Un dépôt peut
avoir un nombre illimité de heads.

### Les états dans Git
* Modifié - Des changements on été faits à un fichier mais ce dernier n'a pas
encore été rajouté à l'ensemble des fichiers Git
* Indexé ("staged") - Indique qu'un fichier modifié ira dans le prochain commit
* Validé ("committed") - Les fichiers ont été validés dans l'ensemble de
fichiers

### Ressources conceptuelles

* [Git pour les informaticiens (EN)](http://eagain.net/articles/git-for-computer-scientists/)
* [Git pour les designers (EN)](http://hoth.entp.com/output/git_for_designers.html)


## Commandes


### init

Créé un dépôt Git vide. Les paramètres du dépôt Git, les informations stockées
et plus sont dans un répertoire (un dossier) nommé ".git".

```bash
$ git init
```

### config

Configuration des paramètres. Que ce soit pour le dépôt, le système lui-même,
ou la configuration globale (le fichier de configuration globale
est `~/.gitconfig`).


```bash
# Lit et assigne quelques variables (globales) de configuration de base
$ git config --global user.email "monEmail@foo.com"
$ git config --global user.name "Mon nom"
```

[Apprenez-en plus à propos de git config.](https://git-scm.com/book/fr/v1/Personnalisation-de-Git-Configuration-de-Git)

### help

Vous donne un accès rapide à un guide extrêmement détaillé de chaque commande.
Ou juste vous donner un rappel rapide de la sémantique.

```bash
# Vérifie rapidement les commandes disponibles
$ git help

# Vérifie toutes les commandes disponibles
$ git help -a

# Aide pour une commande spécifique - manuel utilisateur
# git help <command_here>
$ git help add
$ git help commit
$ git help init
# ou git <command_here> --help
$ git add --help
$ git commit --help
$ git init --help
```

### ignorer des fichiers

Ne plus suivre certains fichiers et dossiers de Git.
Habituellement fait pour les fichiers privés et temporaires qui seraient,
autrement, partagés dans le dépôt.
```bash
$ echo "temp/" >> .gitignore
$ echo "cle_privee" >> .gitignore
```

### status

Montre les différences entre le fichier indexé (typiquement votre copie/dépôt
de travail) et le HEAD actuel.


```bash
# Affiche la branche, les fichiers non suivis, les changements et autres
différences
$ git status

# Pour en apprendre plus sur git status
$ git help status
```

### add

Rajoute des fichiers à la zone d'index. Si vous ne faites pas `git add` sur les
nouveaux fichiers, ils ne seront pas inclus dans les commits !

```bash
# rajoute un fichier dans votre répertoire de travail actuel
$ git add HelloWorld.java

# rajoute un fichier dans un répertoire imbriqué
$ git add /path/to/file/HelloWorld.c

# Gestion des expressions régulières !
$ git add ./*.java
```

On ne fait que rajouter des fichiers dans la zone d'index, on ne valide pas
les changements au répertoire/dépôt de travail.

### branch

Gère vos branches. Vous pouvez voir, éditer, créer et supprimer des branches en
utilisant cette commande.

```bash
# Liste les branches existantes et distantes
$ git branch -a

# Créé une nouvelle branche
$ git branch maNouvelleBranche

# Supprime une branche
$ git branch -d maBranche

# Renomme une branche
# git branch -m <anciennom> <nouveaunom>
$ git branch -m nomDeMaBranche nouveauNomDeMaBranche

# Edite la description d'une branche
$ git branch nomDeMaBranche --edit-description
```

### tag

Gère vos étiquettes

```bash
# Liste les étiquettes
$ git tag

# Créé une étiquette annotée
# L'option -m spécifie un message qui sera stockée dans l'étiquette.
# Si vous ne spécifiez pas de message pour une étiquette annotée,
# Git lance votre éditeur pour que vous puissiez le saisir.
$ git tag -a v2.0 -m 'ma version 2.0'

# Affiche des informations à propos de l'étiquette
# comprenant des informations sur l'auteur, la date du commit correspondant,
# et le message d'annotation avant d'afficher les informations du commit.
$ git show v2.0

# Pousse une seule étiquette dans le dépôt distant
$ git push origin v2.0

# Pousse beaucoup d'étiquettes dans le dépôt distant
$ git push origin --tags
```

### checkout

Met à jour tous les fichiers dans l'arbre de travail afin de correspondre à la
version de la zone d'index ou de l'arbre spécifié.

```bash
# Obtenir une copie de travail du dépôt - par défaut on prend la branche master
$ git checkout

# Bascule vers une branche spéficiée
$ git checkout nomDeLaBranche

# Créé une nouvelle branche et bascule sur celle-ci
# Revient à faire "git branch <name>; git checkout <name>"
$ git checkout -b nouvelleBranche
```

### clone

Clone (ou copie) un dépôt existant dans un nouveau répertoire. Rajoute
également les branches distantes pour chaque branche du dépôt clôné, ce qui
vous permet de pousser vers une branche distante.

```bash
# Clone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git

# Clone superficiel ("shallow clone") - clone plus rapide qui récupère
seulement le dernier instantané ("snapshot")
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git

# Clone seulement une branche spécifique
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Conserve le contenu actuel de la zone d'index dans un nouveau "commit." Ce
commit contient les changements faits, accompagnés d'un message écrit par son
auteur.

```bash
# Commit avec un message
$ git commit -m "Ajout de la fonction multiplierNombres() dans HelloWorld.c"

# Rajoute automatiquement dans l'index les fichiers modifiés ou supprimés,
# à l'exception des nouveaux fichiers, puis commit
$ git commit -a -m "Modification de foo.php et suppression de bar.php"

# Change le dernier commit (ceci supprime le commit précédent avec un
# nouveau commit)
$ git commit --amend -m "Message corrigé"
```

### diff

Montre les différences entre un fichier dans le répertoire de travail, la zone
d'index and les commits.

```bash
# Affiche les différences entre votre répertoire de travail et l'index
$ git diff

# Affiche les différences entre l'index et le plus récent commit.
$ git diff --cached

# Affiche les différences entre votre répertoire de travail et le plus récent
# commit
$ git diff HEAD
```

### grep

Permet de faire une recherche rapide dans le dépôt.

Configurations optionnelles :

```bash
# Merci à Travis Jeffery pour ce qui suit
# Affiche les numéros des lignes dans les résultats de la recherche grep
$ git config --global grep.lineNumber true

# Rend les résultats de recherche plus lisibles, en incluant les groupements
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Recherche de "nomDeVariable" dans tous les fichiers java
$ git grep 'nomDeVariable' -- '*.java'

# Recherche une ligne contenant "nomDeTableau", et "rajouter" ou "enlever"
$ git grep -e 'nomDeTableau' --and \( -e rajouter -e enlever \)
```

Google est votre ami; pour plus d'exemples :
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Affiche les commits d'un dépôt.

```bash
# Montre tous les commits
$ git log

# Montre seulement les messages de commits et leur référence
$ git log --oneline

# Montre seulement les commits commits des merges (fusions)
$ git log --merges
```

### merge

Fusionne les changements provenant de commits externes dans la branche
courante.

```bash
# Fusionne la branche spécifiée dans la branche courante.
$ git merge nomDeBranche

# Génère toujours un commit quand on fusionne
$ git merge --no-ff branchName
```

### mv

Renomme ou déplace un fichier

```bash
# Renomme un fichier
$ git mv HelloWorld.c HelloNewWorld.c

# Déplace un fichier
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Force le renommage ou le déplacement
# Si "fichierExistant" existe déjà dans le répertoire, il sera écrasé
$ git mv -f monFichier fichierExistant
```

### pull

Récupère la version d'un dépôt et la fusionne avec une autre branche.

```bash
# Met à jour votre dépôt local en y intégrant les changements
# depuis la branche "master" du dépôt distant "origin".
# git pull <remote> <branch>
$ git pull origin master

# Par défaut, git pull mettra à jour votre branche actuelle
# en y intégrant les nouveaux changements venant de sa branche distante suivie
$ git pull

# Intègre les changements de la branche distante et "rebase"
# les commits de la branche dans votre dépôt local, comme ceci:
#"git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Pousse et fusionne les changements d'une dépôt local vers une branche distante.

```bash
# Pousse et fusionne les changements d'un dépôt local vers la branche
# appelée "master" du dépôt distant "master".
# git push <remote> <branch>
$ git push origin master

# Par défaut, git push poussera et fusionnera les changements de la branche
# courante vers sa branche distante suivie.
$ git push

# Pour faire le lien entre la branche locale courante et sa branche distante,
# rajouter l'option -u :
$ git push -u origin master
# Dorénavant, à chaque fois que vous voulez pousser depuis cette même branche
# locale, utilisez ce raccourci :
$ git push
```

### stash

Sauvegarde ("stash") l'état actuel de votre espace de travail et le garde dans
pile de changements non finis que vous pouvez réappliquer n'importe quand.

Supposons que vous avez effectué du travail dans votre dépôt git, mais que vous
voulez récupérer la version de la branche distante. Depuis que vous avez des
changements "malpropres" (non commités) à quelques fichiers, vous ne pouvez pas
faire de `git pull`. A la place, vous pouvez utiliser `git stash` afin de
sauvegarder votre travail dans la pile !

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Vous pouvez maintenant pull !

```bash
git pull
```
`...changes apply...`

Vérifiez maintenant que tout est OK

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```

Vous pouvez constater quels "morceaux" vous avez stash jusque là en
utilisant `git stash list`.
Puisque les changements sont gardés dans une pile Last-In-First-Out, notre
changement le plus récent sera en premier.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 rajout du fichier index
stash@{1}: WIP on master: c264051 annulation de "rajout de la taille_fichier"
stash@{2}: WIP on master: 21d80a5 ajout des chiffres aux logs
```

Appliquons maintenant les changements en les enlevant de notre pile.

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

`git stash apply` effectue le même travail

Vous êtes maintenant prêt à retourner sur vos tâches de travail !

[Lecture additionelle.](https://git-scm.com/book/fr/v1/Utilitaires-Git-Le-remisage)

### rebase (attention)

Prend tous les changements qui ont été commités sur une branche, et les
ré-applique sur une autre branche.
*Ne rebasez pas les commits que vous avez poussés sur un dépôt publique*.

```bash
# Expérimentation d'un rebase dans la branche "master"
# git rebase <basebranch> <topicbranch>
$ git rebase master brancheExperience
```

[Lecture additionelle.](https://git-scm.com/book/fr/v1/Les-branches-avec-Git-Rebaser)

### reset (attention)

Réinitialise le pointeur HEAD courant à l'état spécifié. Ceci vous permet
d'annuler des fusions, des pulls, commits, ajouts et autres. C'est une commande
puissante mais également dangereuse si vous ne savez pas ce que vous faites.

```bash
# Réinitialise la zone d'index afin de correspondre au dernier commit (laisse
# le répertoire inchangé).
$ git reset

# Réinitialise la zone d'index afin de correspondre au dernier commit et
# réécrit le répertoire de travail.
$ git reset --hard

# Déplace le pointeur de la branche courante au commit spécifié (laisse
# le répertoire inchangé). Tous les changements existents toujours dans
# le répertoire.
$ git reset 31f2bb1

# Déplace le pointeur de la branche courante en arrière, au commit spécifié
# et fait correspondre le répertoire de travail (supprime les changements
# non commités et tous les commits après le commit spécifié).
$ git reset --hard 31f2bb1
```

### rm

Le contraire de git add, git rm supprime les fichiers de l'arbre de travail
courant.

```bash
# Supprime HelloWorld.c
$ git rm HelloWorld.c

# Enlève un fichier d'un répertoire imbriqué.
$ git rm /chemin/vers/le/fichier/HelloWorld.c
```

## Informations complémentaires

* [tryGit - A fun interactive way to learn Git (EN)](http://try.github.io/levels/1/challenges/1)

* [Udemy Git Tutorial: A Comprehensive Guide (EN)](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [git-scm - Tutoriaux vidéos](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutoriaux et Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet (EN)](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys (EN)](http://www.gitguys.com/)

* [Git - the simple guide (EN)](http://rogerdudler.github.io/git-guide/index.html)

* [Livre Pro Git](http://www.git-scm.com/book/fr/v1)

* [Une introduction à Git et GitHub pour les débutants (tutoriel) (EN)](http://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)
