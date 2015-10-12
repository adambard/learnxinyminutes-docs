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

### Instantané (Commit)

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

Create an empty Git repository. The Git repository's settings, stored information,
and more is stored in a directory (a folder) named ".git".

```bash
$ git init
```

### config

To configure settings. Whether it be for the repository, the system itself,
or global configurations ( global config file is `~/.gitconfig` ).


```bash
# Print & Set Some Basic Config Variables (Global)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Learn More About git config.](http://git-scm.com/docs/git-config)

### help

To give you quick access to an extremely detailed guide of each command. Or to
just give you a quick reminder of some semantics.

```bash
# Quickly check available commands
$ git help

# Check all available commands
$ git help -a

# Command specific help - user manual
# git help <command_here>
$ git help add
$ git help commit
$ git help init
# or git <command_here> --help
$ git add --help
$ git commit --help
$ git init --help
```

### ignore files

To intentionally untrack file(s) & folder(s) from git. Typically meant for
private & temp files which would otherwise be shared in the repository.
```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```


### status

To show differences between the index file (basically your working copy/repo)
and the current HEAD commit.


```bash
# Will display the branch, untracked files, changes and other differences
$ git status

# To learn other "tid bits" about git status
$ git help status
```

### add

To add files to the staging area/index. If you do not `git add` new files to the
staging area/index, they will not be included in commits!

```bash
# add a file in your current working directory
$ git add HelloWorld.java

# add a file in a nested dir
$ git add /path/to/file/HelloWorld.c

# Regular Expression support!
$ git add ./*.java
```

This only adds a file to the staging area/index, it doesn't commit it to the
working directory/repo.

### branch

Manage your branches. You can view, edit, create, delete branches using this command.

```bash
# list existing branches & remotes
$ git branch -a

# create a new branch
$ git branch myNewBranch

# delete a branch
$ git branch -d myBranch

# rename a branch
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# edit a branch's description
$ git branch myBranchName --edit-description
```

### checkout

Updates all files in the working tree to match the version in the index, or specified tree.

```bash
# Checkout a repo - defaults to master branch
$ git checkout
# Checkout a specified branch
$ git checkout branchName
# Create a new branch & switch to it
# equivalent to "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Clones, or copies, an existing repository into a new directory. It also adds
remote-tracking branches for each branch in the cloned repo, which allows you to push
to a remote branch.

```bash
# Clone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
# shallow clone - faster cloning that pulls only latest snapshot
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git
# clone only a specific branch
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Stores the current contents of the index in a new "commit." This commit contains
the changes made and a message created by the user.

```bash
# commit with a message
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

# automatically stage modified or deleted files, except new files, and then commit
$ git commit -a -m "Modified foo.php and removed bar.php"

# change last commit (this deletes previous commit with a fresh commit)
$ git commit --amend -m "Correct message"
```

### diff

Shows differences between a file in the working directory, index and commits.

```bash
# Show difference between your working dir and the index
$ git diff

# Show differences between the index and the most recent commit.
$ git diff --cached

# Show differences between your working dir and the most recent commit
$ git diff HEAD
```

### grep

Allows you to quickly search a repository.

Optional Configurations:

```bash
# Thanks to Travis Jeffery for these
# Set line numbers to be shown in grep search results
$ git config --global grep.lineNumber true

# Make search results more readable, including grouping
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Search for "variableName" in all java files
$ git grep 'variableName' -- '*.java'

# Search for a line that contains "arrayListName" and, "add" or "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Google is your friend; for more examples
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Display commits to the repository.

```bash
# Show all commits
$ git log

# Show only commit message & ref
$ git log --oneline

# Show merge commits only
$ git log --merges
```

### merge

"Merge" in changes from external commits into the current branch.

```bash
# Merge the specified branch into the current.
$ git merge branchName

# Always generate a merge commit when merging
$ git merge --no-ff branchName
```

### mv

Rename or move a file

```bash
# Renaming a file
$ git mv HelloWorld.c HelloNewWorld.c

# Moving a file
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Force rename or move
# "existingFile" already exists in the directory, will be overwritten
$ git mv -f myFile existingFile
```

### pull

Pulls from a repository and merges it with another branch.

```bash
# Update your local repo, by merging in new changes
# from the remote "origin" and "master" branch.
# git pull <remote> <branch>
# git pull => implicitly defaults to => git pull origin master
$ git pull origin master

# Merge in changes from remote branch and rebase
# branch commits onto your local repo, like: "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Push and merge changes from a branch to a remote & branch.

```bash
# Push and merge changes from a local repo to a
# remote named "origin" and "master" branch.
# git push <remote> <branch>
# git push => implicitly defaults to => git push origin master
$ git push origin master

# To link up current local branch with a remote branch, add -u flag:
$ git push -u origin master
# Now, anytime you want to push from that same local branch, use shortcut:
$ git push
```

### stash

Stashing takes the dirty state of your working directory and saves it on a stack
of unfinished changes that you can reapply at any time.

Let's say you've been doing some work in your git repo, but you want to pull
from the remote. Since you have dirty (uncommited) changes to some files, you
are not able to run `git pull`. Instead, you can run `git stash` to save your
changes onto a stack!

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Now you can pull!

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

You can see what "hunks" you've stashed so far using `git stash list`.
Since the "hunks" are stored in a Last-In-First-Out stack, our most recent change will be at top.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

Now let's apply our dirty changes back by popping them off the stack.

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

`git stash apply` does the same thing

Now you're ready to get back to work on your stuff!

[Additional Reading.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (caution)

Take all changes that were committed on one branch, and replay them onto another branch.
*Do not rebase commits that you have pushed to a public repo*.

```bash
# Rebase experimentBranch onto master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Additional Reading.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (caution)

Reset the current HEAD to the specified state. This allows you to undo merges,
pulls, commits, adds, and more. It's a great command but also dangerous if you don't
know what you are doing.

```bash
# Reset the staging area, to match the latest commit (leaves dir unchanged)
$ git reset

# Reset the staging area, to match the latest commit, and overwrite working dir
$ git reset --hard

# Moves the current branch tip to the specified commit (leaves dir unchanged)
# all changes still exist in the directory.
$ git reset 31f2bb1

# Moves the current branch tip backward to the specified commit
# and makes the working dir match (deletes uncommited changes and all commits
# after the specified commit).
$ git reset --hard 31f2bb1
```

### rm

The opposite of git add, git rm removes files from the current working tree.

```bash
# remove HelloWorld.c
$ git rm HelloWorld.c

# Remove a file from a nested dir
$ git rm /pather/to/the/file/HelloWorld.c
```

## Further Information

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
