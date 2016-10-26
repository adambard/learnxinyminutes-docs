---
category: tool
tool: vim
filename: LearnVim.txt
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["Thibault", "https://github.com/napnac"]
lang: fr-fr
---


[Vim](www.vim.org)
(Vi IMproved) est le clone le plus populaire de l'éditeur de texte vi sous Unix.
Vim est un éditeur de texte omniprésent sur les systèmes de type Unix, et a pour
objectif la rapidité ainsi que l'augmentation de la productivité. Il a de 
nombreux raccourcis claviers pour une navigation et une édition plus rapide.

## Navigation basique avec Vim

```
    vim <ficher>     # Ouvre <fichier> avec vim
    :q               # Quitte vim
    :w               # Sauvegarde le fichier actuel
    :wq              # Sauvegarde le fichier actuel et quitte vim
    :q!              # Quitte vim sans sauvegarder
                     # ! *force* l'exécution de :q, ce qui par conséquent 
                     # oblige vim à quitter sans sauvegarder
    :x               # Sauvegarde le fichier et quitte vim (raccourcis de :wq)

    u                # Annuler
    CTRL+R           # Rétablir

    h                # Déplace le curseur vers la gauche
    j                # Déplace le curseur vers le bas
    k                # Déplace le curseur vers le haut
    l                # Déplace le curseur vers la droite

    # Mouvements au sein d'une ligne

    0                # Va au début de la ligne
    $                # Va à la fin de la ligne
    ^                # Va au premier caractère non blanc de la ligne

    # Rechercher dans un texte

    /mot             # Surligne toutes les occurrences du mot après le curseur
    ?mot             # Surligne toutes les occurrences du mot avant le curseur
    n                # Déplace le curseur sur la prochaine occurrence du mot recherché
    N                # Déplace le curseur sur la précédente occurrence du mot recherché

    :%s/abc/def/g    # Transforme les 'abc' en 'def' sur chaque ligne du texte
    :s/abc/def/g     # Transforme les 'abc' en 'def' sur la ligne actuelle

    # Se déplacer vers un caractère

    f<caractère>     # Se déplace en avant jusqu'à <caractère>
    t<caractère>     # Se déplace en avant juste avant <caractère>

    # Par exemple
    f<               # Se déplace en avant jusqu'à <
    t<               # Se déplace en avant juste avant <
    
    # Se déplacer dans un mot

    w                # Avance d'un mot
    b                # Recule d'un mot
    e                # Se déplace jusqu'à la fin du mot actuel

    # D'autres raccourcis pour se déplacer

    gg               # Va au début du fichier
    G                # Va à la fin du fichier
    :NB              # Va à la ligne numéro NB (où NB est un nombre)
    H                # Se déplace jusqu'en haut de l'écran
    M                # Se déplace jusqu'au milieu de l'écran
    L                # Se déplace jusqu'en bas de l'écran
```

## Modes

Vim est basé sur le concept de **modes**.

Mode Commande  - pour se déplacer et exécuter des commandes (vim démarre dans ce mode)
Mode Insertion - pour éditer le fichier
Mode Visuel    - pour sélectionner du texte et réaliser des opérations dessus
Mode Ex        - pour entrer des commandes avec ':'

```
    i                # Mode insertion, avant le curseur
    a                # Mode insertion, après le curseur
    v                # Mode visuel
    :                # Mode ex
    <esc>            # 'Echap' permet de revenir dans le mode commande

    # Copier/Coller du texte

    y                # Copie le texte sélectionné
    yy               # Copie la ligne actuelle
    d                # Supprime ce qui est sélectionné
    dd               # Supprime la ligne actuelle
    p                # Colle après le curseur
    P                # Colle avant le curseur
    x                # Supprime le caractère sous le curseur
```

## La "Grammaire" de Vim

Vim peut être vu comme un ensemble de commande sous la forme
'Verbe-Modificateur-Nom' :

Verbe        - notre action
Modificateur - la manière de faire l'action
Nom          - l'objet désigné par l'action

Quelques exemples importants de 'Verbes', 'Modificateurs', et de 'Noms' :

```
    # 'Verbes'
 
    d                # Supprime
    c                # Transforme
    y                # Copie
    v                # Sélectionne

    # 'Modificateurs'

    i                # A l'intérieur
    a                # Autour
    NB               # Nombre
    f                # Cherche quelque chose et se déplace dessus
    t                # Cherche quelque chose et se déplace juste avant
    /                # Cherche une chaîne de caractères après le curseur
    ?                # Cherche une chaîne de caractères avant le curseur

    # 'Noms'

    w                # Mot
    s                # Phrase
    p                # Paragraphe
    b                # Bloc
    
    # Exemple de 'phrases' ou commandes

    d2w              # Supprime 2 mots
    cis              # Transforme l'intérieur de la phrase
    yip              # Copie l'intérieur du paragraphe
    ct<              # Transforme le texte du curseur jusqu'au caractère avant le <
    d$               # Supprime jusqu'à la fin de la ligne
```

## Quelques raccourcis et astuces

```
    >                # Indente la sélection d'un bloc
    <                # Dé-indente la sélection d'un bloc
    :earlier 15m     # Retrouve le document comme il était il y a 15 minutes
    :later 15m       # Inverse la commande précédente
    ddp              # Echange la position de deux lignes consécutives (dd puis p)
    .                # Répète la dernière action effectuée
```

## Macros

Les macros sont des actions enregistrables.
Quand on commence à enregistrer une macro, Vim enregistre **toutes** les actions
et les commandes que vous utilisez, jusqu'à ce que vous arrêtiez d'enregistrer.
Lorsque vous appelez une macro, elle applique exactement les mêmes actions et 
commandes sur le texte sélectionné.

```
    qa               # Commence l'enregistrement de la macro 'a'
    q                # Arrête l'enregistrement
    @a               # Appelle la macro 'a'
```

### Configuration de ~/.vimrc

Le fichier .vimrc est utilisé pour configurer Vim lors du démarrage.

Voici un exemple de fichier ~/.vimrc :

```
" Exemple de ~/.vimrc
" 2015.10 

" Nécessaire à Vim pour être 'iMproved'
set nocompatible

" Détermine l'extension du fichier à partir du nom pour permettre une indentation
" automatique intelligente, etc.
filetype indent plugin on

" Active la coloration syntaxique
syntax on

" Une meilleure complétion de la ligne de commande
set wildmenu

" Utilise une recherche insensible à la case sauf quand on utilise des majuscules
set ignorecase
set smartcase

" Quand on commence une nouvelle ligne et qu'aucun type d'indentation n'est activé
" on utilise la même indentation que sur la ligne précédente
set autoindent

" Affiche le numéro de la ligne sur la gauche de l'écran
set number

" Options d'indentation, à changer en fonction des préférences personnelles

" Nombre d'espaces visuels par tabulation
set tabstop=4

" Nombre d'espaces par tabulation
set softtabstop=4

" Nombre d'espaces indentés avec les opérations d'indentations (>> et <<)
set shiftwidth=4

" Convertis les tabulations en espaces
set expandtab

" Active des tabulations et des espaces intelligents pour l'indentation et l'alignement
set smarttab
```

### Références

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
