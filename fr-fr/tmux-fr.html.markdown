---
category: tool
tool: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["Xuan-thi Nguyen", "https://github.com/mellenguyen"]
filename: LearnTmux-fr.txt
lang: fr-fr
---


[Tmux](http://tmux.sourceforge.net) est un multiplexeur de terminal: il permet
de créer plusieurs terminaux, accédés et contrôlés depuis un seul écran. Tmux
peut être détaché de l'écran tout en continuant de fonctionner en tâche de
fond, puis rattaché de nouveau.


```

  tmux [command]     # Exécute une commande
                     # 'tmux' sans commande créé une nouvelle session

    new              # Créé une nouvelle session
     -s "Session"    # Créé une session nommée "Session"
     -n "Window"     # Créé une fenêtre nommée "Window"
     -c "/dir"       # Démarre dans le dossier cible "/dir"

    attach           # S'attache à la dernière session ou la session disponible
     -t "#"          # S'attache à la session cible
     -d              # Détache la session des autres instances

    ls               # Liste les sessions ouvertes
     -a              # Liste toutes les sessions ouvertes

    lsw              # Liste les fenêtres de la session courante
     -a              # Liste toutes les fenêtres
     -s              # Liste toutes les fenêtres en session

    lsp              # Liste les panels
     -a              # Liste tous les panels
     -s              # Liste tous les panels en session
     -t              # Liste tous les panels dans la cible

    kill-window      # Tue la fenêtre courante
     -t "#"          # Tue la fenêtre cible
     -a              # Tue toutes les fenêtres
     -a -t "#"       # Tue toutes les fenêtres sauf la cible

    kill-session     # Tue la session courante
     -t "#"          # Tue la session cible
     -a              # Tue toutes les sessions
     -a -t "#"       # Tue toutes les sessions sauf la cible

```


### Raccourcis clavier

Afin de contrôler une session tmux attachée, on utilise une combinaison de
touches appelées 'Préfixe'. Elle doit être pressée afin d'utiliser les
raccourcis.

```
--------------------------------------------------------------------------------
  (C-b) = Ctrl + b  # Combinaison 'Préfixe' requise pour utiliser les raccourcis

  (M-1) = Meta + 1 -ou- Alt + 1
--------------------------------------------------------------------------------

  ?                  # Liste tous les raccourcis
  :                  # Entre dans l'invite de commande de tmux
  r                  # Force la redéfinition du client attaché
  c                  # Créé une nouvelle fenêtre

  !                  # Sépare le panel courant de sa fenêtre
  %                  # Sépare le panel courant en deux, gauche et droite
  "                  # Sépare le panel courant en deux, haut et bas

  n                  # Changer vers la fenêtre suivante
  p                  # Changer vers la fenêtre précédente
  {                  # Echange le panel courant avec le panel précédent
  }                  # Echange le panel courant avec le panel suivant

  s                  # Sélectionne une nouvelle session pour le client attaché
                     # de manière interactive
  w                  # Choisi la fenêtre courante de manière interactive
  0 to 9             # Sélectionne la fenêtre de 0 à 9

  d                  # Détache le client courant
  D                  # Choisi un client à détacher

  &                  # Tue la fenêtre courante
  x                  # Tue le panel courant

  Up, Down           # Change vers le panel au dessus, en dessous, à gauche
  Left, Right        # ou à droite

  M-1 to M-5         # Arrange les panels:
                       # 1) égaliser sur l'horizontale
                       # 2) égaliser sur la verticale
                       # 3) panel principal en haut et le reste en bas
                       #    de gauche à droite
                       # 4) panel principal à gauche et le reste à droite
                       #    de haut en bas
                       # 5) "tiled" : égalise les panels
                       #    sur la hauteur et la largeur

  C-Up, C-Down       # Redimensionne le panel courant par pas de une cellule
  C-Left, C-Right

  M-Up, M-Down       # Redimensionne le panel courant par pas de cinq cellules
  M-Left, M-Right

```


### Configuration de ~/.tmux.conf

tmux.conf peut être utilisé pour fixer les options automatiquement au
démarrage, comme .vimrc ou init.el.

```
# Exemple de tmux.conf
# 2014.10


### Général
###########################################################################

# Active UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Limite de l'historique
set -g history-limit 2048

# Indice de début du nombre de panels
set -g base-index 1

# Souris
set-option -g mouse-select-pane on

# Force le rechargement du fichier de configuration
unbind r
bind r source-file ~/.tmux.conf


### Raccourcis clavier
###########################################################################

# Annule C-b en tant que préfixe par défaut
unbind C-b

# Définit un nouveau préfixe par défaut
set-option -g prefix `

# Retourne à la fenêtre précédente quand le préfixe est pressé deux fois
bind C-a last-window
bind ` last-window

# Permet d'échanger C-a et ` en utilisant F11/F12
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Préférences de raccourcis clavier
setw -g mode-keys vi
set-option -g status-keys vi

# Navigue entre les panels avec les raccourcis clavier de vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Navigation entre les fenêtres
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Commandes simples de séparation des panels
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Active la session la plus imbriquée (en faisant de l'imbrication sous tmux)
# pour envoyer des commandes
bind a send-prefix


### Thème
###########################################################################

# Palette de couleurs pour la barre de statuts
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Palette de couleurs pour les bordures des panels
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Palette de couleurs pour les messages
set-option -g message-fg black
set-option -g message-bg green

# Palette de couleurs pour les fenêtres
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-content-attr default
setw -g window-status-content-fg yellow
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### UI
###########################################################################

# Notification
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Définir automatiquement des titres de fenêtres
set-option -g set-titles on
# Numéro de fenêtre, nom du programme, actif (ou non)
set-option -g set-titles-string '#H:#S.#I.#P #W #T'

# Réglages de la barre de statuts
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Présente des indicateurs de performance dans la barre de statuts
# Recquiert https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Références

[Tmux | Home](http://tmux.sourceforge.net)

[Page du manuel Tmux](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Montrer le pourcentage CPU/MEM dans la barre de statuts](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

[tmuxinator - Gère des sessions tmux complexes](https://github.com/tmuxinator/tmuxinator)
