---
category: tool
tool: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["DamienVGN", "https://github.com/martin-damien"]
lang: fr-fr
filename: LearnTmux-fr.txt
---


[tmux](http://tmux.sourceforge.net) est un multiplexer de terminal:
il permet de créer un certain nombre de terminaux, d'y accéder et de les
contrôler depuis un seul écran. tmux peut être détaché d'un écran et
continuer à tourner en tache de fond pour être repris ensuite.

```

  tmux [command]     # Lancer une commande
                     # 'tmux' sans paramètres créé une nouvelle session

    new              # Créé une nouvelle session
     -s "Session"    # Créé une nouvelle session nommée
     -n "Window"     # Créé une fenêtre nommée
     -c "/dir"       # Commence dans le dossier définis

    attach           # Ré-attache la session donnée si existante
     -t "#"          # Attache la session ciblée
     -d              # Détache la session d'autres instances éventuelles

    ls               # Liste les sessions ouvertes
     -a              # Liste toutes les sessions ouvertes

    lsw              # Liste les fenêtres ("windows" en anglais)
     -a              # Liste toutes les fenêtres
     -s              # Liste toutes les fenêtres dans la session

    lsp              # Liste les zones ("panes" en anglais)
     -a              # Liste toutes les zones
     -s              # Liste toutes les zones de la session
     -t              # Liste toutes les zones de la cible

    kill-window      # Termine ("tue") la fenêtre en cours
     -t "#"          # Termine ("tue") la fenêtre ciblée
     -a              # Termine ("tue") toutes les fenêtres
     -a -t "#"       # Termine ("tue") toues les fenêtres sauf celle ciblée

    kill-session     # Termine ("tue") la session en cours
     -t "#"          # Termine ("tue") la session ciblée
     -a              # Termine ("tue") toutes les sessions
     -a -t "#"       # Termine ("tue") toutes les sessions sauf celle ciblée

```


### Raccourcis clavier

Pour contrôler une session tmux, on utilise une combinaison de touches
appelée "touches préfixées".

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # La combinaison de touches à utiliser en "préfixe"

  (M-1) = Meta + 1 -or- Alt + 1
----------------------------------------------------------------------

  ?                  # Liste toutes les combinaisons de touches possibles
  :                  # Entrer dans l'invite de commande de tmux
  r                  # Forcer la fenêtre à se redessiner à l'écran
  c                  # Créé une nouvelle fenêtre

  !                  # Enlève la zone et créé une fenêtre dédiée
  %                  # Découpe la zone actuelle en deux verticalement
  "                  # Découpe la zone en deux horizontalement

  n                  # Passe à la fenêtre suivante
  p                  # Passe à la fenêtre précédente
  {                  # Passe à la zone suivante
  }                  # Passe à la zone précédente

  s                  # Choix interactif de la session à laquelle attacher
                     # la zone actuelle
  w                  # Choix interactif de la fenêtre
  0 to 9             # Sélection de la fenêtre 0 à 9

  d                  # Détache le client actuel
  D                  # Choisis un client à détacher

  &                  # Termine ("tue") la fenêtre en cours
  x                  # Termine ("tue") la zone en cours

  Up, Down           # Change pour la zone au-dessus, en dessous, à gauche, à droite
  Left, Right

  M-1 to M-5         # Arrange les zones :
                       # 1) horizontal (50/50)
                       # 2) vertical (50/50)
                       # 3) horizontal avec zone principale
                       # 4) verticale avec zone principale
                       # 5) mosaïque

  C-Up, C-Down       # Redimensionne la zone actuelle par pas de 1
  C-Left, C-Right

  M-Up, M-Down       # Redimensionne la zone actuelle par pas de 5
  M-Left, M-Right

```


### Configurer ~/.tmux.conf

tmux.conf peut être utilisé pour définir automatiquement certains paramètres
au démarrage, un peu dans le principe de .vimrc ou .init.el.

```
# Exemple tmux.conf
# 2014.10


### Général
###########################################################################

# Active UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Limite d'historique
set -g history-limit 2048

# Index
set -g base-index 1

# Souris
set-option -g mouse-select-pane on

# Forcer le rechargement du fichier de configuration
unbind r
bind r source-file ~/.tmux.conf


### Raccourcis clavier
###########################################################################

# Désactiver C-b comme préfixe
unbind C-b

# Définis le nouveau préfixe
set-option -g prefix `

# Retourner à la fenêtre précédente quand le préfixe est tapé deux fois
bind C-a last-window
bind ` last-window

# Permettre le roulement C-a et ` en utilisant F1/F2
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Préférences de raccourcis
setw -g mode-keys vi
set-option -g status-keys vi

# Se déplacer entre les zones en utilisant les touches hjkl (comme Vim)
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Déplacement entre les fenêtres
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Raccourcis pour scinder les zones
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Définis une commande pour envoyer un préfixe dans un tmux qui serait appelé dans un tmux
bind a send-prefix


### Thème
###########################################################################

# Barre de status
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Bordure des zones
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Messages
set-option -g message-fg black
set-option -g message-bg green

# Status des fenêtres
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-content-attr default
setw -g window-status-content-fg yellow
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### Interface
###########################################################################

# Notifications
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Définition automatique du titre des fenêtres
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Ajustement de la barre de status
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Affiche un indice de performance dans la barre de status
# Nécessite https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Références

[Tmux | Home (anglais)](http://tmux.sourceforge.net)

[Tmux Manual page (anglais)](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki (anglais)](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki (anglais)](https://wiki.archlinux.org/index.php/Tmux)

[Display CPU/MEM % in statusbar (anglais)](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

[tmuxinator - Manage complex tmux sessions (anglais)](https://github.com/tmuxinator/tmuxinator) 


