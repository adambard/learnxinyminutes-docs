---
category: tool
tool: tmux
filename: tmux.json
lang: lt-lt
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["Zygimantus", "https://github.com/zygimantus"]
---


[tmux](http://tmux.sourceforge.net)
yra terminalo daugintuvas: jis leidžia vienu metu sukurti, turėti
ir valdyti kelis terminalus viename ekrane. tmux gali būti atjungtas
nuo ekrano ir veikti fone, o vėliau gali būti vėl prijungtas.


```

  tmux [komanda]     # Vykdyti komandą
                     # 'tmux' be komandų sukurs naują sesiją

    new              # Sukurti naują sesiją
     -s "Session"    # Sukurti pavadintą sesiją
     -n "Window"     # Sukurti pavadintą langą
     -c "/dir"       # Pradėti nurodytoje direktorijoje

    attach           # Priskirti paskutinę/prienamą sesiją
     -t "#"          # Priskirti nurodytą sesiją
     -d              # Atjungti sesiją nuo kitų langų

    ls               # Aktyvių sesijų sąrašas
     -a              # Visų aktyvių sesijų sąrašas

    lsw              # Langų sąrašas
     -a              # Visų langų sąrašas
     -s              # Visų langų sesijoje sąrašas

    lsp              # Skydelių sąrašas
     -a              # Visų skydelių sąrašas
     -s              # Visų skydelių sesijoje sąrašas
     -t              # Visų skydelių taikinyje sąrašas

    kill-window      # Užbaigti dabartinį langą
     -t "#"          # Užbaigti nurodytą langą
     -a              # Užbaigti visus langus
     -a -t "#"       # Užbaigti visus langus, bet ne taikinį

    kill-session     # Užbaigti dabartinę sesiją
     -t "#"          # Užbaigti nurodytą sesiją
     -a              # Užbaigti visas sesijas
     -a -t "#"       # Užbaigti visas sesijas, bet ne taikinį

```


### Klavišai

Priskirta tmux sesija yra valdoma klavišų kompinacijomis.

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # Kombinacija reikalinga norint naudoti klavišus

  (M-1) = Meta + 1 -or- Alt + 1
----------------------------------------------------------------------

  ?                  # Rodo visų klavišų kombinacijų sąrašą
  :                  # Įjungiama tmux komandinė eilutė
  r                  # Priverstinai perpiešiamas prijungtas klientas
  c                  # Sukurti naują langą

  !                  # Iškelia esamą skydelį iš lango.
  %                  # Perskelia esamą skydelį į du, kairįjį ir dešinį
  "                  # Perskelia esamą skydelį į du, viršutinį ir apatinį

  n                  # Pakeičia į kitą langą
  p                  # Pakeičia į buvusį langą
  {                  # Apkeičia dabartinį skydėlį su buvusiu
  }                  # Apkeičia dabartinį skydėlį su sekančiu

  s                  # Pasirinkti naują sesiją prijungtam klientui interaktyviai
  w                  # Pasirinkti dabartinį langą interaktyviai
  0 to 9             # Pasirinkti langą nuo 0 iki 9

  d                  # Atjungti dabartinį klientą
  D                  # Pasirinkti klientą, kurį atjungti

  &                  # Užbaigti dabartinį langą
  x                  # Užbaigti dabartinį skydelį

  Up, Down           # Pakeisti į skydelį viršuje, apačioje, kairėje arba
                     dešinėje
  Left, Right

  M-1 to M-5         # Rikiuoti skydelius:
                       # 1) even-horizontal
                       # 2) even-vertical
                       # 3) main-horizontal
                       # 4) main-vertical
                       # 5) tiled

  C-Up, C-Down       # Keisti esamo skydelio dydį vienos ląstelės žingsniu
  C-Left, C-Right

  M-Up, M-Down       # Keisti esamo skydelio dydį penkių ląstelių žingsniu
  M-Left, M-Right

```


### Configuring ~/.tmux.conf

tmux.conf gali būti nustatytas automatiškai paleidimo metu, panašiai kaip ir
.vimrc arba init.el.

```
# Pavyzdys tmux.conf
# 2014.10


### General
###########################################################################

# Enable UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Scrollback/History limit
set -g history-limit 2048

# Index Start
set -g base-index 1

# Mouse
set-option -g mouse-select-pane on

# Force reload of config file
unbind r
bind r source-file ~/.tmux.conf


### Keybinds
###########################################################################

# Unbind C-b as the default prefix
unbind C-b

# Set new default prefix
set-option -g prefix `

# Return to previous window when prefix is pressed twice
bind C-a last-window
bind ` last-window

# Allow swapping C-a and ` using F11/F12
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Keybind preference
setw -g mode-keys vi
set-option -g status-keys vi

# Moving between panes with vim movement keys
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Window Cycle/Swap
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Easy split pane commands
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Activate inner-most session (when nesting tmux) to send commands
bind a send-prefix


### Theme
###########################################################################

# Statusbar Color Palatte
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Pane Border Color Palette
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Message Color Palette
set-option -g message-fg black
set-option -g message-bg green

# Window Status Color Palette
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

# Automatically set window titles
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Statusbar Adjustments
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Show performance counters in statusbar
# Requires https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Šaltiniai

[Tmux | Home](http://tmux.sourceforge.net)

[Tmux Manual page](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Display CPU/MEM % in statusbar](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

[tmuxinator - Manage complex tmux sessions](https://github.com/tmuxinator/tmuxinator)
