---
category: tool
tool: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["Ferran Pelayo", "https://github.com/ferranpm"]
filename: LearnTmux-es.txt
lang: es-es
---

[tmux](http://tmux.sourceforge.net) permite crear, controlar y acceder a
multiples terminales desde una sola ventana.  Puede desconectarse una sesión de
la ventana, seguir corriendo en segundo plano y volver a conectar otra ventana
más tarde.


```

  tmux [command]     # Correr un comando de tmux
                     # 'tmux' sin comando crea una nueva sesión.

    new              # Crear una nueva sesión
     -s "Session"    # Crear una sesión con nombre
     -n "Window"     # Crear una ventana con nombre
     -c "/dir"       # Empezar en el directorio "/dir"

    attach           # Atar la ventana a la ultima sesión iniciada
     -t "#"          # Atar la ventana a la sesión "#"
     -d              # Desatar la ventana de la sesión

    ls               # Listar las sesiones abiertas
     -a              # Listar todas las sesiones abiertas

    lsw              # Listar ventanas
     -a              # Listar todas las ventanas
     -s              # Listar todas las ventanas de la sesión

    lsp              # Listar paneles
     -a              # Listar todos los paneles
     -s              # Listar todos los paneles de la sesión
     -t              # Listar paneles de la aplicación en el target

    kill-window      # Eliminar la ventana actual
     -t "#"          # Eliminar la ventana "#"
     -a              # Eliminar todas las ventanas
     -a -t "#"       # Eliminar todas las ventanas menos la "#"

    kill-session     # Eliminar la sesión actual
     -t "#"          # Eliminar la sesión "#"
     -a              # Eliminar todas las sessiones
     -a -t "#"       # Eliminar todas las sessiones menos la "#"

```


### Atajos de teclado

Para controlar una sesión atada se usa la combinación llamada 'Prefijo' + atajo.

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # 'Prefijo' por defecto requerido para usar los atajos

  (M-1) = Meta + 1 -o- Alt + 1
----------------------------------------------------------------------

  ?                  # Listar todos los atajos de teclado
  :                  # Insertar un comando de tmux
  r                  # Forzar refresco gráfico del cliente
  c                  # Crear una nueva ventana

  !                  # Quitar el panel actual de la ventana
  %                  # Dividir el panel actual en dos (derecha e izquierda)
  "                  # Dividir el panel actual en dos (arriba y abajo)

  n                  # Cambiar a la siguiente ventana
  p                  # Cambiar a la ventana anterior
  {                  # Cambiar el panel por el panel anterior
  }                  # Cambiar el panel por el siguiente panel

  s                  # Seleccionar y atar el cliente a una sesión distinta
                     de forma interactiva
  w                  # Seleccionar una ventana de forma interactiva
  0 to 9             # Seleccionar una ventana (del 0 al 9)

  d                  # Desatar el cliente actual de la sesión
  D                  # Escojer un cliente a desatar

  &                  # Eliminar la ventana actual
  x                  # Eliminar el panel actual

  Up, Down           # Cambiar al panel de arriba, debajo, izquierda o derecha
  Left, Right

  M-1 to M-5         # Ordenar los paneles

  C-Up, C-Down       # Dimensionar el panel actual en pasos de una celda
  C-Left, C-Right

  M-Up, M-Down       # Dimensionar el panel actual en pasos de cinco celdas
  M-Left, M-Right

```


### Configurar ~/.tmux.conf

El fichero tmux.conf se puede configurar para establecer unas opciones por
defecto, igual que .vimrc o init.el para vim o emacs.

```
# Ejemplo tmux.conf
# 2014.10


### General
###########################################################################

# Activar UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Limite del historico de comandos
set -g history-limit 2048

# Index Start
set -g base-index 1

# Ratón
set-option -g mouse-select-pane on

# Forzar volver a cargar el fichero de configuración
unbind r
bind r source-file ~/.tmux.conf


### Atajos de teclado
###########################################################################

# Quitar C-b como prefijo por defecto
unbind C-b

# Establecer ` como nuevo prefijo
set-option -g prefix `

# Volver a la ventana anterior cuando el prefijo se pulsa dos veces
bind C-a last-window
bind ` last-window

# Intercambiar entre C-a y ` como prefijo pulsando F11/F12
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Preferencias de los atajos
setw -g mode-keys vi
set-option -g status-keys vi

# Mover entre paneles con atajos de vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Cambiar/Saltar de ventana
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Divisiones de paneles
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

### Tema de colores
###########################################################################

# Barra de estado
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Bordes de paneles
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Color de los mensajes
set-option -g message-fg black
set-option -g message-bg green

# Colores del estado de las ventanas
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

# Notificaciones
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Titulos de las ventanas
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Formato de la barra de estado
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Mostrar estadisticas de rendimiento en la barra de estado
# Requiere https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Referencias

[Tmux | Home](http://tmux.sourceforge.net)

[Tmux Manual page](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Display CPU/MEM % in statusbar](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)
