---
category: tool
tool: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
filename: LearnTmux-es.txt
translators:
    - ["Damaso Sanoja", "https://github.com/damasosanoja"]
lang: es-es
---


[tmux](http://tmux.sourceforge.net)
es un terminal multiplexor: habilita la creación, acceso y control
de múltiples terminales controlados desde una sola pantalla. tmux
puede ser separado de una pantalla y continuar corriendo en el fondo
y luego ser insertado nuevamente.


```

  tmux [command]     # Corre un comando
                     # 'tmux' sin comandos creará una nueva sesión

    new              # Crea una nueva sesión
     -s "Session"    # Crea sesión con nombre
     -n "Window"     # Crea ventana con nombre
     -c "/dir"       # Comienza en el directorio destino 

    attach           # Adjunta sesión última/disponible
     -t "#"          # Adjunta sesión destino
     -d              # Separa la sesión de otras instancias

    ls               # Lista las sesiones abiertas
     -a              # Lista todas las sesiones abiertas

    lsw              # Lista las ventanas
     -a              # Lista todas las ventanas
     -s              # Lista todas las ventanas en la sesión

    lsp              # Lista los páneles
     -a              # Lista todos los páneles
     -s              # Lista todos los páneles de la sesión
     -t              # Lista los páneles de aplicación en el destino

    kill-window      # Cierra la ventana actual
     -t "#"          # Cierra la ventana destino
     -a              # Cierra todas las ventanas
     -a -t "#"       # Cierra todas las ventanas menos el destino

    kill-session     # Cierra la sesión actual
     -t "#"          # Cierra la sesión destino
     -a              # Cierra todas las sesiones
     -a -t "#"       # Cierra todas las sesiones menos el destino

```


### Atajos de Teclado

El método para controlar una sesión adjunta tmux es mediante
combinaciones de teclas llamadas teclas 'Prefijo'.

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # combinación 'Prefijo' necesaria para usar atajos

  (M-1) = Meta + 1 -o- Alt + 1
----------------------------------------------------------------------

  ?                  # Lista todos los atajos de teclado
  :                  # Entra en la línea de comandos tmux
  r                  # Fuerza el redibujado del cliente adjuntado
  c                  # Crea una nueva ventana

  !                  # Separa el panel actual fuera de la ventana.
  %                  # Separa el panel actual en dos, izquierdo y derecho
  "                  # Separa el panel actual en dos, superior e inferior

  n                  # Cambia a la siguiente ventana
  p                  # Cambia a la ventana previa
  {                  # Intercambia el panel actual con el anterior
  }                  # Intercambia el panel actual con el próximo

  s                  # Selecciona una nueva sesión para el cliente adjuntado
                     interactivamente
  w                  # Elegir la ventana actual interactivamente
  0 al 9             # Seleccionar ventanas 0 al 9

  d                  # Separa el cliente actual
  D                  # Elige un cliente para separar

  &                  # Cierra la ventana actual
  x                  # Cierra el panel actual

  Up, Down           # Cambia al panel superior, inferior, izquierdo, o derecho
  Left, Right

  M-1 to M-5         # Organizar páneles:
                       # 1) uniformes horizontales
                       # 2) uniformes verticales
                       # 3) principal horizontal
                       # 4) principal vertical
                       # 5) mozaico

  C-Up, C-Down       # Redimensiona el panel actual en pasos de una celda
  C-Left, C-Right

  M-Up, M-Down       # Redimensiona el panel actual en pasos de cinco celdas
  M-Left, M-Right

```


### Configurando ~/.tmux.conf

tmux.conf puede usarse para establecer opciones automáticas al arrancar, parecido a como .vimrc o init.el hacen.

```
# Ejemplo de tmux.conf
# 2014.10


### General
###########################################################################

# Habilita UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Fuera de pantalla/Historia límite
set -g history-limit 2048

# Comienzo de índice
set -g base-index 1

# Ratón
set-option -g mouse-select-pane on

# Forza recarga de fichero de configuración
unbind r
bind r source-file ~/.tmux.conf


### Atajos de teclado
###########################################################################

# Desvincula C-b como el prefijo por defecto
unbind C-b

# Establece el nuevo prefijo
set-option -g prefix `

# Regresa a la ventana previa cuando el prefijo es accionado dos veces
bind C-a last-window
bind ` last-window

# Permite intercambiar C-a y ` usando F11/F12
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Preferencias de atajos
setw -g mode-keys vi
set-option -g status-keys vi

# Moviéndose entre paneles con movimientos de teclas vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Ciclo/Intercambio de Ventana
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# División rápida de paneles
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Activar sesión mas interna (cuando se anida tmux) para enviar comandos
bind a send-prefix


### Temas
###########################################################################

# Paleta de Colores de la Barra de estado
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Paleta de Colores del Borde del Panel
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Paleta de Colores de Mensajes
set-option -g message-fg black
set-option -g message-bg green

# Paleta de Colores de la Ventana
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

# Notificación
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Establece automáticamente títulos de ventanas
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Ajustes de barra de estado
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Muestra indicadores de rendimiento en barra de estado
# Requiere https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Referencias

[Tmux | Inicio](http://tmux.sourceforge.net)

[Tmux Manual](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Mostrar CPU/MEM % en barra de estado](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)
