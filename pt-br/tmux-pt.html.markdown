---
category: tool
tool: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["Luis Custodio", "http://luiscustodio.com"]
lang: pt-br
filename: LearnTmux-pt.txt
---

O [tmux](http://tmux.sourceforge.net) é um multiplexador de terminal,
ele permite criar vários terminais e gerenciar tudo na mesma interface.
tmux pode também rodar em background e depois ser recuperado(exibido) novamente.

```

  tmux [command]     # Roda um [comando]
                     # 'tmux' sem comandos irá criar uma nova seção

    new              # Cria uma nova seção
     -s "Nome"       # Cria uma nova seção com nome "Nome"
     -n "Janela"     # Cria uma janela com o nome "Janela"
     -c "/dir"       # Inícia em uma pasta específica

    attach           # Acopla a última seção disponível
     -t "#"          # Acopla a seção com nome "#"
     -d              # Separa (Desacopla) a seção de outras instâncias.

    ls               # Lista todas as seções
     -a              # Lista todas as seções abertas

    lsw              # Lista as janelas
     -a              # Lista todas as janelas
     -s              # Lista todas janleas em uma seção

    lsp              # Lista os painéis
     -a              # Lista todos os painéis
     -s              # Lista todos os painéis em uma seção
     -t "#"          # Lista os painéis chamados "#"

    kill-window      # Encerrar a janela atual
     -t "#"          # Encerrar a janela chamada "#"
     -a              # Encerrar todas as janelas
     -a -t "#"       # Encerrar todas as janelas exceto a "#"

    kill-session     # Encerrar seção atual
     -t "#"          # Encerrar seção com nome "#"
     -a              # Encerrar todas as seções
     -a -t "#"       # Encerrar todas as seções exceto a "#"

```

### Teclas de atalhos (comandos)

As seções tmux acopladas são controladas através de teclas de atalho. (prefix key)

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # Abre a opção de receber comandos(atalhos).

  (M-1) = Meta + 1 -or- Alt + 1
----------------------------------------------------------------------

  ?           # Lista todos os comandos.
  :           # Acessa o prompt command do tmux
  r           # Força a reinicialização do cliente acoplado.
  c           # Cria uma nova janela.

  !           # Retira o painel atual da janela.
  %           # Divide o painel atual em dois. Esquerda e direita.
  "           # Divide o painel atual em dois. Para cima e para baixo.

  n           # Muda para a próxima janela.
  p           # Muda para a janela anterior.
  {           # Troca o painel atual pelo anterior.
  }           # Troca o painel corrent pelo posterior.

  s           # Seleciona uma nova seção para o cliente acoplado iterativamente.
  w           # Seleciona a janela atual iterativamente.
  0 to 9      # Seleciona a janela de 0 à 9.

  d           # Separa o cliente atual.
  D           # Seleciona um cliente a ser separado.

  &           # Encerra a janela atual.
  x           # Encerra o painel atual.

  Up, Down    # Move para o painel acima, abaixo, a esquerda ou a direita.
  Left, Right

  M-1 to M-5  # Organiza os paines:
                       # 1) Horizontalmente de maneira igual
                       # 2) Verticalmente de maineira igual.
                       # 3) Principal horizontalmente
                       # 4) Principal verticamente.
                       # 5) Mosaico

  C-Up, C-Down    # Altera o tamanho do painel atual em uma célula.
  C-Left, C-Right

  M-Up, M-Down    # Altera o tamanho do painel atual em cinco células.
  M-Left, M-Right

```


### Configurando ~/.tmux.conf

Existe um arquivo chamado tmux.conf, ele pode ser usado para definir opções no
 momento de inicialização, da mesma maneira que .vimrc, init.el, .bash_profile são usados.


```
# Exemplo tmux.conf
# 2015.12


### General
###########################################################################

# Limite da história de comandos
set -g history-limit 2048

# Indíce de inicialização
set -g base-index 1

# Mouse
set-option -g -q mouse on

# Recarregar o arquivo de configuração sem a necessidade de reiniciar o programa
unbind r
bind r source-file ~/.tmux.conf


### Keybinds / Comandos
###########################################################################

# Desvincular C-b como prefixo padrão.
unbind C-b

# Define um novo prefixo padrão.
set-option -g prefix `

# Voltar janela anterior quando comando for usado duas vezes.
bind C-a last-window
bind ` last-window

# Fazer com que F11 e F12 alterem o comportamento de C-a e `
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Preferencia de comandos
setw -g mode-keys vi
set-option -g status-keys vi

# Alternar enter painéis com teclas de orientaçao do vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Iterar entre as Janelas
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Dividir painéis
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Habilitar a sub-seção a enviar comandos.
bind a send-prefix


### Theme // Estilo
###########################################################################

# Paleta de cores para a barra de status
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Paleta de cores para bordas do painel
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Palta de cores para mensagem
set-option -g message-fg black
set-option -g message-bg green

# Paleta de cores para janela de status
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### UI
###########################################################################

# Notificações
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Definir automaticamente o título de janelas
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Ajustes na barra de status
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Mostrar indicativos de performance na barra de status
# Requires https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### Referências

[Tmux | Início](http://tmux.sourceforge.net)

[Manual Tmux (em inglês)](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Mostrar CPU/MEM % in statusbar](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

Possui uma sugestão? Uma correção, talvez? Abra um issue no Repositório GitHub, ou então faça um pull request.
