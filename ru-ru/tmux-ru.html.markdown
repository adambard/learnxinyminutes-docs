---
category: tool
tool: tmux
contributors:
  - ["mdln", "https://github.com/mdln"]
translators:
  - ["Davydov Anton", "https://github.com/davydovanton"]
filename: LearnTmux-ru.txt
lang: ru-ru
---

[tmux](http://tmux.sourceforge.net) - терминальный мультиплексор.
Он позволяет создавать, получать доступ и контролировать любое
количество терминалов из единого окна.
Сессия tmux также может быть свернута в фоновый режим, и она
будет работать в фоне, а после к ней можно будет подключиться.


```

  tmux [command]     # Запуск команды 'tmux'
                     # без какой-либо команды создаст новую сессию

    new              # Создать новую сессию
     -s "Session"    # Создать именованную сессию
     -n "Window"     # Создать именованное окно
     -c "/dir"       # Запустить сессию в конкретной директории

    attach           # Подключиться к последней/существующей сессии
     -t "№"          # Подключиться к определенной сессии
     -d              # Завершить определенную сессию

    ls               # Список открытых сессий
     -a              # Список всех открытых сессий

    lsw              # Список окон
     -a              # Список всех окон
     -s              # Список всех окон в сессии

    lsp              # Список панелей
     -a              # Список всех панелей
     -s              # Список всех панелей в сессии
     -t              # Список всех панелей для конкретного объекта

    kill-window      # Закрыть текущее окно
     -t "#"          # Закрыть конкретное окно
     -a              # Закрыть все окна
     -a -t "#"       # Закрыть все окна, кроме конкретного

    kill-session     # Завершить текущую сессию
     -t "#"          # Завершить конкретную сессию
     -a              # Завершить все сессии
     -a -t "#"       # Завершить все сессии, кроме конкретной

```


### "Горячие" клавиши

Способ, с помощью которого контролируется любая tmux
сессия, - комбинация клавиш, называемая 'Префиксом'.

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # 'Префикс' необходим для
                      # использования горячих клавиш

  (M-1) = Meta + 1 -или- Alt + 1
----------------------------------------------------------------------

  ?                    # Список всех горячих клавиш
  :                    # Начать ввод в командной строке tmux
  r                    # Принудительная перерисовка текущего клиента
  c                    # Создать новое окно

  !                    # Переместить текущую панель в отдельное окно
  %                    # Разделить текущую панель на две: левую и правую
  "                    # Разделить текущую панель на две: верхнюю и нижнюю

  n                    # Переместиться на следующее окно
  p                    # Переместиться на предыдущее окно
  {                    # Заменить текущую панель на предыдущую
  }                    # Заменить текущую панель на следующую

  s                    # Интерактивный выбор запущенных сессий
  w                    # Интерактивный выбор текущего окна
  от 0 до 9            # Выбрать окно номер 0..9

  d                    # Отключить текущий клиент
  D                    # Выбрать клиент, который будет отключен

  &                    # Закрыть текущее окно
  x                    # Закрыть текущую панель

  Стрелки вверх, вниз  # Переместиться на панель выше, ниже, левее
  влево, вправо        # или правее

  M-1 to M-5           # Расставить панели:
                         # 1) выровнять по горизонтали
                         # 2) выровнять по вертикали
                         # 3) основное горизонтально
                         # 4) основное вертикально
                         # 5) мозаикой

  C-Up, C-Down         # Изменение размера текущей панели с шагом в одну
  C-Left, C-Right      # колонку

  M-Up, M-Down         # Изменение размера текущей панели с шагом в пять
  M-Left, M-Right      # колонок

```


### Настройка ~/.tmux.conf

Файл tmux.conf может быть использован для автоматической установки
опций при старте, как, например, .vimrc или init.el.

```
# Пример файла tmux.conf
# 2014.10


### Общее
###########################################################################

# Включить поддержку UTF-8
setw -g utf8 on
set-option -g status-utf8 on

# Установить лимит истории
set -g history-limit 2048

# Порядковый номер первой панели
set -g base-index 1

# Включить поддержку мыши
set-option -g mouse-select-pane on

# Принудительная перезагрузка конфигурационного файла
unbind r
bind r source-file ~/.tmux.conf


### Горячие клавиши
###########################################################################

# Отменить комбинацию C-b как стандартный префикс
unbind C-b

# Установить новую комбинацию как префикс
set-option -g prefix `

# Вернуть предыдущее окно, если префикс был нажат два раза
bind C-a last-window
bind ` last-window

# Разрешить замену C-a и ` на F11/F12
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Настройки клавиш
setw -g mode-keys vi
set-option -g status-keys vi

# Перемещение между панелями, как в vim
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Переключить/Заменить окно
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# Комманды, упрощающие разделением панелей
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# Активировать центральную сессию (когда вложенный tmux) для отправки команд
bind a send-prefix


### Цветовая схема
###########################################################################

# Цветовая палитра строки состояния
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# Цветовая палитра окантовки панели
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# Цветовая палитра сообщений
set-option -g message-fg black
set-option -g message-bg green

# Цветовая палитра статус окна
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-content-attr default
setw -g window-status-content-fg yellow
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### Интерфейс
###########################################################################

# Уведомления
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# Автоматическая установка заголовка окна
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)

# Настройки строки состояния
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# Показывать системные характеристики в статусбаре
# Требует https://github.com/thewtex/tmux-mem-cpu-load/
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```

### Ссылки

[Tmux | Домашняя страница](http://tmux.sourceforge.net)

[Страница мануала Tmux](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo Wiki](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux)

[Отображение CPU/MEM % в статусбаре](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)
