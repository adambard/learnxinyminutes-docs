---
category: tool
tool: tmux
filename: LearnTmux-cn.txt
contributors:
    - ["mdln", "https://github.com/mdln"]
translators:
    - ["Arnie97", "https://github.com/Arnie97"]
lang: zh-cn
---


[tmux](http://tmux.github.io)是一款终端复用工具。
在它的帮助下，你可以在同一个控制台上建立、访问并控制多个终端。
你可以断开与一个 tmux 终端的连接，此时程序将在后台运行，
当你需要时，可以随时重新连接到这个终端。

```

  tmux [command]     # 运行一条命令
                     # 如果单独使用 'tmux' 而不指定某个命令，将会建立一个新的会话

    new              # 创建一个新的会话
     -s "Session"    # 创建一个会话，并命名为“Session”
     -n "Window"     # 创建一个窗口，并命名为“Window”
     -c "/dir"       # 在指定的工作目录中启动会话

    attach           # 连接到上一次的会话（如果可用）
     -t "#"          # 连接到指定的会话
     -d              # 断开其他客户端的会话

    ls               # 列出打开的会话
     -a              # 列出所有打开的会话

    lsw              # 列出窗口
     -a              # 列出所有窗口
     -s              # 列出会话中的所有窗口

    lsp              # 列出窗格
     -a              # 列出所有窗格
     -s              # 列出会话中的所有窗格
     -t "#"          # 列出指定窗口中的所有窗格

    kill-window      # 关闭当前窗口
     -t "#"          # 关闭指定的窗口
     -a              # 关闭所有窗口
     -a -t "#"       # 关闭除指定窗口以外的所有窗口

    kill-session     # 关闭当前会话
     -t "#"          # 关闭指定的会话
     -a              # 关闭所有会话
     -a -t "#"       # 关闭除指定会话以外的所有会话

```


### 快捷键

通过“前缀”快捷键，可以控制一个已经连入的 tmux 会话。

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # 在使用下列快捷键之前，需要按这个“前缀”快捷键

  (M-1) = Meta + 1 或 Alt + 1
----------------------------------------------------------------------

  ?                  # 列出所有快捷键
  :                  # 进入 tmux 的命令提示符
  r                  # 强制重绘当前客户端
  c                  # 创建一个新窗口

  !                  # 将当前窗格从窗口中移出，成为为一个新的窗口
  %                  # 将当前窗格分为左右两半
  "                  # 将当前窗格分为上下两半

  n                  # 切换到下一个窗口
  p                  # 切换到上一个窗口
  {                  # 将当前窗格与上一个窗格交换
  }                  # 将当前窗格与下一个窗格交换

  s                  # 在交互式界面中，选择并连接至另一个会话
  w                  # 在交互式界面中，选择并激活一个窗口
  0 至 9             # 选择 0 到 9 号窗口

  d                  # 断开当前客户端
  D                  # 选择并断开一个客户端

  &                  # 关闭当前窗口
  x                  # 关闭当前窗格

  Up, Down           # 将焦点移动至相邻的窗格
  Left, Right

  M-1 到 M-5         # 排列窗格：
                       # 1) 水平等分
                       # 2) 垂直等分
                       # 3) 将一个窗格作为主要窗格，其他窗格水平等分
                       # 4) 将一个窗格作为主要窗格，其他窗格垂直等分
                       # 5) 平铺

  C-Up, C-Down       # 改变当前窗格的大小，每按一次增减一个单位
  C-Left, C-Right

  M-Up, M-Down       # 改变当前窗格的大小，每按一次增减五个单位
  M-Left, M-Right

```


### 配置 ~/.tmux.conf

tmux.conf 可以在 tmux 启动时自动设置选项，类似于 .vimrc 或 init.el 的用法。

```
# tmux.conf 示例
# 2014.10


### 通用设置
###########################################################################

# 启用 UTF-8 编码
setw -g utf8 on
set-option -g status-utf8 on

# 命令回滚/历史数量限制
set -g history-limit 2048

# 从 1 开始编号，而不是从 0 开始
set -g base-index 1

# 启用鼠标
set-option -g mouse-select-pane on

# 重新加载配置文件
unbind r
bind r source-file ~/.tmux.conf


### 快捷键设置
###########################################################################

# 取消默认的前缀键 C-b
unbind C-b

# 设置新的前缀键 `
set-option -g prefix `

# 多次按下前缀键时，切换到上一个窗口
bind C-a last-window
bind ` last-window

# 按下F11/F12，可以选择不同的前缀键
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# Vim 风格的快捷键绑定
setw -g mode-keys vi
set-option -g status-keys vi

# 使用 Vim 风格的按键在窗格间移动
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# 循环切换不同的窗口
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# 较易于使用的窗格分割快捷键
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# 在嵌套使用 tmux 的情况下，激活最内层的会话，以便向其发送命令
bind a send-prefix


### 外观主题
###########################################################################

# 状态栏颜色
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# 窗格边框颜色
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# 消息框颜色
set-option -g message-fg black
set-option -g message-bg green

# 窗口状态栏颜色
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-content-attr default
setw -g window-status-content-fg yellow
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### 用户界面
###########################################################################

# 通知方式
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# 自动设置窗口标题
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # 窗口编号,程序名称,是否活动

# 调整状态栏
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# 在状态栏中显示性能计数器
# 需要用到 https://github.com/thewtex/tmux-mem-cpu-load
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"

```


### 参考资料

[Tmux 主页](http://tmux.github.io)

[Tmux 手册](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[FreeBSDChina Wiki](https://wiki.freebsdchina.org/software/t/tmux)

[Archlinux Wiki](https://wiki.archlinux.org/index.php/Tmux_(简体中文))

[Tmux 快速教程](http://blog.jeswang.org/blog/2013/06/24/tmux-kuai-su-jiao-cheng)

[如何在 tmux 状态栏中显示 CPU / 内存占用的百分比](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

[管理复杂 tmux 会话的工具 - tmuxinator](https://github.com/tmuxinator/tmuxinator)
