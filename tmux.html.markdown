---
category: tool
tool: tmux
contributors:
    - ["kaernyk", "http://github.com/kaernyk"]
filename: LearnTmux.txt
---


<a href="http://tmux.sourceforge.net/">
tmux</a> is a terminal multiplexer: it enables a number of terminals to be 
created, accessed, and controlled from a single screen. tmux may be detached 
from a screen and continue running in the background, then later reattached.


```bash
# We’ll start off with managing tmux from a terminal:

  tmux new          # Create a new session
   -s "Session"     # Create named session
   -n "Window"      # Create named Window
   -c "/dir"        # Start in target directory
    
  tmux attach       # Attach last/available session
   -t "#"           # Attach target session
   -d               # Detach the session from other instances

  tmux ls           # List open sessions
        
  kill-session      # Kill current session
   -t "#"           # Kill target session
   -a               # Kill all sessions
   -a -t "#"        # Kill all sessions but the target   



## Key Bindings

#   The method of controlling an attached tmux session is fairly simple, but 
# there are two keys we will see repeated throughout this write-up (as well as
# the man pages). 
	
  (C-b) == Ctrl + b

  (M-1) == Alt + b
  	   -or-
  	   Meta + b


# The default keybinds are as follows:

   C-b             # Send the prefix key (C-b) through to the application.
   C-o             # Rotate the panes in the current window forwards.
   C-z             # Suspend the tmux client.
   ?               # List all key bindings.
   :               # Enter the tmux command prompt.
   r               # Force redraw of the attached client.

   c               # Create a new window.
   $               # Rename the current session.
   ,               # Rename the current window.

   !               # Break the current pane out of the window.
   %               # Split the current pane into two, left and right.
   "               # Split the current pane into two, top and bottom.

   n               # Change to the next window.
   p               # Change to the previous window.
   {               # Swap the current pane with the previous pane.
   }               # Swap the current pane with the next pane.

   s               # Select a new session for the attached client interactively.
   w               # Choose the current window interactively.
   0 to 9          # Select windows 0 to 9.

   d               # Detach the current client.
   D               # Choose a client to detach.
   
   &               # Kill the current window.
   x               # Kill the current pane.

   Up, Down        # Change to the pane above, below, left, or right.
   Left, Right

   M-1 to M-5      # Arrange panes: 
                   #    1) even-horizontal
                   #    2) even-vertical
                   #    3) main-horizontal
                   #    4) main-vertical
                   #    5) tiled.

   C-Up, C-Down    # Resize the current pane in steps of one cell.
   C-Left, C-Right
               
   M-Up, M-Down    # Resize the current pane in steps of five cells.
   M-Left, M-Right


### Configuring ~/.tmux.conf

  tmux.conf can be used to set options automatically on start up, much 
like how .vimrc or init.el are used.

```
	# Example tmux.conf
	# 2014.9
	
	## Prefix Adjustment   
	
	# Unbind C-b as the default prefix 
	unbind-key C-b
	
	# Set ` as the default prefix
	set-option -g prefix `
	
	# Set C-a as the default prefix
	#set option -g prefix C-a
	
	# Return to previous window when prefix is pressed twice
	bind-key C-a last-window  
	bind-key ` last-window 
	
	# Allow swapping C-a and ` using F11/F12 
	bind-key F11 set-option -g prefix C-a  
	bind-key F12 set-option -g prefix `  
	bind-key C-a send-prefix
	  
	## Index Start  
	set -g base-index 1  
	  
	## Window Cycle/Swap  
	bind e previous-window  
	bind f next-window  
	bind E swap-window -t -1  
	bind F swap-window -t +1  
	
	## Statusbar Color Palatte
	set-option -g status-justify left  
	set-option -g status-bg black  
	set-option -g status-fg white  
	set-option -g status-left-length 40  
	set-option -g status-right-length 80  
	  
	## Pane Border Color Palette  
	set-option -g pane-active-border-fg green  
	set-option -g pane-active-border-bg black  
	set-option -g pane-border-fg white  
	set-option -g pane-border-bg black  
	  
	## Message Color Palette  
	set-option -g message-fg black  
	set-option -g message-bg green  
	  
	#setw -g mode-bg black      
	  
	## Window Status Color Palette  
	setw -g window-status-bg black  
	setw -g window-status-current-fg green  
	setw -g window-status-bell-attr default  
	setw -g window-status-bell-fg red  
	setw -g window-status-content-attr default  
	setw -g window-status-content-fg yellow  
	setw -g window-status-activity-attr default  
	setw -g window-status-activity-fg yellow  
	
	## Window Interface Adjustments  
	set-option -g status-utf8 on  
	setw -g mode-keys vi  
	setw -g mode-mouse on  
	setw -g monitor-activity on  
	  
	set-option -g mouse-select-pane on  
	set-option -g status-keys vi  
	set-option -g bell-action any  
	set-option -g set-titles on  
	set-option -g set-titles-string '#H:#S.#I.#P #W #T' # window number,program name,active (or not)  
	set-option -g visual-bell off  
	
	## Statusbar Adjustments  
	set -g status-left ' #[fg=red]#H#[fg=green]:#[fg=white]#S #[fg=green]][#[default] '  
	set -g status-interval 5  
	
	# Statusbar with right-aligned Date / Time
	set -g status-right ' #[fg=green]][#[fg=white] #T #[fg=green]][ #[fg=blue]%Y-%m-%d #[fg=white]%H:%M#[default] '  
	
	## Show performance counters in statusbar
	# Requires https://github.com/thewtex/tmux-mem-cpu-load/
	#set -g status-right ' #[fg=green]][#[fg=white] #(tmux-mem-cpu-load 5 4) #[fg=green]][ #[fg=yellow]%H:%M#[default] '  
	
	## Scrollback/History limit
	set -g history-limit 4096
	
	bind r source-file ~/.tmux.conf
```
```

### External Resources

<a href="http://tmux.sourceforge.net/">Tmux | Home</a><br>
<a href="http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux">Manual page</a><br>
<a href="https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux">Display CPU/MEM % in statusbar</a><br>
<a href="http://wiki.gentoo.org/wiki/Tmux">Archlinux Wiki</a><br>
<a href="https://wiki.archlinux.org/index.php/Tmux">Gentoo Wiki</a><br>
