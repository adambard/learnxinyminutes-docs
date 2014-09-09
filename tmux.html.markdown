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


```
  tmux [command]     # Run a command
                     # 'tmux' with no commands will create a new session

    new              # Create a new session
     -s "Session"    # Create named session
     -n "Window"     # Create named Window
     -c "/dir"       # Start in target directory

    attach           # Attach last/available session
     -t "#"          # Attach target session
     -d              # Detach the session from other instances

    ls               # List open sessions
     -a              # List all open sessions

    lsw              # List windows
     -a              # List all windows
     -s              # List all windows in session

    lsp              # List panes
     -a              # List all panes
     -s              # List all panes in session
     -t              # List app panes in target

    kill-window      # Kill current window
     -t "#"          # Kill target window
     -a              # Kill all windows
     -a -t "#"       # Kill all windows but the target

    kill-session     # Kill current session
     -t "#"          # Kill target session
     -a              # Kill all sessions
     -a -t "#"       # Kill all sessions but the target



## Key Bindings

#   The method of controlling an attached tmux session is via key combinations
# called 'Prefix' keys.

------------------------------------------------------------------------------

  (C-b) = Ctrl + b    # 'Prefix' combination required to use keybinds

  (M-1) = Alt + 1
            -or-
          Meta + 1

------------------------------------------------------------------------------

   ?               # List all key bindings.
   :               # Enter the tmux command prompt.
   r               # Force redraw of the attached client.
   c               # Create a new window.

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

```

### External Resources

<a href="http://tmux.sourceforge.net/">Tmux | Home</a><br>
<a href="http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux">Tmux Manual page</a><br>
<a href="http://wiki.gentoo.org/wiki/Tmux">Archlinux Wiki</a><br>
<a href="https://wiki.archlinux.org/index.php/Tmux">Gentoo Wiki</a><br>
<a href="https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux">Display CPU/MEM % in statusbar</a><br>
