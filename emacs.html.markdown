---
category: tool
tool: emacs
filename: emacs.txt
contributors:
    - ["Joseph Riad", "https://github.com/Joseph-Riad"]
---

Emacs started its life as ["the extensible, customizable display
editor"](https://www.gnu.org/software/emacs/emacs-paper.html) and grew
over the years into a full-blown ecosystem. Many tasks, usually
relegated to a diverse set of tools can be accomplished from within
Emacs in a consistent, familiar interface. Examples include directory
management, viewing PDF documents, editing files over SSH, managing git
repos,… (the list is quite long). In short, Emacs is yours to make of it
what you will: the spectrum of users varies from those who use it to
edit text files to extreme purists who use it to virtually replace their
operating system.

Emacs is extensible via a specialized dialect of Lisp known as Emacs
Lisp (Elisp) which has a lot of macros geared towards editing text and
managing text buffers. Any key (combination) you use in Emacs is bound
to an Emacs Lisp function and may be remapped to any other function,
including ones you write
yourself.

# Key Notation

```text
The Emacs manual and the community in general uses a convention to refer to different key combinations used within Emacs. Specifically, Emacs has the notion of a "modifier key" that is pressed along with another key to modify its action.

An example of this notation is "C-c". In this key combination "C" is the modifier and stands for the "Ctrl" key and "c" is the key whose action is being modified (the literal character "c").

The modifier shorthand:
"C-" --> The "CTRL" key
"M-" --> The "Meta" key (usually, the "Alt" key)
"s-" --> The "Super" key (the "Cmd" key on Macs and the "Windows" key on PCs)

There are other, less commonly used modifiers that I will not get into here.

The key combination "C-x C-s" means you press "Ctrl+x" followed by "Ctrl+s"

In addition to the above modifiers, the special keys "Esc", "Return (Enter)" and "Shift" are denoted by "ESC", "RET" and "S", respectively.
```

# Basic Emacs Concepts

Here, I discuss some basic Emacs concepts and terminology that may be
confusing to newcomers (especially to people used to Vim terminology)

  - A bunch of text that Emacs is editing is known as a **buffer**
  - A buffer does not necessarily correspond to an actual file on disk.
    It may be just a bunch of text in memory.
  - When a buffer corresponds to a file on disk, we say that the buffer
    is **visiting** that file.
  - Emacs typically has many buffers open at once.
  - The display of Emacs may be split into different **windows** (not to
    be confused with your operating system's windows: the operating
    system window for Emacs can have multiple Emacs windows inside it).
  - An operating system window for Emacs is called an Emacs **frame**.
    Thus, when the Emacs manual talks about opening a new frame, this
    essentially means opening a new OS *window* containing an(other)
    instance of Emacs.
  - The concepts conventionally known as cutting and pasting are
    referred to as **killing** and **yanking**, respectively in Emacs
    parlance.
  - The current position of the cursor is called the **point** in Emacs.
    Technically, **point** is defined as the position right before the
    character where the cursor currently is.
  - Finally, each buffer may have several **modes** associated with it:
    a **major mode** and possibly several **minor modes**.
  - The **major mode** defines the main behavior of Emacs in the
    currently selected buffer. This can be roughly thought of as the
    file type. For example, if you're editing a Python file, the major
    mode is (by default) `python-mode` which causes Emacs to highlight
    Python syntax and automatically indent and outdent your code blocks
    as syntactically required by your Python code.
  - **Minor modes** define subtle changes in behavior and several minor
    modes may be active at once in the same buffer. An example minor
    mode is `flyspell-mode` which automatically highlights spelling
    errors in your
buffer.

# Navigation Basics

```text
The GUI version of Emacs can be navigated with the mouse like you would expect from a conventional GUI text editor.

The aim here is to focus on navigation solely using the keyboard as this enhances productivity immensely.


* Line movement

C-n --> Next line
C-p --> Previous line

* Character movement

C-f --> Go forward one character
C-b --> Go backward one character

* Word movement

M-f --> Go forward one word
M-b --> Go backward one word

* Sentence movement

M-a --> Move to the beginning of the sentence
M-e --> Move to the end of the sentence

* Beginning and end of line

C-a --> Move to the beginning of the line
C-e --> Move to the end of the line

* Beginning and end of buffer

M-< ("Meta+Shift+,") --> Go to the beginning of the buffer 
M-> ("Meta+Shift+.") --> Go to the end of the buffer 

* Screen movement

C-v --> Scroll down by one screen-full (the last two lines of the previous screen are kept as overlap for a smoother transition)
M-v --> Scroll up by one screen-full (same as above but with the first two lines)

* Centering the screen

C-l --> Move current line to the screen's center

The above key combination actually cycles through different states depending on how many times it's been pressed.

C-l --> Move current line to the screen's center
C-l C-l --> Move current line to the top of the screen
C-l C-l C-l --> Restore the position of the current line to where it was before the first C-l was pressed

If you press "C-l" a 4th time, it cycles back to centering the current line.

* Repeating movement commands

Most movement commands take a numerical prefix argument that says "repeat the following command that many times".

Example:

C-u 3 C-p  --> Go up 3 lines
C-u 5 C-f  --> Go forward 5 characters

One notable exception are the screen scrolling commands:

C-u 3 C-v  --> Scroll downward 3 lines (maintaining the position of the cursor)
```

Bonus: many of the above navigation commands are the default navigation
commands in Bash (e.g. pressing "C-b" while entering a Bash command
takes you back one
character).

# File editing basics

```text
* Quitting Emacs [ Now you can't say you don't know how to quit Emacs :-) ]

C-x C-c --> Quit Emacs and get prompted to save any unsaved files (buffers not visiting a file will simply be discarded unless you're running in client-server mode)

* Saving a buffer

C-x C-s --> Save the current buffer. If not visiting a file, it will prompt you for a file name to use to save the buffer.

* Searching within a buffer

C-s --> Search forwards within the buffer. Search is incremental and case-insensitive by default.
        Press C-s to move to the next match.
        If you press "RET", point is moved to the currently highlighted word and the search ends.
C-r --> Same as C-s except it searches backward

C-_ or C-/ --> Undo the last action. Keep pressing it to move up the undo tree.
C-? or M-_ --> Redo the previous change

The "undo" and "redo" commands can take prefix numerical arguments to undo or redo that many actions:

C-u 3 C-_ --> Undo the last 3 changes.
```

# Executing Elisp Functions

```text
You can execute any currently loaded Elisp functions (including ones you have written yourself) via "M-x"

M-x RET  --> Prompts you for name of function to execute (Tab completion is available).

Example:

M-x RET search-forward-regexp RET --> Prompts you for a regular expression and searches forward in the buffer for it
```

# Emacs Configuration

Emacs is configured using Elisp. On startup, it looks for a
configuration file either in `~/.emacs` or `~/.emacs.d/init.el` where
`~` refers to your home directory. If you're on Windows, consult [this
article](https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Location-of-init-file.html)
for the appropriate location of your configuration file.

# Vim inside Emacs

If you are considering the transition from Vim to Emacs and you're put
off by the non-modal nature of Emacs editing, there is an Emacs
extension known as `evil-mode` which lets you have many Vim concepts
inside Emacs. Here are some things added to Emacs by `evil-mode`:

  - Modal editing: you get normal, insert, visual and block visual modes
    like Vim. In addition, you get an "Emacs" mode where movement and
    navigation follow the Emacs bindings.
  - Same movement keys as Vim in normal mode
  - Leader key combinations
  - Pressing ":" in normal mode allows you to execute commands
    (including system commands)

In my own experience, `evil-mode` helps make the transition seamless and
allows you to blend the arguably more intuitive and ergonomic
keybindings of Vim with the unbridled power of Emacs for a truly
superior editing experience.

# Discoverable Help

Emacs features a pretty powerful help system that allows you to discover
new functionality all the
time.

```text
Obtaining help on specific topics. Tab completion is available for function and variable names.

C-h f RET --> Prompts you for the name of an elisp function and
              displays help text on it along with a clickable link
              to its source code.
C-h v RET --> Same as above with variables  

C-h k RET --> Allows you to enter a key combination and displays the
              name of the elisp function bound to it.

Searching for help:

C-h a --> Prompts you for a string to search for a command in the
          help system. Similar to the 'apropos' or 'man -k'
          commands in Unix systems.

Starting a tutorial:

C-h C-t --> Starts a tutorial designed to familiarize you with
            basic Emacs functionality.
```

# Emacs "Killer Apps"

As I hinted above, Emacs functionality goes way beyond being a mere text
editor. I will list here a couple of Emacs "apps" that are fairly
powerful and popular and may interest you in and of themselves.

## Org

Technnically, `org-mode`, a major mode for buffer editing that provides
organizational tools. It is very difficult to succinctly describe what
Org can do because it's a behemoth of a tool that has many diverse uses
to different people. I will attempt to describe the main features I use
briefly.

  - Divide your file into sections and sub-sections for easy outlining
    and organizing of concepts.
  - Different headings in the outline are foldable/expandable so that
    you can focus on what you need to focus on and eliminate
    distractions.
  - You can maintain a TODO list within Org
  - You can compile TODO lists from many files into an agenda
  - Track the time you spend on each TODO task
  - Manage tables in plain text (including spreadsheet-like
    capabilities)
  - Using the extension `org-babel`, write and execute code blocks in
    your file. The results are captured and are re-usable within the
    file itself. Think Jupyter notebook for any language.
  - Display inline images and LaTeX formulas as images within your file
    (makes for a great note-taking system and/or personal wiki)
  - Export your file into many different formats (LaTeX, PDF, html,…)

Org mode is a very powerful tool to add to your productivity arsenal
and, on a personal note, was the reason that caused me to start using
Emacs after years of using Vim.

## Magit

This is a frontend to `git` from within Emacs. It features a very
intuitive and discoverable interface, yet exposes very powerful
functionality that allows you to manage commits at the chunk level,
inspect diffs, rebase, cherry-pick, … all from within the comfort of
your own editor.

# A Word of Advice

If you are considering using Emacs, a common trap that beginning users
fall into is to copy someone else's configuration file and use it as is.
I highly recommend against doing this for several reasons:

  - It will discourage you from learning and finding things out for
    yourself
  - Someone else's configuration will probably contain many things
    relevant to them that you won't need or ever use.
  - It defeats the purpose of having a customizable text editor that can
    fit your own needs.

What I encourage you to do is to look at other people's configurations
and seek to understand them and adapt only what makes sense to you. You
can find out about new features of Emacs through many YouTube videos,
screencasts or blog posts and then learn for yourself how to add them to
your configuration and workflow. This way, you grow your configuration
incrementally while increasing your knowledge of Emacs along the way.

# Additional Resources

  - [The GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/emacs.html)
  - [Emacs Stack Exchange](https://emacs.stackexchange.com/)
  - [Emacs Wiki](https://www.emacswiki.org/emacs/EmacsWiki)
