---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
filename: LearnVim.txt
---


[Vim](www.vim.org)
(Vi IMproved) is a clone of the popular vi editor for Unix. It is a text 
editor designed for speed and increased productivity, and is ubiquitous in most 
unix-based systems. It has numerous keybindings for speedy navigation to 
specific points in the file, and for fast editing.

## Basics of navigating Vim

```
    vim <filename>   # Open <filename> in vim
    :q               # Quit vim
    :w               # Save current file
    :wq              # Save file and quit vim 
    :q!              # Quit vim without saving file
                     # ! *forces* :q to execute, hence quiting vim without saving
    :x               # Save file and quit vim, shorter version of :wq

    u                # Undo
    CTRL+R           # Redo

    h                # Move left one character
    j                # Move down one line
    k                # Move up one line
    l                # Move right one character

    # Moving within the line

    0                # Move to beginning of line
    $                # Move to end of line
    ^                # Move to first non-blank character in line

    # Searching in the text

    /word            # Highlights all occurences of word after cursor
    ?word            # Highlights all occurences of word before cursor
    n                # Moves cursor to next occurence of word after search
    N                # Moves cursor to previous occerence of word

    :%s/foo/bar/g    # Change 'foo' to 'bar' on every line in the file
    :s/foo/bar/g     # Change 'foo' to 'bar' on the current line

    # Jumping to characters

    f<character>     # Jump forward and land on <character>
    t<character>     # Jump forward and land right before <character> 

    # For example,    
    f<               # Jump forward and land on <
    t<               # Jump forward and land right before <
    
    # Moving by word

    w                # Move forward by one word
    b                # Move back by one word
    e                # Move to end of current word

    # Other characters for moving around

    gg               # Go to the top of the file
    G                # Go to the bottom of the file
    :NUM             # Go to line number NUM (NUM is any number)
    H                # Move to the top of the screen
    M                # Move to the middle of the screen
    L                # Move to the bottom of the screen
```

## Modes:

Vim is based on the concept on **modes**.

Command Mode - vim starts up in this mode, used to navigate and write commands  
Insert Mode  - used to make changes in your file  
Visual Mode  - used to highlight text and do operations to them  
Ex Mode      - used to drop down to the bottom with the ':' prompt to enter commands

```
    i                # Puts vim into insert mode, before the cursor position
    a                # Puts vim into insert mode, after the cursor position
    v                # Puts vim into visual mode    
    :                # Puts vim into ex mode
    <esc>            # 'Escapes' from whichever mode you're in, into Command mode

    # Copying and pasting text

    y                # Yank whatever is selected
    yy               # Yank the current line
    d                # Delete whatever is selected
    dd               # Delete the current line
    p                # Paste the copied text after the current cursor position
    P                # Paste the copied text before the current cursor position
    x                # Deleting character under current cursor position
```

## The 'Grammar' of vim

Vim can be thought of as a set of commands in a 
'Verb-Modifier-Noun' format, where:

Verb     - your action  
Modifier - how you're doing your action  
Noun     - the object on which your action acts on

A few important examples of 'Verbs, 'Modifiers', and 'Nouns':

```
    # 'Verbs'
    
    d                # Delete
    c                # Change
    y                # Yank (copy)
    v                # Visually select

    # 'Modifiers'

    i                # Inside
    a                # Around
    NUM              # Number (NUM is any number)
    f                # Searches for something and lands on it
    t                # Searches for something and stops before it
    /                # Finds a string from cursor onwards
    ?                # Finds a string before cursor

    # 'Nouns'

    w                # Word
    s                # Sentence
    p                # Paragraph
    b                # Block
    
    # Sample 'sentences' or commands

    d2w              # Delete 2 words
    cis              # Change inside sentence
    yip              # Yank inside paragraph (copy the para you're in)
    ct<              # Change to open bracket
                     # Change the text from where you are to the next open bracket
    d$               # Delete till end of line
```

## Some shortcuts and tricks

        <!--TODO: Add more!-->
```
    >                # Indent selection by one block
    <                # Dedent selection by one block
    :earlier 15m     # Reverts the document back to how it was 15 minutes ago
    :later 15m       # Reverse above command
    ddp              # Swap position of consecutive lines, dd then p
    .                # Repeat previous action
```

## Macros

Macros are basically recordable actions.
When you start recording a macro, it records **every** action and command
you use, until you stop recording. On invoking a macro, it applies the exact
same sequence of actions and commands again on the text selection.

```
    qa               # Start recording a macro named 'a'
    q                # Stop recording
    @a               # Play back the macro
```

### Configuring ~/.vimrc

The .vimrc file can be used to configure Vim on startup.

Here's a sample ~/.vimrc file:

```
" Example ~/.vimrc
" 2015.10 

" Required for vim to be iMproved
set nocompatible

" Determines filetype from name to allow intelligent auto-indenting, etc.
filetype indent plugin on

" Enable syntax highlighting
syntax on

" Better command-line completion
set wildmenu

" Use case insensitive search except when using capital letters
set ignorecase
set smartcase

" When opening a new line and no file-specific indenting is enabled,
" keep same indent as the line you're currently on
set autoindent

" Display line numbers on the left
set number

" Indentation options, change according to personal preference

" Number of visual spaces per TAB
set tabstop=4

" Number of spaces in TAB when editing
set softtabstop=4

" Number of spaces indented when reindent operations (>> and <<) are used
set shiftwidth=4

" Convert TABs to spaces
set expandtab

" Enable intelligent tabbing and spacing for indentation and alignment
set smarttab
```

### References

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
