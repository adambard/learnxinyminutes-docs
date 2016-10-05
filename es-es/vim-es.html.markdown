---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
- ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
filename: LearnVim-es.txt
---


[Vim](www.vim.org)
(Vi IMproved) es un clón del popular editor vi para Unix. Es un editor de texto
diseñado para ser veloz e incrementar la productividad, es ubicuo en la mayoría
de los sistemas basados en UNIX. cuenta con numerosas combinaciones de teclas 
para la navegación rápida a puntos especificos en un archivo y para rápida edición.



## Fundamentos de la navegación en Vim

```
    vim <archivo>    # Abre <archivo> en vim
    :q               # Salir de vim
    :w               # Guardar archivo actual
    :wq              # Guardar archivo y salir de vim
    :q!              # Salir de vim sin grabar el archivo
                     # ! *forza* :q a ejecutarse, por lo tanto sale de vim sin guardar
    :x               # Guardar el archivo y salir de vim, versión corta de :wq

    u                # Deshacer
    CTRL+R           # Rehacer

    h                # Desplazarse un caracter hacía la izquierda
    j                # Desplazarse una línea hacía abajo
    k                # Desplazarse una línea hacía arriba
    l                # Desplazarse un caracter hacía la derecha

    # Desplazarse dentro de la línea 

    0                # Desplazarse hacia el inicio de la línea
    $                # Desplazarse al final de la línea
    ^                # Desplazarse al primer caracter no blanco en la línea

    # Buscando en el texto

    /word            # Resalta todas las ocurrencias de la palabra después del cursor
    ?word            # Resalta todas las ocurrencias de la palabra antes del cursor
    n                # Desplaza el cursor a la siguiente ocurrencia de la palabra después de una búsqueda
    N                # Desplaza el cursor a la anterior ocurrencia de la palabra

    :%s/foo/bar/g    # Cambia 'foo' a 'bar' en cada línea en el archivo
    :s/foo/bar/g     # Cambia 'foo' a 'bar' en la línea actual

    # Saltando caracteres

    f<carácter>     # Salta adelante y aterrizar en <carácter>
    t<carácter>     # Salta adelante y aterrizar antes de <carácter>

    # Por ejemplo,
    f<               # Salta adelante y aterriza en <
    t<               # Salta adelante y aterriza justo antes de <
    
    # Desplazarse por palabras

    w                # Desplazarse hacia adelante por una palabra
    b                # Desplazarse hacia atrás por una palabra
    e                # Desplazarse al final de la palabra actual

    # Otros caracteres para desplazarse

    gg               # Ir al principio del archivo
    G                # Ir al final del archivo
    :NUM             # Ir a la línea número NUM (NUM es cualquier número)
    H                # Desplazarse al principio de la pantalla
    M                # Desplazarse a la mitad de la pantalla
    L                # Desplazarse al final de la pantalla
```

## Modos:

Vim se basa en el concepto de **modos**.


Modo Comando    - vim se pone en marcha en éste modo, se usa para navegar y escribir comandos
Modo Inserción  - Utilizado para realizar cambios en el archivo
Modo Visual     - Utilizado para resaltar texto y operar en ellos
Modo Ex         - utilizado para ir hacia la parte inferior con ':' para introducir comandos


```
    i                # Pone a Vim en modo de inserción, antes de la posición del cursor
    a                # Pone a Vim en modo de inserción, después de la posición del cursor
    v                # Pone a Vim en modo de visual
    :                # Pone a Vim en modo Ex
    <esc>            # Sale de cualquier modo en que se encuentre, al modo comando

    # Copiando y pegando texto

    y                # Copia lo que se encuentre seleccionado
    yy               # Copia la linea actual
    d                # Elimina lo que se encuentre seleccionado
    dd               # Elimina la linea actual
    p                # Pega el texto copiado después de la posición del cursor
    P                # Pega el texto copiado antes de la posición del cursor
    x                # Elimina el carácter debajo de la posición del cursor
```

## La "Gramática" de vim

Vim puede ser pensado como un conjunto de comandos en un
formato "verbo-sustantivo-modificador ', donde:

Verbo       - La acción a realizazr
Modificador - Como vas hacer la acción
Sustantivo  - el objeto al que se le va a aplicar la acción

Algunos ejemplos importantes de "Verbos", "Modificadores" y "Sustantivos":

```
    # 'Verbos'
 
    d                # Eliminar
    c                # Cambiar
    y                # Copiar
    v                # Seleccionar visualmente

    # 'Modificadores'

    i                # Dentro
    a                # Alrededor
    NUM              # Número (NUM es cualquier número)
    f                # Busca algo y aterriza sobre el
    t                # Busca algo y se detiene antes de
    /                # Encuentra una cadena desde el cursor en adelante
    ?                # Encuentra una cadena antes del cursor

    # 'Sustantivos'

    w                # Palabra
    s                # Oración
    p                # Párrafo
    b                # Bloque
    
    # "Frases" de ejemplo o comandos

    d2w              # Elimina 2 palabras
    cis              # Cambia dentro de una oración
    yip              # Copia dentro de un párrafo (copia el párrafo donde estás)
    ct<              # Cambia para abrir un paréntesis
                     # Cambie el texto desde donde está a la siguiente paréntesis abierto
    d$               # Eliminar hasta el final de la línea
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
