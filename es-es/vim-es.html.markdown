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


[Vim](http://www.vim.org)
(Vi IMproved) es un clón del popular editor vi para UNIX. Es un editor de texto
diseñado para ser veloz e incrementar la productividad, es ubicuo en la mayoría
de los sistemas basados en UNIX. Cuenta con numerosas combinaciones de teclas 
para la navegación rápida a puntos especificos en un archivo y para rápida edición.



## Fundamentos de la navegación en Vim

```
    vim <archivo>    # Abre <archivo> en vim
    :q               # Salir de vim
    :w               # Guardar archivo actual
    :wq              # Guardar archivo y salir de vim
    :q!              # Salir de vim sin guardar el archivo
                     # ! *forza* :q a ejecutarse, por lo tanto sale de vim sin guardar
    :x               # Guardar el archivo y salir de vim, versión corta de :wq

    u                # Deshacer
    CTRL+R           # Rehacer

    h                # Desplazarse un carácter hacía la izquierda
    j                # Desplazarse una línea hacía abajo
    k                # Desplazarse una línea hacía arriba
    l                # Desplazarse un carácter hacía la derecha

    # Desplazarse dentro de la línea 

    0                # Desplazarse hacia el inicio de la línea
    $                # Desplazarse al final de la línea
    ^                # Desplazarse al primer carácter no blanco en la línea

    # Buscando en el texto

    /word            # Resalta todas las ocurrencias de la palabra después del cursor
    ?word            # Resalta todas las ocurrencias de la palabra antes del cursor
    n                # Desplaza el cursor a la siguiente ocurrencia de la palabra después de una búsqueda
    N                # Desplaza el cursor a la anterior ocurrencia de la palabra

    :%s/foo/bar/g    # Cambia 'foo' a 'bar' en cada línea en el archivo
    :s/foo/bar/g     # Cambia 'foo' a 'bar' en la línea actual

    # Saltando caracteres

    f<carácter>     # Salta adelante y aterriza en <carácter>
    t<carácter>     # Salta adelante y aterriza antes de <carácter>

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


Modo Comando    - Vim se pone en marcha en éste modo, se usa para navegar y escribir comandos    
Modo Inserción  - Utilizado para realizar cambios en el archivo    
Modo Visual     - Utilizado para resaltar texto y operar en ellos    
Modo Ex         - Utilizado para ir hacia la parte inferior con ':' para introducir comandos


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

Verbo       - La acción a realizar    
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

## Algunos accesos directos y trucos

```
    >                # Sangrar la selección por un bloque
    <                # Desangrar la selección por un bloque
    :earlier 15m     # Devuelve el documento de nuevo a como era hace 15 minutos
    :later 15m       # Deshace el comando anterior
    ddp              # Intercambia la posición de las lineas consecutivas, dd después p
    .                # Repite la acción previa
```

## Macros

Las macros son, básicamente, las acciones que se pueden grabar.
Cuando comienzas a grabar un macro, registra **todas** las acciones y comandos
que se utilizan hasta que detenga la grabación. En la invocación de un macro,
se aplica exactamente la misma secuencia de acciones y comandos de nuevo
en la selección de texto.

```
    qa               # Comienza a grabar un macro llamada 'a'
    q                # Detiene la grabación
    @a               # Comienza la reproducción del macro
```

### Configurando ~/.vimrc

El archivo .vimrc puede ser usado para configurar Vim en el arranque.

Aquí está un ejemplo de un archivo ~ / .vimrc:

```
" Ejemplo ~/.vimrc
" 2015.10 

" Se requiere para que vim sea 'mejor'
set nocompatible

" Determina la extensión del archivo por el nombre para permitir el auto-indentado inteligente, etc...
filetype indent plugin on

" Habilita el resaltado de sintaxis
syntax on

" Mejor terminación de línea de comandos
set wildmenu

" Usa búsqueda sensible a mayúsculas excepto cuando se utilizan letras mayúsculas
set ignorecase
set smartcase

" Al abrir una nueva línea, si la sangría especifica del archivo no está habilitada,
" mantén la misma sangría que la línea que estás actualmente
set autoindent

" Despliega el número de línea a la izquierda
set number

" Opciones de sangría, cambialas de acuerdo a tus preferencias personales

" Número de espacios visuales por tabulación
set tabstop=4

" Número de espacios de las tabulaciones al editar
set softtabstop=4

" Número de espacios sangrados cuando las operaciones de resangrado (>> y <<) son usadas
set shiftwidth=4

" Convertir tabulaciones en espacios
set expandtab

" Habilitar la tabulación inteligente y el espaciamiento para el sangrado y la alineación
set smarttab
```

### Referencias

[Vim | Home (EN)](http://www.vim.org/index.php)

`$ vimtutor` Command

[A vim Tutorial and Primer (EN)](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread) (EN)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki (EN)](https://wiki.archlinux.org/index.php/Vim)