---
language: forth
contributors:
    - ["Horse M.D.", "http://github.com/HorseMD/"]
translators:
    - ["Zach Larsen", "http://zachariahlarsen.com/"]
lang: es-es
filename: learnforth-es.fs
---

Forth fue criado por Charles H. Moore en los 70s. Forth es un lenguaje imperativo, basado en pila y entorno de programación, siendo usado en proyectos como Open Firmware. También esta usado por NASA.

Nota: Este articulo enfoca predominantemente en la Gforth implementación de Forth, pero casi todo 
de lo que esta escrito aquí debe funcionar en otro sitio.

```
\ Este es un comentario
( Este es un comentario también pero solo esta usado cuando definiendo palabras. )

\ --------------------------------- Precursor ----------------------------------

\ Todo programación en Forth se hace manipulando el parámetro pila (mas
\ común se refiere como "el pila").
5 2 3 56 76 23 65    \ ok

\ estos números se añadieron al pila desde izquierda a derecho.
.s    \ <7> 5 2 3 56 76 23 65 ok

\ En Forth, todo es o una palabra o un numero.

\ ------------------------------ Básico Aritmética ------------------------------

\ Aritmética (de hecho casi todas palabras que requieren datos) funciona manipulando datos
\ en el pila.
5 4 +    \ ok

\ `.` saca lo alto resulto desde el pila:
.    \ 9 ok

\ Mas ejemplos de aritmética:
6 7 * .        \ 42 ok
1360 23 - .    \ 1337 ok
12 12 / .      \ 1 ok
13 2 mod .     \ 1 ok

99 negate .    \ -99 ok
-99 abs .      \ 99 ok
52 23 max .    \ 52 ok
52 23 min .    \ 23 ok

\ ----------------------------- Pila Manipulación -----------------------------

\ Naturalmente, cuando trabajaremos con el pila, querremos algunos metidos útiles:

3 dup -          \ duplicar el primero articulo (1ra ahora igual a 2da): 3 - 3
2 5 swap /       \ intercambiar la primera con la segunda elemento:        5 / 2
6 4 5 rot .s     \ rotar los tres primero elementos:                   4 5 6
4 0 drop 2 /     \ sacar el primero articulo (no imprima a la pantalla):  4 / 2
1 2 3 nip .s     \ sacar el segundo articulo (similar a drop):    1 3

\ ---------------------- Mas Avanzado Pila Manipulación ----------------------

1 2 3 4 tuck   \ duplicar el primero articulo en el segundo hueco:      1 2 4 3 4 ok
1 2 3 4 over   \ duplicar el segundo articulo a la primera del pila:      1 2 3 4 3 ok
1 2 3 4 2 roll \ *mover* el articulo en este posición a la primera del pila:      1 3 4 2 ok
1 2 3 4 2 pick \ *duplicar* el articulo en este posición a la primera del pila: 1 2 3 4 2 ok

\ Cuando refiere a pila indices, ellos son basado en cero.

\ ------------------------------ Creando Palabras --------------------------------

\ La `:` palabra hace que Forth entra modo de compilar hasta que se ve la `;` palabra.
: cuadrado ( n -- n ) dup * ;    \ ok
5 cuadrado .                     \ 25 ok

\ Podemos ver lo que hace una palabra también.:
see cuadrado     \ : cuadrado dup * ; ok

\ -------------------------------- Condicionales --------------------------------

\ -1 == cierto, 0 == falso. No obstante, valores que no son cero es usualmente tratado como
\ siendo cierto:
42 42 =    \ -1 ok
12 53 =    \ 0 ok

\ `if` es una palabra que solamente compila. `if` <cosas para hacer> `then` <los de mas del programa>.
: ?>64 ( n -- n ) dup 64 > if ." Mas que 64!" then ; \ ok
100 ?>64                                                  \ Mas que 64! ok

\ Else:
: ?>64 ( n -- n ) dup 64 > if ." Mas que 64!" else ." Menos que 64!" then ;
100 ?>64    \ Mas que 64! ok
20 ?>64     \ Menos que 64! ok 

\ ------------------------------------ Loops -----------------------------------

\ `do` también es una palabra que solamente compila.
: miloop ( -- ) 5 0 do cr ." Hola!" loop ; \ ok
miloop
\ Hola!
\ Hola!
\ Hola!
\ Hola!
\ Hola! ok

\ `do` espera dos números en el pila: el último numero y el primero numero.

\ Podemos recibir el valor del indice mientras damos vuelta con `i`:
: uno-a-12 ( -- ) 12 0 do i . loop ;     \ ok
uno-a-12                                 \ 0 1 2 3 4 5 6 7 8 9 10 11 12 ok

\ `?do` funciona similarmente, pero salta el loop si el último y primero
\ números son iguales.
: cuadrados ( n -- ) 0 ?do i cuadrado . loop ;   \ ok
10 cuadrado                              \ 0 1 4 9 16 25 36 49 64 81 ok

\ cambiar el "paso" con `+loop`:
: treces ( n n -- ) ?do i . 3 +loop ;    \ ok
15 0 treces                             \ 0 3 6 9 12 ok

\ Indefinido loops empiezan `begin` <cosas para hacer> <bandera> `until`:
: death ( -- ) begin ." Ya hemos llegado?" 0 until ;    \ ok

\ ---------------------------- Variables y Memoria ----------------------------

\ Use `variable` declarar `edad` ser un variable.
variable edad    \ ok

\ Ahora escribimos 21 a edad con la palabra `!`.
21 edad !    \ ok

\ Por fin podemos imprimir nuestro variable usando la "leer" palabra `@`, que agregue el
\ valor a la pila, or usa `?` que lee y imprime todo juntos.
edad @ .    \ 21 ok
edad ?      \ 21 ok

\ Constantes son muy similar, pero no nos importa los direcciones de memoria:
100 constant PUNTA-QUE-AQUA-HIERVA   \ ok
PUNTA-QUE-AQUA-HIERVA .               \ 100 ok

\ ----------------------------------- Arrays -----------------------------------

\ Creando arrays es similar a variables, pero necesitamos alocar mas
\ memoria a ellos.

\ Puede usar `2 cells allot` para crear un array que es sea 3 cédulas de tamaño:
variable minumeros 2 cells allot    \ ok

\ Inicializar todos los valores a 0
minumeros 3 cells erase    \ ok

\ Alternativamente podemos usar `fill`:
minumeros 3 cells 0 fill

\ o podemos saltar todo arriba y inicializar con valores específicos:
create minumeros 64 , 9001 , 1337 , \ ok (el último `,` es importante!)

\ ...que es equivalente a:

\ Manualmente escribiendo valores a cada indice:
64 minumeros 0 cells + !      \ ok
9001 minumeros 1 cells + !    \ ok
1337 minumeros 2 cells + !    \ ok

\ Leyendo valores en particular array indices:
0 cells minumeros + ?    \ 64 ok
1 cells minumeros + ?    \ 9001 ok

\ Podemos simplificar un poco cuando hacemos una palabra que ayuda cuando manipulando arrays:
: de-arr ( n n -- n ) cells + ;    \ ok
minumeros 2 de-arr ?               \ 1337 ok

\ Que podemos usar cuando escribimos también:
20 minumeros 1 de-arr !    \ ok
minumeros 1 de-arr ?       \ 20 ok

\ ------------------------------ El Pila de Regreso ------------------------------

\ El pila de regreso se usa para retener punteros a cosas cuando palabras están
\ ejecutando otras palabras como loops.

\ Ya hemos visto un uso de esto: `i`, que duplica el primero del pila
\ de regreso. `i` es equivalente a `r@`.
: miloop ( -- ) 5 0 do r@ . loop ;    \ ok

\ También como leyendo, podemos agregar al pila de regreso y sacarlo:
5 6 4 >r swap r> .s    \ 6 5 4 ok

\ NOTA: Porque Forth usa el pila de regreso por punteros de palabras,  `>r` debe
\ siempre ser seguido por un `r>`.

\ ------------------------- Flotante Punto Operaciones --------------------------

\ La mayoría Forths evitan el uso de flotante punto operaciones.
8.3e 0.8e f+ f.    \ 9.1 ok

\ Usualmente agregamos al frente palabras con 'f' cuando usando flotantes:
variable miflotantevar    \ ok
4.4e miflotantevar f!     \ ok
miflotantevar f@ f.       \ 4.4 ok

\ --------------------------------- Notas al Final --------------------------------

\ Usando una palabra que no existe vaciara el pila. No obstante, también hay una palabra
\ específicamente por esto:
clearstack

\ vaciar la pantalla:
page

\ Cargando Forth archivos:
\ s" archivodeforth.fs" included

\ Puede listar cada palabra en el diccionario de Forth (pero es una lista gigante!):
\ words

\ Terminando Gforth:
\ bye

```

##Listo Para Mas?

* [Starting Forth](http://www.forth.com/starting-forth/)
* [Simple Forth](http://www.murphywong.net/hello/simple.htm)
* [Thinking Forth](http://thinking-forth.sourceforge.net/)
