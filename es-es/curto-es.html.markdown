---
language: curto
contributors:
    - ["Maleza", "https://maleza.srht.site/"]
lang: es-es
filename: learncurto-es.fs
---

Curto es una traducción completa al español de [UF Forth](http://www.call-with-current-continuation.org/uf/uf.html) (de Felix Winkelmann), un Forth para la [Máquina Virtual UXN](https://wiki.xxiivv.com/site/uxn.html).

```
\ Este es un comentario
( Este es un comentario también pero solo es usado en definiciones de palabras. )

\ --------------------------------- La Pila ----------------------------------

\ Todo programación en Curto se hace manipulando la pila de parámetros
\ (habitualmente referida como "la pila").
5 2 3 56 76 23 65    \ ok

\ estos números se añadieron a la pila de izquierda a derecha.
.s    \ <7> 5 2 3 56 76 23 65 ok

\ En Forth, todo es o una palabra o un número.

\ ------------------------------ Aritmética Básica ------------------------------

\ La aritmética (de hecho casi todas palabras que requieren datos) funciona manipulando datos
\ en el pila.
5 4 +    \ ok

\ `.` saca el resultado del tope de la pila:
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

\ ----------------------------- Manipulación de Pila -----------------------------

\ Naturalmente, cuando trabajaremos con la pila, querremos algunos métodos útiles:

3 dup -          \ duplica el primer valor (1ra ahora igual a 2da): 3 - 3
2 5 cambiar /       \ intercambia primer y segundo valor:        5 / 2
6 4 5 rot .p     \ rota los tres primeros valores:                   4 5 6
4 0 soltar 2 /     \ suelta el primer valor (no imprime a pantalla):  4 / 2
1 2 3 pellizcar .p     \ suelta el segundo valor (similar a soltar):    1 3

\ ---------------------- Manipulación de Pila Más Avanzada ----------------------

1 2 3 4 plegar   \ duplicar el primer valor debajo del segundo:      1 2 4 3 4 ok
1 2 3 4 encima   \ empuja segundo valor de la pila:      1 2 3 4 3 ok
1 2 3 4 2 elegir \ duplica el valor en esta posición al tope de la pila: 1 2 3 4 2 ok

\ Los índices de la pila son basados en cero.

\ ------------------------------ Creando Palabras --------------------------------

\ La palabra `:` hace que Curto entre en modo compilar hasta que se ve la palabra `;`.
: cuadrado ( n -- n ) dup * ;    \ ok
5 cuadrado .                     \ 25 ok

\ También podemos ver lo que hace una palabra (código uxntal al que fué compilada):
ver cuadrado     \ DUP2 MUL2 JMP2r ok

\ -------------------------------- Condicionales --------------------------------

\ -1 == verdadero, 0 == falso. No obstante, cualquier valor distinto de cero es
\ considerado verdadero:
42 42 =    \ -1 ok
12 53 =    \ 0 ok

\ `si` es una palabra solo de compilación. `si` <cosas para hacer> `entonces` <resto del programa>.
: ?>64 ( n -- n ) dup 64 > si ." Mas que 64!" entonces ; \ ok
100 ?>64                                                  \ Mas que 64! ok

\ `sino`:
: ?>64 ( n -- n ) dup 64 > si ." Mas que 64!" sino ." Menos que 64!" entonces ;
100 ?>64    \ Mas que 64! ok
20 ?>64     \ Menos que 64! ok 

\ ------------------------------------ Bucles -----------------------------------

\ `hacer` también es una palabra solo de compilación.
: miloop ( -- ) 5 0 hacer rc ." Hola!" bucle ; \ ok
miloop
\ Hola!
\ Hola!
\ Hola!
\ Hola!
\ Hola! ok

\ `hacer` espera dos números en el pila: el de incicio y el de terminación.

\ Podemos recibir el valor del indice mientras iteramos con `i`:
: uno-a-12 ( -- ) 13 1 hacer i . bucle ;     \ ok
uno-a-12                                 \ 0 1 2 3 4 5 6 7 8 9 10 11 12 ok

\ `?hacer` funciona similarmente, pero salta el loop si el último y el primer
\ número son iguales.
: cuadrados ( n -- ) 0 ?hacer i cuadrado . bucle ;   \ ok
10 cuadrados                              \ 0 1 4 9 16 25 36 49 64 81 ok

\ cambiar el "paso" con `+bucle`:
: treces ( n n -- ) ?hacer i . 3 +bucle ;    \ ok
15 0 treces                             \ 0 3 6 9 12 ok

\ Los bucles indefinidos comienzan con `empezar` <cosas para hacer> <bandera> `hasta`:
: death ( -- ) empezar ." Ya hemos llegado?" 0 hasta ;    \ ok

\ ---------------------------- Constantes, Variables y Memoria ----------------------------

\ Declara una constante `dedos` igual a 5
5 constante dedos
dedos .		\ 5 ok

\ Crea la variable `edad`.
variable edad    \ ok

\ Ahora escribimos 21 a edad con la palabra `!`.
21 edad !    \ ok

\ Podemos imprimir nuestra variable usando la palabra leer `@`, que empuja el
\ valor a la pila, o `?` que lee e imprime en un solo paso.
edad @ .    \ 21 ok
edad ?      \ 21 ok

\ ----------------------------------- Arreglos -----------------------------------

\ Crear arreglos es similar a crear variables, pero necesitamos alocar mas
\ memoria para ellos.

\ Podemos usar `2 celdas alocar` para crear un arreglo que sea de 3 celdas de tamaño:
variable misnumeros 2 celdas alocar    \ ok

\ Inicializar todos los valores a 0
misnumeros 3 celdas borrar    \ ok

\ Alternativamente podemos usar `llenar`:
misnumeros 3 celdas 0 llenar

\ o podemos saltearnos todo lo anterior e inicializar con valores específicos:
crear misnumeros 64 , 9001 , 1337 , \ ok (la última `,` es importante!)

\ ...que es equivalente a:

\ Manualmente escribir valores a cada indice:
64 misnumeros 0 celdas + !      \ ok
9001 misnumeros 1 celdas + !    \ ok
1337 misnumeros 2 celdas + !    \ ok

\ Leyendo valores de un índice en particular:
0 celdas misnumeros + ?    \ 64 ok
1 celdas misnumeros + ?    \ 9001 ok

\ Podemos simplificar un poco creando una palabra que ayuda a manipular arreglos:
: de-arr ( n n -- n ) celdas + ;    \ ok
misnumeros 2 de-arr ?               \ 1337 ok

\ Que podemos usar cuando escribimos también:
20 misnumeros 1 de-arr !    \ ok
misnumeros 1 de-arr ?       \ 20 ok

\ ------------------------------ La Pila de Retorno ------------------------------

\ La pila de retorno se usa para retener punteros a cosas cuando las palabras están
\ ejecutando otras palabras como en los bucles.

\ Ya hemos visto un uso de esto: `i`, que duplica el tope de la pila
\ de retorno. `i` es equivalente a `r@`.
: mibucle ( -- ) 5 0 hacer r@ . bucle ;    \ ok

\ También podemos agregar y retirar de la pila de retorno:
5 6 4 >r swap r> .p    \ 6 5 4 ok

\ NOTA: Como Curto usa la pila de retorno para retornar a palabras,  `>r` debe
\ siempre ser seguido por un `r>`.

\ --------------------------------- Notas al Final --------------------------------

\ Usar una palabra que no existe vaciara la pila. No obstante, también hay una palabra
\ específicamente para esto:
limpiar

\ vaciar la pantalla:
pagina

\ Cargando archivos Curto:
\ c" archivodeforth.fs" incluido

\ Puede listar cada palabra en el diccionario de Curto (pero es una lista gigante!):
palabras

\ Terminando Curto:
chau

```

##Listo Para Mas?

* [README](https://git.sr.ht/~maleza/curto/tree/curto/item/README)
* [GLOSARIO](https://git.sr.ht/~maleza/curto/tree/curto/item/GLOSARIO)
