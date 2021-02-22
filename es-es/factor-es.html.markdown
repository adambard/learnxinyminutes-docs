---
language: factor
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
translators:
    - ["Roberto R", "https://github.com/rrodriguze"]
filename: learnfactor-es.factor

lang: es-es
---
Factor es un lenguaje moderno basado en la pila, basado en Forth, creado por
Slava Pestov.

El código de este archivo puede escribirse en Factor, pero no importa
directamente porque el encabezado del vocabulario de importación haria que el
comienzo fuera totalmente confuso.

```factor
! Esto es un comentario

! Como Forth, toda la programación se realiza mediante la manipulación de la 
! pila.
! La intruducción de un valor literal lo coloca en la pila
5 2 3 56 76 23 65   ! No hay salida pero la pila se imprime en modo interactivo

! Esos números se agregan a la pila de izquierda a derecha
! .s imprime la pila de forma no destructiva.
.s     ! 5 2 3 56 76 23 65

! La aritmética funciona manipulando datos en la pila.
5 4 +    ! Sem saída

! `.` muestra el resultado superior de la pila y lo imprime.
.    ! 9

! Más ejemplos de aritmética:
6 7 * .        ! 42
1360 23 - .    ! 1337
12 12 / .      ! 1
13 2 mod .     ! 1

99 neg .       ! -99
-99 abs .      ! 99
52 23 max .    ! 52
52 23 min .    ! 23

! Se proporcionan varias palabras para manipular la pila, conocidas
colectivamente como palabras codificadas.

3 dup -          ! duplica el primer item (1st ahora igual a 2nd):    3 - 3
2 5 swap /       ! intercambia el primero con el segundo elemento:    5 / 2
4 0 drop 2 /     ! elimina el primer item (no imprime en pantalla):   4 / 2
1 2 3 nip .s     ! elimina el segundo item (semejante a drop):        1 3
1 2 clear .s     ! acaba con toda la pila
1 2 3 4 over .s  ! duplica el segundo item superior: 1 2 3 4 3
1 2 3 4 2 pick .s ! duplica el tercer item superior: 1 2 3 4 2 3

! Creando Palabras
! La palabra `:` factoriza los conjuntos en modo de compilación hasta que vea
la palabra`;`.
: square ( n -- n ) dup * ;    ! Sin salida
5 square .                     ! 25

! Podemos ver lo que las palabra hacen también.
! \ suprime la evaluación de una palabra y coloca su identificador en la pila.
\ square see    ! : square ( n -- n ) dup * ;

! Después del nombre de la palabra para crear, la declaración entre paréntesis 
da efecto a la pila.
! Podemos usar los nombres que queramos dentro de la declaración:
: weirdsquare ( camel -- llama ) dup * ;

! Mientras su recuento coincida con el efecto de pila de palabras:
: doubledup ( a -- b ) dup dup ; ! Error: Stack effect declaration is wrong
: doubledup ( a -- a a a ) dup dup ; ! Ok
: weirddoubledup ( i -- am a fish ) dup dup ; ! Além disso Ok

! Donde Factor difiere de Forth es en el uso de las citaciones.
! Una citacion es un bloque de código que se coloca en la pila como un valor.
! [ inicia el modo de citación; ] termina.
[ 2 + ]       ! La cita que suma dos queda en la pila
4 swap call . ! 6

! Y así, palabras de orden superior. TONOS de palabras de orden superior
2 3 [ 2 + ] dip .s      ! Tomar valor de la parte superior de la pilar, cotizar, retroceder: 4 3
3 4 [ + ] keep .s       ! Copiar el valor desde la parte superior de la pila, cotizar, enviar copia: 7 4
1 [ 2 + ] [ 3 + ] bi .s ! Ejecute cada cotización en el valor superior, empuje amabos resultados: 3 4
4 3 1 [ + ] [ + ] bi .s ! Las citas en un bi pueden extraer valores más profundos de la pila: 4 5 ( 1+3 1+4 )
1 2 [ 2 + ] bi@ .s      ! Citar en primer y segundo valor
2 [ + ] curry           ! Inyecta el valor dado al comienzo de la pila: [ 2 + ] se deja en la pila

! Condicionales
! Cualquier valor es verdadero, excepto el valor interno f.
! no existe un valor interno, pero su uso no es esencial.
! Los condicionales son palabras de orden superior, como con los combinadores
! anteriores

5 [ "Five is true" . ] when                     ! Cinco es verdadero
0 [ "Zero is true" . ] when                     ! Cero es verdadero
f [ "F is true" . ] when                        ! Sin salida
f [ "F is false" . ] unless                     ! F es falso
2 [ "Two is true" . ] [ "Two is false" . ] if   ! Two es verdadero

! Por defecto, los condicionales consumen el valor bajo prueba, pero las
! variantes con un 
! asterisco se dejan solo si es verdad:

5 [ . ] when*      ! 5
f [ . ] when*      ! Sin salida, pila vacía, se consume porque f es falso


! Lazos
! Lo has adivinado... estas son palabras de orden superior también.

5 [ . ] each-integer               ! 0 1 2 3 4
4 3 2 1 0 5 [ + . ] each-integer   ! 0 2 4 6 8
5 [ "Hello" . ] times              ! Hello Hello Hello Hello Hello

! Here's a list:
{ 2 4 6 8 }                        ! Goes on the stack as one item

! Aqui está uma lista:
{ 2 4 6 8 } [ 1 + . ] each          ! Exibe 3 5 7 9
{ 2 4 6 8 } [ 1 + ] map             ! Salida { 3 5 7 9 } de la pila

! Reduzir laços ou criar listas:
{ 1 2 3 4 5 } [ 2 mod 0 = ] filter  ! Solo mantenga miembros de la lista para los cuales la cita es verdadera: { 2 4 }
{ 2 4 6 8 } 0 [ + ] reduce .        ! Como "fold" en lenguajes funcinales: exibe 20 (0+2+4+6+8)
{ 2 4 6 8 } 0 [ + ] accumulate . .  ! Como reducir, pero mantiene los valores intermedios en una lista: { 0 2 6 12 } así que 20
1 5 [ 2 * dup ] replicate .         ! Repite la cita 5 veces y recoge los resultados en una lista: { 2 4 8 16 32 }
1 [ dup 100 < ] [ 2 * dup ] produce ! Repite la segunda cita hasta que la primera devuelva falso y recopile los resultados: { 2 4 8 16 32 64 128 }

! Si todo lo demás falla, un propósito general a repetir.
1 [ dup 10 < ] [ "Hello" . 1 + ] while  ! Escribe "Hello" 10 veces
                                        ! Sí, es dificil de leer
                                        ! Para eso están los bucles variantes

! Variables
! Normalmente, se espera que los programas de Factor mantengan todos los datos 
! en la pila.
! El uso de variables con nombre hace que la refactorización sea más difícil 
! (y se llama Factor por una razón)
! Variables globales, si las necesitas:

SYMBOL: name            ! Crea un nombre como palabra de identificación
"Bob" name set-global   ! Sin salída
name get-global .       ! "Bob"

! Las variables locales nombradas se consideran una extensión, pero están 
! disponibles
! En una cita ..
[| m n                  ! La cita captura los dos valores principales de la pila en m y n
 | m n + ]              ! Leerlos

! Ou em uma palavra..
:: lword ( -- )           ! Tenga en cuenta los dos puntos dobles para invocar la extensión de variable léxica
   2 :> c                 ! Declara la variable inmutable c para contener 2
   c . ;                  ! Imprimirlo

! En una palabra declarada de esta manera, el lado de entrada de la declaración
! de la pila
! se vuelve significativo y proporciona los valores de las variables en las que
! se capturan los valores de pila
:: double ( a -- result ) a 2 * ;

! Las variables se declaran mutables al terminar su nombre con su signo de 
! exclamación
:: mword2 ( a! -- x y )   ! Capture la parte superior de la pila en la variable mutable a
   a                      ! Empujar a
   a 2 * a!               ! Multiplique por 2 y almacenar el resultado en a
   a ;                    ! Empujar el nuevo valor de a
5 mword2                  ! Pila: 5 10

! Listas y Secuencias
! Vimos arriba cómo empujar una lista a la pila

0 { 1 2 3 4 } nth         ! Acceder a un miembro específico de una lista: 1
10 { 1 2 3 4 } nth        ! Error: índice de secuencia fuera de los límites
1 { 1 2 3 4 } ?nth        ! Lo mismo que nth si el índice está dentro de los límites: 2
10 { 1 2 3 4 } ?nth       ! Sin errores si está fuera de los límites: f

{ "at" "the" "beginning" } "Append" prefix    ! { "Append" "at" "the" "beginning" }
{ "Append" "at" "the" } "end" suffix          ! { "Append" "at" "the" "end" }
"in" 1 { "Insert" "the" "middle" } insert-nth ! { "Insert" "in" "the" "middle" }
"Concat" "enate" append                       ! "Concatenate" - strings are sequences too
"Concatenate" "Reverse " prepend              ! "Reverse Concatenate"
{ "Concatenate " "seq " "of " "seqs" } concat ! "Concatenate seq of seqs"
{ "Connect" "subseqs" "with" "separators" } " " join  ! "Connect subseqs with separators"

! Y si desea obtener meta, las citas son secuencias y se pueden desmontar
0 [ 2 + ] nth                              ! 2
1 [ 2 + ] nth                              ! +
[ 2 + ] \ - suffix                         ! Quotation [ 2 + - ]


```

##Listo para más?

* [Documentación de Factor](http://docs.factorcode.org/content/article-help.home.html)
