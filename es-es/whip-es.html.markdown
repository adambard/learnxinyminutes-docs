---
language: whip
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
translators:
    - ["Daniel Zendejas", "https://github.com/DanielZendejas"]
author: Tenor Biel
author_url: http://github.com/L8D
filename: whip-es.lisp
lang: es-es
---
Tutorial de Whip en español.

Whip es un dialecto de LISP hecho para escribir código y conceptos
simples. Ha tomado prestado bastante de la sintaxis de Haskell
(un lenguaje no relacionado).

Esta documentación fue escrita por el creador del lenguaje

```scheme
; Los comentarios son como en LISP, con punto y coma...

; La mayoría de las sentencias de primer nivel están dentro de
; "formas". Una forma no es más que cosas dentro de paréntesis
no_en_la_forma
(en_la_form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Números, Strings y Operadores

;Whip tiene un tipo para números (es el estándar 64-bit IEEE 754 double, de JS)
3 ; => 3
1.5 ; => 1.5

; Las funciones son llamadas si son el primer elemento de una forma
(funcion_llamada argumentos)

; La mayoría de los operadores se hacen con funciones
; Toda la aritmética básica es bastante estándar
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2
; incluso el módulo
(% 9 4) ; => 1
; división impar al estilo de JavaScript.
(/ 5 2) ; => 2.5

; Las formas anidadas funcionan como se espera.
(* 2 (+ 1 3)) ; => 8

; Hay un tipo booleano.
true
false

; Los Strings son creados con comillas dobles ".
"Hola mundo"

; Los caracteres solos se declaran con comillas simples '.
'a'

; La negación usa la función 'not'.
(not true) ; => false
(not false) ; => true

; La mayoría de las funcions que no vienen de Haskell tienen
; atajos. La función 'not' también se puede declarar con '!'.
(! (! true)) ; => true

; La igualdad es `equal` o `=`.
(= 1 1) ; => true
(equal 2 1) ; => false

; Por ejemplo, la desigualdad sería combinar la función 'not' con
; la función de igualdad
(! (= 2 1)) ; => true

; Más comparaciones
(< 1 10) ; => true
(> 1 10) ; => false
; y su contraparte textual.
(lesser 1 10) ; => true
(greater 1 10) ; => false

; Los Strings pueden concatenarse con la función +.
(+ "Hola " "mundo!") ; => "Hello world!"

; También puedes usar las comparativas de JavaScript
(< 'a' 'b') ; => true
; ...y la coerción de tipos
(= '5' 5)

; La función 'at' o @ accesa a los caracteres dentro de los strings,
; empezando en 0.
(at 0 'a') ; => 'a'
(@ 3 "foobar") ; => 'b'

; También están las variables  `null` and `undefined`.
null; usado para indicar una falta de valor deliberada.
undefined; usado para indicar un valor que aún no está definido.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Variables, Listas y Diccionarios

; Las variables son declaradas con las funciones `def` o `let`.
; Las variables que aún no son asignadas tendrán el valor `undefined`.
(def mi_variable 5)
; `def` asignará la variable al contexto global.
; `let` asignará la variable al contexto local,  
; y tiene una sintaxis distinta.
(let ((mi_variable 5)) (+ mi_variable 5)) ; => 10
(+ mi_variable 5) ; = undefined + 5 => undefined

; Las listas son arreglos de valores de cualquier tipo.
; Básicamente, son formas sin funciones al inicio.
(1 2 3) ; => [1, 2, 3] (sintaxis JavaScript)

; Los diccionarios son el equivalente en Whip de los 'objetos' de JavaScript,
; los 'dicts' de Python o los 'hashes' de Ruby: una colección desordenada
; de pares llave-valor
{"llave1" "valor1" "llave2" 2 3 3}

; Las llaves son sólo valores, identificadores, números o strings.
(def mi_diccionario {mi_llave "mi_valor" "mi otra llave" 4})
; Pero con Whip, los diccionarios son leidos así:
; "llave" "espacio en blanco" "valor" "espacio en blanco"
{"llave" "valor"
"otra llave"
1234
}

; Las definiciones de los diccionarios pueden accesarse con la función @
; (como los strings y las listas)
(@ "mi otra llave" mi_diccionario) ; => 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Logica y secuencias de control

; La funcion `if` es bastante simple, aunque distinta que en otros lenguajes.
(if true "regresa esto si es true" "regresa esto si es false")
; => "regresa esto si es true"

; Y para el operador ternario `?`
(? false true false) ; => false

? `both` es un 'y' lógico, mientras que la función `either` es un 'o'.
(both true true) ; => true
(both true false) ; => false
(either true false) ; => true
(either false false) ; => false
; Y sus atajos son '&' y '^' respectivamente
; & => both
; ^ => either
(& true true) ; => true
(^ false true) ; => true

;;;;;;;;;
; Lambdas

; Las Lambdas en Whip son declaradas con las funciones `lambda` o `->`.
; Las funciones regulares en realidad sólo son lambdas con nombre.
(def mi_funcion (-> (x y) (+ (+ x y) 10)))
;         |       |   |         |
;         |       |   |    valor regresado(estas son las variables argumentos)
;         |       | argumentos
;         | declaración de lambda
;         |
;   nombre de la lambda

(mi_funcion 10 10) ; = (+ (+ 10 10) 10) => 30

; Obviamente, todas las lambdas por definición son anónimas y
; técnicamente siempre usadas anónimamente. Redundancia.
((lambda (x) x) 10) ; => 10

;;;;;;;;;;;;;;;;
; Comprensiones

; `range` o `..` genera una lista de números que comprende
; cada entero dentro de los argumentos.
(range 1 5) ; => (1 2 3 4 5)
(.. 0 2)    ; => (0 1 2)

; `map` aplica su primer argumento (que debe ser una función)
; al siguiente argumento (que es una lista).
(map (-> (x) (+ x 1)) (1 2 3)) ; => (2 3 4)

; Reducir
(reduce + (.. 1 5))
; equivale a
((+ (+ (+ 1 2) 3) 4) 5)

; Nota: map y reduce no tienen atajos.

; `slice` o `\` es idéntico a la función .slice() de JavaScript
; Pero toma la lista del primer argumento, no del último.
(slice (.. 1 5) 2) ; => (3 4 5)
(\ (.. 0 100) -5) ; => (96 97 98 99 100)

; `append` o `<<` se explica solo.
(append 4 (1 2 3)) ; => (1 2 3 4)
(<< "bar" ("foo")) ; => ("foo" "bar")

; Length se explica solo.
(length (1 2 3)) ; => 3
(_ "foobar") ; => 6

;;;;;;;;;;;;;;;
; Elementos de Haskell

; Primer elemento en una lista
(head (1 2 3)) ; => 1

; Lista del segundo elemento al último en una lista
(tail (1 2 3)) ; => (2 3)

; Último elemento en una lista
(last (1 2 3)) ; => 3

; Contrario a `tail`
(init (1 2 3)) ; => (1 2)

; Lista del primer elemento al argumento
(take 1 (1 2 3 4)) ; (1 2)

; Contrario a `take`
(drop 1 (1 2 3 4)) ; (3 4)

; Valor más pequeño de una lista
(min (1 2 3 4)) ; 1

; Valor más grande de una lista
(max (1 2 3 4)) ; 4

; Comprobar que el elemento está en la lista
(elem 1 (1 2 3)) ; true
(elem "foo" {"foo" "bar"}) ; true
(elem "bar" {"foo" "bar"}) ; false

; Invertir el orden de la lista
(reverse (1 2 3 4)) ; => (4 3 2 1)

; Comprobar si un elemento es par o impar
(even 1) ; => false
(odd 1) ; => true

; Separar string en una lista de strings, separados por espacios
(words "foobar nachos cheese") ; => ("foobar" "nachos" "cheese")
; Juntar lista de strings.
(unwords ("foo" "bar")) ; => "foobar"
(pred 21) ; => 20
(succ 20) ; => 21
```

Para más información, revisa el [repositorio](http://github.com/L8D/whip)
