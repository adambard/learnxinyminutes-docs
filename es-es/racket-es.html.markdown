---
language: racket
filename: learnracket-es.rkt
contributors:
  - ["th3rac25", "https://github.com/voila"]
  - ["Eli Barzilay", "https://github.com/elibarzilay"]
  - ["Gustavo Schmidt", "https://github.com/gustavoschmidt"]
  - ["Duong H. Nguyen", "https://github.com/cmpitg"]
  - ["Keyan Zhang", "https://github.com/keyanzhang"]
translators:
    - ["Carlos Roman", "https://github.com/carlochess"]
lang: es-es
---
Racket es un lenguaje de propósito general, multiparadigma que hace parte de la familia Lisp/Scheme.

```racket
#lang racket ; Define el lenguaje que usas

;;; Comentarios

;; Los comentarios de una sola línea inician con un punto y coma

#| Un bloque de comentarios
   puede distribuirse en varias líneas...
    #|
       ¡Incluso puede estar anidado!
    |#
|#

;; Los comentarios descartan la siguiente expresión,
;; pero son útiles para comentar expresiones al momento de depurar el código
#; (Esta expresión es descartada)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Tipos de datos primitivos y operadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numeros
9999999999999999999999 ; Enteros
#b111                  ; binario => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14                   ; reales
6.02e+23
1/2                    ; racionaless
1+2i                   ; numeros complejos

;; La aplicación de funciones es escrita de la siguiente forma: (f x y z ...)
;; donde f es una función y “x, y, z” son sus operandos 
;; Si quieres crear una lista de literales debes agregar ' al inicio
;; para que no sean evaluados
'(+ 1 2) ; => (+ 1 2)
;; Ahora algunas operaciones aritméticas
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(expt 2 3) ; => 8
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(/ 35 5) ; => 7
(/ 1 3) ; => 1/3
(exact->inexact 1/3) ; => 0.3333333333333333
(+ 1+2i  2-3i) ; => 3-1i

;;; Booleanos
#t ; Para verdadero (true)
#f ; Para falso (false) -- cualquier valor distinto de #f es verdadero
(not #t) ; => #f
(and 0 #f (error "No entra aquí")) ; => #f
(or #f 0 (error "No entra aquí"))  ; => 0

;;; Caracteres
#\A ; => #\A
#\λ ; => #\λ
#\u03BB ; => #\λ

;;; Los Strings tienen una longitud fija
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; backslash es un caracter de escape
"Foo\tbar\41\x21\u0021\a\r\n" ; incluye escape para C, Unicode
"λx:(μα.α→α).xx"              ; Puedes incluir caracteres Unicode

;; ¡Los tipos de dato Strings pueden unirse tambien!
(string-append "Hello " "world!") ; => "Hello world!"

;; Un string puede ser tratado como una lista de caracteres
(string-ref "Apple" 0) ; => #\A

;; la función format puede usarse para darle formato a un string:
(format "~a can be ~a" "strings" "formatted")

;; Imprimir en consola es muy simple
(printf "I'm Racket. Nice to meet you!\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Puedes crear una variable usando define
;; el nombre de una variable puede contener cualquier nombre excepto: ()[]{}",'`;#|\
(define some-var 5)
some-var ; => 5

;; También puedes usar caracteres unicode
(define ⊆ subset?)
(⊆ (set 3 2) (set 1 2 3)) ; => #t

;; Acceder a una variable no definida con anterioridad resulta en una excepción
; x ; => x: undefined ...

;; Local binding: La variable 'me' esta limitada a tomar el valor "Bob" dentro del ambiente (let ...)
(let ([me "Bob"])
  "Alice"
  me) ; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Estructuras y colecciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Estructuras
(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;;; Parejas (Inmutables)
;; 'cons' construye parejas, 'car' y 'cdr' extraen el primer
;; y segundo elemento respectivamente de una pareja
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;;; Listas

;; Las Listas son estructuras secuenciales no indexadas, hechas con ‘cons’ y
;; con un 'null' (o '()) para denotar el final de la lista
(cons 1 (cons 2 (cons 3 null))) ; => '(1 2 3)
;; 'list' es otro constructor apropiado para las listas
(list 1 2 3) ; => '(1 2 3)
;; y el simbolo comilla (') puede ser usado en una lista de valores literales
'(1 2 3) ; => '(1 2 3)

;; Aquí aun se puede usar 'cons' para agregar un elemento al comienzo de la lista
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; El uso de 'append' para unir un par de listas
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; Las listas son un tipo de dato básico, por lo cual proveen numerosas funcionalidades;
;; algunos ejemplos son:
(map add1 '(1 2 3))          ; => '(2 3 4)
(map + '(1 2 3) '(10 20 30)) ; => '(11 22 33)
(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; Vectores

;; Los Vectores son arreglos de longitud fija
#(1 2 3) ; => '#(1 2 3)

;; Se usa 'vector-append' para unir dos vectores
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Conjuntos

;; Crear un conjunto a partir de una lista
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

;; Agregar/Asignar un nuevo elemento 'set-add'
;; (Funcional: retorna un conjunto extendido en vez de una mutar la entrada)
(set-add (set 1 2 3) 4) ; => (set 1 2 3 4)

;; Remueve el elemento agregado anteriormente 'set-remove'
(set-remove (set 1 2 3) 1) ; => (set 2 3)

;; Prueba la existencia de un elemento con la funcion 'set-member?'
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; Tablas Hashs

;; Crea una tabla hash inmutable (Abajo presentamos un ejemplo)
(define m (hash 'a 1 'b 2 'c 3))

;; Conseguir un valor
(hash-ref m 'a) ; => 1

;; Conseguir un valor que no está presente es una excepción
; (hash-ref m 'd) => no value found

;; Puedes proveer un valor por defecto si el valor para la llave no se encuentra
(hash-ref m 'd 0) ; => 0

;; Usa 'hash-set' para ampliar un tabla hash “inmutable”
;; (Retorna la tabla hash extendida en vez de una mutarla)
(define m2 (hash-set m 'd 4))
m2 ; => '#hash((b . 2) (a . 1) (d . 4) (c . 3))

;; ¡Recuerde que estas tablas hash son inmutables!
m ; => '#hash((b . 2) (a . 1) (c . 3))  <-- no 'd'

;; Usa 'hash-remove' para quitar las llaves de la tabla hash (functional tambien)
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Funciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usa 'lambda' para crear funciones.
;; Una función siempre retorna el valor de su última expresión
(lambda () "Hello World") ; => #<procedure>
;; También se puede usar el caracter Unicode 'λ'
(λ () "Hello World")     ; => same function

;; Usa los paréntesis exteriores para llamar la función
((lambda () "Hello World")) ; => "Hello World"
((λ () "Hello World"))      ; => "Hello World"

;; Asigna una función a una variable
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

;; Puede acortar esto usando el azúcar sintáctico para la definición de una función:
(define (hello-world2) "Hello World")

;; El paréntesis () del ejemplo anterior denota la lista de argumentos para la función
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"
;; ... O de forma similar, usando el azúcar sintáctico para una definición:
(define (hello2 name)
  (string-append "Hello " name))

;; Puedes tener una función con parametros variables, using 'case-lambda'
(define hello3
  (case-lambda
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"
;; ... o especificar los argumentos opcionales junto con su valor por defecto
(define (hello4 [name "World"])
  (string-append "Hello " name))

;; Las funciones pueden tener argumentos extra empaquetados como una lista
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"
;; ... o sin usar el azúcar sintáctico:
(define count-args2
  (lambda args
    (format "You passed ~a args: ~a" (length args) args)))

;; Puedes combinar argumentos regulares y empaquetados
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"
;; ... Sin usar azúcar sintáctica:
(define hello-count2
  (lambda (name . args)
    (format "Hello ~a, you passed ~a extra args" name (length args))))

;; Y con keywords
(define (hello-k #:name [name "World"] #:greeting [g "Hello"] . args)
  (format "~a ~a, ~a extra args" g name (length args)))
(hello-k)                 ; => "Hello World, 0 extra args"
(hello-k 1 2 3)           ; => "Hello World, 3 extra args"
(hello-k #:greeting "Hi") ; => "Hi World, 0 extra args"
(hello-k #:name "Finn" #:greeting "Hey") ; => "Hey Finn, 0 extra args"
(hello-k 1 2 3 #:greeting "Hi" #:name "Finn" 4 5 6)
                                         ; => "Hi Finn, 6 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Comparando
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Para números usa '='
(= 3 3.0) ; => #t
(= 2 1)   ; => #f

;; 'eq?' retorna #t si 2 argumentos refieren al mismo objeto en memoria 
;; #f de lo contrario.
;; En otras palabras, es una simple comparación de punteros.
(eq? '() '()) ; => #t, Debido a que existe solo una lista vacia en memoria
(let ([x '()] [y '()])
  (eq? x y))  ; => #t, igual que arriba

(eq? (list 3) (list 3)) ; => #f
(let ([x (list 3)] [y (list 3)])
  (eq? x y))            ; => #f — ¡No es la misma lista en memoria!

(let* ([x (list 3)] [y x])
  (eq? x y)) ; => #t, debido a que ‘x’ y ‘y’ ahora apuntan a la misma posición en memoria

(eq? 'yes 'yes) ; => #t
(eq? 'yes 'no)  ; => #f

(eq? 3 3)   ; => #t — Te cuidado aqui
            ; es mejor usar '=' para comparacion de numeros.
(eq? 3 3.0) ; => #f

(eq? (expt 2 100) (expt 2 100))               ; => #f
(eq? (integer->char 955) (integer->char 955)) ; => #f

(eq? (string-append "foo" "bar") (string-append "foo" "bar")) ; => #f

;; 'eqv?' permite comparar números y caracteres..
;; for other datatypes, 'eqv?' and 'eq?' return the same result.
(eqv? 3 3.0)                                   ; => #f
(eqv? (expt 2 100) (expt 2 100))               ; => #t
(eqv? (integer->char 955) (integer->char 955)) ; => #t

(eqv? (string-append "foo" "bar") (string-append "foo" "bar"))   ; => #f

;; 'equal?' permite comparar los siguientes tipos de datos:
;; strings, byte strings, pairs, mutable pairs, vectors, boxes, 
;; hash tables, and inspectable estructuras.
;; para otros tipos de datos, 'equal?' y 'eqv?' devuelven el mismo resultado.
(equal? 3 3.0)                                                   ; => #f
(equal? (string-append "foo" "bar") (string-append "foo" "bar")) ; => #t
(equal? (list 3) (list 3))                                       ; => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Control de flujo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Condicionales

(if #t               ; expresión de prueba
    "this is true"   ; expresión si la expresión de prueba es verdadera
    "this is false") ; de lo contrario expression
; => "this is true"

;; En condicionales, todos los valores que no son #f son tratados como verdadero
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; Las expresiones 'cond' son una serie de pruebas para seleccionar el resultado
(cond [(> 2 2) (error "wrong!")]
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

;;; Coincidencia de patrones (Pattern Matching)

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;;; Ciclos

;; Los ciclos pueden expresarse a través de recursión (de cola)
(define (loop i)
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i))))
(loop 5) ; => i=5, i=6, ...

;; De igual forma, con un let
(let loop ((i 0))
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i)))) ; => i=0, i=1, ...

;; El siguiente ejemplo muestra cómo expresar un ciclo for, pero Racket tiene
;; otra forma aún más flexible de expresarlos:
(for ([i 10])
  (printf "i=~a\n" i)) ; => i=0, i=1, ...
(for ([i (in-range 5 10)])
  (printf "i=~a\n" i)) ; => i=5, i=6, ...

;;; Iterando sobre otras secuencias
;; 'for' permite iterar sobre varios tipos de secuencias:
;; lists, vectors, strings, sets, hash tables, etc...

(for ([i (in-list '(l i s t))])
  (displayln i))

(for ([i (in-vector #(v e c t o r))])
  (displayln i))

(for ([i (in-string "string")])
  (displayln i))

(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))

(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3 ))])
  (printf "key:~a value:~a\n" k v))

;;; Iteradores mas sofisticados

;; Escaneo paralelo de múltiples secuencias (se detiene en la más pequeña)
(for ([i 10] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x 1:y 2:z

;; Loops anidados
(for* ([i 2] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x, 0:y, 0:z, 1:x, 1:y, 1:z

;; Condicionales
(for ([i 1000]
      #:when (> i 5)
      #:unless (odd? i)
      #:break (> i 10))
  (printf "i=~a\n" i))
; => i=6, i=8, i=10

;;; Secuncias por compresión
;; Muy similar a los ciclos 'for' -- solo recolectando los resultados

(for/list ([i '(1 2 3)])
  (add1 i)) ; => '(2 3 4)

(for/list ([i '(1 2 3)] #:when (even? i))
  i) ; => '(2)

(for/list ([i 10] [j '(x y z)])
  (list i j)) ; => '((0 x) (1 y) (2 z))

(for/list ([i 1000] #:when (> i 5) #:unless (odd? i) #:break (> i 10))
  i) ; => '(6 8 10)

(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
; => '#hash((1 . "1") (2 . "2") (3 . "3"))

;; Existen otras formas de recolectar los valores usando otras expresiones:
(for/sum ([i 10]) (* i i)) ; => 285
(for/product ([i (in-range 1 11)]) (* i i)) ; => 13168189440000
(for/and ([i 10] [j (in-range 10 20)]) (< i j)) ; => #t
(for/or ([i 10] [j (in-range 0 20 2)]) (= i j)) ; => #t
;; Y para usar cualquier combinación arbitraria, use 'for/fold'
(for/fold ([sum 0]) ([i '(1 2 3 4)]) (+ sum i)) ; => 10
;; (Esto frecuentemente reemplaza los ciclos en los lenguajes imperativos)

;;; Excepciones

;; Para atrapar excepciones, usa las funciones 'with-handlers'
(with-handlers ([exn:fail? (lambda (exn) 999)])
  (+ 1 "2")) ; => 999
(with-handlers ([exn:break? (lambda (exn) "no time")])
  (sleep 3)
  "phew") ; => "phew", pero si usa un break => "no time"

;; Usa 'raise' para lanzar una excepción o cualquier otro valor
(with-handlers ([number?    ; atrapa valores numericos lanzados
                 identity]) ; los retorna como valores
  (+ 1 (raise 2))) ; => 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutación
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usa 'set!' para asignar un nuevo valor a una variable existente
(define n 5)
(set! n (add1 n))
n ; => 6

;; Usa boxes para valores explícitamente mutables (similar a punteros o
;; referencias en otros lenguajes)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*) ; => 6

;; Muchos tipos de datos en Racket son inmutables (pairs, lists, etc), algunos poseen
;; ambos sabores mutable e immutable (strings, vectors, hash tables,
;; etc...)

;; Usa 'vector' o 'make-vector' para crear vectores mutables
(define vec (vector 2 2 3 4))
(define wall (make-vector 100 'bottle-of-beer))
;; Usa vector-set! para actualizar una posición
(vector-set! vec 0 1)
(vector-set! wall 99 'down)
vec ; => #(1 2 3 4)

;; Crea una tabla hash vacía  y manipulata
(define m3 (make-hash))
(hash-set! m3 'a 1)
(hash-set! m3 'b 2)
(hash-set! m3 'c 3)
(hash-ref m3 'a)   ; => 1
(hash-ref m3 'd 0) ; => 0
(hash-remove! m3 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Modulos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Los Modulos permiten organizar el código en multiples archivos para reusarlos
;; en bibliotecas; Aquí usamos sub-modules, anidados en todo el modulo que
;; este texto hace (empezando desde la línea "#lang")

(module cake racket/base ; definimos un modulo llamado 'cake' basado en racket/base

  (provide print-cake) ; function exportada por el modulo

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch) ; función interna 
    (printf fmt (make-string n ch))
    (newline)))

;; Usa 'require' para obtener todos los nombre que provee un modulo
(require 'cake) ; el apostrofe ' indica que es un submódulo local
(print-cake 3)
; (show "~a" 1 #\A) ; => error, la función 'show' no fue exportada

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Clases y objectos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Crea una clase llamada fish% (-% Es un una forma de indicar los límites de la clase)
(define fish%
  (class object%
    (init size) ; inicialización del argumento
    (super-new) ; inicialización de la superclase
    ;; Campo
    (define current-size size)
    ;; Metodos públicos
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

;; Crea una instancia de la clase fish%
(define charlie
  (new fish% [size 10]))

;; Usa 'send' para llamar un método de un objeto
(send charlie get-size) ; => 10
(send charlie grow 6)
(send charlie get-size) ; => 16

;; 'fish%' is a plain "first class" value, which can get us mixins
(define (add-color c%)
  (class c%
    (init color)
    (super-new)
    (define my-color color)
    (define/public (get-color) my-color)))
(define colored-fish% (add-color fish%))
(define charlie2 (new colored-fish% [size 10] [color 'red]))
(send charlie2 get-color)
;; o, sin nombres:
(send (new (add-color fish%) [size 10] [color 'red]) get-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Las Macros te permite extender la sintaxis del lenguaje

;; Agreguemos un ciclo while
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(let ([i 0])
  (while (< i  10)
    (displayln i)
    (set! i (add1 i))))

;; Las Macros son higienicas, ¡no puedes aplastar las variables existentes!
(define-syntax-rule (swap! x y) ; -! es un caracter que indica mutación
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
;; La variable 'tmp' es renombrada a 'tmp_1'
;; Para evitar el conflicto de nombres
;; (let ([tmp_1 tmp])
;;   (set! tmp other)
;;   (set! other tmp_1))

;; Pero aun hay algunas transfromaciones de código, por ejemplo:
(define-syntax-rule (bad-while condition body ...)
  (when condition
    body ...
    (bad-while condition body ...)))
;; Esta macro es incorrecta: genera código infinitamente, si tratas de usarla
;; el compilador entrará en un ciclo infinito

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Contratos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Los Contratos imponen restricciones a los valores exportados desde los módulos

(module bank-account racket
  (provide (contract-out
            [deposit (-> positive? any)] ; Los montos siempre son positivos
            [balance (-> positive?)]))

  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount)
  )

(require 'bank-account)
(deposit 5)

(balance) ; => 5

;; El cliente intenta depositar un monto negativo por lo cual es rechazado
;; (deposit -5) ; => depósito: violación del contrato
;; expected: positive?
;; given: -5
;; more details....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Entrada y salida
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Racket tiene el concepto de "port", el cual es muy similar al de descriptores
;; de ficheros en otros lenguajes

;; Abre "/tmp/tmp.txt" y escribe "Hello World"
;; Esto lanzará un error si el archivo existe
(define out-port (open-output-file "/tmp/tmp.txt"))
(displayln "Hello World" out-port)
(close-output-port out-port)

;; Agregar información a "/tmp/tmp.txt" (incluso si el archivo existe)
(define out-port (open-output-file "/tmp/tmp.txt"
                                   #:exists 'append))
(displayln "Hola mundo" out-port)
(close-output-port out-port)

;; Lee del archivo de nuevo
(define in-port (open-input-file "/tmp/tmp.txt"))
(displayln (read-line in-port))
; => "Hello World"
(displayln (read-line in-port))
; => "Hola mundo"
(close-input-port in-port)

;; Alternativamente, haciendo uso de call-with-output-file no necesitas expresamente
;; cerrar el archivo
(call-with-output-file "/tmp/tmp.txt"
  #:exists 'update ; Rewrite the content
  (λ (out-port)
    (displayln "World Hello!" out-port)))

;; Y usar la función call-with-input-file hace lo mismo para la entrada
(call-with-input-file "/tmp/tmp.txt"
  (λ (in-port)
    (displayln (read-line in-port))))
```

## Mas información

¿Quieres saber mas? Prueba en [Empezando con Racket](http://docs.racket-lang.org/getting-started/)




