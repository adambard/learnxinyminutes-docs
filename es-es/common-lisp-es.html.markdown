---

language: "Common Lisp"
filename: commonlisp-es.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
  - ["Rommel Martinez", "https://ebzzry.io"]
translators:
  - ["ivanchoff", "https://github.com/ivanchoff"]
  - ["Andre Polykanine", "https://github.com/Menelion"]
lang: es-es
---

Common Lisp es un lenguaje de proposito general y multiparadigma adecuado para una amplia variedad
de aplicaciones en la industria. Es frecuentemente referenciado como un lenguaje de programación
programable.

EL punto de inicio clásico es [Practical Common Lisp](http://www.gigamonkeys.com/book/). Otro libro
popular y reciente es [Land of Lisp](http://landoflisp.com/). Un nuevo libro acerca de las mejores
prácticas, [Common Lisp Recipes](http://weitz.de/cl-recipes/), fue publicado recientemente.

```lisp

;;;-----------------------------------------------------------------------------
;;; 0. Sintaxis
;;;-----------------------------------------------------------------------------

;;; Forma general

;;; CL tiene dos piezas fundamentales en su sintaxis: ATOM y S-EXPRESSION.
;;; Típicamente, S-expressions agrupadas son llamadas `forms`.

10            ; un atom; se evalua a sí mismo
:thing        ; otro atom; evaluando el símbolo :thing
t             ; otro atom, denotando true
(+ 1 2 3 4)   ; una s-expression
'(4 :foo t)   ; otra s-expression


;;; Comentarios

;;; comentarios de una sola línea empiezan con punto y coma; usa cuatro para
;;; comentarios a nivel de archivo, tres para descripciones de sesiones, dos
;;; adentro de definiciones, y una para líneas simples. Por ejemplo,

;;;; life.lisp

;;; Foo bar baz, porque quu quux. Optimizado para máximo krakaboom y umph.
;;; Requerido por la función LINULUKO.

(defun sentido (vida)
  "Retorna el sentido de la vida calculado"
  (let ((meh "abc"))
    ;; llama krakaboom
    (loop :for x :across meh
       :collect x)))                    ; guarda valores en x, luego lo retorna

;;; Comentarios de bloques, por otro lado, permiten comentarios de forma libre. estos son
;;; delimitados con #| y |#

#| Este es un comentario de bloque el cual
   puede abarcar multiples líneas y
    #|
       estos pueden ser anidados
    |#
|#


;;; Entorno

;;; Existe una variedad de implementaciones; La mayoría son conformes a los estándares. SBCL
;;; es un buen punto de inicio. Bibliotecas de terceros pueden instalarse fácilmente con
;;; Quicklisp

;;; CL es usualmente desarrollado y un bucle de Lectura-Evaluación-Impresión (REPL), corriendo
;;; al mismo tiempo. El REPL permite la exploración interactiva del programa mientras este esta
;;; corriendo


;;;-----------------------------------------------------------------------------
;;; 1. Operadores y tipos de datos primitivos
;;;-----------------------------------------------------------------------------

;;; Símbolos

'foo ; => FOO  Note que el símbolo es pasado a mayúsculas automáticamente.

;;; INTERN manualmente crea un símbolo a partir de una cadena.

(intern "AAAA")        ; => AAAA
(intern "aaa")         ; => |aaa|

;;; Números

9999999999999999999999 ; enteros
#b111                  ; binario=> 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; simple
3.14159d0              ; double
1/2                    ; proporciones
#C(1 2)                ; números complejos

;;; las funciones son escritas como (f x y z ...) donde f es una función y
;;; x, y, z, ... son los argumentos.

(+ 1 2)                ; => 3

;;; Si deseas crear datos literales use QUOTE para prevenir que estos sean evaluados

(quote (+ 1 2))        ; => (+ 1 2)
(quote a)              ; => A

;;; La notación abreviada para QUOTE es '

'(+ 1 2)               ; => (+ 1 2)
'a                     ; => A

;;; Operaciones aritméticas básicas

(+ 1 1)                ; => 2
(- 8 1)                ; => 7
(* 10 2)               ; => 20
(expt 2 3)             ; => 8
(mod 5 2)              ; => 1
(/ 35 5)               ; => 7
(/ 1 3)                ; => 1/3
(+ #C(1 2) #C(6 -4))   ; => #C(7 -2)

;;; Boleanos

t                      ; true; cualquier valor non-NIL es true
nil                    ; false; también, la lista vacia: ()
(not nil)              ; => T
(and 0 t)              ; => T
(or 0 nil)             ; => 0

;;; Caracteres

#\A                    ; => #\A
#\λ                    ; => #\GREEK_SMALL_LETTER_LAMDA
#\u03BB                ; => #\GREEK_SMALL_LETTER_LAMDA

;;; Cadenas son arreglos de caracteres de longitud fija

"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; la barra invertida es un carácter de escape

;;; Las cadenas pueden ser concatenadas

(concatenate 'string "Hello, " "world!") ; => "Hello, world!"

;;; Una cadena puede ser tratada como una secuencia de caracteres

(elt "Apple" 0) ; => #\A

;;; FORMAT es usado para crear salidas formateadas, va desde simple interpolación de cadenas
;;; hasta bucles y condicionales. El primer argumento de FORMAT determina donde irá la cadena
;;; formateada. Si este es NIL, FORMAT simplemente retorna la cadena formateada como un valor;
;;; si es T, FORMAT imprime a la salida estándar, usualmente la pantalla, luego este retorna NIL.

(format nil "~A, ~A!" "Hello" "world")   ; => "Hello, world!"
(format t "~A, ~A!" "Hello" "world")     ; => NIL


;;;-----------------------------------------------------------------------------
;;; 2. Variables
;;;-----------------------------------------------------------------------------

;;; Puedes crear una variable global (ámbito dinámico) usando DEFVAR y DEFPARAMETER
;;; el nombre de la variable puede usar cualquier carácter excepto: ()",'`;#|\

;;; La diferencia entre DEFVAR y DEFPARAMETER es que reevaluando una expresión
;;; DEFVAR no cambia el valor de la variable. DEFPARAMETER, por otro lado sí lo hace.

;;; Por convención, variables de ámbito dinámico tienen "orejeras" en sus nombres.

(defparameter *some-var* 5)
*some-var* ; => 5

;;; Puedes usar también caracteres unicode.
(defparameter *AΛB* nil)

;;; Accediendo a una variable sin asignar tienen como resultado el error
;;; UNBOUND-VARIABLE, sin embargo este es el comportamiento definido. no lo hagas

;;; puedes crear enlaces locales con LET. en el siguiente código, `me` es asignado 
;;; con "dance with you" solo dentro de  (let ...). LET siempre retorna  el valor 
;;; del último `form`.

(let ((me "dance with you")) me) ; => "dance with you"


;;;-----------------------------------------------------------------------------;
;;; 3. Estructuras y colecciones
;;;-----------------------------------------------------------------------------;


;;; Estructuras

(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover*            ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)
(dog-p *rover*)    ; => T
(dog-name *rover*) ; => "rover"

;;; DOG-P, MAKE-DOG, y DOG-NAME son creados automáticamente por DEFSTRUCT


;;; Pares

;;; CONS crea pares. CAR y CDR retornan la cabeza y la cola de un CONS-pair

(cons 'SUBJECT 'VERB)         ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB))   ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB))   ; => VERB


;;; Listas

;;; Listas  son estructuras de datos de listas enlazadas, hechas de pares CONS y terminan con un
;;; NIL (o '()) para marcar el final de la lista

(cons 1 (cons 2 (cons 3 nil)))     ; => '(1 2 3)

;;; LIST es una forma conveniente de crear listas

(list 1 2 3)                       ; => '(1 2 3)

;;; Cuando el primer argumento de CONS es un atom y el segundo argumento es una lista,
;;; CONS retorna un nuevo par CONS con el primer argumento como el primer elemento y el
;;; segundo argumento como el resto del par CONS

(cons 4 '(1 2 3))                  ; => '(4 1 2 3)

;;; Use APPEND para unir listas

(append '(1 2) '(3 4))             ; => '(1 2 3 4)

;;; o CONCATENATE

(concatenate 'list '(1 2) '(3 4))  ; => '(1 2 3 4)

;;; las listas son un tipo de datos centrales en CL, por lo tanto hay una gran variedad
;;; de funcionalidades para ellas, algunos ejemplos son:

(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => NIL
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; Vectores

;;; Vectores literales son arreglos de longitud fija

#(1 2 3) ; => #(1 2 3)

;;; Use CONCATENATE para juntar vectores

(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)


;;; Arreglos

;;; Vectores y cadenas son casos especiales de arreglos.

;;; Arreglos bidimensionales

(make-array (list 2 2))         ; => #2A((0 0) (0 0))
(make-array '(2 2))             ; => #2A((0 0) (0 0))
(make-array (list 2 2 2))       ; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;;; Precaución: los valores iniciales por defecto de MAKE-ARRAY son implementaciones definidas
;;; para definirlos explícitamente:

(make-array '(2) :initial-element 'unset)  ; => #(UNSET UNSET)

;;; Para acceder al elemento en 1, 1, 1:

(aref (make-array (list 2 2 2)) 1 1 1)     ;  => 0

;;; Este valor es definido por implementación:
;;; NIL en ECL, 0 en SBCL and CCL.

;;; vectores ajustables

;;; los vectores ajustables tienen la misma representación en la impresión como los vectores literales
;;; de longitud fija.

(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
                                   :adjustable t :fill-pointer t))
*adjvec* ; => #(1 2 3)

;;; Agregando nuevos elementos

(vector-push-extend 4 *adjvec*)   ; => 3
*adjvec*                          ; => #(1 2 3 4)


;;; Conjuntos, ingenuamente son listas:

(set-difference '(1 2 3 4) '(4 5 6 7))   ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7))     ; => 4
(union '(1 2 3 4) '(4 5 6 7))            ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))                    ; => (1 2 3 4)

;;; Sin embargo, necesitarás una mejor estructura de datos que listas enlazadas
;;; cuando trabajes con conjuntos de datos grandes

;;; Los Diccionarios son implementados como tablas hash.

;;; Crear tablas hash

(defparameter *m* (make-hash-table))

;;; definir valor

(setf (gethash 'a *m*) 1)

;;; obtener valor

(gethash 'a *m*) ; => 1, T

;;; las expresiones en CL tienen la facultad de retornar multiples valores.

(values 1 2) ; => 1, 2

;;; los cuales pueden ser asignados con MULTIPLE-VALUE-BIND

(multiple-value-bind (x y)
    (values 1 2)
  (list y x))

; => '(2 1)

;;; GETHASH es un ejemplo de una función que retorna multiples valores. El primer
;;; valor es el valor de la llave en la tabla hash: si la llave no existe retorna NIL.

;;; El segundo valor determina si la llave existe en la tabla hash. si la llave no existe
;;; en la tabla hash retorna NIL. Este comportamiento permite verificar si el valor de una
;;; llave es actualmente NIL.

;;; Obteniendo un valor no existente retorna NIL

(gethash 'd *m*) ;=> NIL, NIL

;;; Puedes declarar un valor por defecto para las llaves inexistentes

(gethash 'd *m* :not-found) ; => :NOT-FOUND

;;; Vamos a manejar los multiples valores de retornno en el código.

(multiple-value-bind (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)


;;;-----------------------------------------------------------------------------
;;; 3. Funciones
;;;-----------------------------------------------------------------------------

;;; Use LAMBDA para crear funciones anónimas. las funciones siempre retornan el valor 
;;; de la última expresión. la representación imprimible de una función varia entre
;;; implementaciones.

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;;; Use FUNCALL para llamar funciones anónimas.

(funcall (lambda () "Hello World"))   ; => "Hello World"
(funcall #'+ 1 2 3)                   ; => 6

;;; Un llamado a FUNCALL es también realizado cuando la expresión lambda es el CAR de
;;; una lista.

((lambda () "Hello World"))           ; => "Hello World"
((lambda (val) val) "Hello World")    ; => "Hello World"

;;; FUNCALL es usado cuando los argumentos son conocidos de antemano. 
;;; de lo contrario use APPLY

(apply #'+ '(1 2 3))   ; => 6
(apply (lambda () "Hello World") nil) ; => "Hello World"

;;; Para nombrar una funcion use  DEFUN

(defun hello-world () "Hello World")
(hello-world) ; => "Hello World"

;;; Los () en la definición anterior son la lista de argumentos

(defun hello (name) (format nil "Hello, ~A" name))
(hello "Steve") ; => "Hello, Steve"

;;; las functiones pueden tener argumentos opcionales; por defecto son NIL

(defun hello (name &optional from)
  (if from
      (format t "Hello, ~A, from ~A" name from)
      (format t "Hello, ~A" name)))

(hello "Jim" "Alpacas")       ; => Hello, Jim, from Alpacas

;;; Los valores por defecto pueden ser especificados


(defun hello (name &optional (from "The world"))
   (format nil "Hello, ~A, from ~A" name from))

(hello "Steve")               ; => Hello, Steve, from The world
(hello "Steve" "the alpacas") ; => Hello, Steve, from the alpacas

;;; Las funciones también tienen argumentos llaves para permitir argumentos no positionados

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format t "Hello, ~A ~A, from ~A" honorific name from))

(generalized-greeter "Jim")
; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer


;;;-----------------------------------------------------------------------------
;;; 4. Igualdad
;;;-----------------------------------------------------------------------------

;;; CL tiene un sistema sofisticado de igualdad. Una parte es tratada aquí.

;;; Para números use `=`
(= 3 3.0)               ; => T
(= 2 1)                 ; => NIL

;;; Para identidad de objetos (aproximadamente) use EQL
(eql 3 3)               ; => T
(eql 3 3.0)             ; => NIL
(eql (list 3) (list 3)) ; => NIL

;;; para listas, cadenas y bit vectores use EQUAL
(equal (list 'a 'b) (list 'a 'b)) ; => T
(equal (list 'a 'b) (list 'b 'a)) ; => NIL


;;;-----------------------------------------------------------------------------
;;; 5. Control de flujo
;;;-----------------------------------------------------------------------------

;;; Condicionales

(if t                ; testar expresión
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;;; En condicionales, todo valor non-NIL es tratado como true

(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;;; COND en cadena una serie de pruebas para seleccionar un resultado
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;;; TYPECASE evalua sobre el tipo del valor
(typecase 1
  (string :string)
  (integer :int))
; => :int


;;; Bucles

;;; Recursión

(defun fact (n)
  (if (< n 2)
      1
    (* n (fact(- n 1)))))

(fact 5) ; => 120

;;; Iteración

(defun fact (n)
  (loop :for result = 1 :then (* result i)
     :for i :from 2 :to n
     :finally (return result)))

(fact 5) ; => 120

(loop :for x :across "abcd" :collect x)
; => (#\a #\b #\c #\d)

(dolist (i '(1 2 3 4))
  (format t "~A" i))
; => 1234


;;;-----------------------------------------------------------------------------
;;; 6. Mutación
;;;-----------------------------------------------------------------------------

;;; use SETF para asignar un valor nuevo a una variable existente. Esto fue demostrado
;;; previamente en el ejemplo de la tabla hash.

(let ((variable 10))
    (setf variable 2))
; => 2

;;; Un estilo bueno de lisp es minimizar el uso de funciones destructivas y prevenir
;;; la mutación cuando sea posible.


;;;-----------------------------------------------------------------------------
;;; 7. Clases y objetos
;;;-----------------------------------------------------------------------------

;;; No más clases de animales, tengamos transportes mecánicos impulsados por el humano

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;;; Los argumentos de  DEFCLASS, en orden son:
;;; 1. nombre de la clase
;;; 2. lista de superclases
;;; 3. slot list
;;; 4. Especificadores opcionales

;;; cuando no hay lista de superclase, la lista vacia indica clase de 
;;; objeto estándar, esto puede ser cambiado, pero no mientras no sepas
;;; lo que estas haciendo. revisar el arte del protocolo de meta-objetos
;;; para más información.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

;;; Invocando DESCRIBE en la clase  HUMAN-POWERED-CONVEYANCE en REPL obtenemos:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;;; Tenga en cuenta el comportamiento reflexivo disponible. CL fue diseñado
;;; para ser un systema interactivo

;;; para definir un método, encontremos la circunferencia de la rueda usando
;;; la ecuación  C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;;; PI es definido internamente en CL

;;; Supongamos que descubrimos que el valor de eficiencia del número de remeros
;;; en una canoa es aproximadamente logarítmico. Esto probablemente debería
;;; establecerse en el constructor / inicializador.

;;; Para inicializar su instancia después de que CL termine de construirla:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;;; luego para construir una instancia y revisar la eficiencia promedio

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887


;;;-----------------------------------------------------------------------------
;;; 8. Macros
;;;-----------------------------------------------------------------------------

;;; las Macros le permiten extender la sintaxis del lenguaje, CL no viene con
;;; un bucle WHILE, por lo tanto es facil escribirlo, Si obedecemos nuestros
;;; instintos de ensamblador, terminamos con:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        `(tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))

;;; revisemos la versión de alto nivel para esto:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;;; Sin embargo, con un compilador moderno, esto no es necesario; El LOOP se 
;;; compila igualmente bien y es más fácil de leer.

;;; Tenga en cuenta que se utiliza ```, así como `,` y `@`.  ``` es un operador
;;; de tipo de cita conocido como quasiquote; permite el uso de `,` . `,` permite
;;; variables "entre comillas". @ interpola las listas.

;;; GENSYM crea un símbolo único que garantiza que no existe en ninguna otra parte
;;; del sistema. Esto se debe a que las macros se expanden en el momento de la compilación
;;; y las variables declaradas en la macro pueden colisionar con las variables utilizadas
;;; en un código regular.

;;; Consulte Practical Common Lisp y On Lisp para obtener más información sobre macros.
```


## Otras Lecturas

- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/book.pdf)


## Información extra

- [CLiki](http://www.cliki.net/)
- [common-lisp.net](https://common-lisp.net/)
- [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl)
- [Lisp Lang](http://lisp-lang.org/)


## Creditos

Muchas Gracias a la gente de Scheme por proveer un gran punto de inicio
el cual puede ser movido fácilmente a Common Lisp

- [Paul Khuong](https://github.com/pkhuong) para un buen repaso.
