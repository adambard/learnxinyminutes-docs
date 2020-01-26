---
language: hy
filename: learnhy-es.hy
contributors:
    - ["Abhishek L", "http://twitter.com/abhishekl"]
translators:
    - ["Roberto R", "https://github.com/rrodriguze"]
lang: es-es
---

Hy es un lenguaje de Lisp escrito sobre Python. Esto es posible convirtiendo
código Hy en un árbol abstracto de Python (ast). Por lo que, esto permite a
Hy llamar a código Pyhton nativo y viceversa.

Este tutorial funciona para hy >= 0.9.12

```clojure
;; Esto es una intrucción muy básica a Hy, como la del siguiente enlace
;; http://try-hy.appspot.com
;;
; Comentarios usando punto y coma, como en otros LISPS

;; Nociones básicas de expresiones
; Los programas List están hechos de expresiones simbólicas como la siguiente
(some-function args)
; ahora el esencial "Hola Mundo"
(print "hello world")

;; Tipos de datos simples
; Todos los tipos de datos simples son exactamente semejantes a sus homólogos
; en python
42 ; => 42
3.14 ; => 3.14
True ; => True
4+10j ; => (4+10j) un número complejo

; Vamos a comenzar con un poco de arimética simple
(+ 4 1) ;=> 5
; el operador es aplicado a todos los argumentos, como en otros lisps
(+ 4 1 2 3) ;=> 10
(- 2 1) ;=> 1
(* 4 2) ;=> 8
(/ 4 1) ;=> 4
(% 4 2) ;=> 0 o operador módulo
; la exponenciación es representada por el operador ** como python
(** 3 2) ;=> 9
; las funciones anidadas funcionan como lo esperado
(+ 2 (* 4 2)) ;=> 10
; también los operadores lógicos igual o no igual se comportan como se espera
(= 5 4) ;=> False
(not (= 5 4)) ;=> True

;; variables
; las variables se configuran usando SETV, los nombres de las variables pueden
; usar utf-8, excepto for ()[]{}",'`;#|
(setv a 42)
(setv π 3.14159)
(def *foo* 42)
;; otros tipos de datos de almacenamiento
; strings, lists, tuples & dicts
; estos son exactamente los mismos tipos de almacenamiento en python
"hello world" ;=> "hello world"
; las operaciones de cadena funcionan de manera similar en python
(+ "hello " "world") ;=> "hello world"
; Las listas se crean usando [], la indexación comienza en 0
(setv mylist [1 2 3 4])
; las tuplas son estructuras de datos inmutables
(setv mytuple (, 1 2))
; los diccionarios son pares de valor-clave
(setv dict1 {"key1" 42 "key2" 21})
; :nombre se puede usar para definir palabras clave en Hy que se pueden usar para claves
(setv dict2 {:key1 41 :key2 20})
; usar 'get' para obtener un elemento en un índice/key
(get mylist 1) ;=> 2
(get dict1 "key1") ;=> 42
; Alternativamente, si se usan palabras clave que podrían llamarse directamente
(:key1 dict2) ;=> 41

;; funciones y otras estructuras de programa
; las funciones son definidas usando defn, o el último sexp se devuelve por defecto
(defn greet [name]
  "A simple greeting" ; un docstring opcional
  (print "hello " name))

(greet "bilbo") ;=> "hello bilbo"

; las funciones pueden tener argumentos opcionales, así como argumentos-clave
(defn foolists [arg1 &optional [arg2 2]]
  [arg1 arg2])

(foolists 3) ;=> [3 2]
(foolists 10 3) ;=> [10 3]

; las funciones anonimas son creadas usando constructores 'fn' y 'lambda'
; que son similares a 'defn'
(map (fn [x] (* x x)) [1 2 3 4]) ;=> [1 4 9 16]

;; operaciones de secuencia
; hy tiene algunas utilidades incluidas para operaciones de secuencia, etc.
; recuperar el primer elemento usando 'first' o 'car'
(setv mylist [1 2 3 4])
(setv mydict {"a" 1 "b" 2})
(first mylist) ;=> 1

; corte listas usando 'slice'
(slice mylist 1 3) ;=> [2 3]

; obtener elementos de una lista o dict usando 'get'
(get mylist 1) ;=> 2
(get mydict "b") ;=> 2
; la lista de indexación comienza a partir de 0, igual que en python
; assoc puede definir elementos clave/índice
(assoc mylist 2 10) ; crear mylist [1 2 10 4]
(assoc mydict "c" 3) ; crear mydict {"a" 1 "b" 2 "c" 3}
; hay muchas otras funciones que hacen que trabajar con secuencias sea 
; entretenido

;; Python interop
;; los import funcionan exactamente como en python
(import datetime)
(import [functools [partial reduce]]) ; importa fun1 e fun2 del module1
(import [matplotlib.pyplot :as plt]) ; haciendo una importación en foo como en bar
; todos los métodos de python incluídos etc. son accesibles desde hy
; a.foo(arg) is called as (.foo a arg)
(.split (.strip "hello world  ")) ;=> ["hello" "world"]

;; Condicionales
; (if condition (body-if-true) (body-if-false)
(if (= passcode "moria")
  (print "welcome")
  (print "Speak friend, and Enter!"))

; anidar múltiples cláusulas 'if else if' con condiciones
(cond
 [(= someval 42)
  (print "Life, universe and everything else!")]
 [(> someval 42)
  (print "val too large")]
 [(< someval 42)
  (print "val too small")])

; declaraciones de grupo con 'do', son ejecutadas secuencialmente
; formas como defn tienen un 'do' implícito
(do
 (setv someval 10)
 (print "someval is set to " someval)) ;=> 10

; crear enlaces léxicos con 'let', todas las variables definidas de esta manera
; tienen alcance local
(let [[nemesis {"superman" "lex luther"
                "sherlock" "moriarty"
                "seinfeld" "newman"}]]
  (for [(, h v) (.items nemesis)]
    (print (.format "{0}'s nemesis was {1}" h v))))

;; clases
; las clases son definidas de la siguiente manera
(defclass Wizard [object]
  [[--init-- (fn [self spell]
             (setv self.spell spell) ; init the attr magic
             None)]
   [get-spell (fn [self]
              self.spell)]])

;; acesse hylang.org
```

### Otras lecturas

Este tutorial apenas es una introducción básica para hy/lisp/python.

Docs Hy: [http://hy.readthedocs.org](http://hy.readthedocs.org)

Repo Hy en GitHub: [http://github.com/hylang/hy](http://github.com/hylang/hy)

Acceso a freenode irc con #hy, hashtag en twitter: #hylang
