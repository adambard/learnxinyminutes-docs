---
language: clojure
filename: learnclojure-es.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Antonio Hernández Blas", "https://twitter.com/nihilipster"]
    - ["Guillermo Vayá Pérez", "http://willyfrog.es"]
lang: es-es
---

Clojure es un lenguaje de la familia Lisp desarrollado sobre la Máquina Virtual
de Java. Tiene un énfasis mayor en la [programación funcional](https://es.wikipedia.org/wiki/Programación_funcional) pura
que Common Lisp, pero incluyendo la posibilidad de usar [SMT](https://es.wikipedia.org/wiki/Memoria_transacional) para manipular
el estado según se presente.

Esta combinación le permite gestionar la concurrencia de manera muy sencilla
y a menudo automáticamente.

(Necesitas la versión de Clojure 1.2 o posterior)


```clojure
; Los comentatios comienzan con punto y coma.

; Clojure se escribe mediante "forms" (patrones), los cuales son
; listas de objectos entre paréntesis, separados por espacios en blanco.

; El "reader" (lector) de Clojure asume que el primer objeto es una
; función o una macro que se va a llamar, y que el resto son argumentos.

; El primer form en un archivo debe ser ns, para establecer el namespace (espacio de
; nombres)
(ns learnclojure)

; Algunos ejemplos básicos:

; str crea una cadena de caracteres a partir de sus argumentos
(str "Hello" " " "World") ; => "Hello World"

; Las operaciones matemáticas son sencillas
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; La igualdad es =
(= 1 1) ; => true
(= 2 1) ; => false

; También es necesaria la negación para las operaciones lógicas
(not true) ; => false

; Cuando se anidan Los patrones, estos funcionan de la manera esperada
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipos
;;;;;;;;;;;;;

; Clojure usa los tipos de objetos de Java para booleanos, strings (cadenas de
; caracteres) y números.
; Usa class para saber de qué tipo es.
(class 1); Los enteros son java.lang.Long por defecto
(class 1.); Los numeros en coma flotante son java.lang.Double
(class ""); Los strings van entre comillas dobles, y son
; son java.lang.String
(class false); Los Booleanos son java.lang.Boolean
(class nil); El valor "null" se escribe nil

; Si quieres crear una lista de datos, precedela con una comilla
; simple para evitar su evaluación
'(+ 1 2) ; => (+ 1 2)
; (que es una abreviatura de (quote (+ 1 2)) )

; Puedes evaluar una lista precedida por comilla  con eval
(eval '(+ 1 2)) ; => 3

; Colecciones & Secuencias
;;;;;;;;;;;;;;;;;;;

; Las Listas están basadas en las listas enlazadas, mientras que los Vectores en
; arrays.
; ¡Los Vectores y las Listas también son clases de Java!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Una lista podría ser escrita como (1 2 3), pero debemos ponerle una
; comilla simple delante para evitar que el reader piense que es una función.
; Además, (list 1 2 3) es lo mismo que '(1 2 3)

; Las "Colecciones" son solo grupos de datos
; Tanto las listas como los vectores son colecciones:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; Las "Secuencias" (seqs) son descripciones abstractas de listas de datos.
; Solo las listas son seqs.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Una seq solo necesita proporcionar una entrada cuando es accedida.
; Así que, las seqs pueden ser perezosas -- pueden establecer series infinitas:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (una serie infinita)
(take 4 (range)) ;  (0 1 2 3)

; Usa cons para agregar un elemento al inicio de una lista o vector
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; conj agregará un elemento a una colección en la forma más eficiente.
; Para listas, se añade al inicio. Para vectores, al final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Usa concat para concatenar listas o vectores
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Usa filter y map para actuar sobre colecciones
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Usa reduce para combinar sus elementos
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; reduce puede tener un argumento indicando su valor inicial.
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Funciones
;;;;;;;;;;;;;;;;;;;;;

; Usa fn para crear nuevas funciones. Una función siempre devuelve
; su última expresión
(fn [] "Hello World") ; => fn

; (Necesitas rodearlo con paréntesis para invocarla)
((fn [] "Hello World")) ; => "Hello World"

; Puedes crear una var (variable) mediante def
(def x 1)
x ; => 1

; Asigna una función a una var
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Puedes defn como atajo para lo anterior
(defn hello-world [] "Hello World")

; El [] es el vector de argumentos de la función.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Otra abreviatura para crear funciones es:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Puedes tener funciones multi-variadic: funciones con un numero variable de
; argumentos
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Las funciones pueden usar argumentos extras dentro de un seq utilizable en la función
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; Y puedes mezclarlos con el resto de argumentos declarados de la función.
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; Mapas
;;;;;;;;;;

; Mapas de Hash y mapas de arrays comparten una misma interfaz. Los mapas de Hash
; tienen búsquedas más rápidas pero no mantienen el orden de las claves.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Los mapas de arrays se convertidos en mapas de Hash en la mayoría de
; operaciones si crecen mucho, por lo que no debes preocuparte.

; Los mapas pueden usar cualquier tipo para sus claves, pero generalmente las
; keywords (palabras clave) son lo habitual.
; Las keywords son parecidas a cadenas de caracteres con algunas ventajas de eficiencia
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Por cierto, las comas son equivalentes a espacios en blanco y no hacen
; nada.

; Recupera un valor de un mapa tratandolo como una función
(stringmap "a") ; => 1
(keymap :a) ; => 1

; ¡Las keywords pueden ser usadas para recuperar su valor del mapa, también!
(:b keymap) ; => 2

; No lo intentes con strings.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Si preguntamos por una clave que no existe nos devuelve nil
(stringmap "d") ; => nil

; Usa assoc para añadir nuevas claves a los mapas de Hash
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Pero recuerda, ¡los tipos de Clojure son inmutables!
keymap ; => {:a 1, :b 2, :c 3}

; Usa dissoc para eliminar llaves
(dissoc keymap :a :b) ; => {:c 3}

; Conjuntos
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Añade un elemento con conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Elimina elementos con disj
(disj #{1 2 3} 1) ; => #{2 3}

; Comprueba su existencia usando el conjunto como una función:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Hay más funciones en el namespace clojure.sets

; Patrones útiles
;;;;;;;;;;;;;;;;;

; Las construcciones lógicas en clojure son macros, y presentan el mismo aspecto
; que el resto de forms.
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Usa let para crear un binding (asociación) temporal
(let [a 1 b 2]
  (> a b)) ; => false

; Agrupa expresiones mediante do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Las funciones tienen implicita la llamada a do
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; Y el let también
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Módulos
;;;;;;;;;;;;;;;

; Usa use para obtener todas las funciones del módulo
(use 'clojure.set)

; Ahora podemos usar más operaciones de conjuntos
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Puedes escoger un subgrupo de funciones a importar, también
(use '[clojure.set :only [intersection]])

; Usa require para importar un módulo
(require 'clojure.string)

; Usa / para llamar a las funciones de un módulo
; Aquí, el módulo es clojure.string y la función es blank?
(clojure.string/blank? "") ; => true

; Puedes asignarle una abreviatura a un modulo al importarlo
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" es una expresión regular)

; Puedes usar require (y use, pero no lo hagas) desde un espacio de nombre
; usando :require,
; No necesitas preceder con comilla simple tus módulos si lo haces de esta
; forma.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java tiene una enorme librería estándar, por lo que resulta util
; aprender como interactuar con ella.

; Usa import para cargar un módulo de java
(import java.util.Date)

; Puedes importar desde un ns también.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Usa el nombre de la clase con un "." al final para crear una nueva instancia
(Date.) ; <un objeto Date>

; Usa "." para llamar a métodos o usa el atajo ".método"
(. (Date.) getTime) ; <un timestamp>
(.getTime (Date.)) ; exactamente la misma cosa

; Usa / para llamar métodos estáticos.
(System/currentTimeMillis) ; <un timestamp> (System siempre está presente)

; Usa doto para hacer frente al uso de clases (mutables) más tolerable
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory es un mecanismo que usa clojure para gestionar
; el estado persistente. Hay unas cuantas construcciones en clojure que
; hacen uso de este mecanismo.

; Un atom es el más sencillo. Se le da un valor inicial
(def my-atom (atom {}))

; Actualiza un atom con swap!
; swap! toma una función y la llama con el valor actual del atom
; como su primer argumento, y cualquier argumento restante como el segundo
(swap! my-atom assoc :a 1) ; Establece my-atom al resultado de (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Establece my-atom al resultado de (assoc {:a 1} :b 2)

; Usa '@' para no referenciar al atom sino para obtener su valor
my-atom  ;=> Atom<#...> (Regresa el objeto Atom)
@my-atom ; => {:a 1 :b 2}

; Un sencillo contador usando un atom sería
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Otros forms que utilizan STM son refs y agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
### Lectura adicional

Ésto queda lejos de ser exhaustivo, pero espero que sea suficiente para que puedas empezar tu camino.

Clojure.org tiene muchos artículos:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org contiene documentación con ejemplos para la mayoría de
funciones principales (pertenecientes al core):
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure es una genial forma de mejorar tus habilidades con clojure/FP:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (sí, de verdad) tiene un buen número de artículos con los que iniciarse en Clojure:
[http://clojure-doc.org/](http://clojure-doc.org/)
