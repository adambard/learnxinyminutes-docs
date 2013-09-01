---
language: clojure
filename: learnclojure-es.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Antonio Hernández Blas", "https://twitter.com/nihilipster"]
lang: es-es
---

Clojure es un lenguaje de la familia Lisp desarrollado para la Máquina Virtual
de Java. Tiene un énfasis más fuerte en la [programación funcional](https://es.wikipedia.org/wiki/Programación_funcional) pura
que Common Lisp, pero incluye varias facilidades de [SMT](https://es.wikipedia.org/wiki/Memoria_transacional) para manipular
el estado según se presente.

Esta combinación le permite manejar el procesamiento concurrente muy simple,
y a menudo automáticamente.

(Necesitas la versión de Clojure 1.2 o nueva)


```clojure
; Los comentatios inician con punto y coma.

; Clojure es escrito en "forms" (patrones), los cuales son solo
; listas de objectos dentro de paréntesis, separados por espacios en blanco.

; El reader (lector) de Clojure asume que el primer objeto es una
; función o una macro a llamar, y que el resto son argumentos.

; La primera llamada en un archivo debe ser ns, para establecer el espacio de
; nombre
(ns learnclojure)

; Más ejemplos básicos:

; str creará una cadena de caracteres a partir de sus argumentos
(str "Hello" " " "World") ; => "Hello World"

; Las matemáticas son sencillas
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; La igualdad es =
(= 1 1) ; => true
(= 2 1) ; => false

; Necesitas de la negación para la lógica, también
(not true) ; => false

; Los patrones anidados funcionan como lo esperas
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipos
;;;;;;;;;;;;;

; Clojure usa los tipos de objetos de Java para booleanos,cadenas de
; caracteres y números.
; Usa class para inspeccionarlos.
(class 1); Los enteros literales son java.lang.Long por default
(class 1.); Los flotantes literales son java.lang.Double
(class ""); Las cadenas de caracteres van entre comillas dobles, y son
; son java.lang.String
(class false); Los Booleanos son java.lang.Boolean
(class nil); El valor "null" es llamado nil

; Si quieres crear una lista literal de datos, precede la con una comilla
; simple para evitar su evaluación
'(+ 1 2) ; => (+ 1 2)
; (abreviatura de (quote (+ 1 2))

; Puedes evaluar una lista precedida por comilla simple con eval
(eval '(+ 1 2)) ; => 3

; Colecciones & Secuencias
;;;;;;;;;;;;;;;;;;;

; Las Listas están basadas en listas enlazadas, mientras que los Vectores en
; arreglos.
; ¡Los Vectores y las Listas son clases de Java también!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Una lista podría ser escrita como (1 2 3), pero debemos precidirla con
; comilla simple para evitar que el lector piense que es una función.
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
; Para listas, se agrega al inicio. Para vectores, al final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Usa concat para concatenar listas o vectores
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Usa filter, map para actuar sobre colecciones
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Usa reduce para reducirlos
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; reduce puede tomar un argumento como valor inicial también
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Funciones
;;;;;;;;;;;;;;;;;;;;;

; Usa fn para crear nuevas funciones. Una función siempre regresa
; su última expresión
(fn [] "Hello World") ; => fn

; (Necesitas encerrarlo en paréntesis para llamarlo)
((fn [] "Hello World")) ; => "Hello World"

; Puedes crear una var (variable) usando def
(def x 1)
x ; => 1

; Asigna una función a una var
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Puedes acortar este proceso al usar defn
(defn hello-world [] "Hello World")

; El [] es el vector de argumentos para la función.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Puedes usar también esta abreviatura para crear funciones:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Puedes tener funciones multi-variadic (múltiple numero variable de
; argumentos), también
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Las funciones pueden colocar argumentos extras dentro de una seq por ti
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; Puedes mezclar argumentos regulares y dentro de una seq
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; Mapas
;;;;;;;;;;

; Mapas de Hash y mapas de Arreglos comparten una interfaz. Los mapas de Hash
; tienen búsquedas más rápidas pero no mantienen el orden de las llaves.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Los mapas de Arreglos serán convertidos en mapas de Hash en la mayoría de
; operaciones si crecen lo suficiente, así que no necesitas preocuparte.

; Los mapas pueden usar cualquier tipo para sus llaves, pero usualmente las
; keywords (llaves) son mejor.
; Las keywords son como cadenas de caracteres con algunas ventajas en eficiencia
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Por cierto, las comas son siempre tratadas como espacios en blanco y no hacen
; nada.

; Recupera un valor de un mapa tratando la como una función
(stringmap "a") ; => 1
(keymap :a) ; => 1

; ¡Las keywords pueden ser usadas para recuperar su valor del mapa, también!
(:b keymap) ; => 2

; No intentes ésto con cadenas de caracteres.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Recuperando un valor no presente regresa nil
(stringmap "d") ; => nil

; Usa assoc para agregar nuevas llaves a los mapas de Hash
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Pero recuerda, ¡los tipos de clojure son inmutables!
keymap ; => {:a 1, :b 2, :c 3}

; Usa dissoc para remover llaves
(dissoc keymap :a :b) ; => {:c 3}

; Conjuntos
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Agrega un miembro con conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Remueve uno con disj
(disj #{1 2 3} 1) ; => #{2 3}

; Comprueba la existencia tratando al conjunto como una función:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Hay más funciones en el espacio de nombre clojure.sets

; Patrones útiles
;;;;;;;;;;;;;;;;;

; Las construcciones lógicas en clojure son macros, y tienen el mismo aspecto
; que todo lo demás
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Usa let para crear una binding (asociación) temporal
(let [a 1 b 2]
  (> a b)) ; => false

; Agrupa expresiones con do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Las funciones tienen un do implicito
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; De igual forma let
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Modulos
;;;;;;;;;;;;;;;

; Usa use para obtener todas las funciones del modulo
(use 'clojure.set)

; Ahora podemos usar operaciones de conjuntos
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Puedes escoger un subgrupo de funciones a importar, también
(use '[clojure.set :only [intersection]])

; Usa require para importar un modulo
(require 'clojure.string)

; Usa / para llamar funciones de un modulo
; Aquí, el modulo es clojure.string y la función es blank?
(clojure.string/blank? "") ; => true

; Puedes asignarle una abreviatura a un modulo al importarlo
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" es una expresión regular literal)

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

; Java tiene una enorme y útil librería estándar, así que
; querrás aprender como llegar a ella.

; Usa import para cargar un modulo de java
(import java.util.Date)

; Puedes importar desde un ns también.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Usa el nombre de la clase con un "." al final para crear una nueva instancia
(Date.) ; <un objeto Date>

; Usa "." para llamar a métodos. O, usa el atajo ".método"
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

; Software Transactional Memory es un mecanismo que clojure usa para manejar
; el estado persistente. Hay algunas cuantas construcciones en clojure que
; usan esto.

; Un atom es el más simple. Dale una valor inicial
(def my-atom (atom {}))

; Actualiza un atom con swap!
; swap! toma una función y la llama con el valor actual del atom
; como su primer argumento, y cualquier argumento restante como el segundo
(swap! my-atom assoc :a 1) ; Establece my-atom al resultado de (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Establece my-atom al resultado de (assoc {:a 1} :b 2)

; Usa '@' para no referenciar al atom y obtener su valor
my-atom  ;=> Atom<#...> (Regresa el objeto Atom)
@my-atom ; => {:a 1 :b 2}

; Aquí está un simple contador usando un atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Otros constructores STM son refs y agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Lectura adicional

Ésto queda lejos de ser exhaustivo, pero espero que sea suficiente para
encaminarte.

Clojure.org tiene muchos artículos:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org tiene documentación con ejemplos para la mayoría de
funciones core:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure es una grandiosa forma de fortalecer tus habilidades con clojure/FP:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (sí, de verdad) tiene un número de artículos para empezar:
[http://clojure-doc.org/](http://clojure-doc.org/)
