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

Clojure es un lenguaje de la familia Lisp desarrollado para la Máquina Virtual
de Java. Tiene un énfasis mayor en la
[programación funcional](https://es.wikipedia.org/wiki/Programación_funcional)
pura que Common Lisp, pero incluye varias utilidades de
[SMT](https://es.wikipedia.org/wiki/Memoria_transacional) para manipular
el estado según se presente.

Esta combinación le permite gestionar el procesamiento concurrente de manera
muy sencilla, y a menudo automáticamente.

(Necesitas la versión de Clojure 1.2 o reciente)


```clojure
; Los comentarios comienzan con punto y coma.

; Clojure se escribe mediante patrones ("forms"), los cuales son
; listas de cosas entre paréntesis, separados por espacios en blanco.

; El lector ("reader") de Clojure asume que la primera cosa es una
; función o una macro a llamar, y el resto son argumentos.

; La primera llamada en un archivo debe ser ns, para establecer el espacio de
; nombres ("namespace")
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

; Los patrones anidados funcionan como esperas
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipos
;;;;;;;;;;;;;

; Clojure usa los tipos de objetos de Java para booleanos, cadenas de
; caracteres ("strings") y números.
; Usa class para inspeccionarlos.
(class 1); Los números enteros literales son java.lang.Long por defecto
(class 1.); Los números en coma flotante literales son java.lang.Double
(class ""); Los strings siempre van entre comillas dobles, y son
          ; java.lang.String
(class false); Los booleanos son java.lang.Boolean
(class nil); El valor "null" se escribe nil

; Si quieres crear una lista literal de datos, usa ' para evitar su evaluación
'(+ 1 2) ; => (+ 1 2)
; (que es una abreviatura de (quote (+ 1 2)))

; Puedes evaluar una lista precedida por una comilla con eval
(eval '(+ 1 2)) ; => 3

; Colecciones & Secuencias
;;;;;;;;;;;;;;;;;;;

; Las Listas están basadas en listas enlazadas, mientras que los Vectores en
; arreglos.
; ¡Los Vectores y las Listas también son clases de Java!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Una lista podría ser escrita como (1 2 3), pero debemos precederle una
; comilla para evitar que el lector ("reader") piense que es una función.
; Además, (list 1 2 3) es lo mismo que '(1 2 3)

; Las Colecciones ("collections") son solo grupos de datos
; Tanto las Listas como los Vectores son colecciones:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; Las Secuencias ("seqs") son descripciones abstractas de listas de datos.
; Solo las listas son secuencias ("seqs").
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Una secuencia solo necesita proporcionar uno de sus elementos cuando es
; accedido.
; Así que, las secuencias pueden ser perezosas -- pueden definir series
; infinitas:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (una serie infinita)
(take 4 (range)) ;  (0 1 2 3)

; Usa cons para agregar un elemento al inicio de una Lista o Vector
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; conj agregará un elemento a una colección en la forma más eficiente.
; Para Listas, se añade al inicio. Para vectores, al final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Usa concat para concatenar Listas o Vectores
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Usa filter y map para actuar sobre colecciones
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Usa reduce para combinar sus elementos
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; reduce puede tomar un argumento como su valor inicial también
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Funciones
;;;;;;;;;;;;;;;;;;;;;

; Usa fn para crear nuevas funciones. Una función siempre devuelve
; su última expresión
(fn [] "Hello World") ; => fn

; (Necesitas rodearlo con paréntesis para llamarla)
((fn [] "Hello World")) ; => "Hello World"

; Puedes definir una variable ("var") mediante def
(def x 1)
x ; => 1

; Asignar una función a una variable ("var")
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Puedes usar defn como atajo para lo anterior
(defn hello-world [] "Hello World")

; El [] es el Vector de argumentos de la función.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Puedes usar esta abreviatura para definir funciones:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Puedes tener funciones multi-variables ("multi-variadic") también
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Las funciones pueden empaquetar argumentos extras en una secuencia para ti
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; Puedes combinar los argumentos regulares y los empaquetados
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; Mapas
;;;;;;;;;;

; Los Mapas de Hash ("HashMap") y Mapas de Arreglo ("ArrayMap") comparten una
; interfaz. Los Mapas de Hash tienen búsquedas más rápidas pero no mantienen el
; orden de las llaves.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Los Mapas de Arreglo se convierten automáticamente en Mapas de Hash en la
; mayoría de operaciones si crecen mucho, por lo que no debes preocuparte.

; Los Mapas pueden usar cualquier tipo para sus llaves, pero generalmente las
; Claves ("keywords") son lo habitual.
; Las Claves son como strings con algunas ventajas de eficiencia
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Por cierto, las comas son equivalentes a espacios en blanco y no hacen
; nada.

; Recupera un valor de un Mapa tratándola como una función
(stringmap "a") ; => 1
(keymap :a) ; => 1

; ¡Las Claves pueden ser usadas para recuperar su valor del mapa, también!
(:b keymap) ; => 2

; No lo intentes con strings.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Recuperando una clave no existente nos devuelve nil
(stringmap "d") ; => nil

; Usa assoc para añadir nuevas claves a los Mapas de Hash
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Pero recuerda, ¡los tipos de Clojure son inmutables!
keymap ; => {:a 1, :b 2, :c 3}

; Usa dissoc para eliminar claves
(dissoc keymap :a :b) ; => {:c 3}

; Conjuntos
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Añade un elemento con conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Elimina uno con disj
(disj #{1 2 3} 1) ; => #{2 3}

; Comprueba su existencia usando al Conjunto como una función:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Hay más funciones en el espacio de nombres clojure.sets

; Patrones útiles
;;;;;;;;;;;;;;;;;

; Los operadores lógicos en clojure son solo macros, y presentan el mismo
; aspecto que el resto de patrones.
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Usa let para definir ("binding") una variable temporal
(let [a 1 b 2]
  (> a b)) ; => false

; Agrupa sentencias mediante do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Las funciones tienen un do implícito
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; Y let también
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Usa las macros de tubería ("threading", "arrow", "pipeline" o "chain")
; (-> y ->>) para expresar la transformación de datos de una manera más clara.

; La macro Tubería-primero ("Thread-first") (->) inserta en cada patrón el
; resultado de los previos, como el primer argumento (segundo elemento)
(->
   {:a 1 :b 2}
   (assoc :c 3) ;=> (assoc {:a 1 :b 2} :c 3)
   (dissoc :b)) ;=> (dissoc (assoc {:a 1 :b 2} :c 3) :b)

; Esta expresión podría ser escrita como:
; (dissoc (assoc {:a 1 :b 2} :c 3) :b)
; y evalua a {:a 1 :c 3}

; La macro Tubería-último ("Thread-last") hace lo mismo, pero inserta el
; resultado de cada línea al *final* de cada patrón. Esto es útil para las
; operaciones de colecciones en particular:
(->>
   (range 10)
   (map inc)     ;=> (map inc (range 10)
   (filter odd?) ;=> (filter odd? (map inc (range 10))
   (into []))    ;=> (into [] (filter odd? (map inc (range 10)))
                 ; Result: [1 3 5 7 9]

; Cuando estés en una situación donde quieras tener más libertad en donde
; poner el resultado de transformaciones previas de datos en una expresión,
; puedes usar la macro as->. Con ella, puedes asignar un nombre especifico
; a la salida de la transformaciones y usarlo como identificador en tus
; expresiones encadenadas ("chain").

(as-> [1 2 3] input
  (map inc input);=> You can use last transform's output at the last position
  (nth input 2) ;=>  and at the second position, in the same expression
  (conj [4 5 6] input [8 9 10])) ;=> or in the middle !


; Módulos
;;;;;;;;;;;;;;;

; Usa use para obtener todas las funciones del módulo
(use 'clojure.set)

; Ahora podemos usar más operaciones de Conjuntos
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Puedes escoger un subgrupo de funciones a importar, también
(use '[clojure.set :only [intersection]])

; Usa require para importar un módulo
(require 'clojure.string)

; Usa / para llamar las funciones de un módulo
; Aquí, el módulo es clojure.string y la función es blank?
(clojure.string/blank? "") ; => true

; Puedes asignarle una sobrenombre a un modulo al importarlo
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" es una expresión regular literal)

; Puedes usar require (y use, pero no lo hagas) desde un espacio de nombres
; usando :require,
; No necesitas preceder con comilla tus módulos si lo haces de esta manera.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java tiene una enorme y útil librería estándar, por lo que querrás
; aprender como hacer uso de ella.

; Usa import para cargar un módulo de java
(import java.util.Date)

; Puedes importar desde un ns también.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Usa el nombre de la clase con un "." al final para crear una nueva instancia
(Date.) ; <un objeto Date>

; Usa "." para llamar métodos. O, usa el atajo ".método"
(. (Date.) getTime) ; <un timestamp>
(.getTime (Date.)) ; exactamente lo mismo.

; Usa / para llamar métodos estáticos.
(System/currentTimeMillis) ; <un timestamp> (System siempre está presente)

; Usa doto para lidiar con el uso de clases (mutables) de una manera más
; tolerable
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; La Memoria Transaccional ("Software Transactional Memory" / "STM") es un
; mecanismo que usa clojure para gestionar la persistecia de estado. Hay unas
; cuantas construcciones en clojure que hacen uso de él.

; Un atom es el más sencillo. Se le da un valor inicial
(def my-atom (atom {}))

; Actualiza un atom con swap!
; swap! toma una función y la llama con el valor actual del atom
; como su primer argumento, y cualquier argumento restante como el segundo
(swap! my-atom assoc :a 1) ; Establece my-atom al resultado
                           ; de (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Establece my-atom al resultado
                           ; de (assoc {:a 1} :b 2)

; Usa '@' para no referenciar al atom y obtener su valor
my-atom  ;=> Atom<#...> (Regresa el objeto Atom)
@my-atom ; => {:a 1 :b 2}

; Aquí está un sencillo contador usando un atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Otras construcciones de STM son refs y agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Lectura adicional

Ésto queda lejos de ser exhaustivo, pero ojalá que sea suficiente para que
puedas empezar tu camino.

Clojure.org tiene muchos artículos:
[http://clojure.org](http://clojure.org)

Clojuredocs.org contiene documentación con ejemplos para la mayoría de
funciones principales (pertenecientes al core):
[http://clojuredocs.org/quickref](http://clojuredocs.org/quickref)

4Clojure es una genial forma de mejorar tus habilidades con clojure/FP:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (sí, de verdad) tiene un buen número de artículos con los que
iniciarse en Clojure: [http://clojure-doc.org](http://clojure-doc.org)
