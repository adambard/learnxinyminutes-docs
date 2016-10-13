---
language: edn
filename: learnedn.edn
contributors:
  - ["Jason Yeo", "https://github.com/jsyeo"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es 
---

Extensible Data Notation (EDN) is a format for serializing data.

The notation is used internally by Clojure to represent programs. It is also
used as a data transfer format like JSON. Though it is more commonly used in
Clojure, there are implementations of EDN for many other languages.

The main benefit of EDN over JSON and YAML is that it is extensible. We
will see how it is extended later on.

```clojure
; Comments start with a semicolon.
; Anything after the semicolon is ignored.

;;;;;;;;;;;;;;;;;;;
;;;Tipos Basicos;;;
;;;;;;;;;;;;;;;;;;;

nil         ; Tambien conocido en otros lenguajes como nulo (null).

; Booleanos
true
false

; Las cadenas se encierran entre comillas dobles
"hungarian breakfast"
"farmer's cheesy omelette"

; Los caracteres están precedidos por barras invertidas
\g \r \a \c \e

; Las palabras claves comienzan con dos puntos.Se comportan como las enumeraciones. Mas o menos
; Como simbolos en Ruby
:eggs
:cheese
:olives

; Los símbolos se utilizan para representar los identificadores.Estos empiezan con #.
; You can namespace symbols by using /. Whatever preceeds / is
; the namespace of the name.
#spoon
#kitchen/spoon ; not the same as #spoon
#kitchen/fork
#github/fork   ; you can't eat with this

; Números enteros y flotantes
42
3.14159

; Las listas son secuencias de valores.
(:bun :beef-patty 9 "yum!")

; Vectores permiten acceso aleatorio
[:gelato 1 2 -2]

; Los mapas son estructuras de datos asociativos que se asocian con la clave de su valor.
{:eggs        2
 :lemon-juice 3.5
 :butter      1}

; Usted no está restringido a usar palabras clave como claves.
{[1 2 3 4] "tell the people what she wore",
 [5 6 7 8] "the more you see the more you hate"}

; Puede usar comas para facilitar la lectura. Se tratan como espacios en blanco.

; Los conjuntos son colecciones que contienen elementos únicos.
#{:a :b 88 "huat"}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Elementos de etiqueta ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EDN puede ser extendido por elementos de etiqueta con el simbolo #.

#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}

; Permiteme explicar esto con un ejemplo en colujre. Supongamos que quiero
; transformar ese pedazo de EDN en un registro del Menú.

(defrecord MenuItem [name rating])

; Para transformar EDN en valores clojure, necesitaremos usar el constructor en EDN
; lectura, edn/read-string

(edn/read-string "{:eggs 2 :butter 1 :flour 5}")
; -> {:eggs 2 :butter 1 :flour 5}

; Para transformar los elementos de etiqueta, definir la función de lectura y pasar un mapa
; que asigna etiquetas a funciones del lector de edn/read-string al igual que.

(edn/read-string {:readers {'MyYelpClone/MenuItem map->menu-item}}
                 "#MyYelpClone/MenuItem {:name \"eggs-benedict\" :rating 10}")
; -> #user.MenuItem{:name "eggs-benedict", :rating 10}

```

# Referencias

- [EDN spec (EN)](https://github.com/edn-format/edn)
- [Implementations (EN)](https://github.com/edn-format/edn/wiki/Implementations)
- [Tagged Elements (EN)](http://www.compoundtheory.com/clojure-edn-walkthrough/)
