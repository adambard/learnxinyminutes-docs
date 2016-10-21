---
language: edn
filename: learnedn-es.edn
contributors:
  - ["Jason Yeo", "https://github.com/jsyeo"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es 
---

La notación de datos extensible (Extensible Data Notation (EDN)) es un formato para serializar los datos.

La notación se utiliza internamente por Clojure para representar programas. También es
utilizado como un formato de transferencia de datos como JSON. A pesar de que se utiliza más comúnmente en
Clojure, existen implementaciones de EDN para muchos otros lenguajes.

El principal beneficio de EDN sobre JSON y YAML es que es extensible. 
Vamos a ver cómo se extiende más adelante.

```clojure
; Los comentarios comienzan con un punto y coma.
; Cualquier cosa después del punto y coma es ignorado.

;;;;;;;;;;;;;;;;;;;
;;;Tipos Básicos;;;
;;;;;;;;;;;;;;;;;;;

nil         ; También conocido en otros lenguajes como nulo (null).

; Booleanos
true
false

; Las cadenas se encierran entre comillas dobles
"desayuno húngaro"
"tortilla de queso del granjero"

; Los caracteres están precedidos por barras invertidas
\g \r \a \c \e

; Las palabras claves comienzan con dos puntos.Se comportan como las enumeraciones. Más o menos
; Como símbolos en Ruby
:huevos
:queso
:aceitunas

; Los símbolos se utilizan para representar los identificadores.Estos empiezan con #.
; puedes tener espacios usando el símbolo /. cualquier cosa precedida / es
; un espacio en el nombre.
#cuchara
#cocina/cuchara ; no es lo mismo que #spoon
#cocina/tenedor
#github/tenedor   ; no se puede comer con este.

; Números enteros y flotantes
42
3.14159

; Las listas son secuencias de valores.
(:bollo :empanada-de-res 9 "yum!")

; Vectores permiten acceso aleatorio
[:helado 1 2 -2]

; Los mapas son estructuras de datos asociativos que se asocian con la clave de su valor.
{:huevos        2
 :jugo-de-limon 3.5
 :mantequilla     1}

; Usted no está restringido a usar palabras clave como claves.
{[1 2 3 4] "decirle a la gente lo que llevaba",
 [5 6 7 8] "Entre mas tu ves, mas lo odias"}

; Puede usar comas para facilitar la lectura. Se tratan como espacios en blanco.

; Los conjuntos son colecciones que contienen elementos únicos.
#{:a :b 88 "huat"}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Elementos de etiqueta ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EDN puede ser extendido por elementos de etiqueta con el símbolo #.

#MyYelpClone/MenuItem {:nombre "huevos-Benedict" :clasificacion 10}

; Permíteme explicar esto con un ejemplo en colujre. Supongamos que quiero
; transformar ese pedazo de EDN en un registro del Menú.

(defrecord MenuItem [nombre clasificacion])

; Para transformar EDN en valores clojure, necesitaremos usar el constructor en EDN
; lectura, edn/read-string

(edn/read-string "{:huevos 2 :mantequilla 1 :harina 5}")
; -> {:huevos 2 :mantequilla 1 :harina 5}

; Para transformar los elementos de etiqueta, definir la función de lectura y pasar un mapa
; que asigna etiquetas a funciones del lector de edn/read-string al igual que.

(edn/read-string {:lectores {'MyYelpClone/MenuItem map->menu-item}}
                 "#MyYelpClone/MenuItem {:nombre \"huevos-benedict\" :clasificacion 10}")
; -> #user.MenuItem{:nombre "huevos-benedict", :clasificacion 10}

```

# Referencias

- [EDN spec (EN)](https://github.com/edn-format/edn)
- [Implementations (EN)](https://github.com/edn-format/edn/wiki/Implementations)
- [Tagged Elements (EN)](http://www.compoundtheory.com/clojure-edn-walkthrough/)
