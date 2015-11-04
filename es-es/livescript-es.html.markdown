---
language: LiveScript
filename: learnLivescript-es.ls
contributors:
    - ["Christina Whyte", "http://github.com/kurisuwhyte/"]
translators:
    - ["Daniel Zendejas", "http://github.com/DanielZendejas/"]
lang: es-es
---

LiveScript es un lenguaje funcional compilado sobre Javascript. Comparte
la mayoría de la semántica con este mismo lenguaje. Composición de funciones,
comparación de patrones y muchas otras cosas son las adiciones que hace
LiveScript. Está inspirado en lenguajes como Haskell, F# y Scala.

Livescript es un bifurcación de [Coco][], que en sí mismo es una bifurcación
de [CoffeeScript][]. El lenguaje es estable, y una nueva versión está en 
desarrollo activo para traer aún más funciones.

[Coco]: http://satyr.github.io/coco/
[CoffeeScript]: http://coffeescript.org/

La retroalimentación siempre es bienvenida, así que sientete libre de
contactarme en [@kurisuwhyte](https://twitter.com/kurisuwhyte) :)

```coffeescript
# Justo como su primo CoffeeScript, LiveScript usa símbolos de gato para
# comentarios de una sola línea

/*
 Comentarios multi-línea son escritos con estilo de C. Usa este estilo si quieres
 que los comentarios se preserven en el output de Javascript
 */
```
```coffeescript
# En lo que a la sintaxis se refiere, LiveScript usa indentación para delimitar
# bloques en lugar de llaves {} y espacios para aplicar funciones, en lugar de
# paréntesis.

########################################################################
## 1. Valores básicos
########################################################################

# La carencia de valor se define con la palabra `void` en lugar de `undefined`
void            # igual que `undefined` pero más seguro (no puede ser sobre escrito)

# Ningún valor válido se representa con Null.
null

# El valor básico más pequeño es de tipo lógico: 
true
false

# Y tiene múltiples alias que significan lo mismo:
on; off
yes; no

# Luego vienen los números. Estos número con punto flotante tienen la misma 
# precisión que los de JS:
10
0.4     # Note que el `0` al inicio es requerido

# Para fines de fácil lectura, puedes usar guiones bajos y sufijos en un
# número, y estos serán ignorados por el compilador.
12_344km

# Los Strings son secuencias de caracteres inmutables, como en JS:
"Christina"             # ¡Los apóstrofes están bien!
"""Strings
   de muchas
   líneas
   están
   bien
   también."""

# A veces quieres codificar un palabra clave, la diagonal invertida sirve para esto:
\keyword                # => 'keyword'


# Los arreglos son colecciones ordenadas de datos:
frutas =
  * \manzana
  * \naranja
  * \pera

# Una forma más concisa de representarlos son con corchetes: 
frutas = [ \manzana, \naranja, \pera ]

# Esta es una conveniente de crear listas de Strings, usando
# espacio en blanco para delimitar los items:
frutas = <[ manzana naranja pera ]>

# Puedes recuperar un item usando su índice (empezando en 0):
frutas[0]       # => "manzana"

# Los objetos son colecciones de pares llave/valor sin ordenar, entre otras cosas,
# (detallaremos más al respecto en un momento):
persona =
  nombre: "Christina"
  gusta:
    * "gatitos"
    * "otras cosas"

# Otra vez, puedes expresar el objeto con más consistencia con llaves {}:
persona = {nombre: "Christina", gusta: ["gatitos", "otras cosas"]}

# Puedes conseguir un valor por medio de su llave:
persona.nombre    # => "Christina"
persona["nombre"]  # => "Christina"


# Las expresiones regulares tienen la misma sintaxis que en JavaScript:
expresion-regular = /\s$/

# A excepción de que puedes hacer expresiones de múltiples líneas
# (los comentarios y espacios se ignoran):
expresion-regular = //
        function\s+(.+)         # nombre
        \s* \((.*)\) \s*        # argumentos
        { (.*) }                # cuerpo
        //


########################################################################
## 2. Operaciones básicas
########################################################################

# Los operadores aritméticos son los mismos que en JavaScript:
1 + 2   # => 3
2 - 1   # => 1
2 * 3   # => 6
4 / 2   # => 2
3 % 2   # => 1


# Las comparaciones son casi las mismas, excepto `==` que es igual
# a `===` en. El operador `==` de JS en LiveScript es `~=`, y `===`
# permite comparaciones entre objetos y arreglos, y también 
# comparasiones más estrictas:
2 == 2          # => true
2 == "2"        # => false
2 ~= "2"        # => true
2 === "2"       # => false

[1,2,3] == [1,2,3]        # => false
[1,2,3] === [1,2,3]       # => true

+0 == -0     # => true
+0 === -0    # => false

# Otros operadores relacionales incluyen <, <=, > and >=

# Los valores lógicos pueden ser combinados mediante los operadores 
# lógicos `or`, `and` and `not`: 
true and false  # => false
false or true   # => true
not false       # => true

# Las colecciones también tienen algunos operadores adicionales: 
[1, 2] ++ [3, 4]                # => [1, 2, 3, 4]
'a' in <[ a b c ]>              # => true
'nombre' of { nombre: 'Chris' }     # => true


########################################################################
## 3. Funciones
########################################################################        

# Como LiveScript es funcional, uno esperaría que las funciones recibirían
# un buen tratamiento. En LiveScript es más que aparente que las funciones
# son de primera clase: 
suma = (primerElemento, segundoElemento) -> primerElemento + segundoElemento
add 1, 2        # => 3

# Las funciones que no reciben argumentos son llamadas rápidamente
dos = -> 2
dos!

# LiveScript, al igual que JS, aplica ámbitos (alcance) a sus variables y 
# tiene cierres (closures) también. A diferencia de JavaScript, el operador
# `=` sirve como declaración y siempre declarará la variable en lado izquierdo.

# El operador `:=` está disponible para *reusar* un nombre del ámbito del padre.

# Puedes acceder a los argumentos de una función para conseguir
# los datos internos de una estructura de datos rápidamente: 
cola = ([cabeza, ...resto]) -> resto
cola [1, 2, 3]  # => [2, 3]

# También puedes transformar argumentos usando operadores binarios o unarios.
# Argumentos por defecto también son posibles
foo = (a = 1, b = 2) -> a + b
foo!    # => 3

# También puedes usarlo para clonar un argumento en particular para evitar efectos
# secundarios, por ejemplo: 
copiar = (^^objetivo, fuente) ->
  for k,v of fuente => objetivo[k] = v
  objetivo
a = { a: 1 }
copiar a, { b: 2 }        # => { a: 1, b: 2 }
a                       # => { a: 1 }

# Una función puede ser abreviada usando una flecha larga en lugar de una corta:
suma = (primerElemento, segundoElemento) --> primerElemento + segundoElemento
sumaAbreviada = suma 1
sumaAbreviada 2          # => 3

# Las funciones obtienen un argumento `it` implícito, incluso si no declaras
# ningún argument
identidad = -> it
identidad 1      # => 1

# Los operadores no son funciones en LiveScript. ¡Pero se pueden convertir fácilmente
# en una! Presentamos el seccionamiento de operadores: 
dividir-entre-2 = (/ 2)
[2, 4, 8, 16].map(dividir-entre-2) .reduce (+)

# LiveScript vive de otras cosas aparte de las funciones. Como en cualquier lenguaje 
# funcional obtienes medios para componer (juntar) funciones: 
doble-menos-uno = (- 1) . (* 2)

# A parte de la clásica fórmula matemática `f . g`, también cuentas co los operadores
# `>>` y `<<`, que describen el flujo de los valores dentro de las funciones:
double-minus-one = (* 2) >> (- 1)
double-minus-one = (- 1) << (* 2)

# Hablando del flujo de los valores, LiveScript también tiene los operadores `|>` y `<|`
# que aplican un valor a una función:
map = (f, xs) --> xs.map f
[1 2 3] |> map (* 2)            # => [2 4 6]

# También puedes escoger dónde quieres que el valor se posicione, sólo márcalo con un
# guíon bajo:
reducir = (f, xs, initial) --> xs.reducir f, initial
[1 2 3] |> reducir (+), _, 0     # => 6

# El guíon bajo también se usa para apartar lugares para tus argumentos, por ejemplo: 
division = (dividendo,divisor) -> dividendo / divisor
dividir-entre-2 = division _, 2
dividir-entre-2 4      # => 2

# Por último, LiveScript tiene back-calls (útiles mecanismos para hacer
# callbacks.). A pesar de esto deberías intentar formas más funcionales de hacerlo,
# como Promises:
leerArchivo = (nombre, f) -> f name
a <- leerArchivo 'foo'
b <- leerArchivo 'bar'
console.log a + b

# Igual a:
leerArchivo 'foo', (a) -> leerArchivo 'bar', (b) -> console.log a + b


########################################################################
## 4. Patrones, guardias y control de flujo
########################################################################

# Puedes bifurcar cálculos con la expresión `if...else`:
x = if n > 0 then \positive else \negative

# En lugar de `then`, puedes usar `=>`
x = if n > 0 => \positivo
    else        \negativo
    
# A pesar de esto, a las condiciones complejas es mejor expresarlas con el `switch`:
y = {}
x = switch
  | (typeof y) is \number => \numero
  | (typeof y) is \string => \string
  | 'length' of y         => \arreglo
  | otherwise             => \objeto      # `otherwise` y `_` son lo mismo.

# Los cuerpos de las funciones, declaraciones y asignaciones tienen un `switch` por defecto,
# así que no necesitas escribirlo nuevamente: 

take = (n, [x, ...xs]) -->
                        | n == 0 => []
                        | _      => [x] ++ take (n - 1), xs


########################################################################
## 5. Comprehensions (Auxiliares)
########################################################################

# Mientras que los auxiliares funcionales (para lidiar con listas y objetos)
# están en la librería estándar de JavaScript (y complementada con prelude-ls,
# que es una "librería estándar" de LiveScipt) los "comprehensions" (Auxiliares)
# usualemente te permiten hacer lo mismo pero más rápido y con una sintaxis más
# comprehensible (de ahí su nombre en inglés):
unoAVeinte = [1 to 20]
pares       = [x for x in oneToTwenty when x % 2 == 0]

# `when` y `unless` pueden ser usados como filtros en el auxiliar.

# Los auxiliares para objetos funcionan de la misma forma, excepto que regresa un 
# objeto en lugar de un arreglo:
copiar = { [k, v] for k, v of source }


########################################################################
## 4. PROGRAMACIÓN ORIENTADA A OBJETOS
########################################################################

# Mientras que LiveScript es un lenguaje funcional en la mayoría de los 
# aspectos, también brinda ayudas para la programación orientada a objetos.
# Algunas de estas ayudas son la sintaxis para las clases y un poco de "azucar"
# para las clases heredada de CoffeeScript:
class Animal
  (@nombre, tipo) ->
    @tipo = tipo
  action: (accion) -> "*#{@nombre} (un #{@tipo}) #{accion}*"

class Gato extends Animal
  (@nombre) -> super @nombre, 'gato'
  ronronear: -> @action 'ronronea'

gatito = new Gato 'Mei'
gatito.purr!      # => "*Mei (un gato) ronronea*"

# A parte del clásico patrón de herencia simple, también puedes proveer
# cuantas mezclas quieras para una clase. Las mezclas sólo son objetos:
Abrazable =
  abrazar: -> @action 'es abrazado'

class GatoAbrazable extends Gato implements Abrazable

gatito = new GatoAbrazable 'Ronroneo'
gatito.abrazar!     # => "*Mei (un gato) es abrazado*"
```

## Más recursos

Existe mucho más sobre LiveScript, pero esto debe bastar para que empieces.
El [sitio oficial](http://livescript.net/) tiene mucha información sobre el 
lenguaje, y un compilador en linea para que pruebes cosas inmediatamente.

También querras probar un poco de [prelude.ls](http://gkz.github.io/prelude-ls/), 
y probar el canal `#livescript` en la red Freenode.
