---
language: coffeescript
lang: es-es
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
translators:
  - ["Pablo Elices", "http://github.com/pabloelices"]
filename: coffeescript-es.coffee
---

``` coffeescript
# CoffeeScript es un lenguaje hipster.
# Tiene convenciones de muchos lenguajes modernos.
# Los comentarios son como en Ruby y Python, usan almohadillas.

###
Los comentarios en bloque son como estos, y se traducen directamente a '/*' y '*/'
para el código JavaScript resultante.

Deberías entender la mayor parte de la semántica de JavaScript antes de continuar.
###

# Asignación:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Condiciones:
number = -42 if opposite #=> if(opposite) { number = -42; }

# Funciones:
square = (x) -> x * x #=> var square = function(x) { return x * x; }

# Rangos:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Objetos:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# Símbolos:
race = (winner, runners...) ->
  print winner, runners

# Existencia:
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Colecciones por comprensión:
cubes = (math.cube num for num in list) #=> ...
```
