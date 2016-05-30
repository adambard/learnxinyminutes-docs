---
language: self
contributors:
    - ["Russell Allen", "http://github.com/russellallen"]
filename: learnself-es.self
translators:
    - ["Damaso Sanoja", "https://github.com/damasosanoja"]
lang: es-es
---

Self es un lenguaje OO basado en prototipo rápido que corre en su propio vm JIT. La mayoría del desarrollo se hace a través de la interacción con objetos vivos en un entorno de desarrollo visual llamado *morphic* que tiene integrado navegador y depurador.

Todo en Self es un objeto. Todos los cómputos son hechos enviando mensajes a los objetos. En Self se puede entender por Objetos a los conjuntos de pares clave-valor.

# Construyendo objetos

El intérprete incorporado de Self puede construir objetos, incluyendo objetos-métodos.

```
"Esto es un comentario"

"Una cadena de caracteres (string):"
'Esto es un string con \'caracteres\' escapados.\n'

"Un entero de 30 bits"
23

"Un decimal de 30 bits"
3.2

"-20"
-14r16

"Un objeto que solo entiende un mensaje, 'x' que regresa 20"
(|
  x = 20.
|)

"Un objeto que además entiende 'x:' que establece la posición x"
(|
  x <- 20.
|)

"Un objeto que entiende el método 'doubleX' el cual
duplica el valor de x y luego regresa el objeto"
(|
  x <- 20.
  doubleX = (x: x * 2. self)
|)

"Un objeto que entiende todos los mensajes
que 'traits point' entiende". El intérprete
mira a 'traits point' enviando los mensajes
'traits' y luego 'point' a un objeto conocido llamado
el 'lobby'. El mira el objeto 'true' enviando
también el mensaje 'true' al lobby."
(|     parent* = traits point.
       x = 7.
       y <- 5.
       isNice = true.
|)
```

# Enviando mensajes a los objetos

Los mensajes pueden ser unarios, binarios o palabras clave. La precedencia es en ese orden. A diferencia de Smalltalk, la precedencia de los mensajes binarios debe ser especificada, y todas las palabras clave después de la primera deben comenzar con una letra mayúscula. Los mensajes se separan de sus destinos mediante espacios en blanco.

```
"mensaje unario, envía 'printLine' al objeto '23'
que imprime el string '23' en stdout y regresa el objeto recibido (ejem 23)"
23 printLine

"envía el mensaje '+' con '7' para '23', luego el mensaje '*' con '8' para el resultado"
(23 + 7) * 8

"envía 'power:' para '2' con '8' regresa 256"
2 power: 8

"envía 'keyOf:IfAbsent:' para 'hello' con los argumentos 'e' y '-1'.
Regresa 1, el índice de 'e' en 'hello'."
'hello' keyOf: 'e' IfAbsent: -1
```

# Bloques

Self define el control de flujo como Smalltalk y Ruby mediante bloques Los bloques son cómputos demorados de la forma.:

```
[|:x. localVar| x doSomething with: localVar]
```

Ejemplos del uso de bloques:

```
"regresa 'HELLO'"
'hello' copyMutable mapBy: [|:c| c capitalize]

"regresa 'Nah'"
'hello' size > 5 ifTrue: ['Yay'] False: ['Nah']

"regresa 'HaLLO'"
'hello' copyMutable mapBy: [|:c|
   c = 'e' ifTrue: [c capitalize]
            False: ['a']]
```

Las expresiones múltiples son separadas por un punto. ^ retorna inmediatamente.

```
"returns An 'E'! How icky!"
'hello' copyMutable mapBy: [|:c. tmp <- ''|
   tmp: c capitalize.
   tmp = 'E' ifTrue: [^ 'An \'E\'! How icky!'].
   c capitalize
   ]
```

Los bloques son ejecutados al enviales el mensaje 'value' y son inherentes (delegados a) sus contextos:
```
"returns 0"
[|x|
    x: 15.
    "Envía repetidamente 'value' al primer bloque mientras el resultado de enviar 'value' al segundo bloque es el objeto 'true'"
    [x > 0] whileTrue: [x: x - 1].
    x
] value
```

# Métodos

Los métodos son como los bloques pero no están dentro de un contexto sino que son almacenados como valores de ranuras. A diferencia de Smalltalk, los métodos no regresan por defecto 'self' sino su valor final.

```
"Aquí tenemos un objeto con una ranura asignable 'x' y un método 'reduceXTo: y'.
Enviando el mensaje 'reduceXTo: 10' a este objeto pondrá
el objeto '10' en la ranura 'x' y regresará el objeto original"
(|
    x <- 50.
    reduceXTo: y = (
        [x > y] whileTrue: [x: x - 1].
        self)
|)
.
```

# Prototipos

Self no posee clases. La forma de acceder a un objeto es encontrando un prototipo y copiándolo.

```
| d |
d: dictionary copy.
d at: 'hello' Put: 23 + 8.
d at: 'goodbye' Put: 'No!.
"Prints No!"
( d at: 'goodbye' IfAbsent: 'Yes! ) printLine.
"Prints 31"
( d at: 'hello' IfAbsent: -1 ) printLine.
```

# Para mayor información

El [Manual de Self](http://handbook.selflanguage.org) tiene mucha más información, y nada mejor que experiencia de primera mano con Self descargándolo de su [página web](http://www.selflanguage.org).
