---
language: bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Daniel Zendejas", "https://github.com/DanielZendejas"]
lang: es-es
---

Brainfuck (con mayúscula sólo al inicio de una oración) es un
lenguaje de programación extremadamente pequeño, Turing completo con sólo 8 comandos.

Puedes probar brainfuck en tu navegador con [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).


```

Cualquier caracter que no sea "><+-.,[]" (sin incluir las comillas)
será ignorado.

Brainfuck es representado por un arreglo de 30,000 celdas inicializadas
en cero y un puntero apuntando la celda actual.

Existen ocho comandos:

+ : Incrementa 1 al valor de la celda actual.
- : Decrementa 1 al valor de la celda actual.
> : Mueve el apuntador a la siguiente celda. (a la derecha)
< : Mueve el apuntador a la celda anterior. (a la izquierda)
. : Imprime el valor en ASCII de la celda actual (p.e. 65 = 'A')
, : Lee un caracter como input y lo escribe en la celda actual.
[ : Si el valor en la celda actual es cero mueve el apuntador
	hasta el primer ']' que encuentre. Si no es cero sigue a la
	siguiente instrucción.
] : Si el valor en la celda actual es cero, entonces sigue con 
	la siguiente instrucción. Si no entonces mueve el apuntador
	hacia atrás	hasta encontrar el primer '['.

[ y ] forman un while. Obviamente, deben estar balanceados.

Estos son algunos ejemplos de programas escritos con brainfuck.

++++++ [ > ++++++++++ < - ] > +++++ .

Este programa imprime la letra 'A'. Primero, incrementa la celda #1 a 
6. La celda #1 será usada para hacer los ciclos. Después entra al ciclo
([) y se mueve a la celda #2 (>). Después incrementa la celda #2 10 veces,
y se regresa a la celda #1 (<), para después decrementarla en 1 (-).
Este ciclo ocurre 6 veces (le toma 6 decrementos a la celda #1 volverse 0),
cuando esto pasa se salta a (]) y continúa.

En este punto estamos en la celda #1, que tiene un valor de 0, mientras
que la celda #2 tiene un valor de 60. Nos movemos a la celda #2 (>),
la incrementamos 5 veces para tener un valor de 65 y luego imprimimos
el valor de la celda #2 (.). 65 es 'A' en ASCII así que la letra 'A'
se imprime.

, [ > + < - ] > .

Este programa lee un caracter del input y lo copia en la celda #2 (,). 
Después empieza un ciclo. Nos movemos a la celda #2 (>) e incrementamos su
valor (+). Regresamos a la celda #1 y decrementamos su valor en 1 (-).
Esto continúa hasta que la celda #1 contenga un cero. Cuando #1 contenga un 
cero la celda #2 tendrá el valor inicial de #1. Como este ciclo siempre
terminara en la celda #1 nos movemos a la celda #2 e imprimimos (.).

Ten en cuenta que los espacios son sólo para fines de legibilidad.
Es lo mismo escribir el ejemplo de arriba que esto:
,[>+<-]>.

Intenta descrifrar lo que hace este programa:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Este programa toma dos números como input y los multiplica.

Primero recibe dos números del usuario. Luego empieza el ciclo externo,
condicionado en la celda #1. Luego se mueve a la celda #2, comenzando
el ciclo interno condicionado en la celda #2 incrementando la celda #3.
Sin embargo viene un problema: El ciclo interior no funcionará nuevamente
hasta la próxima vez. Para resolver este problema también incrementamos la
celda #4 y luego copiamos la celda #4 a la celda #2. La celda #3 contiene
el resultado.
```
Y eso es brainfuck. No es tan difícil, ¿verdad? Como diversión, puedes escribir
tu propio intérprete de brainfuck o tu propio programa en brainfuck. El
intérprete es relativamente sencillo de hacer, pero si eres masoquista,
puedes intentar construir tu propio intérprete de brainfuck... en brainfuck.
