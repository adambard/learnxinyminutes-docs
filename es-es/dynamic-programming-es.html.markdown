---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es 
---

# Programación Dinámica

## Introducción

La programación dinámica es una técnica poderosa usada para resolver una clase particular de problemas como veremos más adelante. 
La idea es muy simple: si has solucionado un problema con la entrada dada, entonces, guardaremos el resultado para una futura referencia, con el fin de evitar la solución del mismo problema de nuevo.

Recuerda siempre:
"Aquellos que no pueden recordar el pasado están condenados a repetirlo"

## Formas de resolver este tipo de problemas

1. *De arriba hacia abajo (Top-Down)* : Empezamos resolviendo el problema dado descomponiendolo. Si ves que el problema fue resuelto, entonces retorna la respuesta guardada. Si no se ha resuelto, resuélvelo y guarda la respuesta. Esto suele ser fácil de pensar y es muy intuitivo. A esto se le conoce como memoización.

2. *De abajo hacia arriba (Bottom-Up)* : Analiza el problema y ve el orden en que los subproblemas deben ser resueltos y empieza resolviendo el subproblema más trivial, hacia el problema dado. En este proceso, se garantiza que los subproblemas se resuelven antes de resolver el problema. Esto se conoce como Programación Dinámica.

## Ejemplo de Programación Dinámica

El problema de la subsecuencia creciente máxima consiste en encontrar la subsecuencia creciente máxima en una secuencia dada. Dada la secuencia `S= {a1 , a2 , a3, a4, ............., an-1, an }`, tenemos que encontrar un subconjunto más largo tal que para todo `j` y `i`, `j<i` en el subconjunto `aj<ai`.
En primer lugar tenemos que encontrar el valor de las subsecuencias más largas (LSi) en cada índice `i` con el último elemento de la secuencia que es `ai`. El mayor LSi sería la subsecuencia más larga de la secuencia dada. Para empezar, LSi=1 ya que `ai` es un elemento de la secuencia (el último elemento). Entonces, para todo `j` tal que `j<i` y `aj<ai`, encontramos el LSj más grande y lo agregamos al LSi, por lo que el algoritmo toma un tiempo de *O(n2)*.
Pseudocódigo para encontrar la longitud de la subsecuencia creciente máxima:
La complejidad de este algoritmo podría reducirse mediante el uso de una mejor estructura de datos que los arreglos. Guardar un arreglo de predecesores y una variable como `secuencia_mas_grande_hasta_ahora` y su índice podría ahorrar mucho tiempo.

Un concepto similar se podría aplicar para encontrar la trayectoria más larga en un grafo acíclico dirigido (DAG).

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
```

### Algunos problemas famosos de Programación Dinámica (DP).

- Algoritmo Floyd Warshall(EN) - [Tutorial y código fuente del programa en C](http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code)
- Problema de la Mochila(EN) - [Tutorial y código fuente del programa en C](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem)
- Problema de Subsecuencia Común mas Larga(EN) - [Tutorial y código fuente del programa en C](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence)

## Recursos en línea

* [codechef EN](https://www.codechef.com/wiki/tutorial-dynamic-programming)
