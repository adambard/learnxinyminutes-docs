---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es 
---

# programación dinámica

## Introducción

La programación dinámica es una técnica poderosa usada para resolver una clase particular de problemas como veremos más adelante. La idea es muy simple, si usted ha solucionado un problema con la entrada dada, entonces , guardaremos el resultado para una futura referencia, con el fin de evitar la solución del mismo problema de nuevo.


Recuerde siempre!!
"Aquellos que no pueden recordar el pasado están condenados a repetirlo"

## Formas de resolver este tipo de problemas

1.) De arriba hacia abajo : Empezamos resolviendo el problema dado descomponiendolo. Si ves que el problema fue resuelto, entonces retorna la respuesta guardada. si no se ha resuelto, resuélvelo y guarda la respuesta. Esto suele ser fácil pensar y muy intuitivo. Esto se conoce como memorización.

2.) De abajo hacia arriba : Analiza el problema y mira el orden en que los subproblemas deben ser resueltos y empieza resolviendo el subproblema más trivial, hacia el problema dado.En este proceso, se garantiza que los subproblemas se resuelven antes de resolver el problema. Esto se conoce como programación dinámica.

## Ejemplo de Programación Dinámica

El problema de la subsecuencia creciente máxima consiste en encontrar la subsecuencia creciente máxima en una secuencia dada . Dada la secuencia S= {a1 , a2 , a3, a4, ............., an-1, an } tenemos que encontrar un subconjunto más largo tal que para todo j y i, j <i en el subconjunto aj <ai.
En primer lugar tenemos que encontrar el valor de las subsecuencias más largas (LSI) en cada índice con el último elemento de la secuencia que es ai. El mayor LSi sería la subsecuencia más larga de la secuencia dada. Para empezar LSI es asignado a uno ya que ai es un elemento de la secuencia(El último elemento).Entonces, para todo j tal que j <i aj <ai, nos encontramos con Lsj más grande y lo agregamos a la LSI. A continuación, el algoritmo toma un tiempo de O (n2).
Pseudocódigo para encontrar la longitud de la más larga subsecuencia creciente:
La complejidad de este algoritmos podría reducirse mediante el uso de una mejor estructura de datos en lugar de una array. Almacenamiento de una matriz predecesora y una variable como Secuencia_mas_Grande_hasta_ahora y su índice podría ahorrar mucho tiempo.
concepto similar se podría aplicar en encontrar el camino más largo de grafo acíclico dirigido.
---------------------------------------------------------------------------
 for i=0 to n-1
            LS[i]=1
            for j=0 to i-1
                        if (a[i] >  a[j] and LS[i]<LS[j])
                                    LS[i] = LS[j]+1
 for i=0 to n-1
            if (largest < LS[i])

### Algunos problemas famosos de Programación Dinámica (DP).
```
Algoritmo Floyd Warshall(EN) - Tutorial y código fuente del programa en C:http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code 

Problema de la Mochila(EN) - Tutorial y código fuente del programa en C: http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem 


Problema de Subsecuencia Común mas Larga(EN) - Tutorial y código fuente del programa en C : http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence 

## Recursos en línea

* [codechef EN](https://www.codechef.com/wiki/tutorial-dynamic-programming)