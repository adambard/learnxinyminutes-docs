---
category: Algorithms & Data Structures
name: Binary Search
contributors:
    - ["Abhishek Jaisingh", "http://github.com/abhishekjiitr"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es    
---

# Búsqueda Binaria

## Por qué Búsqueda Binaria?

La búsqueda es uno de los problemas principales en el dominio de la ciencia de la computación. Hoy en dia hay mas de 1 billon de búsquedas por año, y necesitamos tener algoritmos que puedan hacer esto muy rápido. La búsqueda binaria es uno de los algoritmos fundamentales en la ciencia de la computación. Con el fin de explorarlo, vamos a construir por primera vez un esqueleto teórico y lo utilizaremos para implementar el algoritmo apropiadamente.

## Introducción

Un método sencillo para poner en práctica la búsqueda es hacer una búsqueda lineal, pero este método requiere mucho tiempo y este crece linealmente con la cantidad o el número de datos. es decir, empezar desde el elemento a la izquierda de la matriz [] y uno por uno compara x con cada elemento de la matriz [], si x coincide con un elemento, devuelve el índice. Si x no coincide con ninguno de los elementos, devuelve -1.

```
Búsqueda Lineal: O (n)               Tiempo lineal

Búsqueda Binaria: O ( log(n) )		   Tiempo logarítmico

```
```
def search(arr, x):
 
    for i in range(len(arr)):
 
        if arr[i] == x:
            return i
 
    return -1

```
## Algoritmo de Búsqueda Binaria 

El requisito básico para que la búsqueda binaria funcione es que los datos a buscar deben estar ordenados (en cualquier orden).


### Algo

```
La idea de la búsqueda binaria es usar la información de que la matriz está ordenada y así reducir la complejidad del tiempo a O(Logn). Básicamente ignoramos la mitad de los elementos después de la primera comparación.
1) Compare x con el elemento del medio.
2) si x coincide con el elemento del medio , retornamos el índice del elemento del medio.
3) Si no coincide, si x es mayor que el elemento del medio, entonces x solo puede estar en la mitad derecha justo después del elemento del medio. Así que recurrimos a la mitad derecha. 
4) Si no (x es más pequeño) recurrimos a la mitad izquierda.
Siguiendo la implementación recursiva de búsqueda binaria.

```

### Notas finales 

Hay otra forma de búsqueda binaria que es muy útil.

## Libros

* [CLRS EN](https://mitpress.mit.edu/books/introduction-algorithms)
* [Algoritmos EN](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Diseño de Algoritmos EN](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Recursos en línea 

* [GeeksforGeeks EN](http://www.geeksforgeeks.org/the-ubiquitous-binary-search-set-1/)
* [Topcoder Tutorial EN](https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/)
