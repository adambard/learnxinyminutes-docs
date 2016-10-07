---
category: Algorithms & Data Structures
name: Binary Search
contributors:
    - ["Abhishek Jaisingh", "http://github.com/abhishekjiitr"]
translators:
    - ["Claudson Martins", "https://github.com/claudsonm"]
lang: pt-br
---

# Busca Binária

## Por Que Busca Binária?

Operações de busca são um dos principais problemas na Ciência da Computação. 
Atualmente existem mais de 1 trilhão de buscas por ano, e nós precisamos de 
algoritmos que possam realizá-las rapidamente. Busca binária é um dos algoritmos 
fundamentais em ciência da computação. A fim de explorá-la, iremos primeiro 
construir um conhecimento teórico, e então utilizá-lo para implementar o 
algoritmo apropriadamente.

## Introdução

Uma abordagem simples para implementar uma busca é realizar uma busca linear, 
mas algoritmos nessa abordagem levam muito tempo, o qual cresce linearmente de 
acordo com a quantidade ou número de dados. Por exemplo, iniciando do elemento 
mais a esquerda de arr[] e um a um comparar x com cada elemento de arr[], se x 
coincide com um elemento, retornar seu índice. Se x não coincide com nenhum dos 
elementos, retornar -1.

```
Busca Linear: O (n)               Tempo Linear

Busca Binária: O ( log(n) )		   Tempo Logarítmico

```
```
def busca(arr, x):
 
    for i in range(len(arr)):
 
        if arr[i] == x:
            return i
 
    return -1

```
## Algoritmo de Busca Binária

O pré-requisito básico para que uma busca binária funcione é que os dados que se
desejam buscar devem estar ordenados (em qualquer ordem).

### Pseudocódigo

```
A ideia da busca binária é usar a informação de que o array está ordenado e 
reduzir a complexidade de tempo para O(Log n). Nós basicamente ignoramos metade 
dos elementos após uma comparação.

1) Compare x com o elemento do meio.
2) Se x coincide com o elemento do meio, retorne o índice do meio.
3) Senão Se x é maior que o elemento do meio, então x só pode estar no lado 
direito do elemento do meio. Portanto nós pulamos para a metade direita.
4) Senão (x é menor) pulamos para a metade esquerda.

Essa é a ideia da implementação recursiva da busca binária.

```

### Considerações Finais

Existem outras formas de busca binária que são muito úteis.

## Recursos Online

* [GeeksforGeeks](http://www.geeksforgeeks.org/the-ubiquitous-binary-search-set-1/)
* [Topcoder Tutorial](https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/)
