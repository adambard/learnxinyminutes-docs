---
category: Algorithms & Data Structures
name: Binary Search
contributors:
    - ["Abhishek Jaisingh", "http://github.com/abhishekjiitr"]
translators:
    - ["Hughes Perreault", "https://github.com/hperreault"]
lang: fr-fr
---

# Recherche Binaire

## Pourquoi la Recherche Binaire ?

La recherche est un des principaux problèmes dans le domaine de l'informatique. De nos jours, il y a plus de 1 milliard de recherches par année, et nous avons besoin d'algorithmes pour faire cela rapidement. La recherche binaire est un des algorithmes les plus fondamentaux en informatique. Pour pouvoir l'explorer en détail, nous allons d'abord établir une base théorique, puis nous allons utiliser cette base pour implémenter l'algorithme en soi.

## Introduction

Une façon simple d'implémenter la recherche est de faire une recherche linéaire. Cependant, cette approche prend beaucoup de temps, et ce temps augmente linéairement avec la quantité de données. Par exemple, partons du premier élément d'un tableau t[], et un par un, comparons x avec chaque élément de t[]. Si x est égal à un élément, nous retournons l'index, si x n'égale aucun élément, nous retournons -1.

```
Recherche Linéaire: O (n)               Temps Linéaire

Recherche Binaire:  O ( log(n) )        Temps Logarithmique

```
```
def search(arr, x):

    for i in range(len(arr)):

        if arr[i] == x:
            return i

    return -1

```
## L'Algorithme de Recherche Binaire

Le prérequis fondamental de la recherche binaire est que les éléments soient triés.

### Algo

```
L'idée derrière la recherche binaire est d'utiliser le fait que le tableau est trié afin de réduire la complexité à O(Log(n)). Nous pouvons ignorer la moitié des éléments après la première comparaison.
1) Comparons x avec l'élément du milieu.
2) Si x est égal à cet élément, nous retournons l'index du milieu.
3) Sinon, si x est plus grand que l'élément du milieu, alors x peut seulement être dans la dernière moitié du tableau. Donc, nous recommençons la procédure avec cette dernière moitié.
4) Sinon (x est plus petit), nous recommençons la procédure avec la première moitié du tableau.
Ensuite nous avons une implémentation récursive de la recherche binaire.

```

### Note de la fin

Partie en construction.

## Livre

* [CLRS EN](https://mitpress.mit.edu/books/introduction-algorithms)
* [Algorithmes EN](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Design d'Algorithmes EN](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Ressources en ligne

* [GeeksforGeeks EN](http://www.geeksforgeeks.org/the-ubiquitous-binary-search-set-1/)
* [Topcoder Tutorial EN](https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/)
