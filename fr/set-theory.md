---
category: Algorithms & Data Structures
contributors: 
  - ["kieutrang", "https://github.com/kieutrang1729"]
---

La théorie des ensembles est une branche des mathématiques qui étudie les ensembles, leurs opérations et leurs propriétés.

* Un ensemble est une collection d'éléments disjoints.

## Symboles de base

### Opérateurs
* l'opérateur réunion, `∪`, signifie "ou" ;
* l'opérateur intersection, `∩`, signifie "et" ;
* l'opérateur différence, `\`, signifie "sans", (lire "A moins B") ;
* l'opérateur complémentaire, `'`, signifie "le complémentaire de" ;
* l'opérateur croix, `×`, signifie "le produit cartésien de".

### Autres symboles
* le symbole deux-points, `:`, signifie "tel que" ;
* le symbole d'appartenance, `∈`, signifie "appartient à" ;
* le symbole sous-ensemble, `⊆`, signifie "est un sous-ensemble de" ;
* le symbole sous-ensemble propre, `⊂`, signifie "est un sous-ensemble de mais n'est pas égal à".

### Ensembles importants
* `∅`, l'ensemble vide, c'est-à-dire l'ensemble ne contenant aucun élément ;
* `ℕ`, l'ensemble des nombres naturels ;
* `ℤ`, l'ensemble des entiers ;
* `ℚ`, l'ensemble des nombres rationnels ;
* `ℝ`, l'ensemble des nombres réels.

Quelques mise en gardes sur les ensembles définis ci-dessus:
1. Même si l'ensemble vide ne contient aucun élément, il est lui-même un sous-ensemble de n'importe quel ensemble.
2. Il n'y a pas d'accord général sur l'appartenance de zéro dans l'ensemble des nombres naturels, et les livres indiquent explicitement si l'auteur considère le zéro comme nombre naturel ou pas.


### Cardinalité

La cardinalité, ou taille, d'un ensemble est déterminée par le nombre d'éléments dans l'ensemble. L'opérateur de cardinalité s'écrit, `| ... |`.
Par exemple, si `S = { 1, 2, 4 }`, alors `|S| = 3`.

### L'ensemble vide
* L'ensemble vide peut se définir en compréhension à l'aide d'une propriété qui n'est satisfaite par nul élément, e.g. `∅ = { x : x ≠ x }`, ou `∅ = { x : x ∈ N, x < 0 }`.
* il n'y a qu'un seul ensemble vide.
* l'ensemble vide est sous-ensemble de tout ensemble.
* la cardinalité de l'ensemble vide est 0, ou `|∅| = 0`.

## Notation ensembliste

### Définition par extension

Un ensemble peut être defini en extension par une liste de tous les éléments qui sont contenus dans l'ensemble. Par exemple, `S = { a, b, c, d }`.

Quand le contexte est clair, on peut raccourcir la liste en utilisant des points de suspension. Par exemple, `E = { 2, 4, 6, 8, ... }` est clairement l'ensemble de tous les nombres pairs, contenant un nombre infini des éléments, même si on a explicitement écrit seulement les quatre premiers.

### Définition par compréhension

C'est une notation plus descriptif qui permet de définir un ensemble à l'aide d'un sujet et d'une propriété, et il est noté `S = { sujet : propriété }`. Par exemple,

```
A = { x : x est une voyelle } = { a, e, i, o, u, y}
B = { x : x ∈ N, x < 10 } = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }
C = { x : x = 2k, k ∈ N } = { 0, 2, 4, 6, 8, ... }
```

On peut même appliquer une fonction au sujet, e.g.

```
D = { 2x : x ∈ N } = { 0, 2, 4, 6, 8, ... }
```

## Relations

### Appartenance

* Si l'élément `a` est dans l'ensemble `A`, on dit que `a` appartient à `A` et on le note `a ∈ A`.
* Si l'élément `a` n'est pas dans l'ensemble `A`, on dit que `a` n'appartient pas à `A` et on le note `a ∉ A`.

### Égalité

* On dit que deux ensembles `A` et `B` sont égaux s'ils contiennent les mêmes éléments, et on le note `A = B`.
* Les ensembles n'ont pas de notion d'ordre, par exemple `{ 1, 2, 3, 4 } = { 2, 3, 1, 4 }`.
* Un élément ne peut apparaître qu'au plus une seule fois - il n'y a jamais de répétition, e.g. `{ 1, 2, 2, 3, 4, 3, 4, 2 } = { 1, 2, 3, 4 }`.
* Deux ensembles `A` et `B` sont égaux si et seulement si `A ⊆ B` et `B ⊆ A`.

## Ensemble puissance
* L'ensemble puissance d'un ensemble `A` est l'ensemble contenant tous les sous-ensembles de `A`. Il est noté `P(A)`. Si la cardinalité de `A` est `n`, la cardinalité de `P(A)` est `2^n`.

```
P(A) = { x : x ⊆ A }
```

## Opérations ensemblistes
### Réunion
La réunion de deux ensembles `A` et `B` est l'ensemble contenant tous les éléments qui appartient à `A` ou à `B`.

```
A ∪ B = { x : x ∈ A ∪ x ∈ B }
```

### Intersection
L'intersection de deux ensembles `A` et `B` est l'ensemble contenant tous les éléments qui appartient à la fois à `A` et à `B`.

```
A ∩ B = { x : x ∈ A, x ∈ B }
```

### Différence
La différence de deux ensembles `A` et `B` est l'ensemble contenant tous les éléments de l'ensemble `A` qui n'appartient pas à `B`.

```
A \ B = { x : x ∈ A, x ∉ B }
```

### Différence symétrique
Le différence symétrique de deux ensembles `A` et `B` est l'ensemble contenant tous les éléments de `A` et `B` qui n'apparaissent pas dans leur intersection.

```
A △ B = { x : ((x ∈ A) ∩ (x ∉ B)) ∪ ((x ∈ B) ∩ (x ∉ A)) }

A △ B = (A \ B) ∪ (B \ A)
```

### Produit cartésien
Le produit cartésien de deux ensembles `A` et `B` est l'ensemble contenant tous les couples dont le premier élément appartient à `A` et le deuxième à `B`.

```
A × B = { (x, y) | x ∈ A, y ∈ B }
```
