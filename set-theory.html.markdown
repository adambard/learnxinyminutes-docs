---
category: Algorithms & Data Structures
name: Set theory
contributors:
---
The set theory is a study for sets, their operations, and their properties. It is the basis of the whole mathematical system.

* A set is a collection of definite distinct items.

## Basic operators
These operators don't require a lot of text to describe.

* `∨` means or.
* `∧` means and.
* `,` separates the filters that determine the items in the set.

## A brief history of the set theory
### Naive set theory
* Cantor invented the naive set theory.
* It has lots of paradoxes and initiated the third mathematical crisis.

### Axiomatic set theory
* It uses axioms to define the set theory.
* It prevents paradoxes from happening.

## Built-in sets
* `∅`, the set of no items.
* `N`, the set of all natural numbers. `{0,1,2,3,…}`
* `Z`, the set of all integers. `{…,-2,-1,0,1,2,…}`
* `Q`, the set of all rational numbers.
* `R`, the set of all real numbers.
### The empty set
* The set containing no items is called the empty set. Representation: `∅`
* The empty set can be described as `∅ = {x|x ≠ x}`
* The empty set is always unique.
* The empty set is the subset of all sets.
```
A = {x|x∈N,x < 0}
A = ∅
∅ = {}              (Sometimes)

|∅|   = 0
|{∅}| = 1
```
## Representing sets
### Enumeration
* List all items of the set, e.g. `A = {a,b,c,d}`
* List some of the items of the set. Ignored items are represented with `…`. E.g. `B = {2,4,6,8,10,…}`

### Description
* Describes the features of all items in the set. Syntax: `{body|condtion}`
```
A = {x|x is a vowel}
B = {x|x ∈ N, x < 10l}
C = {x|x = 2k, k ∈ N}
C = {2x|x ∈ N}
```

## Relations between sets
### Belongs to
* If the value `a` is one of the items of the set `A`, `a` belongs to `A`. Representation: `a∈A`
* If the value `a` is not one of the items of the set `A`, `a` does not belong to `A`. Representation: `a∉A`

### Equals
* If all items in a set are exactly the same to another set, they are equal. Representation: `a=b`
* Items in a set are not order sensitive. `{1,2,3,4}={2,3,1,4}`
* Items in a set are unique. `{1,2,2,3,4,3,4,2}={1,2,3,4}`
* Two sets are equal if and only if all of their items are exactly equal to each other. Representation: `A=B`. Otherwise, they are not equal. Representation: `A≠B`.
* `A=B` if `A ⊆ B` and `B ⊆ A`

### Belongs to
* If the set A contains an item `x`, `x` belongs to A (`x∈A`).
* Otherwise, `x` does not belong to A (`x∉A`).

### Subsets
* If all items in a set `B` are items of set `A`, we say that `B` is a subset of `A` (`B⊆A`).
* If B is not a subset of A, the representation is `B⊈A`.

### Proper subsets
* If `B ⊆ A` and `B ≠ A`, B is a proper subset of A (`B ⊂ A`). Otherwise, B is not a proper subset of A (`B ⊄ A`).

## Set operations
### Base number
* The number of items in a set is called the base number of that set. Representation: `|A|`
* If the base number of the set is finite, this set is a finite set.
* If the base number of the set is infinite, this set is an infinite set.
```
A   = {A,B,C}
|A| = 3
B   = {a,{b,c}}
|B| = 2
|∅| = 0         (it has no items)
```

### Powerset
* Let `A` be any set. The set that contains all possible subsets of `A` is called a powerset (written as `P(A)`).
```
P(A) = {x|x ⊆ A}

|A| = N, |P(A)| = 2^N
```

## Set operations among two sets
### Union
Given two sets `A` and `B`, the union of the two sets are the items that appear in either `A` or `B`, written as `A ∪ B`.
```
A ∪ B = {x|x∈A∨x∈B}
```
### Intersection
Given two sets `A` and `B`, the intersection of the two sets are the items that appear in both `A` and `B`, written as `A ∩ B`.
```
A ∩ B = {x|x∈A,x∈B}
```
### Difference
Given two sets `A` and `B`, the set difference of `A` with `B` is every item in `A` that does not belong to `B`.
```
A \ B = {x|x∈A,x∉B}
```
### Symmetrical difference
Given two sets `A` and `B`, the symmetrical difference is all items among `A` and `B` that doesn't appear in their intersections.
```
A △ B = {x|(x∈A∧x∉B)∨(x∈B∧x∉A)}

A △ B = (A \ B) ∪ (B \ A)
```
### Cartesian product
Given two sets `A` and `B`, the cartesian product between `A` and `B` consists of a set containing all combinations of items of `A` and `B`.
```
A × B = { {x, y} | x ∈ A, y ∈ B }
```
## "Generalized" operations
### General union
Better known as "flattening" of a set of sets.
```
∪A = {x|X∈A,x∈X}
∪A={a,b,c,d,e,f}
∪B={a}
∪C=a∪{c,d}
```
### General intersection
```
∩ A = A1 ∩ A2 ∩ … ∩ An
```
