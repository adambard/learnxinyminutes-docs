---
category: Algorithms & Data Structures
name: Set theory
contributors:
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
---

Set theory is a branch of mathematics that studies sets, their operations, and their properties.

* A set is a collection of disjoint items.

## Basic symbols

### Operators
* the union operator, `∪`, pronounced "cup", means "or";
* the intersection operator, `∩`, pronounced "cap", means "and";
* the exclusion operator, `\`, means "without";
* the complement operator, `'`, means "the inverse of";
* the cross operator, `×`, means "the Cartesian product of".

### Qualifiers 
* the colon, `:`, or the vertical bar `|` qualifiers are interchangeable and mean "such that";
* the membership qualifier, `∈`, means "belongs to";
* the subset qualifier, `⊆`, means "is a subset of";
* the proper subset qualifier, `⊂`, means "is a subset of but is not equal to".

### Canonical sets
* `∅`, the empty set, i.e. the set containing no items;
* `ℕ`, the set of all natural numbers;
* `ℤ`, the set of all integers;
* `ℚ`, the set of all rational numbers;
* `ℝ`, the set of all real numbers.

There are a few caveats to mention regarding the canonical sets:
1. Even though the empty set contains no items, the empty set is a subset of itself (and indeed every other set);
2. Mathematicians generally do not universally agree on whether zero is a natural number, and textbooks will typically explicitly state whether or not the author considers zero to be a natural number.


### Cardinality

The cardinality, or size, of a set is determined by the number of items in the set. The cardinality operator is given by a double pipe, `|...|`.

For example, if `S = { 1, 2, 4 }`, then `|S| = 3`.

### The Empty Set
* The empty set can be constructed in set builder notation using impossible conditions, e.g. `∅ = { x : x ≠ x }`, or `∅ = { x : x ∈ N, x < 0 }`;
* the empty set is always unique (i.e. there is one and only one empty set);
* the empty set is a subset of all sets;
* the cardinality of the empty set is 0, i.e. `|∅| = 0`.

## Representing sets

### Literal Sets

A set can be constructed literally by supplying a complete list of objects contained in the set. For example, `S = { a, b, c, d }`.

Long lists may be shortened with ellipses as long as the context is clear. For example, `E = { 2, 4, 6, 8, ... }` is clearly the set of all even numbers, containing an infinite number of objects, even though we've only explicitly written four of them.

### Set Builder

Set builder notation is a more descriptive way of constructing a set. It relies on a _subject_ and a _predicate_ such that `S = { subject : predicate }`. For example,

```
A = { x : x is a vowel } = { a, e, i, o, u }
B = { x : x ∈ N, x < 10 } = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }
C = { x : x = 2k, k ∈ N } = { 0, 2, 4, 6, 8, ... }
```

Sometimes the predicate may "leak" into the subject, e.g.

```
D = { 2x : x ∈ N } = { 0, 2, 4, 6, 8, ... }
```

## Relations

### Membership

* If the value `a` is contained in the set `A`, then we say `a` belongs to `A` and represent this symbolically as `a ∈ A`.
* If the value `a` is not contained in the set `A`, then we say `a` does not belong to `A` and represent this symbolically as `a ∉ A`.

### Equality

* If two sets contain the same items then we say the sets are equal, e.g. `A = B`.
* Order does not matter when determining set equality, e.g. `{ 1, 2, 3, 4 } = { 2, 3, 1, 4 }`.
* Sets are disjoint, meaning elements cannot be repeated, e.g. `{ 1, 2, 2, 3, 4, 3, 4, 2 } = { 1, 2, 3, 4 }`.
* Two sets `A` and `B` are equal if and only if `A ⊆ B` and `B ⊆ A`.

## Special Sets

### The Power Set
* Let `A` be any set. The set that contains all possible subsets of `A` is called a "power set" and is written as `P(A)`. If the set `A` contains `n` elements, then `P(A)` contains `2^n` elements.

```
P(A) = { x : x ⊆ A }
```

## Set operations among two sets
### Union
Given two sets `A` and `B`, the union of the two sets are the items that appear in either `A` or `B`, written as `A ∪ B`.

```
A ∪ B = { x : x ∈ A ∪ x ∈ B }
```

### Intersection
Given two sets `A` and `B`, the intersection of the two sets are the items that appear in both `A` and `B`, written as `A ∩ B`.

```
A ∩ B = { x : x ∈ A, x ∈ B }
```

### Difference
Given two sets `A` and `B`, the set difference of `A` with `B` is every item in `A` that does not belong to `B`.

```
A \ B = { x : x ∈ A, x ∉ B }
```

### Symmetrical difference
Given two sets `A` and `B`, the symmetrical difference is all items among `A` and `B` that doesn't appear in their intersections.

```
A △ B = { x : ((x ∈ A) ∩ (x ∉ B)) ∪ ((x ∈ B) ∩ (x ∉ A)) }

A △ B = (A \ B) ∪ (B \ A)
```

### Cartesian product
Given two sets `A` and `B`, the cartesian product between `A` and `B` consists of a set containing all combinations of items of `A` and `B`.

```
A × B = { (x, y) | x ∈ A, y ∈ B }
```
