---
category: Algorithms & Data Structures
name: Set Theory
contributors:
  - ["Peter McGoron", "https://github.com/phm19a"]
---

## Introduction

Set theory is the mathematical study of collections of objects. These
objects can be anything from numbers, computer programs, beach balls,
or other sets. Axiomatic set theory forms the foundation of mathematics,
and set theory appears everywhere in Computer Science.

Set theory as we know it was innovated in the 19th century by
[Georg Cantor](https://en.wikipedia.org/wiki/Georg_Cantor),
with important contributions from
[Ernst Zermelo](https://en.wikipedia.org/wiki/Ernst_Zermelo) and
[Abraham Fraenkel](https://en.wikipedia.org/wiki/Abraham_Fraenkel) that
formed `ZF`, a set of axioms for set theory.

## Basic Concepts

Sets can usually be written in two ways, either by specifying all of
the elements, like `{1,2,3}`, or by specifying the kind of elements the
set has, and some formula that the elements have to satisfy, written as
`{element | formula}`. The second type is called *set builder notation*.

For instance,
* `{x | x > 0}` denotes all numbers (depending on context, they might
  be real numbers, integers, etc.) greater than zero
* `{f(x) | f(0) = 0}` denotes all functions over a single numerical
  variable (again, this variable might be a real number, or an integer,
  or some other number) that is defined such that the function is zero at
  zero. If `x` is a real number, then `f(x) = x` and `sin(x)` are
  members of this set.
* `{(x,y) | x^2 + y^2 = 1}` denotes the unit circle on a graph.

The members of a set do not need to be homogeneous. For instance, we can
define `{b | b is a function or b is a integer}` is a set. In formulas,
"and" is abbreviated `∧` and "or" is abbreviated as `∨`.

Sets are defined to be equal when they have the same elements as each
other. The elements of a set are not ordered, so `{1,2,3}` and `{3,1,2}`
are equal. This also means that sets don't have duplicate elements:
`{1,2,3} = {1,2,3,3}`, for instance.

When we want to say "`x` is an element of the set `S`", we abbreviate
it as `x ∈ S`, and the opposite would be `x ∉ S`.

Sets may be finite, like `{1,2,3}`, or infinite, like the set of all
real numbers. There is a set that is *empty*, which is denoted by
`∅`. Nothing is an element of `∅`.

## Definitions

The *union* of `A` and `B`, denoted `A∪B`, is the set containing elements
of both `A` and `B`. This can be defined as `{x|x ∈ A ∨ x ∈ B}`.
For instance, `{1,2,3}∪{3,4,5} = {1,2,3,4,5}`.

The *arbitrary union* or *n-ary union* of `A`, denoted `⋃A`, is the
union of all sets inside of it. For instance,
`⋃{{1},{1,2},{3,4,5}} = {1,2,3,4,5}`.

We say `A` is a *subset* of `B`, denoted `A ⊆ B`, if all the elements
of `A` are elements of `B`. For instance, `{1,2,3} ⊆ {1,2,3,4}`.
Note that `A` can actually be equal to `B`, since the elements of `B`
are all elements of `B`. If `A` is a subset of `B` and `B` is a subset
of `A`, then they contain the same elements, and are equal. Another
important fact is that the empty set is a subset of any set, since all
zero elements of it are elements of any set (since there are none).

A proper subset (`A ⊂ B`) is the same as a subset, but `B ≠ A`. The
example used above is a proper subset.

The set containing all subsets of some set `A` is called the *power set*,
abbreviated `𝒫A`. For instance, the power set of `{1,2,3}` is `{∅,
{1}, {2}, {3}, {1,2}, {2,3}, {1,2,3}}`.

The *cardinality* of a set is, roughly, the number of elements the set
has. Since some sets are infinite, the cardinality is sometimes not a
number, but if two sets have the same number of elements, then their
cardinalities are equal. The cardinality of a set `S` is denoted `|S|`.
For instance, `|{green,blue,orange}| = 3`, and `|∅| = 0`.

The *intersection of `A` and `B`*, denoted `A∩B`, is the set containing
elements that are elements of *both* `A` and `B`. We can describe this
with set builder notation as `{x|x ∈ A ∧ x ∈ B}`. For instance,
`{a,b,c} ∩ {a,z,c,r} = {a,c}`.

The *n-ary intersection* or *arbitrary intersection of the set `A`*
is the intersection applied to all sets that are elements of `A`.
For instance, `⋂{{1,2},{2,3,4}{-1,0,1,2}} = {2}`.

The *set difference between `A` and `B`*, denoted `A \ B`, is the
set that contains all elements of `A` that are not elements of `B`.
We can describe this in set builder notation as `{x| x ∈ A ∧ x ∉
B}`. For instance, `{1,2,3,4} - {3,6,9} = {1,2,4}`, and as an example
for infinite sets,`{…,-2,-1,0,1,2,…} \ {0,1,2,…} = {-1,-2,-3,…}`.

The *symmetrical difference of `A` and `B`* is the elements of `A` and `B`
that are only elements of one of the sets, defined as `A △ B`. That is,
if `x ∈ A △ B`, then either `x ∈ A` and `x ∉ B`, or `x ∈ B`
and hence `x ∉ A`.

Sets are not ordered, but frequently in computer science and mathematics
we require things to be ordered (like coordinate pairs or arrays). A
collection of objects that is ordered is called an *`n`-tuple*, where
`n` is the positive integer amount of elements. For instance `(1,2)`
is a 2-tuple, and `(a,b,c)` is a 3-tuple.

The *cartesian product of `A` and `B`*, denoted `A×B`, is the set of
all 2-tuples `(a,b)`, where `a ∈ A` and `b ∈ B`,  The set of all
three tuples made from sets `A`, `B`, and `C` is `A×B×C`, and so on.
A 2d graph is an example of the Cartesian product `ℝ×ℝ`.

## Paradoxes in Set Theory and Axiomatic Set Theory

The way we have defined sets is usually called "naïve set theory." This
is because it is not on an axiomatic basis. For most of your needs, you
will never have to extend past naïve set theory. But the importance
of non-naïve set theory (axiomatic set theory) is that it rids us of
paradoxes that happen when we consider just about anything to be a set.

The most famous paradox is
[Russel's Paradox](https://en.wikipedia.org/wiki/Russell%27s_paradox).
Consider the set containing all sets that do not contain themselves,
that is, `R = {x|x∈x}`. This seems weird, but nothing we have said so
far *forbids* this to be a set, since sets can contain other sets. The
question is, is `R` in `R`? Because if `R ∈ R`, then `R` contains
itself, so it must be that `R ∉ R`, while if `R` does not contain
itself, then `R ∈ R`.

This "set" `R` (sometimes called the "Russell class") presented a
great problem for the early development of set theory, because it was a
contradictory object, and something clearly cannot be both an element and
not an element of a set at the same time. The response of axiomatic set
theory was to define "sets" in such a way as to avoid calling `R` a set.

There are many axiomatic set theories, but by and large
the most popular and accepted one is [Zermelo-Fraenkel set
theory](https://en.wikipedia.org/wiki/Zermelo%E2%80%93Fraenkel_set_theory)
called ZF (and with the addition of the Axiom of Choice, ZFC). ZFC
declares what is a set (specifically, the empty set and the non-negative
integers, called *natural numbers*), and how to make new sets from those
sets. It also specifies that

* Sets are in a hierarchy, and you can go down this hierarchy until you
  get to the bottom (the empty set).
* Set builder notation must be made from *subsets of other sets*,
  and cannot be self-referential (for instance, `y = {x∈A|x=y}` is
  disallowed). When we build a set `A` from another set `B`, we write
  it as `A = {x ∈ B|formula}`.
* Only sets are "things," so sets can *only* contain other sets (or
  nothing).

Other interesting consequences of this is that no set contains itself,
there is no set containing all other sets, and there are some collections
(like the set of all cardinalities) that cannot be sets. Numbers, graphs,
and computer programs are defined in terms of sets.

## The Axiom of Choice

There is only one axiom with considerable history and significance that
has to be discussed in detail, and that is the *Axiom of Choice*. If
I have a set `S` of any size (finite or infinite), then there exists a
function `f`, such that the arguments are subsets `A` of `S` that are
not empty, and the result is some *fixed* element of `A`, which is then
an element of `S`. The preceding sentence *cannot be shown to be either
true or false in ZF*, and cannot be constructed manually for most sets
(a notable exception is finite sets).

The Axiom of Choice is important because many important and
striking results can be proven with it, like the [Banach-Tarski
Paradox](https://en.wikipedia.org/wiki/Banach_Tarski_Paradox),
where we can take a ball, decompose it, and recompose it into
two identical copies of the original ball.
Nonetheless, many important facts in mathematics not only require the
Axiom of Choice to be true, they are *equivalent* to it, in that if you
know the fact to be true, then you can *prove* the Axiom of Choice.

## Recommended Reading

*Naive Set Theory* by Paul Halmos is an introduction to set theory,
which, although "naive," deeply develops the theory of ordinal numbers
that is central to the development of higher set theory and certain
aspects of computer science.

*Elements of Set Theory* by Herbert Enderton is another introductory
textbook about set theory, which develops axiomatic set theory in a
rigorous and thorough matter. If you are interested in the development
of ordinal and cardinal numbers, the usage of set theory to define the
integers, rationals, and the real numbers, and a deeper look into the
Axiom of Choice, you should read this book.
