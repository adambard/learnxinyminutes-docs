---
language: (Dyalog) APL
filename: learnapl.apl
contributors:
    - ["Fawn Locke", "https://awagga.xyz/"]
---

[APL](https://aplwiki.com/wiki/Main_Page) is an array-oriented programming language developed in the 1960s
by [Kenneth E.Iverson](https://en.wikipedia.org/wiki/Kenneth_E._Iverson). 

The distinguishing feature of APL is it's focus on arrays - being the only first class datatype.

```apl
⍝ These examples can be tested at tryapl.org

⍝ Arrays are created by juxtaposing it's elements, also known as "Stranding":
2 3e7 ¯4 50.3           ⍝ ([2, 30000000, -4, 50.3] in Python) 

⍝ In APL, there's no order of operations; everything is parsed right-to-left:
1 + 2 × 3 ÷ 4 - 5       ⍝ ¯5
⍝ This is equivalent to 1 + (2 × (3 ÷ (4 - 5))).

⍝ Operations are applied array-wise:
1 2 3 4 × 5 6 7 8       ⍝ 5 12 21 32

⍝ APL features scalar-extension, a method of distributing a scalar
⍝ (applied with a function) across an argument:
2 × 1 2 3 4             ⍝ 2 4 6 8

⍝ All functions can be called either with a single argument (monadically),
⍝ or two arguments (dyadically).

⍝ For example, "×" applied between two arguments means multiply, but when applied to
⍝ a single argument, it returns the sign:
× ¯4 ¯2 0 2 4           ⍝ ¯1 ¯1 0 1 1

⍝ Values can be compared using these functions (1 means "true", 0 means "false"):
10 20 30 = 10 20 99     ⍝ 1 1 0

10 20 30 ≠ 10 20 99     ⍝ 0 0 1

10 20 30 < 10 20 99     ⍝ 0 0 1

⍝ "⍳n" (indices) returns a vector containing the first n natural numbers:
⍳5                      ⍝ 1 2 3 4 5

⍝ All arrays have dimensions which elements are organized across,
⍝ monadic ⍴ (shape), gives you the dimension('s) length back:
⍴1 2 3 4 5              ⍝ 5

⍝ You can retrieve rank (number of dimensions) by applying the functions
⍝ "length of shape" to an array:
≢⍴1 2 3 4 5             ⍝ 1 (vectors have a single dimension)
≢⍴5                     ⍝ 0 (scalars have 0 dimensions)

⍝ APL supports higher-rank (or "multi-dimensional") arrays; a rank 2 array is called a 
⍝ matrix, which can be constructed using dyadic ⍴ (reshape).
4 3 ⍴ ⍳12               ⍝  1  2  3
                        ⍝  4  5  6
                        ⍝  7  8  9
                        ⍝ 10 11 12

⍝ Values can be stored using ← 
mat ← 4 3 ⍴ ⍳12
≢⍴mat                   ⍝ 2

⍝ Let's calculate the mean!
A ← ⍳10
                   
⍝ APL also has higher-order functions, named operators. Which allow for
⍝ functional-style programming.

⍝ Sum the elements of A (/ is reduce, similar to Haskell's foldr1):
+/A                      ⍝ 55

⍝ Length of A:
≢A                       ⍝ 10

⍝ Mean:
(+/A) ÷ (≢A)             ⍝ 5.5

⍝ We can create a function using {} and ⍵ to reference the function's argument explicitly.
mean ← {(+/⍵)÷≢⍵}
mean A                   ⍝ 5.5

⍝ Functions juxtaposed together (f g h) create a "train" - this is a "3-train" or "fork":
mean ← +/÷≢ 
mean A                   ⍝ 5.5
⍝ (f g h)Y is precisely equal to (f Y) g (h Y)

⍝ Arrays can also contain other arrays and become "nested":
(1 2 3)(4 5 6)           ⍝ ┌─────┬─────┐
                         ⍝ │1 2 3│4 5 6│
                         ⍝ └─────┴─────┘

⍝ To apply a function to the elements of a nested array,
⍝ we use the operator ¨ (each, analogous to map):
+/¨(1 2 3)(4 5 6)        ⍝ 6 15

⍝ You can trade rank for nesting depth with ↓ (split):
↓3 3⍴⍳9                  ⍝ ┌─────┬─────┬─────┐
                         ⍝ │1 2 3│4 5 6│7 8 9│
                         ⍝ └─────┴─────┴─────┘

⍝ And trade depth for rank with ↑ (mix):
↑(1 2 3)(4 5 6)(7 8 9)   ⍝ 1 2 3
                         ⍝ 4 5 6
                         ⍝ 7 8 9
                         
⍝ You can join arrays with , (catenate):
1,3,5                    ⍝ 1 3 5 

⍝ Characters are deliminated with single quotes:
'a' 'b' 'c'              ⍝ abc

⍝ A string in APL is a character vector, not a fundamental type,
⍝ for convenience you can write them as normal:
'abc'                    ⍝ 'abc'

⍝ APL is not simply a math DSL, it's useful across multiple domains,
⍝ including text processing. For example, to create a split function:
    ','≠'comma,delimited,text' 
⍝ 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1

    ','⊢'comma,delimited,text'
⍝ comma,delimited,text

⍝ ⊆ (partition) returns a list of partitions as indicated by runs of 1s,
⍝ leaving out elements corresponding with 0s:
1 0 1 0 1 1 ⊆ 'Hello!'   ⍝ ┌─┬─┬──┐
                         ⍝ │H│l│o!│
                         ⍝ └─┴─┴──┘

    ','(≠⊆⊢)'comma,delimited,text'
⍝ ┌─────┬─────────┬────┐
⍝ │comma│delimited│text│
⍝ └─────┴─────────┴────┘

⍝ Monadic ⍨ (Self) takes a function and duplicates it's right argument
+⍨ 1                     ⍝ 2
⍝ Equivalent to 1 + 1

⍝ Dyadically, ⍨ swaps a function's arguments
10 ÷⍨ 5                   ⍝ 0.5

⍝ ∘. is an operator (composed of two glyphs) that returns the outer 
⍝ product of it's arguments:
∘.×⍨⍳5                   ⍝ 1  2  3  4  5
                         ⍝ 2  4  6  8 10
                         ⍝ 3  6  9 12 15
                         ⍝ 4  8 12 16 20
                         ⍝ 5 10 15 20 25
```

# Resources
- [xpqz's "Learning APL"](https://xpqz.github.io/learnapl/intro.html)
- [APL Wiki](https://aplwiki.com/wiki/Main_Page)
- [APL Chat rooms and forums](https://aplwiki.com/wiki/Chat_rooms_and_forums)
- [Awagga's Language Overview](https://awagga.github.io/dyalog/voc/)
