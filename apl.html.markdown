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
⍝ Arrays are created by juxtaposing it's elements, also known as "Stranding".
2 3e7 ¯4 50.3           ⍝ ([2, 30000000, -4, 50.3] in Python) 

⍝ In APL, there's no order of operations; everything is parsed right-to-left.
1 + 2 × 3 ÷ 4 - 5       ⍝ ¯5
⍝ This is equivalent to 1 + (2 × (3 ÷ (4 - 5))).

⍝ Operations are applied array-wise.
1 2 3 4 × 5 6 7 8       ⍝ 5 12 21 32

⍝ APL features scalar-extension, a method of distributing a scalar
⍝ (applied with a function) across an argument.
2 × 1 2 3 4             ⍝ 2 4 6 8

⍝ All functions have single-argument (monadic) and dual-argument (dyadic) meanings.

⍝ For example, "×" applied between two arguments means multiply, but when applied to
⍝ a single argument, it returns the sign.
× ¯4 ¯2 0 2 4           ⍝ ¯1 ¯1 0 1 1

⍝ Values can be compared using these functions (1 means "true", 0 means "false").
10 20 30 = 10 20 99     ⍝ 1 1 0

10 20 30 < 10 20 99     ⍝ 0 0 1

⍝ "⍳n' (indices) returns a vector containing the first n natural numbers
⍳5                      ⍝ 1 2 3 4 5

⍝ All arrays have a *number* of dimensions, called rank. Scalars, have 0 dimensions,
⍝ and vectors have 1 dimension.

⍝ Monadic ⍴ (shape), gives you the dimensions back.
⍴1 2 3 4 5              ⍝ 5

⍝ You can retrieve rank by applying the functions "length of shape" to an array
≢⍴1 2 3 4 5             ⍝ 1 

⍝ APL supports higher-rank (or "multi-dimensional") arrays; a rank 2 array is called a 
⍝ matrix, which can be constructed using dyadic ⍴ (reshape).
4 3 ⍴ ⍳5                ⍝ 0 1 2
                        ⍝ 3 4 0
                        ⍝ 1 2 3
                        ⍝ 4 0 1

⍝ Let's calculate the mean!

⍝ Values can be stored using ← 
A ← ⍳10
                   
⍝ APL also has higher-order functions, named operators. Which allow for
⍝ functional-style programming.
⍝ Sum the elements of A (/ is reduce, similar to Haskell's foldr1).
+/A                      ⍝ 55

⍝ Length of A:
≢A                       ⍝ 10

⍝ Mean:
(+/A) ÷ (≢A)             ⍝ 5.5

⍝ We can create a function using {} and ⍵ to reference the function's argument explicitly
mean ← {(+/⍵)÷⍴⍵}
mean A                   ⍝ 5.5

⍝ Alternatively, you can make use of a "Tacit" or "Point-Free" style. These are functions
⍝ defined in terms of implicit arguments.
⍝ Functions juxtaposed together (f g h) create a "train" - this is a "3-train" or "fork".
mean ← +/÷≢ 
⍝ (f g h)Y is precisely equal to (f Y) g (h Y)

mean A                   ⍝ 5.5

```

# Resources
- [xpqz's "Learning APL"](https://xpqz.github.io/learnapl/intro.html)
- [APL Chat rooms and forums](https://aplwiki.com/wiki/Chat_rooms_and_forums)
