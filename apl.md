---
language: APL
contributors:
    - ["nooodl", "https://github.com/nooodl"]
filename: learnapl.apl
---

```apl
⍝ Comments in APL are prefixed by ⍝.

⍝ A list of numbers. (¯ is negative)
2 3e7 ¯4 50.3

⍝ An expression, showing some functions. In APL, there's
⍝ no order of operations: everything is parsed right-to-
⍝ left. This is equal to 5 + (4 × (2 ÷ (5 - 3))) = 9:
5 + 4 × 2 ÷ 5 - 3        ⍝ 9

⍝ These functions work on lists, too:
1 2 3 4 × 5              ⍝ 5 10 15 20
1 2 3 4 × 5 6 7 8        ⍝ 5 12 21 32

⍝ All functions have single-argument and dual-argument
⍝ meanings. For example, "×" applied to two arguments
⍝ means multiply, but when applied to only a right-hand
⍝ side, it returns the sign:

× ¯4 ¯2 0 2 4            ⍝ ¯1 ¯1 0 1 1

⍝ Values can be compared using these operators (1 means
⍝ "true", 0 means "false"):

10 20 30 = 10 20 99      ⍝ 1 1 0

10 20 30 < 10 20 99      ⍝ 0 0 1

⍝ "⍳n" returns a vector containing the first n naturals.
⍝ Matrices can be constructed using ⍴ (reshape):
4 3 ⍴ ⍳5                 ⍝ 0 1 2
                         ⍝ 3 4 0
                         ⍝ 1 2 3
                         ⍝ 4 0 1

⍝ Single-argument ⍴ gives you the dimensions back:
⍴ 4 3 ⍴ ⍳5               ⍝ 4 3

⍝ Values can be stored using ←. Let's calculate the mean
⍝ value of a vector of numbers:
A ← 10 60 55 23

⍝ Sum of elements of A (/ is reduce):
+/A                      ⍝ 148

⍝ Length of A:
⍴A                       ⍝ 4

⍝ Mean:
(+/A) ÷ (⍴A)             ⍝ 37

⍝ We can define this as a function using {} and ⍵:
mean ← {(+/⍵)÷⍴⍵}
mean A                   ⍝ 37
```

## Further Reading

- [APL Wiki](https://aplwiki.com/)
- An older version of APL book by the creator: [Kenneth Iverson - A Programming Language](https://www.softwarepreservation.org/projects/apl/Books/APROGRAMMING%20LANGUAGE/view)
- Additional Books: [APL Books](https://aplwiki.com/wiki/Books)
