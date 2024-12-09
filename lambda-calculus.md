---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
    - ["Yan Hui Hang", "http://github.com/yanhh0"]
---

# Lambda Calculus

Lambda calculus (λ-calculus), originally created by 
[Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church),
is the world's smallest programming language.
Despite not having numbers, strings, booleans, or any non-function datatype,
lambda calculus can be used to represent any Turing Machine!

Lambda calculus is composed of 3 elements: **variables**, **functions**, and
**applications**.


| Name        | Syntax                             | Example   | Explanation                                   |
|-------------|------------------------------------|-----------|-----------------------------------------------|
| Variable    | `<name>`                           | `x`       | a variable named "x"                          |
| Function    | `λ<parameters>.<body>`             | `λx.x`    | a function with parameter "x" and body "x"    |
| Application | `<function><variable or function>` | `(λx.x)a` | calling the function "λx.x" with argument "a" |

The most basic function is the identity function: `λx.x` which is equivalent to
`f(x) = x`. The first "x" is the function's argument, and the second is the
body of the function.

## Free vs. Bound Variables:

- In the function `λx.x`, "x" is called a bound variable because it is both in
the body of the function and a parameter.
- In `λx.y`, "y" is called a free variable because it is never declared before hand.

## Evaluation:

Evaluation is done via 
[β-Reduction](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction),
which is essentially lexically-scoped substitution.

When evaluating the
expression `(λx.x)a`, we replace all occurrences of "x" in the function's body
with "a".

- `(λx.x)a` evaluates to: `a`
- `(λx.y)a` evaluates to: `y`

You can even create higher-order functions:

- `(λx.(λy.x))a` evaluates to: `λy.a`

Although lambda calculus traditionally supports only single parameter 
functions, we can create multi-parameter functions using a technique called 
[currying](https://en.wikipedia.org/wiki/Currying).

- `(λx.λy.λz.xyz)` is equivalent to `f(x, y, z) = ((x y) z)`

Sometimes `λxy.<body>` is used interchangeably with: `λx.λy.<body>`

----

It's important to recognize that traditional **lambda calculus doesn't have
numbers, characters, or any non-function datatype!**

## Boolean Logic:

There is no "True" or "False" in lambda calculus. There isn't even a 1 or 0.

Instead:

`T` is represented by: `λx.λy.x`

`F` is represented by: `λx.λy.y`

First, we can define an "if" function `λbtf` that
returns `t` if `b` is True and `f` if `b` is False

`IF` is equivalent to: `λb.λt.λf.b t f`

Using `IF`, we can define the basic boolean logic operators:

`a AND b` is equivalent to: `λab.IF a b F`

`a OR b` is equivalent to: `λab.IF a T b`

`NOT a` is equivalent to: `λa.IF a F T`

*Note: `IF a b c` is essentially saying: `IF((a b) c)`*

## Numbers:

Although there are no numbers in lambda calculus, we can encode numbers using
[Church numerals](https://en.wikipedia.org/wiki/Church_encoding).

For any number n: <code>n = λf.f<sup>n</sup></code> so:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

To increment a Church numeral,
we use the successor function `S(n) = n + 1` which is:

`S = λn.λf.λx.f((n f) x)`

Using successor, we can define add:

`ADD = λab.(a S)b`

**Challenge:** try defining your own multiplication function!

## Get even smaller: SKI, SK and Iota

### SKI Combinator Calculus

Let S, K, I be the following functions:

`I x = x`

`K x y =  x`

`S x y z = x z (y z)`

We can convert an expression in the lambda calculus to an expression
in the SKI combinator calculus:

1. `λx.x = I`
2. `λx.c = Kc` provided that `x` does not occur free in `c`
3. `λx.(y z) = S (λx.y) (λx.z)`

Take the church number 2 for example:

`2 = λf.λx.f(f x)`

For the inner part `λx.f(f x)`:

```
  λx.f(f x)
= S (λx.f) (λx.(f x))          (case 3)
= S (K f)  (S (λx.f) (λx.x))   (case 2, 3)
= S (K f)  (S (K f) I)         (case 2, 1)
```

So:

```
  2
= λf.λx.f(f x)
= λf.(S (K f) (S (K f) I))
= λf.((S (K f)) (S (K f) I))
= S (λf.(S (K f))) (λf.(S (K f) I)) (case 3)
```

For the first argument `λf.(S (K f))`:

```
  λf.(S (K f))
= S (λf.S) (λf.(K f))       (case 3)
= S (K S) (S (λf.K) (λf.f)) (case 2, 3)
= S (K S) (S (K K) I)       (case 2, 3)
```

For the second argument `λf.(S (K f) I)`:

```
  λf.(S (K f) I)
= λf.((S (K f)) I)
= S (λf.(S (K f))) (λf.I)             (case 3)
= S (S (λf.S) (λf.(K f))) (K I)       (case 2, 3)
= S (S (K S) (S (λf.K) (λf.f))) (K I) (case 1, 3)
= S (S (K S) (S (K K) I)) (K I)       (case 1, 2)
```

Merging them up:

```
  2
= S (λf.(S (K f))) (λf.(S (K f) I))
= S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))
```

Expanding this, we would end up with the same expression for the
church number 2 again.

### SK Combinator Calculus

The SKI combinator calculus can still be reduced further. We can
remove the I combinator by noting that `I = SKK`. We can substitute
all `I`'s with `SKK`.

### Iota Combinator

The SK combinator calculus is still not minimal. Defining:

```
ι = λf.((f S) K)
```

We have:

```
I = ιι
K = ι(ιI) = ι(ι(ιι))
S = ι(K) = ι(ι(ι(ιι)))
```

## For more advanced reading:

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
2. [Cornell CS 312 Recitation 26: The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
3. [Wikipedia - Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
4. [Wikipedia - SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
5. [Wikipedia - Iota and Jot](https://en.wikipedia.org/wiki/Iota_and_Jot)
