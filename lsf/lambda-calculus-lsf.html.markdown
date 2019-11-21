---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
translators:
    - ["Victore Leve", "https://github.com/AcProIL"]
lang: lsf
---

# Calculo λ

Calculo lambda, creato principto per Alonzo Church, es lingua de programmatura
computatro maximo parvo. Quamquam non habe numero, serie de charactere vel ullo
typo de data non functionale, id pote repraesenta omne machina de Turing.

Tres elemento compone calculo lambda: **quantitate variabile** (q.v.),
**functione** et **applicatione**.

| Elemento             | Syntaxe                           | Exemplo   |
|----------------------|-----------------------------------|-----------|
| Quantitate variabile | `<nomine>`                        | `x`       |
| Functione            | `λ<parametro>.<corpore>`          | `λx.x`    |
| Applicatione         | `<functione><q.v. aut functione>` | `(λx.x)a` |

Functione fundamentale es identitate: `λx.x` cum argumento primo `x` et cum
corpore secundo `x`. In mathematica, nos scribe `id: x↦x`.

## Quantitate variabile libero et ligato

* In functione praecedente, `x` es q.v. ligato nam id es et in copore et
  argumento.
* In `λx.y`, `y` es q.v. libero nam non es declarato ante.

## Valutatione

Valutatione es facto per reductione beta (reductione β) que es essentialiter 
substitutione lexicale.

Dum valutatione de formula `(λx.x)a`, nos substitue omne evento de `x` in
corpore de functione pro `a`.

* `(λx.x)a` vale `a`
* `(λx.y)a` vale `y`

Pote etiam crea functione de ordine supero: `(λx.(λy.x))a` vale `λy.a`.

Etsi calculo lambda solo tracta functione de uno parametro, nos pote crea
functione cum plure argumento utente methodo de Curry: `λx.(λy.(λz.xyz))`
es scriptura informatica de formula mathematico `f: x, y, z ↦ x(y(z)))`.

Ergo, interdum, nos ute `λxy.<corpore>` pro `λx.λy.<corpore>`.

## Arithmetica

### Logica de Boole

Es nec numero nec booleano in calculo lambda.

* «vero» es `v = λx.λy.x`
* «falso» es `f = λx.λy.y`

Primo, nos pote defini functione «si t tunc a alio b» per `si = λtab.tab`.
Si `t` es vero, valutatione da `(λxy.x) a b` id es `a`. Similiter si `t` es
falso, nos obtine `b`.

Secundo, nos pote defini operatore de logica:

* «a et b» es `et = λa.λb.si a b f`
* «a vel b» es `vel = λa.λb.si a t b`
* «non a» es `non = λa.si a f t`

### Numeros

Nos pone:

* `0 = λf.λx.x` (`0: f↦id`)
* `1 = λf.λx.f x` (`1: f↦f`)
* `2 = λf.λx.f(f x)` (`2: f↦f⚬f`)

Cum mente generale, successore de numero `n` es `S n = λf.λx.f((n f) x)`
(`n+1: f↦f⚬fⁿ`). Id es **`n` est functione que da `fⁿ` ex functione `f`**.

Postremo additione es `λab.(a S)b`

## Ut progrede

### In lingua anglo

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf) per Raúl Roja
2. [The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html), CS 312 Recitation 26
