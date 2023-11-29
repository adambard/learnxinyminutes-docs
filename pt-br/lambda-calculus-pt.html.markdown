---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
    - ["Yan Hui Hang", "http://github.com/yanhh0"]
translators:
    - ["Gustavo Tramontin", "https://github.com/gustatramontin"]
lang: pt-br
---

# Cálculo Lambda

Cálculo Lambda (cálculo-λ), originalmente criada por
[Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church),
é a menor linguagem de programação do mundo.
Composta apenas por funções, sem números, texto, booleans, ou qualquer outro tipo, 
apesar dessa limitação, cálculo lambda é capaz de representar qualquer Máquina de Turing!

Cálculo lambda é composto por 3 elementos: **variáveis**, **funções** e **aplicações**.


| Nome      | Sintaxe                        | Exemplo   | Explicação                                 |
|-----------|--------------------------------|-----------|--------------------------------------------|
| Variável  | `<nome>`                       | `x`       | uma variável chamada "x"                   |
| Função    | `λ<parâmetro>.<corpo>`         | `λx.x`    | uma função com parâmetro "x" e corpo "x"   |
| Aplicação | `<função><variável ou função>` | `(λx.x)a` | aplicando a função "λx.x" ao argumento "a" |

A função mais simples é a função indentidade: `λx.x` equivalente a `f(x) = x`.
O primeiro "x" é o argumento da função, e o segundo o corpo da função.

## Variáveis Livres e Ligadas:

- Na função `λx.x`, "x" é uma variável ligada porque ela está 
no corpo e em um dos parâmetros da função.
- Na função `λx.y`, "y" é uma variável livre porque ela não foi definida anteriormente.

## Avaliação:

Avaliação é realizada por
[Redução-β](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction),
que é essencialmente substituição léxica

Ao avaliar `(λx.x)a`, todo "x" no corpo da função é substituído por "a".

- `(λx.x)a` avalia para: `a`
- `(λx.y)a` avalia para: `y`

Você ainda pode criar funções de ordem superior

- `(λx.(λy.x))a` avalia para: `λy.a`

Tradicionalmente funções no cálculo lambda possuem um único parâmetro, 
porém usando a técnina de [currying](https://en.wikipedia.org/wiki/Currying) 
podes criar funções com múltiplos argumentos.

- `(λx.λy.λz.xyz)` equivale a `f(x, y, z) = ((x y) z)`

Às vezes `λxy.<corpo>` é usado como notação para: `λx.λy.<corpo>`

----

É importante ressaltar que **cálculo lambda não tem números, carácteres, 
ou qualquer tipo que não seja uma função!**

## Lógica Booleana:

Cálculo lambda não tem booleans, valores lógicos de "verdade" e "falso".

No lugar temos:

`T` representado por: `λx.λy.x`

`F` representado por: `λx.λy.y`

* `T` e `F` para Verdade e Falso respectivamente.

Assim representamos os operadores lógicos:

`Não a` como: `λa.a F T`

`a E b` como: `λa.λb.a b F`

`a OU b` como: `λa.λb.a T b`

## Números:

Apesar do cálculo lambda não ter números, podemos representa-los usando [numerais Church](https://en.wikipedia.org/wiki/Church_encoding).

Para todo número n: <code>n = λf.f<sup>n</sup></code> assim:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

Para incrementar um numeral Church, 
usamos a função sucessora `S(n) = n + 1` definida como:

`S = λn.λf.λx.f((n f) x)`

Usando-a definimos a função soma:

`SOMA = λab.(a S)b`

**Desafio:** defina sua própria função de multiplicação!

## Ainda Menor: SKI, SK E Iota

### Cálculo Combinador SKI

Seja, S, K, I as funções seguintes:

`I x = x`

`k x y =  x`

`S x y z = x z (y z)`

Podemos converter uma expressão no cálculo lambda para uma no cálculo combinador SKI:

1. `λx.x = I`
2. `λx.c = Kc` desde que `x` não ocorra livre em `c`
3. `λx.(y z) = S (λx.y) (λx.z)`

Exemplo com numeral church 2:

`2 = λf.λx.f(f x)`

Para a parte interna `λx.f(f x)`:

```
  λx.f(f x)
= S (λx.f) (λx.(f x))          (caso 3)
= S (K f)  (S (λx.f) (λx.x))   (caso 2, 3)
= S (K f)  (S (K f) I)         (caso 2, 1)
```

Então:

```
  2
= λf.λx.f(f x)
= λf.(S (K f) (S (K f) I))
= λf.((S (K f)) (S (K f) I))
= S (λf.(S (K f))) (λf.(S (K f) I)) (caso 3)
```

Para o primeiro argumento `λf.(S (K f))`:

```
  λf.(S (K f))
= S (λf.S) (λf.(K f))       (caso 3)
= S (K S) (S (λf.K) (λf.f)) (caso 2, 3)
= S (K S) (S (K K) I)       (caso 2, 3)
```

Para o segundo argumento `λf.(S (K f) I)`:

```
  λf.(S (K f) I)
= λf.((S (K f)) I)
= S (λf.(S (K f))) (λf.I)             (caso 3)
= S (S (λf.S) (λf.(K f))) (K I)       (caso 2, 3)
= S (S (K S) (S (λf.K) (λf.f))) (K I) (caso 1, 3)
= S (S (K S) (S (K K) I)) (K I)       (caso 1, 2)
```

Juntando-os:

```
  2
= S (λf.(S (K f))) (λf.(S (K f) I))
= S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))
```

Expandindo isso, finalizamos com a mesma expressão para o numeral Church 2.

### Cálculo Combinador SK

O cálculo combinador SKI pode ser reduzido ainda mais. 
Ao notar que `I = SKK`, podemos remover o combinador I 
substituindo-o por `SKK`.

### Combinador Iota

Cálculo combinador SK ainda não é mínimo. Definindo:

```
ι = λf.((f S) K)
```

Temos:

```
I = ιι
K = ι(ιI) = ι(ι(ιι))
S = ι(K) = ι(ι(ι(ιι)))
```

## Para leituras mais avançadas:

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
2. [Cornell CS 312 Recitation 26: The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
3. [Wikipedia - Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
4. [Wikipedia - SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
5. [Wikipedia - Iota and Jot](https://en.wikipedia.org/wiki/Iota_and_Jot)
