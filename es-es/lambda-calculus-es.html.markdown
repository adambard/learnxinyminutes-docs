---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
    - ["Yan Hui Hang", "http://github.com/yanhh0"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
---

# Cálculo Lambda

Cálculo Lambda (Cálculo-λ), originalmente creado por 
[Alonzo Church](https://es.wikipedia.org/wiki/Alonzo_Church),
es el lenguaje de programación más pequeño del mundo.
A pesar de no tener números, cadenas, valores booleanos o cualquier 
tipo de datos no funcional, el cálculo lambda se puede utilizar para 
representar cualquier máquina de Turing.

El cálculo lambda se compone de 3 elementos: **variables**, **funciones** y
**aplicaciones**.

| Nombre        | Sintaxis                             | Ejemplo   | Explicación                                   |
|-------------|------------------------------------|-----------|-----------------------------------------------|
| Variable    | `<nombre>`                           | `x`       | una variable llamada "x"                          |
| Función    | `λ<parámetro>.<cuerpo>`             | `λx.x`    | una función con parámetro "x" y cuerpo "x"    |
| Aplicación | `<función><variable o función>` | `(λx.x)a` | llamando a la función "λx.x" con el argumento "a" |

La función más básica es la función de identidad: `λx.x` que es equivalente a
`f(x) = x`. La primera "x" es el argumento de la función y la segunda es el 
cuerpo de la función.

## Variables Libres vs. Enlazadas:

- En la función `λx.x`, "x" se llama una variable enlazada porque está tanto en
 el cuerpo de la función como en el parámetro.
- En `λx.y`, "y" se llama variable libre porque nunca se declara de antemano.

## Evaluación:

Evaluación se realiza a través de
[β-Reduction](https://es.wikipedia.org/wiki/C%C3%A1lculo_lambda#%CE%B2-reducci%C3%B3n),
que es, esencialmente, sustitución de ámbito léxico.

Al evaluar la expresión `(λx.x)a`, reemplazamos todas las ocurrencias de "x" 
en el cuerpo de la función con "a".

- `(λx.x)a` evalúa a: `a`
- `(λx.y)a` evalúa a: `y`

Incluso puedes crear funciones de orden superior:

- `(λx.(λy.x))a` evalúa a: `λy.a`

Aunque el cálculo lambda tradicionalmente solo admite funciones 
de un solo parámetro, podemos crear funciones multiparamétricas usando 
una técnica llamada [Currificación](https://es.wikipedia.org/wiki/Currificación).

- `(λx.λy.λz.xyz)` es equivalente a `f(x, y, z) = ((x y) z)`

Algunas veces `λxy.<cuerpo>` es usado indistintamente con: `λx.λy.<cuerpo>`

----

Es importante reconocer que el cálculo lambda tradicional **no tiene números, 
caracteres ni ningún tipo de datos que no sea de función.**

## Lógica Booleana:

No hay "Verdadero" o "Falso" en el cálculo lambda. Ni siquiera hay un 1 o un 0.

En vez:

`T` es representado por: `λx.λy.x`

`F` es representado por: `λx.λy.y`

Primero, podemos definir una función "if" `λbtf` que devuelve 
`t` si `b` es Verdadero y `f` si `b` es Falso

`IF` es equivalente a: `λb.λt.λf.b t f`

Usando `IF` podemos definir los operadores lógicos booleanos básicos:

`a AND b` es equivalente a: `λab.IF a b F`

`a OR b` es equivalente a: `λab.IF a T b`

`a NOT b` es equivalente a: `λa.IF a F T`

*Note: `IF a b c` es esencialmente diciendo: `IF((a b) c)`*

## Números:

Aunque no hay números en el cálculo lambda, podemos codificar números usando 
[Númeral de Church](https://en.wikipedia.org/wiki/Church_encoding).

Para cualquier número n: <code>n = λf.f <sup> n </sup></code> así:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

Para incrementar un númeral de Church, usamos la función sucesora 
`S(n) = n + 1` que es:

`S = λn.λf.λx.f((n f) x)`

Usando el sucesor, podemos definir AGREGAR:

`AGREGAR = λab.(a S)n`

**Desafío:** intenta definir tu propia función de multiplicación!

## Vamos más pequeño: SKI, SK y Iota

### Combinador de SKI

Sean S, K, I las siguientes funciones:

`I x = x`

`K x y =  x`

`S x y z = x z (y z)`

Podemos convertir una expresión en el cálculo lambda en una expresión 
en el cálculo del combinador de SKI:

1. `λx.x = I`
2. `λx.c = Kc`
3. `λx.(y z) = S (λx.y) (λx.z)`

Tome el número 2 de Church por ejemplo:

`2 = λf.λx.f(f x)`

Para la parte interior `λx.f(f x)`:
```
  λx.f(f x)
= S (λx.f) (λx.(f x))          (case 3)
= S (K f)  (S (λx.f) (λx.x))   (case 2, 3)
= S (K f)  (S (K f) I)         (case 2, 1)
```

Así que:
```
  2
= λf.λx.f(f x)
= λf.(S (K f) (S (K f) I))
= λf.((S (K f)) (S (K f) I))
= S (λf.(S (K f))) (λf.(S (K f) I)) (case 3)
```

Para el primer argumento `λf.(S (K f))`:
```
  λf.(S (K f))
= S (λf.S) (λf.(K f))       (case 3)
= S (K S) (S (λf.K) (λf.f)) (case 2, 3)
= S (K S) (S (K K) I)       (case 2, 3)
```

Para el segundo argumento `λf.(S (K f) I)`:
```
  λf.(S (K f) I)
= λf.((S (K f)) I)
= S (λf.(S (K f))) (λf.I)             (case 3)
= S (S (λf.S) (λf.(K f))) (K I)       (case 2, 3)
= S (S (K S) (S (λf.K) (λf.f))) (K I) (case 1, 3)
= S (S (K S) (S (K K) I)) (K I)       (case 1, 2)
```

Uniéndolos:
```
  2
= S (λf.(S (K f))) (λf.(S (K f) I))
= S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))
```

Al expandir esto, terminaríamos con la misma expresión para el número 2 de Church nuevamente.

### Cálculo del combinador SKI

El cálculo del combinador SKI puede reducirse aún más. Podemos eliminar 
el combinador I observando que `I = SKK`. Podemos sustituir 
todos los 'I' con `SKK`.

### Combinador Iota

El cálculo del combinador SK todavía no se encuentra en su expresión mínima. 
Definiendo:

```
ι = λf.((f S) K)
```

Tenemos que:

```
I = ιι
K = ι(ιI) = ι(ι(ιι))
S = ι(K) = ι(ι(ι(ιι)))
```

## Para una lectura más avanzada:

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
2. [Cornell CS 312 Recitation 26: The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
3. [Wikipedia - Lambda Calculus](https://es.wikipedia.org/wiki/Cálculo_lambda)
4. [Wikipedia - SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
5. [Wikipedia - Iota and Jot](https://en.wikipedia.org/wiki/Iota_and_Jot)
