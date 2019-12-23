---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
translators:
    - ["Yvan Sraka", "https://github.com/yvan-sraka"]
lang: fr-fr
---

# Lambda-calcul

Le Lambda-calcul (λ-calcul), créé à l'origine par [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church), est le plus petit langage de programmation au monde. En dépit de ne pas avoir de nombres, de chaînes, de booléens, ou de tout type de données sans fonction, le lambda calcul peut être utilisé pour représenter n'importe quelle machine de Turing!

Le Lambda-calcul est composé de 3 éléments : **variables**, **fonctions** et **applications**.


| Nom         | Syntaxe                            | Exemple   | Explication                                       |
|-------------|------------------------------------|-----------|---------------------------------------------------|
| Variable    | `<nom>`                            | `x`       | une variable nommée "x"                           |
| Fonction    | `λ<paramètres>.<corps>`            | `λx.x`    | une fonction avec le paramètre "x" et le corps "x"|
| Application | `<fonction><variable ou function>` | `(λx.x)a` | appel de la fonction "λx.x" avec l'argument "a"   |

La fonction la plus fondamentale est la fonction identité: `λx.x` qui est équivalente à `f(x) = x`. Le premier "x" est l'argument de la fonction, et le second est le corps de la fonction.

## Variables libres et liées :

- Dans la fonction `λx.x`, "x" s'appelle une variable liée car elle est à la fois dans le corps de la fonction et l'un des paramètres.
- Dans `λx.y`, "y" est appelé une variable libre car elle n'a pas été déclarée plus tôt.

## Évaluation :

L'évaluation est réalisée par [β-Réduction](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction), qui est essentiellement une substitution lexicale.

Lors de l'évaluation de l'expression `(λx.x)a`, nous remplaçons toutes les occurrences de "x" dans le corps de la fonction par "a".

- `(λx.x)a` vaut après évaluation: `a`
- `(λx.y)a` vaut après évaluation: `y`

Vous pouvez même créer des fonctions d'ordre supérieur:

- `(λx.(λy.x))a` vaut après évaluation: `λy.a`

Bien que le lambda-calcul ne prenne traditionnellement en charge que les fonctions à un seul paramètre, nous pouvons créer des fonctions multi-paramètres en utilisant une technique appelée currying.

- `(λx.λy.λz.xyz)` est équivalent à `f(x, y, z) = x(y(z))`

Parfois, `λxy.<corps>` est utilisé de manière interchangeable avec: `λx.λy.<corps>`

----

Il est important de reconnaître que le lambda-calcul traditionnel n'a pas de nombres, de caractères ou tout autre type de données sans fonction!

## Logique booléenne :

Il n'y a pas de "Vrai" ou de "Faux" dans le calcul lambda. Il n'y a même pas 1 ou 0.

Au lieu:

`T` est représenté par: `λx.λy.x`

`F` est représenté par: `λx.λy.y`

Premièrement, nous pouvons définir une fonction "if" `λbtf` qui renvoie `t` si `b` est vrai et `f` si `b` est faux

`IF` est équivalent à: `λb.λt.λf.b t f`

En utilisant `IF`, nous pouvons définir les opérateurs logiques de base booléens:

`a AND b` est équivalent à: `λab.IF a b F`

`a OR b` est équivalent à: `λab.IF a T b`

`a NOT b` est équivalent à: `λa.IF a F T`

*Note: `IF a b c` est equivalent à : `IF(a(b(c)))`*

## Nombres :

Bien qu'il n'y ait pas de nombres dans le lambda-calcul, nous pouvons encoder des nombres en utilisant les [nombres de Church](https://en.wikipedia.org/wiki/Church_encoding).

Pour tout nombre n: <code>n = λf.f<sup>n</sup></code> donc:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

Pour incrémenter un nombre de Church, nous utilisons la fonction successeur `S(n) = n + 1` qui est:

`S = λn.λf.λx.f((n f) x)`

En utilisant `S`, nous pouvons définir la fonction `ADD`:

`ADD = λab.(a S)n`

**Défi:** essayez de définir votre propre fonction de multiplication!

## Pour aller plus loin :

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
2. [Cornell CS 312 Recitation 26: The Lambda Calculus](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
3. [Wikipedia - Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
