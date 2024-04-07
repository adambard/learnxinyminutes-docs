---
category: Algorithms & Data Structures
name: Set theory
lang: pt-br
contributors:
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
translators:
    - ["Bárbara Luz", "https://github.com/barbluz"]
---

Teoria de conjuntos é uma área da matemática que estuda conjuntos, suas operações e propriedades.
- Um conjunto é uma coleção de itens disjuntos.

## Símbolos básicos

### Operações
- a operação de união `∪`, significa "ou"
- a operação de interseção `∩`, que significa "e"
- a operação de exclusão `\`, significa "sem" ou "menos"
- a operação de conjunto complementar `'`, que significa "o inverso de"
- a operação de produto cartesiano `×`,que significa "o produto cartesiano de"

### Outros símbolos
- `:` ou `|`, símbolos que significam "tal que"
- o símbolo de pertencimento `∈`, que significa "pertence a"
- o símbolo `⊆`, que significa "subconjunto de" (neste caso, o subconjunto pode ser igual ao conjunto)
- o símbolo `⊂`, que significa "subconjunto próprio" (neste caso, o subconjunto não pode ser igual ao conjunto)

### Conjuntos canônicos
- `∅`, o conjunto vazio, isto é, o conjunto que não possui itens
- `ℕ`, o conjunto de todos os números naturais
- `ℤ`, o conjunto de todos os números inteiros
- `ℚ`, o conjunto de todos os números racionais
- `ℝ`, o conjunto de todos os números reais

Existem algumas ressalvas sobre os conjuntos canônicos:
- Apesar de o conjunto vazio não conter itens, o conjunto vazio é subconjunto de si mesmo (e portanto de todos os outros conjuntos)
- Matemáticos geralmente não concordam sobre o zero ser um número natural e os livros tipicamente explicitam se o autor considera ou não o zero como um número natural


### Cardinalidade
A cardinalidade (ou tamanho) de um conjunto é determinado pela quantidade de itens no conjunto. O operador de cardinalidade é `|...|`

Por exemplo, se `S = {1, 2, 4}`, então `|S| = 3`.

### O Conjunto Vazio
- o conjunto vazio pode ser contruído em notação de conjuntos utilizando condições impossíveis, como por exemplo: `∅ = { x : x ≠ x }`, ou `∅ = { x : x ∈ N, x < 0 }`
- o conjunto vazio é sempre único (ou seja, existe um e apenas um conjunto vazio)
- o conjunto vazio é subconjunto de todos os conjuntos
- a cardinalidade do conjunto vazio é `0`, ou seja, `|∅| = 0`.

## Representando conjuntos

### Definição Literal
Um conjunto pode ser contruído literalmente fornecendo uma lista completa dos itens contigos no conjunto. Por exemplo `S = { a, b, c, d }`

Listas longas podem ser encurtadas com reticências, desde que o contexto seja claro. Por exemplo, `E = { 2, 4, 6, 8, ... }` é claramente o conjunto de todos os números pares, contendo um número infinito de objetos, embora só tenhamos escrito explicitamente quatro deles.

### Definição por compreensão
Conjuntos também podem ser descritos de uma maneira mais descritiva, baseando-se em sujeito e predicado, de forma tal que `S = {sujeito : predicado}`. Por exemplo:

```
A = { x : x é uma vogal } = { a, e, i, o, u }         (lê-se x, tal que x é uma vogal)
B = { x : x ∈ N, x < 10 } = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }
C = { x : x = 2k, k ∈ N } = { 0, 2, 4, 6, 8, ... }
```

Ou pode-se também aplicar uma função ao sujeito, ex:

```
D = { 2x : x ∈ N } = { 0, 2, 4, 6, 8, ... }
```

## Relações

### Pertencimento
- Se um valor `a` está contido num conjunto `A`, então dizemos que `a` pertence a `A` e denotamos por `a ∈ A`
- Se o valor `a` não está contido no conjunto `A`, então dizemos que `a` não pertence a `A` e denotamos por `a ∉ A`

### Igualdade
- Se dois conjuntos contém os mesmos itens, então dizemos que os conjuntos são iguals, ex. `A = B`
- A ordenação não importa quando vamos avaliar a igualdade, ex: `{ 1, 2, 3, 4 } = { 2, 3, 1, 4 }`
- Conjuntos são disjuntos, o que significa que os elementos não podem ser repetidos, ex: `{ 1, 2, 2, 3, 4, 3, 4, 2 } = { 1, 2, 3, 4 }`
- Dois conjuntos `A` e `B` são iguais se, e somente se, `A ⊆ B` e `B ⊆ A`

### Conjuntos especiais
O Conjunto das Partes
- Seja `A` um conjunto qualquer. O conjunto que contém todos os possíveis subconjuntos de `A` é chamado "conjunto das partes" e é denotado como `P(A)`. Se o conjunto `A` contém `n` elementos, então o conjunto das partes conterá `2^n` elementos.

```
P(A) = { x : x ⊆ A }
```

## Operações entre dois conjuntos

### União
Dados dois conjuntos `A` e `B`, a união entre esses dois conjuntos são os itens que aparecem em `A` ou em `B`, denotado por `A ∪ B`.

```
A ∪ B = { x : x ∈ A ∪ x ∈ B }
```

### Interseção
Dados dois conjuntos `A` e `B`, a interseção entre esses dois conjuntos são os itens que aparecem em `A` e em `B`, denotado por `A ∩ B`.

```
A ∩ B = { x : x ∈ A, x ∈ B }
```

### Diferença
Dados dois conjuntos `A` e `B`, o conjunto da diferença entre `A` e `B` são todos os itens de `A` que não pertencem a `B`.

```
A \ B = { x : x ∈ A, x ∉ B }
```

### Diferença simétrica
Dados dois conjuntos `A` e `B`, a diferença simétrica são todos os itens entre `A` e `B` que não aparecem na interseção desses dois conjuntos.

```
A △ B = { x : ((x ∈ A) ∩ (x ∉ B)) ∪ ((x ∈ B) ∩ (x ∉ A)) }

A △ B = (A \ B) ∪ (B \ A)
```

### Produto Cartesiano
Dados dois conjuntos `A` e `B`, o produto cartesiano de `A` e `B` consiste no conjunto contendo todas as combinações dos itens de `A` e `B`.

```
A × B = { (x, y) | x ∈ A, y ∈ B }
```


