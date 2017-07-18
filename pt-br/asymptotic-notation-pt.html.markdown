---
category: Algorithms & Data Structures
name: Asymptotic Notation
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["João Farias", "https://github.com/JoaoGFarias"]
lang: pt-br
---

# Notação Assintótica

## O que é?

Notação Assintótica é uma linguagem que nos permite analisar o tempo de execução
 de um algoritmo através da indentificação de seu comportamento com o
 crescimento da entrada oferecida. Isso também é conhecido como taxa de
 crescimento do algoritmo. O algoritmo de repente torna-se lento quando o
 tamanho da entrada cresce? O algoritmo mantém, em geral, seu tempo de execução
 rápido mesmo com aumento da entrada? Notação Assintótica nos dá a habilidade de
 responder estas questões.

## Quais são as alternativas para responder a estas questões?

Um modo seria contar o número de operações primitivas com diferentes tamanhos de
 entrada. Apesar desta ser uma solução válida, o trabalho que ela requer, mesmo para algoritmos simples, não a justifica.

 Outro modo é fisicamente medir a quantidade de tempo que um algoritmo requer
 para terminar com diferentes tamanhos de entrada. Entretanto, a precisão e
 relatividade (tempo obtido seria relativo apenas à máquina onde ocorreu a
   execução) deste método está limitado a variáveis de ambiente, como hardware,
   poder de processamento, etc.

## Tipos de Notação Assintótica

Na primeira seção desse documento, descrevemos como Notação Assintótica identifica o comportamento de um algoritmo
 a medida que o tamanho da entrada cresce. Imaginemos um algoritmo como uma função
 *f*, *n* como o tamanho da entrada e *f(n)* sendo o tempo de execução. Então,
 para dado algoritmo *f*, com entrada de tamanho *n*, você terá tempo de execução
 *f(n)*. Isto resulta em um gráfico onde a coordernada Y é o tempo de execução
, a coordernada X representa o tamanho da entrada e os pontos representao o tempo
de execução para dado tamanho de entrada.

Você pode representar a função, ou o algoritmo, com Notação Assintótica de várias
maneiras. Você pode representar um algoritmo nas formas de Melhor Caso, Pior Caso
ou Caso Médio.
A maneira mais comum de analisar um algoritmo é pelo Pior Caso. Você tipicamente
não avalia o melhor caso, porque essas condições não são atingidas com frequência.
Um bom exemplo disto seria em algoritmos de ordenação; especificamente, na adição
de elementos à árvores. O melhor caso na maioria de algoritmos pode ser de apenas
uma operação. Entretanto, na maioria dos casos, o elemento a ser adicionado terá
que percorrer a árvore de forma apropriada, o que pode causar a analise de um
ramo inteiro.
Este é o pior caso, e isto é o que você está se preparando.

### Tipos de funções, limites e simplificação

```
Função Logarítmica - log n
Função Linear - an + b
Função Quadrática - an^2 + bn + c
Função Polinomial - an^z + . . . + an^2 + a*n^1 + a*n^0, onde *z* é uma constante
Função Exponencial -  a^n, onde a é alguma constante
```
Estas são as funções básicas de crescimento usadas em várias notações. A lista
 começa com a de crescimento mais lento (logarítima, a de execução mais rápida)
e segue para a de crescimento mais rápido (exponencial, de execução mais lenta).
Repare que enquando *n*, a entrada, cresce, cada uma dessas funções cresce mais
rápido que quadrático, polinimial e exponencial, comparadas com logaritma e linear.

Uma nota extremamente importante para notações é tentar usar os termos mais simples.
Isto significa descartar constantes e termos de ordem mais baixa, pois quando o
tamanho da entrada cresce para o infinito (limites matemáticos), os termos de ordem
mais baixa e constantes tornam-se irrelevantes. Por exemplo, se você tiver uma
constante muito grande, 2^9001, a simplificação não afeterá sua notação.

Já que queremos as formas mais simples, mudemos nossa tabela um pouco...

```
Função Logarítmica - log n
Função Linear - n
Função Quadrática - n^2
Função Polinomial - n^z, onde *z* é uma constante
Função Exponencial - a^n, onde *a* é uma constante
```

### Big-O

Big-O, também escrita como O, é uma Notação Assintótica para o pior caso. Digamos
*f(n)* seja o tempo de exeução de um algoritmo e *g(n)) um tempo de complexidade
arbritário que você quer relacionar com seu algoritmo. *f(n)* é O(g(n)), se, para
quando constante real c (c > 0), *f(n)* <= *c g(n)* para todo tamanho de entrada
n (n > 0).


*Exemplo 1*

```
f(n) = 3log n + 100
g(n) = log n
```

`f(n)` é O(g(n))?

`3 log n + 100` é  O(log n)?

Vejamos a definição de Big-O:

```
3log n + 100 <= c * log n
```

Há alguma constante c que satisfaça a definição para todo n?

```
3log n + 100 <= 150 * log n, n > 2 (Indefinido em n = 1)
```

Sim! A definição de Big-I for atentida, portante `f(n)` é `O(g(n))`.

*Exemplo 2*

```
f(n) = 3*n^2
g(n) = n
```

`f(n)` é O(g(n))?

`3 * n^2` é O(n)?
Vejamos a definição de Big-O:

```
3 * n^2 <= c * n
```

Há alguma constante c que satisfaça a definição para todo n?

Não, não há. `f(n)` não é O(g(n)).

### Big-Omega
Big-Omega, também escrita como Ω, é uma Notação Assintótica para o melhor caso.

`f(n)`é Ω(g(n)), se para qualquer constante real c (c > 0), `f(n)` é >= `c g(n)` para todo tamanho de entrada n (n > 0).

Sinta-se livre para adicionar mais exemplos. Big-O é a notação primária usada para medir complexidade de algoritmos.

### Notas Finais
É difícil manter esse tipo de tópico curto e você deveria ler os livros e artigos listados abaixo. Eles cobrem muito mais profundamente definições e exemplos. Mais x='Algoritms & Data Structures' virá; teremos um documento sobre analisar código em breve.

## Livros

* [Algorithms](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Algorithm Design](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Artigos Online

* [MIT](http://web.mit.edu/16.070/www/lecture/big_o.pdf)
* [KhanAcademy](https://www.khanacademy.org/computing/computer-science/algorithms/asymptotic-notation/a/asymptotic-notation)
