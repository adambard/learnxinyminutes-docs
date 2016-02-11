---
category: Algorithms & Data Structures
name: Asymptotic Notation
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["Claudson Martins", "https://github.com/claudsonm"]
lang: pt-br
---

# Notações Assintóticas

## O que são?

Notações assintóticas são linguagens que nos permitem analisar o tempo de
execução de um algoritmo pela identificação do seu comportamento de acordo com o
aumento do tamanho de sua entrada. Isso também é chamado de taxa de crescimento.
Será que o algoritmo de repente torna-se incrivelmente lento quando o tamanho da
entrada aumenta? Será que ele mantêm-se rápido na maior parte do tempo de
execução quando o tamanho da entrada aumenta? Notações assintóticas nos dão a
capacidade de responder essas questões.

## Existem alternativas para responder essas perguntas?

Uma forma seria contar o número de operações primitivas em diferentes tamanhos
de entrada. Embora seja uma solução válida, a quantidade de trabalho que isso
gera, até mesmo para simples algoritmos, não justifica sua aplicação.

Uma outra maneira seria medindo o tempo que um algoritmo leva para finalizar sua
execução para diferentes tamanhos de entrada. No entanto, a precisão e relatividade
(o tempo obtido seria apenas relativo à máquina em que estão sendo executados) 
deste método está ligado a variáveis do ambiente, tais como as especificações de
hardware do computador, poder de processamento, etc.

## Tipos de Notações Assintóticas

Na primeira seção deste documento descrevemos como uma notação assintótica
identifica o comportamento de um algoritmo de acordo com a mudança do tamanho
da entrada. Vamos imaginar um algoritmo como uma função f, n como o tamanho da
entrada, e f(n) sendo o tempo de execução. Portanto, para um determinado
algoritmo f, com o tamanho da entrada n, você obtém algum tempo de execução f(n).
Isto resulta em um gráfico onde o eixo Y é o tempo de execução, o eixo X é o
tamanho da entrada, e os pontos do gráfico são as quantidades de tempo para um
determinado tamanho de entrada.

Você pode rotular uma função, ou algoritmo, com uma notação assintótica de 
diferentes maneiras. Alguns exemplos são: você pode descrever um algoritmo pelo
seu melhor caso, pior caso, ou caso equivalente. O mais comum é avaliar um 
algoritmo pelo seu pior caso. Tipicamente não avalia-se pelo melhor caso pois
essas condições não são as que você está planejando. Um ótimo exemplo disso são
algoritmos de ordenação; especificamente, a adição de elementos a uma estrutura
de árvore. O melhor caso para a maioria dos algoritmos pode ser tão baixo quanto
uma única operação. No entanto, na maioria dos casos, o elemento que você está
adicionando precisará ser ordenado apropriadamente dentro da árvore, o que pode
significar verificar um ramo inteiro. Este é o pior caso, e é para ele que nos
planejamos.

### Tipos de funções, limites, e simplificação

```
Função Logarítmica - log n
Função Linear - an + b
Função Quadrática - an^2 + bn + c
Função Polinomial - an^z + . . . + an^2 + a*n^1 + a*n^0, onde z é alguma constante
Função Exponencial - a^n, onde a é alguma constante
```

Essas são algumas classificações básicas de funções de crescimento usadas em
várias notações. A lista começa com a função de mais lento crescimento (logarítmica,
tempo de execução mais rápido) e vai para a de crescimento mais rápido (exponencial,
tempo de execução mais lento). Note que 'n', ou a entrada, aumenta em cada uma
dessas funções, o resultado claramente aumenta muito mais rápido na quadrática,
polinomial, e exponencial, comparado à logarítmica e a linear.

Uma observação extramamente importante é que para as notações que estão para ser
discutidas você deve fazer o seu melhor para utilizar termos mais simples. Isso
significa desrespeitar constantes, e diminuir termos de ordem, porque como o
tamanho da entrada (ou n, em nosso exemplo f(n)) aumenta ao infinito (limites
matemáticos), os termos de ordem inferior e constantes têm de pouca a nenhuma
importância. Dito isso, se você possui constantes que são 2^9001, ou qualquer
outra quantidade ridícula inimaginável, perceba que a simplificação distorce a
precisão de sua notação.

Já que queremos a forma mais simples, vamos modificar nossa tabela um pouco...

```
Logarítmica - log n
Linear - n
Quadrática - n^2
Polinomial - n^z, onde z é alguma constante
Exponencial - a^n, onde a é alguma constante
```

### Big-O
Big-O, normalmente escrito O, é uma Notação Assintótica para o pior caso, ou
teto de crescimento para uma determinada função. Digamos que `f(n)` é o tempo
de execução do seu algoritmo, e que `g(n)` é uma complexibilidade de tempo
arbitrária que você está tentanto relacionar com o seu algoritmo. `f(n)` é 
O(g(n)), se para qualquer constante real c (c > 0), `f(n)` <= `c g(n)` para toda
entrada de tamanho n (n > 0).

*Examplo 1*

```
f(n) = 3log n + 100
g(n) = log n
```

É `f(n)` O(g(n))?
É `3 log n + 100` O(log n)?
Vamos olhar para a definição do Big-O.

```
3log n + 100 <= c * log n
```

Existe alguma constante c que satisfaz isto para todo n?

```
3log n + 100 <= 150 * log n, n > 2 (indefinido em n = 1)
```

Sim! A definição do Big-O foi cumprida, portanto `f(n)` é O(g(n)).

*Examplo 2*

```
f(n) = 3*n^2
g(n) = n
```

`f(n)` é O(g(n))?
`3 * n^2` é O(n)?
Vamos olhar para a definição do Big-O.

```
3 * n^2 <= c * n
```

Existe alguma constante c que satisfaz isto para todo n?
Não, não há. `f(n)` NÃO é O(g(n)).

### Big-Omega
Big-Omega, normalmente escrito Ω, é uma Notação Assintótica para o melhor caso,
ou um piso de crescimento para uma determinada função.

`f(n)` é Ω(g(n)), para qualquer constante real c(c > 0), `f(n)` é >= `c g(n)`
para toda entrada de tamanho n (n > 0).

Sinta-se livre para visitar outras fontes adicionais para exemplos disso. Big-O
é a notação primária utilizada para tempo de complexidade de algoritmos.

### Notas Finais
É difícil manter curto este tipo de assunto, e você definitivamente deve dar uma
olhada nos livros e nos conteúdos online listados. Eles abordam em uma profundidade
muito maior, com definições e exemplos. Mais onde x='Algorithms & Data Structures'
está a caminho; teremos um documento com análise de exemplos de códigos em breve.

## Livros

* [Algoritmos](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Design de Algoritmos](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Material Online

* [MIT](http://web.mit.edu/16.070/www/lecture/big_o.pdf)
* [KhanAcademy](https://www.khanacademy.org/computing/computer-science/algorithms/asymptotic-notation/a/asymptotic-notation)
