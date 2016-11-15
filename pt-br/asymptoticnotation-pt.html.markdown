---
category: Algorithms & Data Structures
name: Asymptotic Notation
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators: 
    - ["Carolina Knoll", "http://github.com/carolinaknoll"]
lang: pt-br
---

# Aprenda X em Y minutos
## Onde X=Notação Assintótica

# Notações Assintóticas
## O que são?

Notações assintóticas são notações matemáticas que nos permitem analisar tempo de execução  
de um algoritmo, identificando o seu comportamento de acordo como o tamanho de entrada para  
o algoritmo aumenta. Também é conhecido como taxa de "crescimento" de um algoritmo. O algoritmo  
simplesmente se torna incrivelmente lento conforme o seu tamanho aumenta? Será que pode-se na  
maior parte manter o seu tempo de execução rápido mesmo quando o tamanho de entrada aumenta?  
A notação assintótica nos dá a capacidade de responder a essas perguntas. 

## Além desta, existem outras alternativas para responder a essas perguntas?

Uma forma seria a de contar o número de operações primitivas em diferentes tamanhos de entrada. 
Embora esta seja uma solução válida, a quantidade de trabalho necessário, mesmo para algoritmos 
simples, não justifica a sua utilização.

Outra maneira é a de medir fisicamente a quantidade de tempo que leva para se executar um algoritmo 
de diferentes tamanhos. No entanto, a precisão e a relatividade (já que tempos obtidos só teriam 
relação à máquina em que eles foram testados) deste método estão ligadas a variáveis ambientais, 
tais como especificações de hardware, poder de processamento, etc.

## Tipos de Notação Assintótica

Na primeira seção deste documento nós descrevemos como uma notação assintótica identifica o comportamento  
de um algoritmo como as alterações de tamanho de entrada (input). Imaginemos um algoritmo como uma função  
f, n como o tamanho da entrada, e f (n) sendo o tempo de execução. Assim, para um determinado algoritmo f,  
com tamanho de entrada n você obtenha algum tempo de execução resultante f (n). Isto resulta num gráfico,  
em que o eixo Y representa o tempo de execução, o eixo X é o tamanho da entrada, e os pontos marcados são  
os resultantes da quantidade de tempo para um dado tamanho de entrada.

Pode-se rotular uma função ou algoritmo com uma notação assintótica de diversas maneiras diferentes.  
Dentre seus exemplos, está descrever um algoritmo pelo seu melhor caso, pior caso, ou caso equivalente.  
O mais comum é o de analisar um algoritmo pelo seu pior caso. Isso porque você normalmente não avaliaria  
pelo melhor caso, já que essas condições não são as que você está planejando. Um bom exemplo disto é o de  
algoritmos de ordenação; especificamente, a adição de elementos a uma estrutura de tipo árvore. O melhor  
caso para a maioria dos algoritmos pode ser tão simples como uma única operação. No entanto, na maioria   
dos casos, o elemento que você está adicionando terá de ser ordenado de forma adequada através da árvore,  
o que poderia significar a análise de um ramo inteiro. Este é o pior caso, e é por ele que precisamos seguir.  

### Tipos de funções, limites, e simplificação

```
Função Logaritmica - log n  
Função Linear - an + b  
Função Quadrática - an^2 + bn + c  
Função Polinomial - an^z + . . . + an^2 + a*n^1 + a*n^0, onde z é uma constante  
Função Exponencial - a^n, onde a é uma constante  
```

Estas são algumas classificações básicas de crescimento de função usados em várias notações. A lista  
começa com a função crescimento mais lento (logarítmica, com tempo de execução mais rápido) e vai até  
a mais rápida (exponencial, com tempo de execução mais lento). Observe que 'n', ou nossa entrada,  
cresce em cada uma dessas funções, e o resultado claramente aumenta muito mais rapidamente em função  
quadrática, polinomial e exponencial, em comparação com a logarítmica e a linear.

Uma observação de boa importância é que, para as notações a serem discutidas, deve-se fazer o melhor  
para utilizar termos mais simples. Isto significa desrespeitar constantes, e simplificar termos de  
ordem, porque, como o tamanho da entrada (ou n no nosso f (n) exemplo) aumenta infinitamente (limites  
matemáticos), os termos em ordens mais baixas e constantes são de pouca ou nenhuma importância. Dito  
isto, se você possui constantes com valor 2^9001, ou alguma outra quantidade ridícula, inimaginável,  
perceberá que a simplificação distorcerá a precisão de sua notação.

Já que nós queremos a forma mais simples, vamos modificar nossas funções um pouco.

```
Logaritmica - log n  
Linear - n  
Quadrática - n^2  
Polinomial - n^z, onde z é uma constante  
Exponencial - a^n, onde a é uma constante
```

### O Grande-O

Grande-O, geralmente escrita como O, é uma Notação Assintótica para o pior caso para uma dada função. Digamos  
que `f(n)` é o tempo de execução de seu algoritmo, e `g(n)` é uma complexidade de tempo arbitrário que você está  
tentando se relacionar com o seu algoritmo. `f(n)` será O(g(n)), se, por qualquer constante real c (c > 0),  
`f(n)` <= `c g(n)` para cada tamanho de entrada n (n > 0).  

*Exemplo 1*

```
f(n) = 3log n + 100  
g(n) = log n
```

É `f(n)` um O(g(n))?  
É 3 `log n + 100` igual a O(log n)?  
Vamos checar na definição de Grande-O.  

```
3log n + 100 <= c * log n
```

Existe alguma constante c que satisfaça isso para todo n?

```
3log n + 100 <= 150 * log n, n > 2 (indefinido em n = 1)
```

Sim! A definição de Grande-O foi satisfeita. Sendo assim, `f(n)` é O(g(n)).

*Exemplo 2*

```
f(n) = 3 * n^2
g(n) = n
```

É `f(n)` um O(g(n))? 
É `3 * n^2` um O(n)? 
Vamos ver na definição de Grande-O.

```
3 * n^2 <= c * n
```

Existe alguma constante que satisfaça isso para todo n? 
Não, não existe. `f(n)` NÃO É O(g(n)).

### Grande-Omega

Grande-Omega, comumente escrito como Ω, é uma Notação Assintótica para o melhor caso, ou  
uma taxa de crescimento padrão para uma determinada função.

`f(n)` é Ω(g(n)), se, por qualquer constante c real (c > 0), `f(n)` é >= `c g(n)` para cada  
tamanho de entrada n (n > 0).

Sinta-se livre para pesquisar recursos adicionais e obter mais exemplos sobre este assunto!  
Grande-O é a notação primária utilizada para tempo de execução de algoritmos, de modo geral.  

### Notas de Finalização

É complicado exibir este tipo de assunto de forma tão curta, então é definitivamente recomendado  
pesquisar além dos livros e recursos on-line listados. Eles serão capazes de analisar o assunto com  
uma profundidade muito maior, além de ter definições e exemplos. Mais sobre onde X="Algoritmos e  
Estruturas de Dados" está a caminho: Haverá conteúdo focado na análise de exemplos de códigos reais  
em breve. 

## Livros

* [Algorithms] (http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)  
* [Algorithm Design] (http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Recursos Online

* [MIT] (http://web.mit.edu/16.070/www/lecture/big_o.pdf)  
* [KhanAcademy] (https://www.khanacademy.org/computing/computer-science/algorithms/asymptotic-notation/a/asymptotic-notation)
