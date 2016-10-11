---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Claudson Martins", "https://github.com/claudsonm"]
lang: pt-br
---

# Programação Dinâmica

## Introdução

Programação Dinâmica é uma técnica poderosa utilizada para resolver uma classe 
particular de problemas como veremos. A ideia é bastante simples, se você 
solucionou um problema com uma dada entrada, então salve o resultado para 
referência futura, e também para evitar resolver o mesmo problema novamente.

Sempre se lembre!!
"Aqueles que não conseguem lembrar o passado estão condenados a repeti-lo"

## Maneiras de Solucionar tais Problemas

1. Top-Down (De cima para baixo): Começe solucionando o problema quebrando-o em 
partes. Se você perceber que o problema já foi resolvido, então simplemente 
pegue a resposta salva. Se ainda não foi resolvido, solucione-o e salve a 
resposta. Isso é geralmente fácil de pensar e muito intuitivo. É geralmente 
referenciado como Memorização.

2. Bottom-Up (De baixo para cima): Analise o problema e veja a ordem em que os 
subproblemas são resolvidos e começe a solucionar dos problemas mais triviais, 
até o problema dado. Neste processo, é garantido que os subproblemas são 
resolvidos antes de resoler o problema. Isto é referenciado como Programação Dinâmica.

## Exemplo de Programação Dinâmica

O problema da subsequência crescente máxima consiste em encontrar a maior 
subsequência crescente de uma dada sequência. Dada uma sequência 
S= {a1 , a2 , a3, a4, ... , an-1, an} nós temos que encontrar o maior subconjunto 
de forma que para todo j e i,  j < i no subconjunto aj < ai. Antes de mais nada 
nós temos que encontrar o valor das maiores subsequências (LSi) para cada índice 
i com o último elemento da sequência sendo ai. Então a maior LSi será a maior 
subsequência na sequência dada. Para começar LSi é atribuído a um pois ai é 
elemento da sequência (último elemento). Então para todo j tal que j < i e aj < 
ai, nós procuramos o maior LSj e o adicionamos a LSi. Portanto o algoritmo tem 
complexidade de tempo O(n2). O pseudocódigo para procurar o comprimento da 
subsequência crescente máxima: A complexidade desse algoritmo poderia ser 
reduzida utilizando uma estrutura de dados melhor que um array. Armazenando o 
array antecedente e uma variável como maiorSequenciasAteAgora e seu índice 
ajudariam a poupar muito tempo.
Um conceito similar poderia ser aplicado ao procurar o maior caminho em um 
grafo acíclico dirigido.
---------------------------------------------------------------------------
```
 for i=0 to n-1
            LS[i]=1
            for j=0 to i-1
                        if (a[i] >  a[j] and LS[i]<LS[j])
                                    LS[i] = LS[j]+1
 for i=0 to n-1
            if (largest < LS[i])
```

### Alguns Problemas Famosos de Programação Dinâmica
```
Floyd Warshall Algorithm - Tutorial and C Program source code:http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code 

Integer Knapsack Problem - Tutorial and C Program source code: http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem 

Longest Common Subsequence - Tutorial and C Program source code : http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence 
```

## Recursos Online (EN)

* [codechef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
