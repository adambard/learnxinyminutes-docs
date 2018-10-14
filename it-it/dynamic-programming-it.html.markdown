---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Ale46", "https://github.com/ale46"]
lang: it-it
---

# Programmazione dinamica

## Introduzione

La programmazione dinamica è una tecnica potente utilizzata per risolvere una particolare classe di problemi, come vedremo. L'idea è molto semplice, se hai risolto un problema con l'input dato, salva il risultato come riferimento futuro, in modo da evitare di risolvere nuovamente lo stesso problema.

Ricordate sempre!
"Chi non ricorda il passato è condannato a ripeterlo"

## Modi per risolvere questi problemi

1. *Top-Down* : Inizia a risolvere il problema specifico suddividendolo. Se vedi che il problema è già stato risolto, rispondi semplicemente con la risposta già salvata. Se non è stato risolto, risolvilo e salva la risposta. Di solito è facile da pensare e molto intuitivo. Questo è indicato come Memoization.

2. *Bottom-Up* : Analizza il problema e vedi l'ordine in cui i sotto-problemi sono risolti e inizia a risolvere dal sottoproblema banale, verso il problema dato. In questo processo, è garantito che i sottoproblemi vengono risolti prima di risolvere il problema. Si parla di programmazione dinamica.

## Esempio di programmazione dinamica

Il problema di "Longest Increasing Subsequence" consiste nel trovare la sottosequenza crescente più lunga di una determinata sequenza. Data una sequenza `S= {a1 , a2 , a3, a4, ............., an-1, an }` dobbiamo trovare il sottoinsieme più lungo tale che per tutti gli `j` e gli `i`,  `j<i` nel sotto-insieme `aj<ai`.
Prima di tutto dobbiamo trovare il valore delle sottosequenze più lunghe (LSi) ad ogni indice i con l'ultimo elemento della sequenza che è ai. Quindi il più grande LSi sarebbe la sottosequenza più lunga nella sequenza data. Per iniziare LSi viene inizializzato ad 1, dato che ai è un element della sequenza (Ultimo elemento). Quindi per tutti gli `j` tale che `j<i` e `aj<ai`, troviamo il più grande LSj e lo aggiungiamo a LSi. Quindi l'algoritmo richiede un tempo di *O(n2)*.

Pseudo-codice per trovare la lunghezza della sottosequenza crescente più lunga:
Questa complessità degli algoritmi potrebbe essere ridotta usando una migliore struttura dei dati piuttosto che una matrice. La memorizzazione dell'array predecessore e della variabile come `largest_sequences_so_far` e il suo indice farebbero risparmiare molto tempo.

Un concetto simile potrebbe essere applicato nel trovare il percorso più lungo nel grafico aciclico diretto.

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
```

### Alcuni famosi problemi DP

- Floyd Warshall Algorithm - Tutorial e Codice sorgente in C del programma: [http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code]()
- Integer Knapsack Problem - Tutorial e Codice sorgente in C del programma: [http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem]()
- Longest Common Subsequence - Tutorial e Codice sorgente in C del programma: [http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence]()


## Risorse online

* [codechef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
