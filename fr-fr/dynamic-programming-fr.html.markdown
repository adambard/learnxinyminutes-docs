---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Hughes Perreault", "https://github.com/hperreault"]
lang: fr-fr
---


# Programmation dynamique

## Introduction

La programmation dynamique est une technique très efficace pour résoudre une certaine classe de problèmes, comme nous allons le voir. L'idée est très simple, si nous avons résolu un problème avec une certaine entrée, alors nous sauvons le résultat pour pouvoir y accéder plus tard, pour éviter d'avoir à le calculer à nouveau.

## Moyens de résoudre ces problèmes

1.) *De haut en bas* : Commençons à résoudre le problème en le séparant en morceaux. Si nous voyons que le problème a déjà été résolu, alors nous retournons la réponse précédemment sauvegardée. Si le problème n'a pas été résolu, alors nous le résolvons et sauvegardons la réponse. C'est généralement facile et intuitif de réfléchir de cette façon. Cela s'appelle la Mémorisation.

2.) *De bas en haut* : Il faut analyser le problème et trouver les sous-problèmes, et l'ordre dans lequel il faut les résoudre. Ensuite, nous devons résoudre les sous-problèmes et monter jusqu'au problème que nous voulons résoudre. De cette façon, nous sommes assurés que les sous-problèmes sont résolus avant de résoudre le vrai problème. Cela s'appelle la Programmation Dynamique.

## Exemple de Programmation Dynamique

Le problème de la plus grande sous-chaîne croissante est de trouver la plus grande sous-chaîne croissante dans une chaîne. Soit la chaîne `S = {a1, a2, a3, a4, ............., an-1, an}`, nous avons à trouver la plus grande chaîne telle que pour tout `j` et `i`, `j<i` dans la chaîne `aj<ai`.
Premièrement, nous avons à trouver la valeur de la plus grande sous-chaîne (LSi) à chaque index `i`, avec le dernier élément de la sous-chaîne étant ai. Alors, la plus grande sous-chaîne sera le plus gros LSi. Pour commencer, LSi est égal à 1, car ai est le seul élément de la chaîne (le dernier). Ensuite, pour chaque `j` tel que `j<i` et `aj<ai`, nous trouvons le plus grand LSj et ajoutons le à LSi. L'algorithme fonctionne en temps *O(n2)*.   

Pseudo-code pour trouver la longueur de la plus grande sous-chaîne croissante :
La complexité de cet algorithme peut être réduite en utilisant une meilleure structure de données qu'un tableau. Par exemple, si nous sauvegardions le tableau d'origine, ou une variable comme plus_grande_chaîne_jusqu'à_maintenant et son index, nous pourrions sauver beaucoup de temps. 

Le même concept peut être appliqué pour trouver le chemin le plus long dans un graphe acyclique orienté.  

```python
 for i=0 to n-1
            LS[i]=1
            for j=0 to i-1
                        if (a[i] >  a[j] and LS[i]<LS[j])
                                    LS[i] = LS[j]+1
 for i=0 to n-1
            if (largest < LS[i])
```

### Problèmes classiques de programmation dynamique

L'algorithme de Floyd Warshall(EN)) - Tutorial and C Program source code:http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code

Problème du sac à dos(EN) - Tutorial and C Program source code: http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem


Plus longue sous-chaîne commune(EN) - Tutorial and C Program source code : http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence

## Online Resources

* [codechef EN](https://www.codechef.com/wiki/tutorial-dynamic-programming)
