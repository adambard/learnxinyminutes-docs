---
category: Algorithms & Data Structures
name: Asymptotic Notation
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Divay Prakash", "http://github.com/divayprakash"]
translators:
    - ["Agathe Begault", "https://github.com/begault"]
lang: fr-fr    
---

# Notations Asymptotiques

## Qu'est ce que c'est?

Les notations asymptotiques sont des langages qui nous permettent d'analyser l'ordre de grandeur du temps d'exécution d'un algorithme en identifiant son comportement à mesure que les données d'entrée de l'algorithme augmentent. On appelle également cela le taux de croissance d'un algorithme. 

Est ce que l'algorithme devient soudainement extrêmement lent si on augmente sa taille d'entrée ? Est ce qu'il arrive à maintenir un temps d'exécution rapide ? La notation asymptotique nous permet simplement de répondre à ces questions et d'ainsi comparer différents algorithmes. 

## Y a t-il des alternatives pour répondre à ces problématiques ? 

Une première solution serait de compter le nombre d'opérations primitives, en fonction de la taille d'entrée. 
Cette méthode est viable mais nécessite une quantité de travail trop importante par rapport à son utilisation, même sur des algorithmes simples. 

Une autre approche serait de mesurer physiquement le temps qu'un algorithme met pour traiter différentes quantités de données. Cependant, la précision et la relativité de cette méthode (les temps obtenus n'étant relatifs qu'à la machine sur laquelle ils ont été calculés) est liée à des variables environnementales comme les spécifications du matériel informatique utilisé, la puissance de traitement de la machine, etc. 

## Les types de Notations asymptotiques

En première partie de ce document, nous avons décris comment une notation asymptotique identifie le comportement d'un algorithme à mesure que la taille des données d'entrée change. Imaginons un algorithme comme une fonction f, ayant comme taille de donnée d'entrée n, et f(n) étant le temps d'exécution. Ainsi, pour un algorithme donné f, avec une taille d'entrée  n, nous obtenons en résultat un temps d'exécution f(n). Cela nous fournit un graphique où l'axe Y est le temps d'exécution, l'axe X est la taille d'entrée et la courbe tracée est le résultat du temps utilisé pour traiter chaque quantité de données.  

Vous pouvez décrire une fonction ou un algorithme avec une notation asymptotique de plusieurs manières. Par exemple, vous pouvez décrire un algorithme en partant du meilleur des cas, du pire ou d'un cas intermédiaire. Le plus courant est de commencer à analyser un algorithme avec le pire cas. Le meilleur cas n'est pas nécessaire car ce ne sont pas les conditions que vous envisagez. L'algorithme de tri est un très bon exemple, particulièrement en ajoutant des éléments à une structure arborescente. Le meilleur cas pour la plupart des algorithmes ne nécessite qu'une simple opération alors que dans la plupart des cas, l'élément à ajouter aura besoin d'être  trié de manière appropriée à travers l'arbre de données. Cette action pourrait signifier l'examen d'une branche entière de l'arbre. C'est d'ailleurs le pire cas et celui que nous prévoyons. 

### Les types de fonctions, limites et simplifications

```
Fonction logarithmique - log n
Fonction linéaire - an + b
Fonction quadratique - an^2 + bn + c
Fonction polynomiale - an^z + . . . + an^2 + a*n^1 + a*n^0, où z est une constante
Fonction exponentielle - a^n, où a est une constante 
```

Voici une classification de fonctions croissantes de base, utilisées dans de nombreuses notations. La liste commence par la plus lente des fonctions croissantes (logarithmique, le temps d'exécution le plus rapide) et finit avec la plus croissante des fonctions (exponentielle, le temps d'exécution le plus lent). Notez que lorsque 'n' ou les données d'entrée augmentent pour chacune de ces fonctions, le résultat augmente clairement plus rapidement avec les fonctions quadratique, polynomiale et exponentielle qu'avec les fonctions logarithmique et linéaire. 

Il est important de noter que les notations suivantes doivent être utilisées avec les termes les plus simples. Cela signifie d'ignorer les constantes et termes de l'ordre inférieur. En effet, puisque la taille d'entrée (ou n dans notre exemple f(n)) peut augmenter à l'infini (limites mathématiques), les termes et constantes de l'ordre inférieur sont insignifiants. Ceci dit, si vous avez une constante égale à 2^9001, ou toute autre valeur ridicule et inimaginable, dans ce cas la simplification nuira à votre précision de notation. 

Puisque nous voulons la forme la plus simple, modifions un peu notre table... 

```
Logarithmique - log n
Linéaire - n
Quadratique - n^2
Polynomiale - n^z, où z est une constante
Exponentielle - a^n, où a est une constante 
```

### Big-O

Big-O, couramment écris **O**, est une notation asymptotique pour le cas le plus mauvais (ou plafond de croissance) d'une fonction donnée. Il nous fournit une _**limite supérieure asymptotique**_ pour le taux de croissance du temps d'exécution d'un algorithme. 

Prenons 'f(n)' comme temps d'exécution de notre algorithme et 'g(n)' comme complexité de temps arbitraire que nous essayons d'appliquer à notre algorithme. 'f(n)' est O(g(n)), si pour certaines constantes c (c > 0) et n<sub>0</sub>,  'f(n)' <= 'c g(n)' pour toute taille d'entrée n (n > n<sub>0</sub>).

*Exemple 1*

```
f(n) = 3log n + 100
g(n) = log n
```

Est-ce que `f(n)` est égal à O(g(n))?
Est-ce que `3 log n + 100` est égal à O(log n)?
Regardons maintenant la définition de Big-O.

```
3log n + 100 <= c * log n
```

Existe t-il une paire de constantes c, n<sub>0</sub> qui satisfait cela pour tout n > n<sub>0</sub>?

```
3log n + 100 <= 150 * log n, n > 2 (Indéfini avec n = 1)
```

Oui ! La définition de Big-O a été satisfaite, donc `f(n)` est égal à O(g(n)).

*Exemple 2*

```
f(n) = 3*n^2
g(n) = n
```

Est-ce que `f(n)` est égal à O(g(n))?  
Est-ce que `3 * n^2` est égal à O(n)?
Regardons de nouveau la définition de Big-O.

```
3 * n^2 <= c * n
```

Existe t-il une paire de constantes c, n<sub>0</sub> qui satisfait cela pour tout n > n<sub>0</sub>?
Non, il n'en existe pas. `f(n)` n'est pas égal à O(g(n)).

### Big-Omega

Big-Omega, courrament écris **Ω**, est une notation asymptotique pour le meilleur cas (ou limite de croissance basse) d'une fonction donnée. Il nous fournit une _**limite inférieure asymptotique**_ pour le taux de croissance du temps d'exécution d'un algorithme. 

Prenons 'f(n)' comme temps d'exécution de notre algorithme et 'g(n)' comme complexité de temps arbitraire que nous essayons d'appliquer à notre algorithme. 'f(n)' est Ω(g(n)), si pour certaines constantes c (c > 0) et n<sub>0</sub>, 'f(n)' >= 'c g(n)' pour toute taille d'entrée n (n > n<sub>0</sub>).

### Remarque

Les taux de croissance asymptotiques fournis par les notations big-O et big-omega peuvent ou non être asymptotiquement serrés. Nous utilisons ainsi les notations small-o et small-omega pour désigner des limites qui ne sont pas asymptotiquement serrées. 

### Small-o
Small-o, couramment écris **o**, est une notation asymptotique pour désigner la limite supérieure (ce qui n'est pas asymptotiquement serré) du taux de croissance du temps d'exécution d'un algorithme. 

`f(n)` est o(g(n)), si pour certaines constantes c (c > 0) et n<sub>0</sub> (n<sub>0</sub> > 0), `f(n)` < `c g(n)` 
pour toute taille d'entrée n (n > n<sub>0</sub>).

Les définitions de O-notation et o-notation sont similaires. La principale différence est visible quand f(n) = O(g(n)). Dans ce cas, la limite f(n) <= g(n) est appliquée pour _**quelques**_ constantes c > 0. Lorsque f(n) = o(g(n)), la limite f(n) < c g(n) est appliquée pour _**toute**_ constante c > 0.

### Small-omega
Small-omega, couramment écris **ω**, est une notation asymptotique pour désigner la limite inférieure (ce qui n'est pas asymptotiquement serré) du taux de croissance du temps d'exécution d'un algorithme.

`f(n)` est ω(g(n)), si pour certaines constantes c (c > 0) et n<sub>0</sub> (n<sub>0</sub> > 0), `f(n)` > `c g(n)` 
pour toute taille d'entrée n (n > n<sub>0</sub>).

Les définitions de Ω-notation et ω-notation sont similaires. La principale différence est visible quand f(n) = Ω(g(n)). Dans ce cas, la limite f(n) >= g(n) est appliquée pour _**quelques**_ constantes c > 0. Lorsque f(n) = ω(g(n)), la limite f(n) > c g(n) est appliquée pour _**toute**_ constante c > 0.

### Theta
Theta, couramment écris **Θ**, est une notation asymptotique pour désigner la _**borne asymptotique sous contrainte**_ du taux de croissance du temps d'exécution d'un algorithme. 

`f(n)` est Θ(g(n)), si pour certaines constantes réelles c1, c2 et n<sub>0</sub> (c1 > 0, c2 > 0, n<sub>0</sub> > 0), `c1 g(n)` < `f(n)` < `c2 g(n)` pour toute taille d'entrée n (n > n<sub>0</sub>).

∴ `f(n)` est Θ(g(n)) implique que `f(n)` est égal à O(g(n)) autant que `f(n)` est égal à Ω(g(n)).

N'hésitez pas à trouver de plus amples informations à ce sujet. Big-O est la notation la plus couramment utilisée pour le calcul de complexité du temps d'un algorithme. 

### Notes de fin
Il est difficile de traiter ce type de sujets dans un article court tant les exemples, méthodes et informations sont nombreuses. C'est pourquoi nous vous invitons à jeter un oeil aux livres et liens listés ci-dessous.  
Ces ressources apportent plus de détails avec des exemples et des définitions. 

## Livres

* [Algorithmes](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Conception algorithmique](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

##  Ressources en ligne

* [MIT](http://web.mit.edu/16.070/www/lecture/big_o.pdf)
* [KhanAcademy](https://www.khanacademy.org/computing/computer-science/algorithms/asymptotic-notation/a/asymptotic-notation)
* [Big-O Cheatsheet](http://bigocheatsheet.com/) - Structures, opérations, et algorithmes communs, classés par complexité.
