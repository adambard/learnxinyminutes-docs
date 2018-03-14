---
language: bf
filename: learnbrainfuck-fr.bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Baptiste Fontaine", "http://bfontaine.net"]
lang: fr-fr
---

Brainfuck (sans majuscule à part au début d’une phrase) est un langage
Turing-complet extrêmement simple avec seulement 8 commandes.

```
Tout caractère en dehors de "><+-.,[]" (en dehors des guillemets) est ignoré.

Brainfuck est représenté par un tableau de 30 000 cellules initialisées à 0 et
un pointeur de données pointant sur la cellule courante.

Il y a huit commandes :
+ : Incrémente la valeur de la cellule courante de un.
- : Décrémente la valeur de la cellule courante de un.
> : Déplace le pointeur de données sur la cellule suivante (à droite).
< : Déplace le pointeur de données sur la cellule précédente (à gauche).
. : Affiche la valeur ASCII de la cellule courante (par ex. 65 = 'A').
, : Lit un caractère et le place dans la cellule courante.
[ : Si la valeur dans la cellule courante vaut 0, saute au ] correspondant.
    Sinon, continue avec la commande suivante.
] : Si la valeur dans la cellule courante vaut 0, continue avec la commande
    suivante. Sinon, retourne au [ correspondant.

[ et ] forment une boucle « tant que » (« while »). Ils doivent évidemment
aller par paires.

Regardons quelques programmes simples en brainfuck.

++++++ [ > ++++++++++ < - ] > +++++ .

Ce programme affiche la lettre 'A'. Il commence par incrémenter la première
cellule à 6. Il entre ensuite dans une boucle et se déplace sur la seconde
cellule. Il l’incrémente 10 fois, retourne sur la première cellule, et la
décrémente. Cette boucle est exécutée 6 fois (ce qui correspond aux 6
décrémentations de la première cellule pour la faire atteindre 0, ce qui fait
sortir de la boucle).

À ce moment-là, nous sommes sur la première cellule, qui a une valeur de 0,
tandis que la seconde cellule a une valeur de 60. Nous nous déplaçons sur
celle-ci, l’incrémentons 5 fois, pour une valeur de 65, et affichons sa valeur.
En ASCII, 65 correspond à 'A' donc le programme affiche 'A' dans le terminal.

, [ > + < - ] > .

Ce programme lit un caractère sur l’entrée standard et le copie dans la
première cellule. Il commence ensuite une boucle : il bouge sur la seconde
cellule, incrémente sa valeur, retourne sur la première et décrémente sa
valeur. Il continue jusqu’à ce que la valeur de la première cellule soit à 0,
et que la seconde cellule contienne l’ancienne valeur de la première. Comme
nous sommes sur la première cellule à la fin de la boucle, il bouge sur la
seconde et affiche sa valeur en ASCII.

Souvenez-vous que les espaces sont uniquement là pour favoriser la lisibilité,
vous pourriez tout aussi aisément écrire le programme comme ceci :

,[>+<-]>.

Essayez et devinez ce que ce programme fait :

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Ce programme prend deux nombres en entrée, et les multiplie.

Il commence par lire deux entrées, puis commence une boucle externe, qui a une
condition sur la première cellule. Il bouge ensuite sur la seconde, et commence
une boucle interne sur celle-ci, en incrémentant la troisième cellule. Il y a
cependant un problème : à la fin de la boucle interne, la valeur de la seconde
cellule est à zéro. Dans ce cas, la boucle interne ne fonctionnera pas une
seconde fois. Pour régler le problème, nous incrémentons aussi la quatrième
cellule, puis recopions sa valeur dans la seconde cellule.
À la fin, la troisième cellule contient le résultat de la multiplication.
```

Et voilà ce qu’est le brainfuck. Pas très dur, hein ? Pour le fun, vous pouvez
écrire vos propres programmes en brainfuck, ou écrire un interpréteur brainfuck
dans un autre langage. L’interpréteur est relativement simple à implémenter,
mais si vous êtes un masochiste, essayez d’écrire un interpréteur brainfuck en…
brainfuck.
