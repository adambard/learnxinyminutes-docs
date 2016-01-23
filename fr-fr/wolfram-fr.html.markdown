---
language: wolfram
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
translators:
    - ["altaris", "http://github.com/altaris/"]
filename: learnwolfram-fr.nb
lang: fr-fr
---

Le langage Wolfram est utilisé dans les programmes suivants :
* La ligne de commandes interactive noyau du Raspberry Pi, mais elle ne peut pas
gérer des éléments graphiques.
* _Mathematica_, un éditeur de texte riche spécialisé pour les mathématiques :
appuyer sur `Shift + Entrée` dans une cellule de code crée un nouvelle cellule 
contenant le résultat.
* _Wolfram Wokbench_, une variante d'Eclipse spécialisée pour le langage
Wolfram.

Ce code d'exemple peut être utilisé et modifié dans ces logiciels. Cependant, le
copier-coller directement dans Mathematica peut causer des problèmes de
formatage, car il ne contient aucune information de mise en page.

```
(* Ceci est un commentaire *)

(* Dans Mathematica, au lieu d'utiliser ces commentaires, vous pouvez créer des
   cellules de texte et insérer de jolies images *)

(* Saisissez une opération et appuyez sur Shift + Entrée pour obtenir le
   résultat *)
2*2              (* 4 *)
5+8              (* 13 *)

(* Appels de fonction *)
Sin[Pi/2]        (* 1 *)
(* Syntaxe alternative pour les appels de fonction à 1 paramètre *)
Sin@(Pi/2)       (* 1 *)
(Pi/2) // Sin    (* 1 *)

(* Attention : le langage est sensible à la casse ! *)

(* Toutes les expressions sont en réalité des appels de fonction *)
Times[2, 2]      (* 4 *)
Plus[5, 8]       (* 13 *)

(* Utiliser une variable pour la première fois la déclare globalement *)
x = 5            (* 5 *)
x == 5           (* True, l'assignation et le test d'égalité est écrit comme
                    en C *)
x                (* 5 *)
x = x + 5        (* 10 *)
x                (* 10 *)
Set[x, 20]       (* TOUT est un appel de fonction, TOUUUUUUUUT *)
x                (* 20 *)

(* Le langage Wolfram effectue des manipulations symboliques, donc utiliser des
   variables non déclarées est légal *)
truc + 5         (* 5 + truc, comme truc n'est pas déclarée, l'évaluation
                    s'arrête là *)
truc + 5 + 10    (* 15 + truc, on évalue ce qu'on peut... *)
%                (* 15 + truc, % représente le dernier résultat *)
% - truc         (* 15, les variables non déclarées peuvent quand même
                    s'annuler *)
chose = truc + 5 (* Attention : chose est ici une expression et non un nombre *)

(* Déclaration d'une fonction *)
Double[x_] := x * 2     (* Le symbole := empêche l'évaluation immédiate du terme
                           à droite *)
Double[10]              (* 20 *)
Double[Sin[Pi/2]]       (* 2 *)
Double @ Sin @ (Pi/2)   (* 2, Utiliser @ évite les paquets de crochets
                           fermants si moches *)
(Pi/2) // Sin // Double (* 2, Utiliser // permet d'écrire les fonctions dans
                           l'ordre d'appel *)

(* En programmation impérative, utiliser ; pour séparer les expressions *)
Salut[] := (Print@"Hello"; Print@"World")  (* Les parenthèses sont nécessaires
                                              car ; est prioritaire sur := *)
Salut[]                                    (* Hello World *)

(* Boucles For à la C *)
Compter[x_] := For[y=0, y<x, y++, (Print[y])]  (* L'évaluation des boucles For
                                                  se fait comme en C *)
Compter[5]                                     (* 0 1 2 3 4 *)

(* Boucles While *)
x = 0; While[x < 2, (Print@x; x++)]     (* De nouveau, comme en C *)

(* Expressions conditionnelles et If *)
x = 8; If[x==8, Print@"Huit", Print@"Pas huit"] (* If [condition, si vrai,
                                                   si faux] *)
Switch[x, 2, Print@"Deux", 8, Print@"Huit"]     (* Switch par valeur *)
Which[x==2, Print@"Deux", x==8, Print@"Huit"]   (* Switch du type if, else if,
                                                   else if, ..., else *)

(* Les variables autres que les paramètres de fonctions sont par défaut
   globales, même à l'intérieur des fonctions *)
y = 10             (* 10, y est une variable globale *)
Compter[5]         (* 0 1 2 3 4 *)
y                  (* 5, y a été modifiée par Compter *)
x = 20             (* 20, x est une variable globale *)
Compter[5]         (* 0 1 2 3 4 *)
x                  (* 20, dans Compter, le paramètre x masque la variable
                      globale x *)

(* La fonction Module permet d'utiliser des variables locales *)
MieuxCompter[x_] := Module[{y}, (For[y=0, y<x, y++, (Print@y)])]
y = 20             (* y est une variable globale *)
MieuxCompter[5]    (* 0 1 2 3 4 *)
y                  (* 20, y n'a pas été modifiée car le y du Module masque le
                          y global. C'est bien mieux comme ça ! *)

(* Module permet de faire des déclarations globales aussi *)
Module[{compte}, compte=0;      (* compte est une variable locale *)
  (Incrementer[] := ++compte);  (* Ce module déclare des fonctions, mais elles
                                   ne sont globales. Elles ont cependant accès
                                   aux variables locales au module. *)
  (Decrementer[] := --compte)]
compte             (* compte, car il n'y a pas de variable globale nommée
                      compte *)
Incrementer[]      (* 1, la fonction utilise la variable compte du module *)
Incrementer[]      (* 2, le précédent appel de Incrementer a modifié compte *)
Decrementer[]      (* 1 *)
compte             (* compte, car il n'existe toujours pas de variable globale
                      nommé compte *)

(* Listes *)
liste = {1, 2, 3, 4}     (* {1, 2, 3, 4} *)
liste[[1]]               (* 1, les indexes commencent à 1 et non 0 !!! *)
Map[Double, liste]       (* {2, 4, 6, 8}, appliquer une fonction à une liste de
                             manière fonctionnelle *)
Double /@ liste          (* {2, 4, 6, 8}, syntaxe abrégée de la ligne
                             précédente *)
Scan[Print, liste]       (* 1 2 3 4, boucle impérative sur une liste *)
Fold[Plus, 0, liste]     (* 10 (0+1+2+3+4) *)
FoldList[Plus, 0, liste] (* {0, 1, 3, 6, 10}, variante de la fonction
                             précédente qui donne aussi les résultats
                             intermédiaires *)
Append[liste, 5]         (* {1, 2, 3, 4, 5}, liste n'est pas modifiée... *)
Prepend[liste, 5]        (* {5, 1, 2, 3, 4}, ... mais elle peut l'être en 
                             écrivant "liste = " *)
Join[liste, {3, 4}]      (* {1, 2, 3, 4, 3, 4} *)
liste[[2]] = 5           (* {1, 5, 3, 4}, ceci modifie bien la liste *)

(* Tables associatives, ou dictionnaires *)
table = <|"Vert" -> 2, "Rouge" -> 1|> (* Crée une table associative *)
table[["Vert"]]                       (* 2, l'utilise *)
table[["Vert"]] := 5                  (* 5, la modifie *)
table[["Bleu"]] := 3.5                (* 3.5, l'étend *)
KeyDropFrom[table, "Vert"]            (* Supprime la clé "Vert" *)
Keys[table]                           (* {Rouge, Bleu} *)
Values[table]                         (* {1, 3.5} *)

(* Pour finir, toute bonne démonstration du langage Wolfram contient un
   Manipulate ! *)
Manipulate[y^2, {y, 0, 20}] (* Crée une interface graphique interactive qui
                               affiche y^2, permettant à l'utilisateur de
                               modifier la valeur de y grâce à un contrôle
                               allant de 0 à 20. Ne fonctionne que si le
                               logiciel utilisé gère les éléments graphiques. *)
```

## Envie d'aller plus loin ?

* [Documentation du langage Wolfram (en anglais)]
(http://reference.wolfram.com/language/)
