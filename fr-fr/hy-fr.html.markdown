---
language: hy
filename: learnhy-fr.hy
contributors:
    - ["Abhishek L", "http://twitter.com/abhishekl"]
translators:
  - ["Hughes Perreault", "https://github.com/hperreault"]
lang: fr-fr
---

Hy est un dialecte du lisp bâti par dessus python. Il fonctionne en
convertissant le code hy en un arbre de syntaxe abstraite de python (ast).
Ceci permet à hy d'appeler du code python et à python d'appeler du code hy.

Ce tutoriel fonctionne pour hy > 0.9.12

```clojure
;; Ceci est une introduction simple à hy, pour un tutoriel rapide aller à
;; http://try-hy.appspot.com
;;
; Les commentaires se font avec des points-virgules, comme les autres LISPS

;; les s-expression de bases
; Les programmes Lisp sont fait d'expressions symboliques ou sexps qui
; ressemblent à
(some-function args)
; maintenant le quintessentiel hello world
(print "hello world")

;; les types de données simples
; Tous les types de données simples sont exactement similaires à leurs
; homologues de python
42 ; => 42
3.14 ; => 3.14
True ; => True
4+10j ; => (4+10j) un nombre complexe

; Commençons par un peu d'arithmétique très simple
(+ 4 1) ;=> 5
; l'opérateur est appliqué à tous les arguments, comme les autres lisps
(+ 4 1 2 3) ;=> 10
(- 2 1) ;=> 1
(* 4 2) ;=> 8
(/ 4 1) ;=> 4
(% 4 2) ;=> 0 l'opérateur modulo
; l'opérateur d'élévation à la puissance est représenté par ** comme en python
(** 3 2) ;=> 9
; les expressions imbriquées vont se comporter comme on s'y attend
(+ 2 (* 4 2)) ;=> 10
; aussi, les opérateurs logiques and or not et equal to etc. vont se comporter
; comme on s'y attend
(= 5 4) ;=> False
(not (= 5 4)) ;=> True

;; variables
; les variables sont déclarées en utilisant setv, les noms de variables
; peuvent utiliser l'UTF-8 à l'exception de ()[]{}",'`;#|
(setv a 42)
(setv π 3.14159)
(def *foo* 42)
;; d'autres types de conteneurs
; les chaînes, les listes, les tuples et dicts
; ce sont exactement les mêmes que les types de conteneurs de python
"hello world" ;=> "hello world"
; les opérations sur les chaînes fonctionnent comme en python
(+ "hello " "world") ;=> "hello world"
; les listes sont créés en utilisant [], l'indexation commence à 0
(setv mylist [1 2 3 4])
; les tuples sont des structures de données immuables
(setv mytuple (, 1 2))
; les dictionnaires sont des paires clé-valeur
(setv dict1 {"key1" 42 "key2" 21})
; :nom peut être utilisé pour définir des mots clés dans hy qui peuvent être
;  utilisées comme clés
(setv dict2 {:key1 41 :key2 20})
; utilisez `get' pour obtenir l'élément à l'index / clé
(get mylist 1) ;=> 2
(get dict1 "key1") ;=> 42
; Alternativement, si des mots clés ont été utilisés, l'élément peut être
; obtenu directement
(:key1 dict2) ;=> 41

;; fonctions et autres constructions de programme
; les fonctions sont définies en utilisant defn, la dernière sexp est renvoyé par défaut
(defn greet [name]
  "A simple greeting" ; une docstring optionnelle
  (print "hello " name))

(greet "bilbo") ;=> "hello bilbo"

; les fonctions peuvent prendre des arguments optionnels ainsi que des
; arguments sous forme de mots clés
(defn foolists [arg1 &optional [arg2 2]]
  [arg1 arg2])

(foolists 3) ;=> [3 2]
(foolists 10 3) ;=> [10 3]

; les fonctions anonymes sont créés en utilisant `fn' ou `lambda'
; qui sont semblable à `defn '
(map (fn [x] (* x x)) [1 2 3 4]) ;=> [1 4 9 16]

;; Opérations sur les séquences
; hy a des utilitaires natifs pour les opérations sur les séquences etc.
; récupérez le premier élément en utilisant  `first' ou `car'
(setv mylist [1 2 3 4])
(setv mydict {"a" 1 "b" 2})
(first mylist) ;=> 1

; découpez les listes en utilisant slice
(slice mylist 1 3) ;=> [2 3]

; obtenez les éléments d'une liste ou dict en utilisant `get'
(get mylist 1) ;=> 2
(get mydict "b") ;=> 2
; l'indexation des listes commence à 0 comme en python
; assoc peut définir les éléments à clés/index
(assoc mylist 2 10) ; makes mylist [1 2 10 4]
(assoc mydict "c" 3) ; makes mydict {"a" 1 "b" 2 "c" 3}
; il ya tout un tas d'autres fonctions de base qui rend le travail avec
; les séquences amusant

;; les importations fonctionnent comme en pyhtonn
(import datetime)
(import [functools [partial reduce]]) ; importe fun1 et fun2 de module1
(import [matplotlib.pyplot :as plt]) ; faire une importation foo comme bar
; toutes les méthodes natives de python sont accessibles à partir de hy
; a.foo(arg) est appelé (.foo a arg)
(.split (.strip "hello world  ")) ;=> ["hello" "world"]

;; Conditionelles
; (if condition (body-if-true) (body-if-false)
(if (= passcode "moria")
  (print "welcome")
  (print "Speak friend, and Enter!"))

; imbriquez plusieurs if else if avec le mot clé cond
(cond
 [(= someval 42)
  (print "Life, universe and everything else!")]
 [(> someval 42)
  (print "val too large")]
 [(< someval 42)
  (print "val too small")])

; groupez les expressions avec do, ceux-ci seront executé séquentiellemnt
; les expressions comme defn ont un do implicite
(do
 (setv someval 10)
 (print "someval is set to " someval)) ;=> 10

; créer une liaison lexicale avec `let', toutes les variables déclarées
; comme cela ont une portée locale
(let [[nemesis {"superman" "lex luther"
                "sherlock" "moriarty"
                "seinfeld" "newman"}]]
  (for [(, h v) (.items nemesis)]
	(print (.format "{0}'s nemesis was {1}" h v))))

;; classes
; les classes sont définies comme ceci
(defclass Wizard [object]
  [[--init-- (fn [self spell]
             (setv self.spell spell) ; init the spell attr
             None)]
   [get-spell (fn [self]
              self.spell)]])

;; allez voir hylang.org
```

### Lectures complémentaires

Ce tutoriel est juste une simple introduction à hy/lisp/python.

La documentation de HY: [http://hy.readthedocs.org](http://hy.readthedocs.org)

Le repo GitHub de HY: [http://github.com/hylang/hy](http://github.com/hylang/hy)

Sur freenode irc #hy, twitter hashtag #hylang
