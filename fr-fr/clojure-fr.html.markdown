---
language: clojure
filename: learnclojure-fr.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Bastien Guerry", "https://github.com/bzg"]
lang: fr-fr
---

Clojure est un langage de la famille des Lisp développé pour la machine
virtuelle Java.  Ce langage insiste beaucoup plus sur la [programmation
fonctionnelle](https://fr.wikipedia.org/wiki/Programmation_fonctionnelle) pure
que Common Lisp, mais comprend plusieurs outils de gestion de la mémoire
transactionnelle
[STM](https://en.wikipedia.org/wiki/Software_transactional_memory) pour gérer
les changements d'états si besoin.

Cette combinaison permet de gérer le parallélisme très simplement, et
souvent de façon automatique.

(Vous avez besoin de Clojure 1.2 ou plus récent pour ce tutoriel.)

```clojure
; Les commentaires commencent avec un point-virgule.

; Clojure est composé de « formes », qui sont simplement des listes
; d'expressions entre parenthèses, séparées par une ou des espaces.
;
; L'interpréteur Clojure suppose que le premier élément est une fonction
; ou une macro, et que le reste contient des arguments.

; Le premier appel dans un fichier doit être ns, pour définir
; l'espace de nom
(ns learnclojure)

; D'autres d'exemples basiques:

; str va créer une chaîne de caractères à partir de tous ses arguments
(str "Hello" " " "World") ; => "Hello World"

; Les opérations mathématiques sont simples
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; L'égalité est =
(= 1 1) ; => true
(= 2 1) ; => false

; Vous avez aussi besoin de not pour la négation logique
(not true) ; => false

; Les formes imbriquées fonctionnent comme on s'y attend
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Types
;;;;;;;;;;;;;

; Clojure utilise les types d'objets Java pour les booléens, les chaînes de
; caractères et les nombres.
; Utilisez `class` pour inspecter les types.
(class 1) ; Les nombres entiers littéraux sont java.lang.Long par défaut
(class 1.); Les flottants littéraux sont java.lang.Double
(class ""); Les chaînes sont toujours entourées de guillemets doubles, et sont java.lang.String
(class false) ; Les booléens sont java.lang.Boolean
(class nil); La valeur "null" est appelée nil

; Si vous voulez créer une liste littérale de données, utilisez ' pour en
; empêcher son évaluation
'(+ 1 2) ; => (+ 1 2)
; (qui est un raccourci pour (quote (+ 1 2)))

; Vous pouvez évaluer une liste "quotée":
(eval '(+ 1 2)) ; => 3

; Collections & séquences
;;;;;;;;;;;;;;;;;;;;;;;;;

; Les listes sont des structures de données en listes chaînées, alors que les
; vecteurs reposent sur des tableaux.
; Les vecteurs et les listes sont des classes Java aussi !
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Une liste serait écrite comme (1 2 3), mais nous devons la quoter
; pour empêcher l'interpréteur de penser que c'est une fonction.
; Et (list 1 2 3) est la même chose que '(1 2 3)

; Les "Collections" sont juste des groupes de données
; Les listes et les vecteurs sont tous deux des collections:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; Les "séquences" (seqs) sont des abstractions à partir de listes de données.
; Seules les listes sont elles-mêmes des séquences.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Une séquence n'a besoin de fournir une entrée que lorsqu'on y accède.
; Donc, les séquences peuvent être "lazy" -- et définir une série infinie:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (une série infinie)
(take 4 (range)) ;  (0 1 2 3)

; Utilisez cons pour ajouter un item au début d'une liste ou d'un vecteur
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Conj ajoutera un item à une collection de la manière la plus efficace
; Pour les listes, conj ajoute l'item au début; pour les vecteurs, à la fin.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Utilisez concat pour ajouter des listes ou vecteurs:
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Utilisez filter, map pour interagir avec des collections
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Utilisez reduce pour les réduire
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce peut aussi prendre un argument pour la valeur initiale
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Fonctions
;;;;;;;;;;;;;;;;;;;;;

; Utilisez fn pour créer de nouvelles fonctions.
; Une fonction renvoie toujours sa dernière expression.
(fn [] "Hello World") ; => fn

; (Vous devez ajouter des parenthèses pour l'appeler)
((fn [] "Hello World")) ; => "Hello World"

; Vous pouvez créer une variable en utilisant def
(def x 1)
x ; => 1

; Assignez une fonction à une variable
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Vous pouvez raccourcir le procédé en utilisant defn
(defn hello-world [] "Hello World")

; [] contient la liste des arguments de la fonction
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Vous pouvez aussi utiliser ce raccourci pour créer des fonctions
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Vous pouvez avoir des fonctions multi-variadiques
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Les fonctions peuvent inclure des arguments supplémentaires dans une séquence
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "Vous avez passé 3 args: (1 2 3)"

; Vous pouvez combiner les arguments normaux et supplémentaires
(defn hello-count [name & args]
  (str "Hello " name ", vous avez passé " (count args) " args supplémentaires"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, vous avez passé 3 args supplémentaires"


; Maps
;;;;;;;;;;;;;;;

; Les hashmaps et les arraymaps partagent une interface. Les hashmaps
; sont interrogés plus rapidement mais ne retiennent pas l'ordre des clefs.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Les array maps deviennent automatiquement des hashmaps pour la
; plupart des opérations si elles deviennent assez larges, donc vous
; n'avez pas à vous en faire.

; Tous les types "hashables" sont acceptés comme clefs, mais en
; général on utilise des mots-clefs ("keywords")
; Les mots-clefs sont comme les chaînes de caractères mais en plus efficaces
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Au passage, les virgules sont toujours traitées comme des espaces et
; ne font rien.

; Sélectionnez une valeur dans une map en l'appelant comme fonction
(stringmap "a") ; => 1
(keymap :a) ; => 1

; Les mots-clefs peuvent aussi être utilisés pour sélectionner leur
; valeur dans une map !
(:b keymap) ; => 2

; N'essayez pas ça avec les chaînes de caractères
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Sélectionner une clef absente renvoie nil
(stringmap "d") ; => nil

; Use assoc to add new keys to hash-maps
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Mais souvenez-vous, les types en Clojure sont immuables !
keymap ; => {:a 1, :b 2, :c 3}

; Utilisez dissoc pour retirer des clefs
(dissoc keymap :a :b) ; => {:c 3}

; Ensembles
;;;;;;;;;;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Ajoutez un élément avec conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Retirez-en un avec disj
(disj #{1 2 3} 1) ; => #{2 3}

; Testez la présence en utilisant l'ensemble comme une fonction
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Il y a encore d'autres fonctions dans l'espace de nom clojure.sets.

; Formes et macros utiles
;;;;;;;;;;;;;;;

; Les constructions logiques en Clojure sont juste des macros, et
ressemblent à toutes les autres formes:
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Utilisez let pour créer des assignations temporaires
(let [a 1 b 2]
  (> a b)) ; => false

; Groupez les énoncés ensemble avec do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Les fonctions ont un do implicit
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; De même pour let
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Utilisez les Threading Macros (-> et ->>) pour exprimer plus
; clairement vos transformations, en y pensant de manière multi-niveaux.

; La "flèche simple" ou "Thread-first", insère, à chaque niveau
; de la transformation, la forme courante en la seconde position
; de la forme suivante, constituant à chaque fois un nouvel étage
; de transformation. Par exemple:
(->  
   {:a 1 :b 2}
   (assoc :c 3) ;=> Génère ici (assoc {:a 1 :b 2} :c 3)
   (dissoc :b)) ;=> Génère ici (dissoc (assoc {:a 1 :b 2} :c 3) :b)

; Cette expression est ré-écrite en:
; (dissoc (assoc {:a 1 :b 2} :c 3) :b)
; et est évaluée en : {:a 1 :c 3}

; La "flèche double" ou "Thread-last" procède de la même manière
; que "->", mais insère le résultat de la réécriture de chaque
; étage en dernière position. Par exemple:
(->>
   (range 10)
   (map inc)     ;=> Génère ici (map inc (range 10)
   (filter odd?) ;=> Génère ici (filter odd? (map inc (range 10))
   (into []))    ;=> Génère ici (into [] (filter odd? (map inc (range 10))), ce qui est évalué au final à;
                 ; [1 3 5 7 9]

; Quand vous êtes dans une situation où vous voulez plus de liberté pour choisir 
; où mettre le résultat des étages précédents, vous pouvez utiliser la
; macro as->. Avec cette macro, donnez un nom spécifique au résultat de la transformation
; précédente pour le placer, à votre guise, où bon vous semble dans l'étage courant:
(as-> [1 2 3] input
  (map inc input);=> Utilisation du résultat en dernière position
  (nth input 4) ;=> et en deuxième position, dans la même expression
  (conj [4 5 6] input [8 9 10])) ;=> ou au milieu !

; Modules
;;;;;;;;;;;;;;;

; Utilisez "use" pour obtenir toutes les fonctions d'un module
(use 'clojure.set)

; Maintenant nous pouvons utiliser les opération de set
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Vous pouvez aussi choisir un sous-ensemble de fonctions à importer
(use '[clojure.set :only [intersection]])

; Utilisez require pour importer un module
(require 'clojure.string)

; Utilisez / pour appeler les fonctions d'un module
; Ici, le module est clojure.string et la fonction est blank?
(clojure.string/blank? "") ; => true

; Vous pouvez associer un nom plus court au module au moment de l'importer
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" dénote une expression régulière)

; Vous pouvez utiliser require (et use, mais ne le faites pas) en
; appelant :require depuis un espace de noms.
; Dans ce cas-là, vous n'avez pas besoin de "quoter" vos modules:
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java a une librairie standard énorme, donc vous voudrez apprendre à
; vous familiariser avec.

; Utilisez import pour charger un module java
(import java.util.Date)

; Vous pouvez importer depuis un ns aussi.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Utilisez les noms de classes avec "." à la fin pour créer une instance
(Date.) ; <un objet date>

; Utilisez . pour invoquer des méthodes. Ou utilisez le raccourci ".method"
(. (Date.) getTime) ; <un timestamp>
(.getTime (Date.)) ; exactement la même chose

; Utilisez / pour appeler des méthodes statiques
(System/currentTimeMillis) ; <un timestamp> (system est toujours présent)

; Utilisez doto to rendre plus tolérable l'interaction avec des
; classes (mutables)
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => Une classe Date. définie comme 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; La mémoire logiciel transactionnelle ("Software Transactional Memory")
; est le mécanisme que Clojure utilise pour gérer les états persistents.
; Il y a plusieurs formes en Clojure qui utilisent cela.

; L'atome est la plus simple. Passez-lui une valeur initiale
(def my-atom (atom {}))

; Mettez à jour un atome avec swap!.
; swap! prend une fonction en argument et l'appelle avec la valeur
; actuelle de l'atome comme premier argument, et les autres arguments
; comme second argument.
(swap! my-atom assoc :a 1) ; Définit my-atom comme le résultat de (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Définit my-atom comme le résultat de (assoc {:a 1} :b 2)

; Use '@' to dereference the atom and get the value
my-atom  ;=> Atom<#...> (Renvoie l'objet Atom)
@my-atom ; => {:a 1 :b 2}

; Voici un simple compteur utilisant un atome
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Les autres formes STM sont les refs et les agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Lectures complémentaires

C'est loin d'être exhaustif, mais assez pour vous permettre de continuer.

Clojure.org propose de nombreux articles:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org a de la documentation avec des exemples pour la
plupart des fonctions principales :
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure est une super manière d'augmenter vos compétences en Clojure et
en programmation fonctionnelle :
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org a pas mal d'article pour débuter :
[http://clojure-doc.org/](http://clojure-doc.org/)
