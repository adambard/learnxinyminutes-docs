---
language: racket
filename: learnracket-fr.rkt
contributors:
  - ["th3rac25", "https://github.com/voila"]
  - ["Eli Barzilay", "https://github.com/elibarzilay"]
  - ["Gustavo Schmidt", "https://github.com/gustavoschmidt"]
translators:
  - ["Xavier Nayrac", "https://github.com/lkdjiin"]
lang: fr-fr
---

Racket est un langage de programmation généraliste, multi-paradigme,
descendant de Lisp/Scheme.

Les retours et commentaires sont appréciés ! Vous pouvez joindre l'auteur
original ici :
[@th3rac25](http://twitter.com/th3rac25) ou là : th3rac25 [at] [google's email
service]. Vous pouvez joindre le traducteur de ce document ici :
[@lkdjiin](http://twitter.com/lkdjiin).

```racket
#lang racket ; défini le dialecte à utiliser.

;;; Commentaires

;; Une ligne de commentaire commence par un point-virgule.

#| Un bloc de commentaires
   peut tenir sur plusieurs lignes…
    #|
       et on peut les imbriquer !
    |#
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Types de données et opérateurs primitifs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nombres
9999999999999999999999 ; entier
#b111                  ; binaire => 7
#o111                  ; octal => 73
#x111                  ; hexadécimal => 273
3.14                   ; réel
6.02e+23
1/2                    ; rationnel
1+2i                   ; complexe

;; Un appel de fonction s'écrit (f x y z ...)
;; où f est une fonction et x, y, z, ... sont des arguments.
;; Si vous voulez créer une liste littérales, utilisez ' pour
;; empécher l'évaluation de la liste.
'(+ 1 2) ; => (+ 1 2)
;; Et maintenant, un peu d'arithmétique
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(expt 2 3) ; => 8
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(/ 35 5) ; => 7
(/ 1 3) ; => 1/3
(exact->inexact 1/3) ; => 0.3333333333333333
(+ 1+2i  2-3i) ; => 3-1i

;;; Booléens
#t ; pour vrai
#f ; pour faux -- Toute autre valeur que #f est vraie
(not #t) ; => #f
(and 0 #f (error "doesn't get here")) ; => #f
(or #f 0 (error "doesn't get here"))  ; => 0

;;; Caractères
#\A ; => #\A
#\λ ; => #\λ
#\u03BB ; => #\λ

;;; Une chaîne de caractères est un tableau de caractères de longueur
;;; fixe.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; Le backslash est le caractère d'échappement
"Foo\tbar\41\x21\u0021\a\r\n" ; Sont inclus les échappements de type C
                              ; et unicode
"λx:(μα.α→α).xx"              ; une chaîne peut inclure de l'unicode

;; On peut ajouter une chaîne à une autre
(string-append "Hello " "world!") ; => "Hello world!"

;; Une chaîne peut être traitée comme une liste de caractères
(string-ref "Apple" 0) ; => #\A

;; format est utilisé pour formatter une chaîne
(format "~a can be ~a" "strings" "formatted")

;; L'affichage est tout simple
(printf "I'm Racket. Nice to meet you!\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vous pouvez créer une variable à l'aide de define
;; Une variable peut contenir n'importe quel caractères, à l'exception
;; de : ()[]{}",'`;#|\
(define some-var 5)
some-var ; => 5

;; Vous pouvez aussi utiliser des caractères unicode
(define ⊆ subset?)
(⊆ (set 3 2) (set 1 2 3)) ; => #t

;; Accéder à une variable non-initialisée provoque une exception
; x ; => x: indéfini ...

;; Déclaration locale : `me` est attaché à "Bob" seulement à l'intérieur
;; de (let ...)
(let ([me "Bob"])
  "Alice"
  me) ; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Structures and Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structures
(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;;; Paires (non mutable)
;; `cons` construit une paire, `car` et `cdr` extraient respectivement le
;; premier et le second élément.
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;;; Listes

;; Les listes en Racket sont des structures de données de type *linked-list*,
;; produites avec des paires assemblées avec `cons` et terminée par `null`
;; (ou '()).
(cons 1 (cons 2 (cons 3 null))) ; => '(1 2 3)
;; `list` est un constructeur variadique plus commode à utiliser
(list 1 2 3) ; => '(1 2 3)
;; et un guillemet simple peut aussi être utilisé pour une liste littérale
'(1 2 3) ; => '(1 2 3)

;; On peut toujours utiliser `cons` pour ajouter un élément au début
;; d'une liste
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; Utilisez `append` pour ajouter une liste à une autre
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; Une liste est un type très basique, il y a donc *beaucoup* de
;; fonctionnalités qui leur sont dédiées, quelques exemples :
(map add1 '(1 2 3))          ; => '(2 3 4)
(map + '(1 2 3) '(10 20 30)) ; => '(11 22 33)
(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; Vecteurs

;; Un vecteur est un tableau de taille fixe
#(1 2 3) ; => '#(1 2 3)

;; Utilisez `vector-append` pour additionner des vecteurs entre eux
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Sets

;; Créez un set à partir d'une liste
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

;; Ajoutez un membre avec `set-add`
;; (Fonctionnel: renvoit le set étendu, plutôt que de muter le set en entrée)
(set-add (set 1 2 3) 4) ; => (set 1 2 3 4)

;; Retirez un membre avec `set-remove`
(set-remove (set 1 2 3) 1) ; => (set 2 3)

;; Testez l'existence d'un membre avec `set-member?`
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; Tables de hashage

;; Créer une table de hashage non-mutable (un exemple mutable plus loin)
(define m (hash 'a 1 'b 2 'c 3))

;; Retrouver une valeur
(hash-ref m 'a) ; => 1

;; Chercher une valeur inexistante provoque une exceptions
; (hash-ref m 'd) => no value found

;; Vous pouvez fournir une valeur par défaut pour les clés manquantes
(hash-ref m 'd 0) ; => 0

;; Utilisez `hash-set` pour étendre une table de hashage non-mutable
;; (Renvoit la table étendu, plutôt que de la muter)
(define m2 (hash-set m 'd 4))
m2 ; => '#hash((b . 2) (a . 1) (d . 4) (c . 3))

;; Rappelez-vous, ces tables de hashage sont non-mutables !
m ; => '#hash((b . 2) (a . 1) (c . 3))  <-- no `d'

;; Utilisez `hash-remove` pour supprimer des clés (également fonctionnel)
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Fonctions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utilisez `lambda` pour créer des fonctions.
;; Une fonction renvoie toujours la valeur de sa dernière expression.
(lambda () "Hello World") ; => #<procedure>
;; On peut aussi utiliser le caractère unicode `λ'
(λ () "Hello World")     ; => même fonction

;; Utilisez des parenthèses pour appeler toutes les fonctions, ce qui
;; inclus aussi les expressions lambda
((lambda () "Hello World")) ; => "Hello World"
((λ () "Hello World"))      ; => "Hello World"

;; Assignez une fonction à une variable
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

;; Vous pouvez raccourcir ceci en utilisant le sucre syntaxique pour la
;; définition de fonction :
(define (hello-world2) "Hello World")

;; Entre les () après lambda, vous déclarez la liste des arguments de la
;; fonction
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"
;; … ou alors, en utilisant le sucre syntaxique, ce qui suit est équivalent
(define (hello2 name)
  (string-append "Hello " name))

;; Vous pouvez obtenir des fonctions variadique en utilisant `case-lambda`
(define hello3
  (case-lambda
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"
;; … ou spécifier des arguments optionnels avec une valeur par défaut
(define (hello4 [name "World"])
  (string-append "Hello " name))

;; Les fonctions peuvent rassembler des arguments supplémentaires dans une
;; liste
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"
;; … ou bien avec `lambda`, sans sucre syntaxique
(define count-args2
  (lambda args
    (format "You passed ~a args: ~a" (length args) args)))

;; Vous pouvez mixer arguments réguliers et supplémentaires
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"
;; … sans sucre syntaxique
(define hello-count2
  (lambda (name . args)
    (format "Hello ~a, you passed ~a extra args" name (length args))))

;; Avec des mot-clés cette fois
(define (hello-k #:name [name "World"] #:greeting [g "Hello"] . args)
  (format "~a ~a, ~a extra args" g name (length args)))
(hello-k)                 ; => "Hello World, 0 extra args"
(hello-k 1 2 3)           ; => "Hello World, 3 extra args"
(hello-k #:greeting "Hi") ; => "Hi World, 0 extra args"
(hello-k #:name "Finn" #:greeting "Hey") ; => "Hey Finn, 0 extra args"
(hello-k 1 2 3 #:greeting "Hi" #:name "Finn" 4 5 6)
                                         ; => "Hi Finn, 6 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Égalité
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour les nombres, utilisez `=`
(= 3 3.0) ; => #t
(= 2 1) ; => #f

;; Pour tester l'identité des objets, utilisez `eq?`
(eq? 3 3) ; => #t
(eq? 3 3.0) ; => #f
(eq? (list 3) (list 3)) ; => #f

;; Pour les collections, utilisez `equal?`
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Structures de contrôle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conditions

(if #t               ; expression pour le test
    "this is true"   ; expression si vrai
    "this is false") ; expression si faux
; => "this is true"

;; Dans les condition, toutes les valeurs non-fausses sont traitées commentaires
;; étant vraies (c'est à dire toutes sauf #f)
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; `cond` permet d'enchaîner une série de tests afin d'obtenir un résultat
(cond [(> 2 2) (error "wrong!")]
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

;;; Filtrage par motif (*pattern matching*)

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;;; Les boucles

;; On peut boucler en utilisant la récursion (terminale)
(define (loop i)
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i))))
(loop 5) ; => i=5, i=6, ...

;; D'une manière similaire, avec un `let` nommé
(let loop ((i 0))
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i)))) ; => i=0, i=1, ...

;; Voir plus loin pour l'ajout d'une nouvelle forme `loop`, mais Racket
;; possède déjà une forme `for` flexible et élaborée pour les itérations
(for ([i 10])
  (printf "i=~a\n" i)) ; => i=0, i=1, ...
(for ([i (in-range 5 10)])
  (printf "i=~a\n" i)) ; => i=5, i=6, ...

;;; Itérer sur autre chose que des nombres
;; `for` permet d'itérer sur plein de type de séquences:
;; listes, vecteurs, chaînes de caractères, sets, tables de hashage, etc

(for ([i (in-list '(l i s t))])
  (displayln i))

(for ([i (in-vector #(v e c t o r))])
  (displayln i))

(for ([i (in-string "string")])
  (displayln i))

(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))

(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3 ))])
  (printf "key:~a value:~a\n" k v))

;;; Itérations plus complexes

;; Balayage parallèle de plusieurs séquences (on stoppe sur la plus petite)
(for ([i 10] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x 1:y 2:z

;; Boucles imbriquées
(for* ([i 2] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x, 0:y, 0:z, 1:x, 1:y, 1:z

;; Conditions dans les boucles
(for ([i 1000]
      #:when (> i 5)
      #:unless (odd? i)
      #:break (> i 10))
  (printf "i=~a\n" i))
; => i=6, i=8, i=10

;;; Compréhensions de liste
;; Très similaires aux boucles `for` -- renvoient en plus une collection

(for/list ([i '(1 2 3)])
  (add1 i)) ; => '(2 3 4)

(for/list ([i '(1 2 3)] #:when (even? i))
  i) ; => '(2)

(for/list ([i 10] [j '(x y z)])
  (list i j)) ; => '((0 x) (1 y) (2 z))

(for/list ([i 1000] #:when (> i 5) #:unless (odd? i) #:break (> i 10))
  i) ; => '(6 8 10)

(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
; => '#hash((1 . "1") (2 . "2") (3 . "3"))

;; Il y a plein d'autres fonctions natives pour collecter des données à
;; l'aide de boucles
(for/sum ([i 10]) (* i i)) ; => 285
(for/product ([i (in-range 1 11)]) (* i i)) ; => 13168189440000
(for/and ([i 10] [j (in-range 10 20)]) (< i j)) ; => #t
(for/or ([i 10] [j (in-range 0 20 2)]) (= i j)) ; => #t
;; Et pour n'importe quell combinaison arbitraire, utilisez `for/fold`
(for/fold ([sum 0]) ([i '(1 2 3 4)]) (+ sum i)) ; => 10
;; (Ceci peut souvent remplacer des boucles communes de style impératif)

;;; Exceptions

;; Pour capturer une exception, utilisez la forme `with-handlers`
(with-handlers ([exn:fail? (lambda (exn) 999)])
  (+ 1 "2")) ; => 999
(with-handlers ([exn:break? (lambda (exn) "no time")])
  (sleep 3)
  "phew") ; => "phew", but if you break it => "no time"

;; Utilisez `raise` pour soulever une exception, ou encore n'importe quelle
;; autre valeur
(with-handlers ([number?    ; capturer la valeur numérique soulevée
                 identity]) ; la renvoyer en tant que valeur simple
  (+ 1 (raise 2))) ; => 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutabilité
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utilisez `set!` pour réassigner une valeur à une variable existante
(define n 5)
(set! n (add1 n))
n ; => 6

;; Utilisez le mécanisme des boites (*box*) pour les valeurs explicitement
;; mutables (similaire aux pointeurs ou références dans d'autres langages)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*) ; => 6

;; Beaucoup de types de données en Racket sont non-mutables (paires, listes,
;; etc), certains ont à la fois une version mutable et une version
;; non-mutable (chaînes, vecteurs, tables de hashage, etc)

;; Utilisez `vector` ou `make-vector` pour créer des vecteurs mutables
(define vec (vector 2 2 3 4))
(define wall (make-vector 100 'bottle-of-beer))
;; Utilisez `vector-set!` pour mettre à jour un emplacement
(vector-set! vec 0 1)
(vector-set! wall 99 'down)
vec ; => #(1 2 3 4)

;; Créer une table de hashage mutable vide et la manipuler
(define m3 (make-hash))
(hash-set! m3 'a 1)
(hash-set! m3 'b 2)
(hash-set! m3 'c 3)
(hash-ref m3 'a)   ; => 1
(hash-ref m3 'd 0) ; => 0
(hash-remove! m3 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les modules permettent d'organiser le code en plusieurs fichiers
;; et bibliothèques réutilisables. Ici, nous utiliserons des sous-modules,
;; imbriqués dans le grand module que forme ce texte (qui démarre à la
;; ligne `#lang`).

(module cake racket/base ; défini un module `cake', basé sur racket/base

  (provide print-cake) ; fonction exportée par le module (publique)

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch) ; fonction interne/privée
    (printf fmt (make-string n ch))
    (newline)))

;; Utilisez `require` pour importer les fonctions fournies par un
;; module (provide)
(require 'cake) ; le ' est pour un sous-module local
(print-cake 3)
; (show "~a" 1 #\A) ; => erreur, `show` n'est pas exportée

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Classes et objets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Créer une classe fish% (% est idiomatique pour les noms de classes)
(define fish%
  (class object%
    (init size) ; argument pour l'initialisation
    (super-new) ; initialisation de la super-classe
    ;; Les champs/membres/variables de classe
    (define current-size size)
    ;; Méthodes publiques
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

;; Créer une instance de fish%
(define charlie
  (new fish% [size 10]))

;; Utilisez `send` pour appeler une méthode d'un objet
(send charlie get-size) ; => 10
(send charlie grow 6)
(send charlie get-size) ; => 16

;; `fish%` est une simple valeur de «première classe», ce qui va permettre
;; la composition (*mixins*)
(define (add-color c%)
  (class c%
    (init color)
    (super-new)
    (define my-color color)
    (define/public (get-color) my-color)))
(define colored-fish% (add-color fish%))
(define charlie2 (new colored-fish% [size 10] [color 'red]))
(send charlie2 get-color)
;; ou, sans les noms:
(send (new (add-color fish%) [size 10] [color 'red]) get-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les macros permettent d'étendre la syntaxe du langage

;; Ajoutons une boucle `loop`
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(let ([i 0])
  (while (< i  10)
    (displayln i)
    (set! i (add1 i))))

;; Les macros sont hygiéniques, vous ne pouvez pas *clasher* avec les
;; variables existantes !
(define-syntax-rule (swap! x y) ; ! est idiomatique pour la mutation
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
;; La variable `tmp` est renommée en `tmp_1`
;; dans le but d'éviter un conflit de nom
;; (let ([tmp_1 tmp])
;;   (set! tmp other)
;;   (set! other tmp_1))

;; Mais il faut quand même faire bien attention avec les macros, par exemple:
(define-syntax-rule (bad-while condition body ...)
  (when condition
    body ...
    (bad-while condition body ...)))
;; cette macro est cassée : ell génère un code infini, si vous l'essayez
;; le compilateur va entrer dans une boucle infinie.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Contrats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les contrats imposent des contraintes aux valeurs exportées depuis
;; les modules

(module bank-account racket
  (provide (contract-out
            [deposit (-> positive? any)] ; un dépot est toujours positif
            [balance (-> positive?)]))

  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount)
  )

(require 'bank-account)
(deposit 5)

(balance) ; => 5

;; Les clients qui essaient de déposer un montant non-positif sont blamés
;; (deposit -5) ; => deposit: contract violation
;; expected: positive?
;; given: -5
;; more details....
```

## Pour aller plus loin

Vous en voulez plus ? Essayez
[Getting Started with Racket](http://docs.racket-lang.org/getting-started/)
