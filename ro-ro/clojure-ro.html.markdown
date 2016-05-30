---
language: clojure
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Bogdan Paun", "http://twitter.com/bgdnpn"]
filename: learnclojure-ro.clj
lang: ro-ro
---

Clojure este un limbaj din familia Lisp dezvoltat pentru Masina Virtuala Java
(Java Virtual Machine - JVM). Pune un accent mult mai puternic pe
[programarea funcionala](https://en.wikipedia.org/wiki/Functional_programming)
pura decat Common Lisp, dar include utilitare [STM](https://en.wikipedia.org/wiki/Software_transactional_memory)
pentru a gestiona starea, atunci cand aceasta apare.

Combinatia aceasta ii permite sa gestioneze procese concurente foarte usor,
de multe ori in mod automat.

(Aveti nevoie deo versiune Clojure 1.2 sau mai noua)


```clojure
; Comentariile incep cu punct si virgula.

; Clojure se scrie in "forme", care nu sunt decat
; liste de lucruri in interiorul unor paranteze, separate prin spatii.
;
; Reader-ul Clojure presupune ca primul lucru este o
; functie sau un macro de apelat, iar restul sunt argumente.

; Prima apelare intr-un fisier ar trebui sa fie ns, pentru a configura namespace-ul
(ns learnclojure)

; Mai multe exemple de baza:

; str va crea un string folosint toate argumentele sale
(str "Hello" " " "World") ; => "Hello World"

; Matematica este simpla
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Egalitatea este =
(= 1 1) ; => true
(= 2 1) ; => false

; Folosim si not pentru logica
(not true) ; => false

; Formele imbricate functioneaza asa
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipuri
;;;;;;;;;;;;;

; Clojure foloseste sistemul de obiecte Java pentru boolean, string si numere.
; Folositi `class` pentru a le inspecta.
(class 1) ; Numere intregi sunt jaba.lang.Long, in mod normal
(class 1.); Numelere reale sunt java.lang.Double
(class ""); Sirurile de caractere sunt mere intre apostrofuri duble, si sunt java.lang.String
(class false) ; Booleanele sunt java.lang.Boolean
(class nil); Valoarea "null" este numita nil

; Daca doriti sa creati o lista de date literale, folositi ' pentru a preveni
; evaluarea ei
'(+ 1 2) ; => (+ 1 2)
; (prescurtare pentru (quote (+ 1 2)))

; Puteti evalua o lista cu apostrof
(eval '(+ 1 2)) ; => 3

; Colectii & Secvente
;;;;;;;;;;;;;;;;;;;

; Listele sunt structuri de date lista-inlantuita, spre deosebire de Vectori
; Vectorii si Listele sunt si ele clase Java!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; O liste ar putea fi scrisa direct ca (1 2 3), dar trebuie sa folosim apostrof
; pentru a preveni reader-ul din a crede ca e o functie.
; De asemenea, (list 1 2 3) este acelasi lucru cu '(1 2 3)

; "Colectiile" sunt grupuri de date
; Atat listele cat si vectorii sunt colectii:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "Sequences" (seqs) are abstract descriptions of lists of data.
; Only lists are seqs.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; O secventa necesita un punct de intrare doar cand este accesata.
; Deci, secventele, care pot fi "lazy" -- pot defini serii infinite:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (o serie infinita)
(take 4 (range)) ;  (0 1 2 3)

; Folositi cons pentru a adauga un element la inceputul unei liste sau unui vector
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Conj va adauga un element unei colectii in modul cel mai eficient.
; Pentru liste, aceastea sunt inserate la inceput. Pentru vectori, sunt inserate la final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Folositi concat pentru a uni liste sau vectori
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Folositi filter, map pentru a interactiona cu colectiile
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Folositi reduce pentru a le reduce
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce poate lua un argument valoare-initiala
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Functii
;;;;;;;;;;;;;;;;;;;;;

; Folositi fn pentru a crea functii noi. O functie returneaza intotdeauna
; ultima sa instructiune.
(fn [] "Hello World") ; => fn

; (Necesita paranteze suplimentare pentru a fi apelata)
((fn [] "Hello World")) ; => "Hello World"

; Puteti crea o variabila folosind def
(def x 1)
x ; => 1

; Atribuiti o functie unei variabile
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Puteti scurta acest proces folosind defn
(defn hello-world [] "Hello World")

; Elementul [] este lista de argumente a functiei.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Puteti, de asemenea, folosi aceasta prescurtare pentru a crea functii:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Puteti avea si functii cu mai multe variabile
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Functiile pot primi mai mult argumente dintr-o secventa
(defn count-args [& args]
  (str "Ati specificat " (count args) " argumente: " args))
(count-args 1 2 3) ; => "Ati specificat 3 argumente: (1 2 3)"

; Puteti interschimba argumente normale si argumente-secventa
(defn hello-count [name & args]
  (str "Salut " name ", ati specificat " (count args) " argumente extra"))
(hello-count "Finn" 1 2 3)
; => "Salut Finn, ai specificat 3 argumente extra"


; Maps (Dictionare)
;;;;;;;;;;

; Hash maps si Array maps impart o interfata. Hash maps au cautari mai rapide
; dar nu retin ordinea cheilor.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Arraymaps de vin automat hashmaps prin majoritatea operatiilor
; daca sunt suficient de mari, asa ca nu trebuie sa va preocupe acest aspect.

; Dictionarele pot folosi orice tip hashable ca si cheie, dar cuvintele cheie
; (keywords) sunt, de obicei, cele mai indicate. Cuvintele cheie sunt ca niste
; siruri de caractere cu un plus de eficienta
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Apropo, virgulele sunt intotdeauna considerate echivalente cu spatiile.

; Apelati un dictionar (map) ca pe o functie pentru a primi o valoare anume
(stringmap "a") ; => 1
(keymap :a) ; => 1

; Cuvintele cheie pot fi folosite si ele pentru a "cere" dictionarului valorile lor!
(:b keymap) ; => 2

; Nu incercati asta cu siruri de caractere.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Recuperarea unei chei inexistente returneaza nil
(stringmap "d") ; => nil

; Folositi assoc pentru a adauga nou chei unui ductionar
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Dar retineti ca tipurile sunt imuabile in clojure
keymap ; => {:a 1, :b 2, :c 3}

; Folositi dissoc pentru a elimina chei
(dissoc keymap :a :b) ; => {:c 3}

; Seturi (multimi)
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Adaugati un membru cu conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Eliminati unul cu disj
(disj #{1 2 3} 1) ; => #{2 3}

; Testati existenta unuia folosing setul ca o functie:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Exista mai multe functii in namespace-ul clojure.sets.

; Forme utile
;;;;;;;;;;;;;;;;;

; In Clojure constructiile logice sunt macro-uri, si arata ca
; oricare alta forma
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Folositi let pentru a crea atribuiri temporare
(let [a 1 b 2]
  (> a b)) ; => false

; Grupati instructiuni impreuna folosind do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Functiile contin un do implicit
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; Asemanator pentru let
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Module
;;;;;;;;;;;;;;;

; Folositi "use" pentru a recupera toate functiile dintr-un modul
(use 'clojure.set)

; Acum putem folosi operatiuni pe seturi
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Puteri de asemenea alege un subset al functiilor de importat
(use '[clojure.set :only [intersection]])

; Folositi require pentru a importa un modul
(require 'clojure.string)

; Folositi / pentru a apela functii dintr-un modul
; In acest caz, modulul este clojure.string, iar functia este blank?
(clojure.string/blank? "") ; => true

; Puteti atribui un nume mai scurt unui modul in momentul importului
(require '[clojure.string :as str])
(str/replace "Acesta este un test." #"[a-o]" str/upper-case) ; => "ACEstA EstE un tEst."
; (#"" denota o expresie regulata)

; Puteti folsi require (sau use, contraindicat) dintr-un namespace folosind :require.
; Nu trebuie sa folositi apostrof pentru module daca procedati astfel.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java are o biblioteca standard imensa si folositoare, deci
; ar fi util sa stiti cum sa o folositi.

; Folositi import pentru a incarca un modul Java
(import java.util.Date)

; Puteti importa si dintr-un namesopace.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Folositi numele clasei cu "." la final pentru a crea o noua instanta
(Date.) ; <a date object>

; Folositi . pentru a apela metode. Pe scurt, folositi ".method"
(. (Date.) getTime) ; <a timestamp>
(.getTime (Date.)) ; exact acelasi lucru.

; Folositi / pentru a apela metode statice
(System/currentTimeMillis) ; <a timestamp> (System este prezent intotdeauna)

; Folositi doto pentru a gestiona clase (mutable) mai usor
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory este un mecanism folost de Clojure pentru
; a gestiona stari persistente. Sunt putine instante in care este folosit.

; Un atom este cel mai simplu exemplu. Dati-i o valoare initiala
(def my-atom (atom {}))

; Modificati-l cu swap!.
; swap! primeste o functie si o apeleaza cu valoarea actuala a atomului
; ca prim argument si orice argumente suplimentare ca al doilea
(swap! my-atom assoc :a 1) ; Atomul ia valoarea rezultata din (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Atomul ia valoarea rezultata din (assoc {:a 1} :b 2)

; Folositi '@' pentru a dereferentia atomul si a-i recupera valoarea
my-atom  ;=> Atom<#...> (Returmeaza obiectul Atom)
@my-atom ; => {:a 1 :b 2}

; Aici avem un contor simplu care foloseste un atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Alte utilizari ale STM sunt referintele (refs) si agentii (agents).
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Lectura suplimentara

Lista nu este in niciun caz exhaustiva, dar speram ca este suficienta pentru
a va oferi un inceput bun in Clojure.

Clojure.org contine multe articole:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org contine documentatie cu exemple pentru majoritatea functiilor de baza:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure este o metoda excelenta pentru a exersa Clojure/FP (Programarea Functionala):
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org are un numar de article pentru incepatori:
[http://clojure-doc.org/](http://clojure-doc.org/)
