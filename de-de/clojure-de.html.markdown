---
language: clojure
filename: learnclojure-de.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

Clojure ist ein Lispdialekt, der für die Java Virtual Maschine entwickelt worden ist. Sie hat eine stärkere Betonung auf reine [funktionale Programmierung](https://en.wikipedia.org/wiki/Functional_programming) als Common Lisp. Jedoch besitzt sie je nach Bedarf mehrere [STM](https://en.wikipedia.org/wiki/Software_transactional_memory) Werkzeuge zur Handhabung von Zustand.

Diese Verknüpfung erlaubt es, parallele Verarbeitung sehr einfach und häufig automatisch zu verarbeiten.

(Du brauchst die Clojure Version 1.2 oder neuer)

```clojure
; Kommentare starten mit einem Semikolon.

; Clojure wird in "Forms" geschrieben, was nur Listen von Dingen
; in Klammern sind, getrennt durch Leerzeichen.
;
; Der Clojure Leser nimmt an, dass das Erste, was aufgerufen wird
; eine Funktion oder ein Makro ist, der Rest sind Argumente.

; Der erste Aufruf in einer Datei sollte ns sein, um den Namespace zu setzen.
(ns learnclojure)

; Weitere einfache Beispiele:

; str erstellt einen String aus allen Argumenten
(str "Hallo" " " "Welt") ; => "Hallo Welt"

; Mathe ist einfach
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Gleichheit ist =
(= 1 1) ; => true
(= 2 1) ; => false

; Du brauchst auch not für Logik
(not true) ; => false

; Verschachtelte Forms funktionieren, wie erwartet
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Typen
;;;;;;;;;;;;;

; Clojure verwendet Javas Objekt Typen für Booleans, Strings und Zahlen.
; Verwende `class` um sie zu inspizieren.
(class 1) ; Integer-Literale sind standardmäßig java.lang.Long
(class 1.); Float-Literale sind java.lang.Double
(class ""); Strings sind immer in doppelten Anführungszeichen notiert und sind java.lang.String
(class false) ; Booleans sind java.lang.Boolean
(class nil); Der "null" Wert heißt nil

; Wenn du ein literale Liste aus Daten erzeugen willst, verwendest du ' um
; zu verhindern dass es evaluiert wird
'(+ 1 2) ; => (+ 1 2)
; (Kurzform für (quote (+ 1 2)))

; Du kannst eine zitierte Liste evaluieren
(eval '(+ 1 2)) ; => 3

; Kollektionen & Sequenzen
;;;;;;;;;;;;;;;;;;;

; Listen sind Linked-Lists Datenstrukturen, während Vektoren arraybasierend sind.
; Vektoren und Listen sind auch Java Klassen!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Eine Liste würde nur als (1 2 3) geschrieben, aber wir müssen es zitieren
; damit der Leser aufhört zu denken, es sei eine Funktion.
; Außerdem ist (list 1 2 3) dasselbe, wie '(1 2 3)

; "Kollektionen" sind nur Gruppen von Daten
; Listen und Vektoren sind Kollektionen:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "Sequenzen" (seqs) sind abstrakte Beschreibungen von Listen von Daten.
; Nur Listen sind seqs.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Ein seq muss nur einen Eintrittspunkt bereitstellen, wenn auf ihm zugegriffen wird.
; Das heißt, dass seqs faul sein können -- Mit ihnen kann man unendliche Serien beschreiben.
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (eine unendliche Serie)
(take 4 (range)) ;  (0 1 2 3)

; Verwende cons um ein Item zum Anfang einer Liste oder eines Vektors hinzuzufügen.
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Conj fügt ein Item auf die effizienteste Weise zu einer Kollektion hinzu.
; Für Listen fügt er sie an den Anfang hinzu. Für Vektoren fügt er sie an das Ende hinzu.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Verwende concat um Listen und Vektoren miteinander zu verbinden
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Verwende filter, map um mit Kollektionen zu interagieren
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Verwende reduce um sie zu reduzieren
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce kann auch einen Initialwert als Argument verwenden
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Funktionen
;;;;;;;;;;;;;;;;;;;;;

; Verwende fn um neue Funktionen zu erstellen. Eine Funktion gibt immer ihr
; letztes Statement zurück.
(fn [] "Hallo Welt") ; => fn

; (Du brauchst exta Klammern um sie aufzurufen)
((fn [] "Hallo Welt")) ; => "Hallo Welt"

; Du kannst eine Variable mit def erstellen
(def x 1)
x ; => 1

; Weise eine Funktion einer Variable zu
(def hello-world (fn [] "Hallo Welt"))
(hello-world) ; => "Hallo Welt"

; Du kannst den Prozess verkürzen indem du defn verwendest
(defn hello-world [] "Hallo Welt")

; [] ist die Liste der Argumente für die Funktion
; The [] is the list of arguments for the function.
(defn hello [name]
  (str "Hallo " name))
(hello "Steve") ; => "Hallo Steve"

; Du kannst diese Kurzschreibweise verwenden um Funktionen zu erstellen:
(def hello2 #(str "Hallo " %1))
(hello2 "Julie") ; => "Hallo Julie"

; Du kannst auch multi-variadische Funktionen haben
(defn hello3
  ([] "Hallo Welt")
  ([name] (str "Hallo " name)))
(hello3 "Jake") ; => "Hallo Jake"
(hello3) ; => "Hallo Welt"

; Funktionen können auch extra Argumente in einem seq für dich speichern
(defn count-args [& args]
  (str "Du hast " (count args) " Argumente übergeben: " args))
(count-args 1 2 3) ; => "Du hast 3 Argumente übergeben: (1 2 3)"

; Du kannst reguläre und gepackte Argumente mischen
(defn hello-count [name & args]
  (str "Hallo " name ", Du hast " (count args) " extra Argumente übergeben"))
(hello-count "Finn" 1 2 3)
; => "Hallo Finn, Du hast 3 extra Argumente übergeben"


; Maps
;;;;;;;;;;

; Hash maps und Array maps teilen sich ein Interface. Hash maps haben eine schnellere Zugriffszeit,
; aber behalten keine Schlüsselreihenfolge.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Arraymaps werden durch die meisten Operationen automatisch zu Hashmaps,
; sobald sie groß genug werden. Das heißt du musst dich nicht darum sorgen.

; Maps können einen beliebigen Hash-Typ als Schlüssel verwenden, in der Regel
; sind jedoch Keywords am besten. Keywords sind wie Strings, jedoch besitzen sie
; Performance-Vorteile
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Übrigens werden Kommas als Leerzeichen behandelt und machen nichts.

; Rufe einen Wert von einer Map ab, indem du sie als Funktion aufrufst
(stringmap "a") ; => 1
(keymap :a) ; => 1

; Keywords können auch verwendet werden um ihren Wert aus der Map zu bekommen!
(:b keymap) ; => 2

; Versuche es nicht mit Strings.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Das Abrufen eines nicht vorhandenen Keys gibt nil zurück
(stringmap "d") ; => nil

; Verwende assoc um einen neuen Key zu einer Hash-map hinzuzufügen
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Aber denk daran, Clojure Typen sind unveränderlich!
keymap ; => {:a 1, :b 2, :c 3}

; Verwende dissoc um Keys zu entfernen
(dissoc keymap :a :b) ; => {:c 3}

; Sets
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Füge ein Element mit conj hinzu
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Entferne ein Element mit disj
(disj #{1 2 3} 1) ; => #{2 3}

; Teste auf Existenz, indem du das Set als Funktion verwendest:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Es gibt mehr Funktionen in dem clojure.sets Namespace.

; Nützliche Forms
;;;;;;;;;;;;;;;;;

; Logische Konstrukte in Clojure sind nur Makros und sie sehen, wie alles
; andere aus
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Verwende let um temporäre Bindungen aufzubauen
(let [a 1 b 2]
  (> a b)) ; => false

; Gruppiere Statements mit do zusammen
; Group statements together with do
(do
  (print "Hallo")
  "Welt") ; => "Welt" (prints "Hallo")

; Funktionen haben ein implizites do
(defn print-and-say-hello [name]
  (print "Sage Hallo zu " name)
  (str "Hallo " name))
(print-and-say-hello "Jeff") ;=> "Hallo Jeff" (prints "Sage Hallo zu Jeff")

; let macht das auch
(let [name "Urkel"]
  (print "Sage Hallo zu " name)
  (str "Hallo " name)) ; => "Hallo Urkel" (prints "Sage Hallo zu Urkel")


; Verwende die Threading Makros (-> and ->>) um Transformationen von
; Daten deutlicher auszudrücken.

; Das "Thread-zuerst" Makro (->) fügt in jede Form das Ergebnis des
; Vorherigen als erstes Argument (zweites Element) ein.
(->
   {:a 1 :b 2}
   (assoc :c 3) ;=> (assoc {:a 1 :b 2} :c 3)
   (dissoc :b)) ;=> (dissoc (assoc {:a 1 :b 2} :c 3) :b)

; Dieser Ausdruck kann auch als so geschrieben werden:
; (dissoc (assoc {:a 1 :b 2} :c 3) :b)
; and evaluates to {:a 1 :c 3}

; Der Doppelpfeil macht das Selbe, aber er fügt das Ergebnis von jeder
; Zeile an das Ende der Form, Das ist vor allem für Operationen auf
; Kollektionen nützlich:
(->>
   (range 10)
   (map inc)     ;=> (map inc (range 10)
   (filter odd?) ;=> (filter odd? (map inc (range 10))
   (into []))    ;=> (into [] (filter odd? (map inc (range 10)))
                 ; Result: [1 3 5 7 9]

; Wenn du in einer Situation bist, in der du mehr Freiheit willst,
; wohin du das Ergebnis vorheriger Datentransformationen in einem Ausdruck
; platzieren möchtest, kannst du das as-> Macro verwenden. Mit diesem Macro
; kannst du einen speziellen Namen auf die Ausgabe einer Transformationen geben.
; Du kannst es als Platzhalter in verketteten Ausdrücken verwenden:

(as-> [1 2 3] input
  (map inc input);=> Du kannst die letzte Ausgabe der Transformation in der letzten Position verwenden
  (nth input 2) ;=>  und auch in der zweiten Position, im selben Ausdruck verwenden
  (conj [4 5 6] input [8 9 10])) ;=> oder auch in der Mitte!



; Module
;;;;;;;;;;;;;;;

; Verwende "use" um alle Funktionen aus einem Modul zu bekommen
(use 'clojure.set)

; Nun können wir set Operationen verwenden
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Du kannst auch auswählen nur ein Subset von Funktionen zu importieren
(use '[clojure.set :only [intersection]])

; Verwende require um ein Modul zu importieren
(require 'clojure.string)

; Verwende / um eine Funktion aus einem Modul aufzurufen
; Hier verwenden wir das Modul clojure.string und die Funktion blank?
(clojure.string/blank? "") ; => true

; Du kannst auch einem Modul einen kürzeren Namen beim Import geben
(require '[clojure.string :as str])
(str/replace "Das ist ein Test." #"[a-o]" str/upper-case) ; => "DAs IsT EIN TEsT."
; (#"" bezeichnet einen regulären literalen Ausdruck)

; Du kannst require aus einem Namespace verwenden (auch use ist möglich, aber nicht zu empfehlen)
; indem du :require verwendest.
; Du brauchst keine Zitierzeichen für deine Module verwenden, wenn du
; es auf diese Weise machst.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java hat eine riesige und nützliche Standardbibliothek,
; du möchtest lernen wie man sie verwendet.

; Verwende import um ein Java modul zu laden.
(import java.util.Date)

; Du kannst auch von einem ns importieren.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Verwende den Klassennamen mit einem "." am Ende, um eine neue Instanz zu erstellen
(Date.) ; <a date object>

; Verwende . um Methoden aufzurufen oder verwende die ".method" Abkürzung
(. (Date.) getTime) ; <a timestamp>
(.getTime (Date.)) ; Genau das Selbe

; Verwende / um statische Methoden aufzurufen
(System/currentTimeMillis) ; <a timestamp> (system ist immer da)

; Verwende doto um mit veränderliche Klassen besser umzugehen
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory ist der Mechanismus, den Clojure verwendet
; um mit persistenten Zuständen umzugehen. Es gibt ein Paar Konstrukte in
; Clojure die es verwenden.

; Ein Atom ist das Einfachste. Gebe es einen Initialwert
(def my-atom (atom {}))

; Update ein Atom mit swap!.
; swap! nimmt eine Funktion und ruft sie mit dem aktuellen Zustand des
; Atoms auf und alle nachfolgenden Argumente als das Zweite
(swap! my-atom assoc :a 1) ; Setzt my-atom zu dem Ergebnis von (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Setzt my-atom zu dem Ergebnis von (assoc {:a 1} :b 2)

; Verwende '@' um das Atom zu dereferenzieren und den Wert zu bekommen
my-atom  ;=> Atom<#...> (Gibt das Atom Objekt zurück
@my-atom ; => {:a 1 :b 2}

; Hier ist ein einfacher Zähler mit einem Atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Andere STM Konstrukte sind refs und agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Weiterführende Literatur

Das ist alles andere als erschöpfend, aber hoffentlich ist es genug, um dich auf die Beine zu stellen.

Clojure.org hat eine Menge von Artikeln:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org hat eine Dokumentation mit Beispielen für die meisten Kernfunktionen
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure ist eine gute Möglichkeit um deine clojure/FP zu verbessern:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (ja, wirklich) hat eine Reihe von Artikeln zum Starten:
[http://clojure-doc.org/](http://clojure-doc.org/)
