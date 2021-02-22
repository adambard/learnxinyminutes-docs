---
language: "clojure macros"
filename: learnclojuremacros-de.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

Wie mit allen Lisps besitzt auch Clojure die inhärente [Homoikonizität](https://en.wikipedia.org/wiki/Homoiconic),
die dir den vollen Zugang der Sprache gibt, um
 Code-Generierungsroutinen zu schreiben. Diese werden "Macros" genannt.
Macros geben dir eine leistungsarke Möglichkeit, die Sprache
an deine Bedürfnisse anzupassen.

Sei aber vorsichtig, es wird als schlechter Stil angesehen, wenn du
ein Macro schreibst, obwohl eine Funktion genausogut funktionieren würde.
Verwende nur dann ein Macro, wenn du Kontrolle darüber brauchst, wann oder ob Argumente in einer Form evaluiert werden.

Wenn du mit Clojure vertraut sein möchtest, stelle sicher, dass du alles in [Clojure in Y Minutes](/docs/clojure/) verstehst.

```clojure
;; Definiere ein Macro mit defmacro. Dein Macro sollte eine Liste zurückgeben,
;; die als Clojure Code evaluiert werden kann.
;;
;; Dieses Macro ist das Gleiche, als ob du (reverse "Hallo Welt") geschrieben
;; hättest
(defmacro my-first-macro []
  (list reverse "Hallo Welt"))

;; Inspiziere das Ergebnis eines Macros mit macroexpand oder macroexpand-1.
;;
;; Beachte, dass der Aufruf zitiert sein muss.
(macroexpand '(my-first-macro))
;; -> (#<core$reverse clojure.core$reverse@xxxxxxxx> "Hallo Welt")

;; Du kannst das Ergebnis von macroexpand direkt auswerten.
(eval (macroexpand '(my-first-macro)))
; -> (\t \l \e \W \space \o \l \l \a \H)

;; Aber du solltest diese prägnante und funktionsähnliche Syntax verwenden:
(my-first-macro)  ; -> (\t \l \e \W \space \o \l \l \a \H)

;; Du kannst es dir leichter machen, indem du die Zitiersyntax verwendest
;; um Listen in ihren Makros zu erstellen:
(defmacro my-first-quoted-macro []
  '(reverse "Hallo Welt"))

(macroexpand '(my-first-quoted-macro))
;; -> (reverse "Hallo Welt")
;; Beachte, dass reverse nicht mehr ein Funktionsobjekt ist, sondern ein Symbol

;; Macros können Argumente haben.
(defmacro inc2 [arg]
  (list + 2 arg))

(inc2 2) ; -> 4

;; Aber wenn du versuchst das mit einer zitierten Liste zu machen wirst du
;; einen Fehler bekommen, weil das Argument auch zitiert sein wird.
;; Um dies zu umgehen, bietet Clojure einee Art und Weise Macros zu zitieren: `
;; In ` kannst du ~ verwenden um in den äußeren Bereich zu kommen.
(defmacro inc2-quoted [arg]
  `(+ 2 ~arg))

(inc2-quoted 2)

;; Du kannst die normalen destruktuierungs Argumente verwenden. Expandiere
;; Listenvariablen mit ~@.
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do ~@body))) ; Erinnere dich an das do!

(macroexpand '(unless true (reverse "Hallo Welt")))
;; ->
;; (if (clojure.core/not true) (do (reverse "Hallo Welt")))

;; (unless) evaluiert und gibt body zurück, wenn das erste Argument falsch ist.
;; Andernfalls gibt es nil zurück

(unless true "Hallo") ; -> nil
(unless false "Hallo") ; -> "Hallo"

;; Die Verwendung Macros ohne Sorgfalt kann viel Böses auslösen, indem es
;; deine Variablen überschreibt
(defmacro define-x []
  '(do
     (def x 2)
     (list x)))

(def x 4)
(define-x) ; -> (2)
(list x) ; -> (2)

;; Um das zu verhindern kannst du gensym verwenden um einen eindeutigen
;; Identifikator zu bekommen
(gensym 'x) ; -> x1281 (oder etwas Ähnliches)

(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(def x 4)
(define-x-safely) ; -> (2)
(list x) ; -> (4)

;; Du kannst # innerhalb von ` verwenden um für jedes Symbol automatisch
;; ein gensym zu erstellen
(defmacro define-x-hygienically []
  `(do
     (def x# 2)
     (list x#)))

(def x 4)
(define-x-hygienically) ; -> (2)
(list x) ; -> (4)

;; Es ist üblich, Hilfsfunktionen mit Macros zu verwenden. Lass uns einige
;; erstellen, die uns helfen , eine (dumme) arithmetische Syntax
;; zu unterstützen
(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Bekomme die Argumente [x (+ y)], gebe (+ x y) zurück"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

;; Wir können es sofort testen, ohne ein Macro zu erstellen
(inline-2-helper '(a + (b - 2) - (c * 5))) ; -> (- (+ a (- b 2)) (* c 5))

; Allerdings, brauchen wir ein Macro, wenn wir es zur Kompilierungszeit
; ausführen wollen
(defmacro inline-2 [form]
  (inline-2-helper form))

(macroexpand '(inline-2 (1 + (3 / 2) - (1 / 2) + 1)))
; -> (+ (- (+ 1 (/ 3 2)) (/ 1 2)) 1)

(inline-2 (1 + (3 / 2) - (1 / 2) + 1))
; -> 3 (eigentlich, 3N, da die Zahl zu einem rationalen Bruch mit / umgewandelt wird)
```

### Weiterführende Literatur

[Macros schreiben](http://www.braveclojure.com/writing-macros/)

[Offiziele Docs](http://clojure.org/macros)

[Wann verwendet man Macros?](https://lispcast.com/when-to-use-a-macro/)
