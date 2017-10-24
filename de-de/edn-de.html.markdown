---
language: edn
filename: learnedn-de.edn
contributors:
  - ["Jason Yeo", "https://github.com/jsyeo"]
  - ["Jonathan D Johnston", "https://github.com/jdjohnston"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

Extensible Data Notation (EDN) ist ein Format für serialisierte Daten.

EDN ist ein Subset der von Clojure verwendeten Syntax. Das Lesen von Daten, die durch EDN definiert werden, ist
sicherer als das, was durch die vollständige Clojure-Syntax definiert wird, insbesondere von nicht
vertrauenswürdigen Quellen. EDN ist beschränkt auf Daten, kein Code. Es ist ähnlich in seinen Zielen zu JSON.
Obwohl es mehr in Clojure verwendet wird, gibt es verschiedene Implementationen von EDN in vielen
verschiedenen anderen Sprachen.

Der Hauptvorteil von EDN im Gegensatz zu JSON und YAML ist, dass es erweiterbar ist.
Wir werden später sehen wie es erweitert werden kann.

```clojure
; Kommentare starten mit einem Semikolon.
; Alles nach dem Semikolon wird ignoriert.

;;;;;;;;;;;;;;;;;;;
;;; Basistypen  ;;;
;;;;;;;;;;;;;;;;;;;

nil         ; auch bekannt in anderen Sprachen als null

; Booleans
true
false

; Strings werden in Gänsefüßchen eingeschlossen.
"hungarian breakfast"
"farmer's cheesy omelette"

; Charaktere werden einem Backslash vorangestellt
\g \r \a \c \e

; Schlüsselwörter beginnen mit einem Doppelpunkt. Sie verhalten sich wie Enums.
; Ähnlich, wie Symbole in Ruby.
:eggs
:cheese
:olives

; Symbole werden verwendet um Identifier zu repräsentieren. Sie beginnen mit einem #.
; Du kannst einen Namespace für Symbole nutzen, wenn du / verwendest. Egal was / vorangestellt wird
; ist der Namespace dieses Namens.
#spoon
#kitchen/spoon ; nicht das selbe, wie #spoon
#kitchen/fork
#github/fork   ; damit kannst du nicht essen

; Integers und Floats
42
3.14159

; Listen sind Sequenzen von Werten  
(:bun :beef-patty 9 "yum!")

; Vektoren erlauben zufälligen Zugriff
[:gelato 1 2 -2]

; Maps sind assoziative Datenstrukturen, die einen Schlüssel mit einem Wert verbinden.
{:eggs        2
 :lemon-juice 3.5
 :butter      1}                

; Du bist nicht beschränkt ausschließlich Schlüsselwörter als Schlüssel zu verwenden.
{[1 2 3 4] "tell the people what she wore",
 [5 6 7 8] "the more you see the more you hate"}

; Du kannst Kommas für eine bessere Lesbarkeit verwenden. Sie werden wie Leerraum behandelt.
; Sets sind Sammlungen, die eindeutige Elemente enthalten.
#{:a :b 88 "huat"}

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markierte Elemente ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; EDN kann erweitert werden, indem Elemente mit # Symbolen makiert werden.

#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}

; Lass mich das mit einem Clojure Beispiel erklären.
; Angenommen ich möchte dieses Stück EDM in einen MenuItem record umwandeln.
(defrecord MenuItem [name rating])
   
; Um EDN in clojure Werte umzuwandeln, muss ich den eingebauten EDN Leser 
; edn/read-string verwenden

(edn/read-string "{:eggs 2 :butter 1 :flour 5}")
; -> {:eggs 2 :butter 1 :flour 5}

; Definiere die Leserfunktion, um markierte Elemente zu transformieren
; und übergebe eine Map, die Tags den Lesefunktionen als edn / read-string zuweisen

(edn/read-string {:readers {'MyYelpClone/MenuItem map->menu-item}}
                 "#MyYelpClone/MenuItem {:name \"eggs-benedict\" :rating 10}")
; -> #user.MenuItem{:name "eggs-benedict", :rating 10}

```

# Referenzen

- [EDN spec](https://github.com/edn-format/edn)
- [Implementationen](https://github.com/edn-format/edn/wiki/Implementations)
- [makierte Elemente](http://www.compoundtheory.com/clojure-edn-walkthrough/)
