---
name: APL
contributors:
    - ["nooodl", "https://github.com/nooodl"]
translators:
    - ["antonkesy", "https://github.com/antonkesy"]
filename: learnapl.apl
---

```apl
⍝ Kommentare in APL beginnen mit ⍝.

⍝ Eine Liste von Zahlen. (¯ ist negativ)
2 3e7 ¯4 50.3

⍝ Ein Ausdruck, der einige Funktionen zeigt. In APL gibt es
⍝ keine Operatorrangfolge: alles wird von rechts nach
⍝ links geparst. Dies entspricht 5 + (4 × (2 ÷ (5 - 3))) = 9:
5 + 4 × 2 ÷ 5 - 3        ⍝ 9

⍝ Diese Funktionen funktionieren auch mit Listen:
1 2 3 4 × 5              ⍝ 5 10 15 20
1 2 3 4 × 5 6 7 8        ⍝ 5 12 21 32

⍝ Alle Funktionen haben sowohl Einzelargument- als auch Doppelargument-
⍝ Bedeutungen. Zum Beispiel bedeutet "×" mit zwei Argumenten
⍝ multiplizieren, aber wenn es nur auf eine rechte Seite
⍝ angewendet wird, gibt es das Vorzeichen zurück:

× ¯4 ¯2 0 2 4            ⍝ ¯1 ¯1 0 1 1

⍝ Werte können mit diesen Operatoren verglichen werden (1 bedeutet
⍝ "wahr", 0 bedeutet "falsch"):

10 20 30 = 10 20 99      ⍝ 1 1 0

10 20 30 < 10 20 99      ⍝ 0 0 1

⍝ "⍳n" gibt einen Vektor mit den ersten n natürlichen Zahlen zurück.
⍝ Matrizen können mit ⍴ (umformen) konstruiert werden:
4 3 ⍴ ⍳5                 ⍝ 0 1 2
                         ⍝ 3 4 0
                         ⍝ 1 2 3
                         ⍝ 4 0 1

⍝ Einzelargument ⍴ gibt die Dimensionen zurück:
⍴ 4 3 ⍴ ⍳5               ⍝ 4 3

⍝ Werte können mit ← gespeichert werden. Lass uns den Mittelwert
⍝ eines Vektors von Zahlen berechnen:
A ← 10 60 55 23

⍝ Summe der Elemente von A (/ ist Reduktion):
+/A                      ⍝ 148

⍝ Länge von A:
⍴A                       ⍝ 4

⍝ Mittelwert:
(+/A) ÷ (⍴A)             ⍝ 37

⍝ Wir können dies als Funktion mit {} und ⍵ definieren:
mean ← {(+/⍵)÷⍴⍵}
mean A                   ⍝ 37
```

## Weitere Literatur

- [APL Wiki](https://aplwiki.com/)
- Eine ältere Version des APL-Buchs vom Erfinder: [Kenneth Iverson - A Programming Language](https://archive.org/details/aprogramminglanguage1962)
- Weitere Bücher: [APL Books](https://aplwiki.com/wiki/Books)
