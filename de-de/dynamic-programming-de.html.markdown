---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
translators:
    - ["Henrik Jürges", "http://github.com/santifa"]
lang: de-de
---

# Dynamische Programmierung

## Einführung
Dynamische Programmierung ist eine leistungsfähige Technik, die zur Lösung
einer bestimmten Klasse von Problemen verwendet wird.
Die Idee ist sehr einfach, wenn Sie ein Problem mit der gegebenen Eingabe
gelöst haben, dann speichern Sie das Ergebnis für die spätere Referenz, um zu
vermeiden, das gleiche Problem noch einmal zu lösen.

Denken Sie immer daran!
"Diejenigen, die sich nicht an die Vergangenheit erinnern können, 
sind dazu verdammt, sie zu wiederholen."

## Wege zur Lösung solcher Probleme

1. *Top-Down*: Lösen Sie das gegebene Problem, indem Sie es aufteilen.
Wenn Sie sehen, dass das Problem bereits gelöst ist, geben Sie einfach die
gespeicherte Antwort zurück. Wenn es nicht gelöst wurde, lösen Sie es und
speichern Sie die Antwort. Dieser Ansatz ist leicht zu verfolgen und sehr
intuitiv. Er wird als Memoization bezeichnet.

2. *Bottom-Up*: Analysieren Sie das Problem und beobachten Sie, in welcher
Reihenfolge die Teilprobleme gelöst werden können. Beginnen Sie mit der
Lösung vom trivialen Teilproblem bis zum gegebenen Problem. Dabei wird
sichergestellt, dass die Teilprobleme vor der Problemlösung gelöst werden.
Dies wird als Dynamische Programmierung bezeichnet.

## Ein Beispiel für Dynamische Programmierung

Das Problem mit der längsten ansteigenden Subsequenz besteht darin,
die längste ansteigende Subsequenz einer gegebenen Sequenz zu finden.
Gegeben die Sequenz `S= {a1, a2, a3, a3, a4,..............., an-1, an }`,
müssen wir die größte Teilmenge finden, so daß für alle `j` und `i`, `j<i`
in der Teilmenge `aj<ai` gilt.
Zuerst müssen wir bei jedem Index i den Wert der längsten Subsequenzen (LSi)
finden, wobei das letzte Element der Sequenz ai ist. Dann wäre die größte LSi
die längste Subsequenz in der gegebenen Sequenz. Am Anfang wird der LSi mit
eins belegt, da ai ein Element der Sequenz (Letztes Element) ist.
Dann ist für alle `j` mit  `j<i` und `aj<ai`, so dass wir den größten LSj finden
und zum LSi hinzufügen. Der Algorithmus hat eine Laufzeit von *O(n2)*.

Pseudocode zur Bestimmung der Länge der am längsten ansteigenden Subsequenz:
Die Komplexität des Algorithmus könnte durch eine bessere Datenstruktur anstelle
von Arrays reduziert werden. Das Speichern von Vorgänger-Array's und Variablen
wie `largest_sequences_so_far` und dessen Index würde eine Menge Zeit sparen.

Ein ähnliches Konzept könnte auch bei der Suche nach dem längsten Weg
in gerichteten azyklischen Graphen angewandt werden.
```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
```

### Einige bekannte DP Probleme

- [Floyd Warshall Algorithm - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code)
- [Integer Knapsack Problem - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem)
- [Longest Common Subsequence - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence)

## Online Ressourcen

* [codechef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
