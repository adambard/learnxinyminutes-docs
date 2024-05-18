---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
    - ["Miltiadis Stouras", "https://github.com/mstou"]
translators:
    - ["Albina Gimaletdinova", "https://github.com/albina-astr"]
lang: ru-ru
---

# Динамическое программирование

## Введение

Динамическое программирование (dynamic programming, DP) — мощный инструмент для решения определенного класса задач. Идея очень проста: если вы решили задачу для каких-то вводных данных, сохраните этот результат для будущих вычислений, чтобы снова не решать ту же самую задачу с теми же данными.  

Запомните!
«Кто не помнит своего прошлого, обречен на то, чтобы пережить его вновь»

## Способы решения подобных задач

1. *Сверху-вниз*: Начните с разбиения задачи на подзадачи. Если вы видите, что подзадача уже была решена, тогда используйте сохраненный ранее результат. Иначе решите подзадачу и сохраните её результат. Эта техника интуитивно понятна и называется мемоизацией.

2. *Снизу-вверх*: Проанализируйте задачу и определите порядок, в котором решаются подзадачи, и начните решать от тривиальной подзадачи до изначальной задачи. Это гарантирует, что подзадачи будут решены, прежде чем решится вся задача. В этом и заключается динамическое программирование.

## Пример задачи динамического программирования

В задаче по определению самой длинной возрастающей подпоследовательности необходимо найти найти самую длинную возрастающую подпоследовательность для заданной последовательности.
Для последовательности `S={ a1, a2, a3, a4, ............., an-1, an }` мы должны найти самое длинное подмножество, такое, что для всех `j` и `i`, `j<i` в подмножестве `aj<ai`.

Прежде всего, мы должны найти значение самых длинных подпоследовательностей (`LSi`) для каждого индекса `i` с последним элементом последовательности, равным `ai`. Тогда наибольшая `LSi` будет самой длинной подпоследовательностью в данной последовательности. Для начала `LSi` равна единице, поскольку `ai` является элементом последовательности (последний элемент). Затем для всех `j` таких, что `j<i` и `aj<ai`, мы находим наибольшую `LSj` и добавляем ее к `LSi`. Тогда алгоритм выполняется за *O(n2)*.

Псевдокод для определения длины самой длинной возрастающей подпоследовательности:
сложность этого алгоритма можно уменьшить, если использовать структуру данных получше, а не массив. Использование массива с предшественниками и переменной `largest_sequences_so_far` («наибольшие последовательности на данный момент») и ее индекса сэкономит много времени.

Аналогичная концепция может быть применена для определения самого длинного пути в направленном ациклическом графе.

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
```

### Некоторые известные задачи DP

* [Floyd Warshall Algorithm - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code)
* [Integer Knapsack Problem - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem)
* [Longest Common Subsequence - Tutorial and C Program source code](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence)

## Онлайн-ресурсы

* MIT 6.006: [Lessons 19,20,21,22](https://www.youtube.com/playlist?list=PLUl4u3cNGP61Oq3tWYp6V_F-5jb5L2iHb)
* TopCoder: [Dynamic Programming from Novice to Advanced](https://www.topcoder.com/community/data-science/data-science-tutorials/dynamic-programming-from-novice-to-advanced/)
* [CodeChef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
* [InterviewBit](https://www.interviewbit.com/courses/programming/topics/dynamic-programming/)
* GeeksForGeeks:
  * [Overlapping Subproblems](https://www.geeksforgeeks.org/dynamic-programming-set-1/)
  * [Tabulation vs Memoization](https://www.geeksforgeeks.org/tabulation-vs-memoizatation/)
  * [Optimal Substructure Property](https://www.geeksforgeeks.org/dynamic-programming-set-2-optimal-substructure-property/)
  * [How to solve a DP problem](https://www.geeksforgeeks.org/solve-dynamic-programming-problem/)
* [How to write DP solutions](https://www.quora.com/Are-there-any-good-resources-or-tutorials-for-dynamic-programming-DP-besides-the-TopCoder-tutorial/answer/Michal-Danilák)
