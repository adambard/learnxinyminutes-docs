---
category: Algorithms & Data Structures
name: Dynamic Programming
contributors:
    - ["Akashdeep Goel", "http://github.com/akashdeepgoel"]
    - ["Miltiadis Stouras", "https://github.com/mstou"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

# 동적 프로그래밍

## 소개

동적 프로그래밍은 우리가 보게 될 특정 종류의 문제를 해결하는 데 사용되는 강력한 기술입니다. 아이디어는 매우 간단합니다. 주어진 입력으로 문제를 해결했다면 나중에 참조할 수 있도록 결과를 저장하여 동일한 문제를 다시 해결하는 것을 피하십시오.

항상 기억하십시오!
"과거를 기억하지 못하는 자는 그것을 반복할 수밖에 없다"

## 이러한 문제를 해결하는 방법

1. *하향식* : 주어진 문제를 분해하여 해결하기 시작합니다. 문제가 이미 해결된 것을 보면 저장된 답을 반환하기만 하면 됩니다. 해결되지 않은 경우 해결하고 답을 저장하십시오. 이것은 일반적으로 생각하기 쉽고 매우 직관적입니다. 이것을 메모이제이션이라고 합니다.

2. *상향식* : 문제를 분석하고 하위 문제가 해결되는 순서를 확인하고 사소한 하위 문제부터 주어진 문제까지 해결하기 시작합니다. 이 과정에서 문제를 해결하기 전에 하위 문제가 해결되는 것이 보장됩니다. 이것을 동적 프로그래밍이라고 합니다.

## 동적 프로그래밍의 예

최장 증가 부분 수열 문제는 주어진 수열의 가장 긴 증가 부분 수열을 찾는 것입니다. 수열 `S={ a1, a2, a3, a4, ............., an-1, an }`이 주어지면, 부분 집합에서 모든 `j`와 `i`에 대해 `j<i`인 가장 긴 부분 집합을 찾아야 합니다. `aj<ai`.
우선, 수열의 마지막 요소가 ai인 모든 인덱스 i에서 가장 긴 부분 수열(LSi)의 값을 찾아야 합니다. 그런 다음 가장 큰 LSi가 주어진 수열에서 가장 긴 부분 수열이 됩니다. 시작하려면 LSi는 ai가 수열의 요소(마지막 요소)이므로 1로 할당됩니다. 그런 다음 `j<i`이고 `aj<ai`인 모든 `j`에 대해 가장 큰 LSj를 찾아 LSi에 추가합니다. 그런 다음 알고리즘은 *O(n2)* 시간이 걸립니다.

가장 긴 증가 부분 수열의 길이를 찾기 위한 의사 코드:
이 알고리즘의 복잡성은 배열 대신 더 나은 데이터 구조를 사용하여 줄일 수 있습니다. 전임자 배열과 `largest_sequences_so_far`와 같은 변수와 해당 인덱스를 저장하면 많은 시간을 절약할 수 있습니다.

유사한 개념을 방향성 비순환 그래프에서 가장 긴 경로를 찾는 데 적용할 수 있습니다.

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
```

### 일부 유명한 DP 문제

- [플로이드-워셜 알고리즘 - 튜토리얼 및 C 프로그램 소스 코드](http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code)
- [정수 배낭 문제 - 튜토리얼 및 C 프로그램 소스 코드](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem)
- [최장 공통 부분 수열 - 튜토리얼 및 C 프로그램 소스 코드](http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence)

## 온라인 자료

* MIT 6.006: [강의 19,20,21,22](https://www.youtube.com/playlist?list=PLUl4u3cNGP61Oq3tWYp6V_F-5jb5L2iHb)
* TopCoder: [초보자부터 고급까지의 동적 프로그래밍](https://www.topcoder.com/community/data-science/data-science-tutorials/dynamic-programming-from-novice-to-advanced/)
* [CodeChef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
* [InterviewBit](https://www.interviewbit.com/courses/programming/topics/dynamic-programming/)
* GeeksForGeeks:
  * [중복 하위 문제](https://www.geeksforgeeks.org/dynamic-programming-set-1/)
  * [테이블화 대 메모이제이션](https://www.geeksforgeeks.org/tabulation-vs-memoizatation/)
  * [최적 하위 구조 속성](https://www.geeksforgeeks.org/dynamic-programming-set-2-optimal-substructure-property/)
  * [DP 문제 해결 방법](https://www.geeksforgeeks.org/solve-dynamic-programming-problem/)
* [DP 솔루션 작성 방법](https://www.quora.com/Are-there-any-good-resources-or-tutorials-for-dynamic-programming-DP-besides-the-TopCoder-tutorial/answer/Michal-Danil%C3%A1k)