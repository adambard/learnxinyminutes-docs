---
name: APL
contributors:
    - ["nooodl", "https://github.com/nooodl"]
filename: learnapl.apl
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```apl
⍝ APL의 주석은 ⍝로 시작합니다.

⍝ 숫자 목록. (¯는 음수입니다)
2 3e7 ¯4 50.3

⍝ 표현식, 몇 가지 함수를 보여줍니다. APL에는
⍝ 연산 순서가 없습니다: 모든 것이 오른쪽에서 왼쪽으로
⍝ 구문 분석됩니다. 이것은 5 + (4 × (2 ÷ (5 - 3))) = 9와 같습니다:
5 + 4 × 2 ÷ 5 - 3        ⍝ 9

⍝ 이 함수들은 목록에서도 작동합니다:
1 2 3 4 × 5              ⍝ 5 10 15 20
1 2 3 4 × 5 6 7 8        ⍝ 5 12 21 32

⍝ 모든 함수는 단일 인수와 이중 인수
⍝ 의미를 가집니다. 예를 들어, 두 인수에 적용된 "×"는
⍝ 곱셈을 의미하지만, 오른쪽
⍝ 면에만 적용될 때는 부호를 반환합니다:

× ¯4 ¯2 0 2 4            ⍝ ¯1 ¯1 0 1 1

⍝ 값은 다음 연산자를 사용하여 비교할 수 있습니다 (1은
⍝ "참", 0은 "거짓"을 의미합니다):

10 20 30 = 10 20 99      ⍝ 1 1 0

10 20 30 < 10 20 99      ⍝ 0 0 1

⍝ "⍳n"은 처음 n개의 자연수를 포함하는 벡터를 반환합니다.
⍝ 행렬은 ⍴ (재구성)을 사용하여 구성할 수 있습니다:
4 3 ⍴ ⍳5                 ⍝ 0 1 2
                         ⍝ 3 4 0
                         ⍝ 1 2 3
                         ⍝ 4 0 1

⍝ 단일 인수 ⍴는 차원을 다시 제공합니다:
⍴ 4 3 ⍴ ⍳5               ⍝ 4 3

⍝ 값은 ←를 사용하여 저장할 수 있습니다. 숫자 벡터의 평균값을
⍝ 계산해 보겠습니다:
A ← 10 60 55 23

⍝ A의 요소 합계 (/는 축소입니다):
+/A                      ⍝ 148

⍝ A의 길이:
⍴A                       ⍝ 4

⍝ 평균:
(+/A) ÷ (⍴A)             ⍝ 37

⍝ 이것을 {}와 ⍵를 사용하여 함수로 정의할 수 있습니다:
mean ← {(+/⍵)÷⍴⍵}
mean A                   ⍝ 37
```

## 더 읽을거리

- [APL 위키](https://aplwiki.com/)
- 제작자가 쓴 APL 책의 이전 버전: [Kenneth Iverson - A Programming Language](https://www.softwarepreservation.org/projects/apl/Books/APROGRAMMING%20LANGUAGE/view)
- 추가 도서: [APL Books](https://aplwiki.com/wiki/Books)
