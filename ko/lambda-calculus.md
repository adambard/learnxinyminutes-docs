---
category: Algorithms & Data Structures
name: Lambda Calculus
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
    - ["Yan Hui Hang", "http://github.com/yanhh0"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

# 람다 대수

람다 대수(λ-대수)는 원래 [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church)가 만든 세계에서 가장 작은 프로그래밍 언어입니다. 숫자, 문자열, 부울 또는 함수가 아닌 데이터 유형이 없음에도 불구하고 람다 대수는 모든 튜링 기계를 나타내는 데 사용할 수 있습니다!

람다 대수는 **변수**, **함수** 및 **적용**의 3가지 요소로 구성됩니다.


| 이름        | 구문                             | 예시   | 설명                                   |
|-------------|------------------------------------|-----------|-----------------------------------------------|
| 변수    | `<name>`                           | `x`       | "x"라는 이름의 변수                          |
| 함수    | `λ<parameters>.<body>`             | `λx.x`    | 매개변수 "x"와 본문 "x"가 있는 함수    |
| 적용 | `<function><variable or function>` | `(λx.x)a` | 인수 "a"로 함수 "λx.x" 호출 | 

가장 기본적인 함수는 `f(x) = x`와 동일한 항등 함수 `λx.x`입니다. 첫 번째 "x"는 함수의 인수이고 두 번째는 함수의 본문입니다.

## 자유 변수 대 바인딩된 변수:

- 함수 `λx.x`에서 "x"는 함수의 본문과 매개변수 모두에 있으므로 바인딩된 변수라고 합니다.
- `λx.y`에서 "y"는 이전에 선언되지 않았으므로 자유 변수라고 합니다.

## 평가:

평가는 본질적으로 어휘적으로 범위가 지정된 대체인 [β-축소](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction)를 통해 수행됩니다.

표현식 `(λx.x)a`를 평가할 때 함수의 본문에서 "x"의 모든 발생을 "a"로 바꿉니다.

- `(λx.x)a`는 `a`로 평가됩니다.
- `(λx.y)a`는 `y`로 평가됩니다.

고차 함수를 만들 수도 있습니다:

- `(λx.(λy.x))a`는 `λy.a`로 평가됩니다.

전통적으로 람다 대수는 단일 매개변수 함수만 지원하지만 [커링](https://en.wikipedia.org/wiki/Currying)이라는 기술을 사용하여 다중 매개변수 함수를 만들 수 있습니다.

- `(λx.λy.λz.xyz)`는 `f(x, y, z) = ((x y) z)`와 동일합니다.

때로는 `λxy.<body>`가 `λx.λy.<body>`와 상호 교환적으로 사용됩니다.

----

전통적인 **람다 대수에는 숫자, 문자 또는 함수가 아닌 데이터 유형이 없습니다!**

## 부울 논리:

람다 대수에는 "True" 또는 "False"가 없습니다. 1이나 0도 없습니다.

대신:

`T`는 `λx.λy.x`로 표시됩니다.

`F`는 `λx.λy.y`로 표시됩니다.

먼저, `b`가 True이면 `t`를 반환하고 `b`가 False이면 `f`를 반환하는 "if" 함수 `λbtf`를 정의할 수 있습니다.

`IF`는 `λb.λt.λf.b t f`와 동일합니다.

`IF`를 사용하여 기본 부울 논리 연산자를 정의할 수 있습니다:

`a AND b`는 `λab.IF a b F`와 동일합니다.

`a OR b`는 `λab.IF a T b`와 동일합니다.

`NOT a`는 `λa.IF a F T`와 동일합니다.

*참고: `IF a b c`는 본질적으로 `IF((a b) c)`를 의미합니다.* 

## 숫자:

람다 대수에는 숫자가 없지만 [처치 숫자](https://en.wikipedia.org/wiki/Church_encoding)를 사용하여 숫자를 인코딩할 수 있습니다.

모든 숫자 n에 대해: <code>n = λf.f<sup>n</sup></code>이므로:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

처치 숫자를 증가시키려면 다음인 후계자 함수 `S(n) = n + 1`을 사용합니다:

`S = λn.λf.λx.f((n f) x)`

후계자를 사용하여 덧셈을 정의할 수 있습니다:

`ADD = λab.(a S)b`

**도전:** 자신만의 곱셈 함수를 정의해 보십시오!

## 더 작게 만들기: SKI, SK 및 Iota

### SKI 조합자 미적분학

S, K, I를 다음 함수라고 합시다:

`I x = x`

`K x y =  x`

`S x y z = x z (y z)`

람다 대수의 표현식을 SKI 조합자 미적분학의 표현식으로 변환할 수 있습니다:

1. `λx.x = I`
2. `x`가 `c`에서 자유롭게 발생하지 않는 경우 `λx.c = Kc`
3. `λx.(y z) = S (λx.y) (λx.z)`

예를 들어 처치 숫자 2를 들어보겠습니다:

`2 = λf.λx.f(f x)`

내부 부분 `λx.f(f x)`의 경우:

```
  λx.f(f x)
= S (λx.f) (λx.(f x))          (사례 3)
= S (K f)  (S (λx.f) (λx.x))   (사례 2, 3)
= S (K f)  (S (K f) I)         (사례 2, 1)
```

따라서:

```
  2
= λf.λx.f(f x)
= λf.(S (K f) (S (K f) I))
= λf.((S (K f)) (S (K f) I))
= S (λf.(S (K f))) (λf.(S (K f) I)) (사례 3)
```

첫 번째 인수 `λf.(S (K f))`의 경우:

```
  λf.(S (K f))
= S (λf.S) (λf.(K f))       (사례 3)
= S (K S) (S (λf.K) (λf.f)) (사례 2, 3)
= S (K S) (S (K K) I)       (사례 2, 3)
```

두 번째 인수 `λf.(S (K f) I)`의 경우:

```
  λf.(S (K f) I)
= λf.((S (K f)) I)
= S (λf.(S (K f))) (λf.I)             (사례 3)
= S (S (λf.S) (λf.(K f))) (K I)       (사례 2, 3)
= S (S (K S) (S (λf.K) (λf.f))) (K I) (사례 1, 3)
= S (S (K S) (S (K K) I)) (K I)       (사례 1, 2)
```

병합:

```
  2
= S (λf.(S (K f))) (λf.(S (K f) I))
= S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))
```

이것을 확장하면 처치 숫자 2에 대한 동일한 표현식을 다시 얻게 됩니다.

### SK 조합자 미적분학

SKI 조합자 미적분학은 여전히 더 줄일 수 있습니다. `I = SKK`라는 점에 유의하여 I 조합자를 제거할 수 있습니다. 모든 `I`를 `SKK`로 대체할 수 있습니다.

### Iota 조합자

SK 조합자 미적분학은 여전히 최소가 아닙니다. 정의:

```
ι = λf.((f S) K)
```

우리는 다음을 가지고 있습니다:

```
I = ιι
K = ι(ιI) = ι(ι(ιι))
S = ι(K) = ι(ι(ι(ιι)))
```

## 더 고급 읽기:

1. [람다 대수 튜토리얼 소개](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)
2. [Cornell CS 312 Recitation 26: 람다 대수](http://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
3. [위키백과 - 람다 대수](https://en.wikipedia.org/wiki/Lambda_calculus)
4. [위키백과 - SKI 조합자 미적분학](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
5. [위키백과 - Iota 및 Jot](https://en.wikipedia.org/wiki/Iota_and_Jot)