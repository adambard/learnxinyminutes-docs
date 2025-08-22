---
name: PureScript
filename: purescript.purs
contributors:
    - ["Fredrik Dyrkell", "http://www.lexicallyscoped.com"]
    - ["Thimoteus", "https://github.com/Thimoteus"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

PureScript는 JavaScript로 컴파일되는 작고 강력하며 정적으로 유형이 지정된 언어입니다.

* 자세히 알아보기: [https://www.purescript.org/](https://www.purescript.org/)
* 문서: [https://pursuit.purescript.org/](https://pursuit.purescript.org/)
* 책: Purescript by Example, [https://book.purescript.org/](https://book.purescript.org/)

주석 처리되지 않은 모든 코드 줄은 PSCi REPL에서 실행할 수 있지만, 일부는
"붙여넣기" 모드(`:paste` 다음에 여러 줄을 입력하고 ^D로 종료)가
필요합니다.

```haskell
--
-- 1. 런타임에 JavaScript와 동등한 기본 데이터 유형입니다.

import Prelude
-- 숫자
1.0 + 7.2*5.5 :: Number -- 40.6
-- 정수
1 + 2*5 :: Int -- 11
-- 유형이 추론되므로 다음은 잘 작동합니다
9.0/2.5 + 4.4 -- 8.0
-- 그러나 Int와 Number는 혼합되지 않으므로 다음은 작동하지 않습니다
5/2 + 2.5 -- 표현식 2.5에 Int 유형이 없습니다
-- 16진수 리터럴
0xff + 1 -- 256
-- 단항 부정
6 * -3 -- -18
6 * negate 3 -- -18
-- 모듈러스, purescript-math (Math)에서
3.0 % 2.0 -- 1.0
4.0 % 2.0 -- 0.0
-- psci에서 표현식의 유형 검사
:t 9.5/2.5 + 4.4 -- Number

-- 불리언
true :: Boolean -- true
false :: Boolean -- false
-- 부정
not true -- false
23 == 23 -- true
1 /= 4 -- true
1 >= 4 -- false
-- 비교 < <= > >=
-- compare로 정의됨
compare 1 2 -- LT
compare 2 2 -- EQ
compare 3 2 -- GT
-- 논리곱 및 논리합
true && (9 >= 19 || 1 < 2) -- true

-- 문자열
"Hello" :: String -- "Hello"
-- 개행 없는 여러 줄 문자열, PSCi에서 실행하려면 "paste" 모드 사용
"Hello\
\orld" -- "Helloworld"
-- 개행 있는 여러 줄 문자열
"""Hello
world""" -- "Hello\nworld"
-- 연결
"such " <> "amaze" -- "such amaze"

--
-- 2. 배열은 JavaScript 배열이지만 동종이어야 합니다.

[1,1,2,3,5,8] :: Array Int -- [1,1,2,3,5,8]
[1.2,2.0,3.14] :: Array Number -- [1.2,2.0,3.14]
[true, true, false] :: Array Boolean -- [true,true,false]
-- [1,2, true, "false"]는 작동하지 않음
-- `Int를 Boolean과 통합할 수 없음`

-- purescript-arrays (Data.Array) 필요
-- Cons (앞에 추가)
1 : [2,4,3] -- [1,2,4,3]

-- 및 purescript-maybe (Data.Maybe)
-- 안전한 액세스는 Maybe a를 반환
head [1,2,3] -- (Just 1)
tail [3,2,1] -- (Just [2,1])
init [1,2,3] -- (Just [1,2])
last [3,2,1] -- (Just 1)
-- 배열 액세스 - 인덱싱
[3,4,5,6,7] !! 2 -- (Just 5)
-- 범위
1..5 -- [1,2,3,4,5]
length [2,2,2] -- 3
drop 3 [5,4,3,2,1] -- [2,1]
take 3 [5,4,3,2,1] -- [5,4,3]
append [1,2,3] [4,5,6] -- [1,2,3,4,5,6]

--
-- 3. 레코드는 0개 이상의 필드가 있는 JavaScript 객체이며,
-- 다른 유형을 가질 수 있습니다.
book = {title: "Foucault's pendulum", author: "Umberto Eco"}
-- 속성 액세스
book.title -- "Foucault's pendulum"

getTitle b = b.title
-- 제목이 있는 모든 레코드에서 작동 (다른 필드는 필요하지 않음)
getTitle book -- "Foucault's pendulum"
getTitle {title: "Weekend in Monaco", artist: "The Rippingtons"} -- "Weekend in Monaco"
-- 약식으로 밑줄 사용 가능
_.title book -- "Foucault's pendulum"
-- 레코드 업데이트
changeTitle b t = b {title = t}
getTitle (changeTitle book "Ill nome della rosa") -- "Ill nome della rosa"

--
-- 4. 함수
-- PSCi의 붙여넣기 모드에서
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x*x + y*y
sumOfSquares 3 4 -- 25

myMod x y = x % y
myMod 3.0 2.0 -- 1.0
-- 함수의 중위 적용
3 `mod` 2 -- 1

-- 함수 적용은 다른 모든 연산자보다 우선순위가 높습니다
sumOfSquares 3 4 * sumOfSquares 4 5 -- 1025

-- 조건부
abs' n = if n>=0 then n else -n
abs' (-3) -- 3

-- 가드된 방정식
-- PSCi의 붙여넣기 모드에서
abs'' n | n >= 0    = n
        | otherwise = -n

-- 패턴 매칭

-- 유형 서명을 참고하십시오. 입력은 숫자 목록입니다. 패턴 매칭은
-- 목록을 분해하고 부분을 바인딩합니다.
-- purescript-lists (Data.List) 및 purescript-maybe (Data.Maybe) 필요
first :: forall a. List a -> Maybe a
first (x : _) = Just x
first Nil = Nothing
first (fromFoldable [3,4,5]) -- (Just 3)

second :: forall a. List a -> Maybe a
second Nil = Nothing
second (_ : Nil) = Nothing
second (_ : (y : _)) = Just y
second (fromFoldable [3,4,5]) -- (Just 4)

-- 일치시킬 보완 패턴
-- 좋은 옛 피보나치
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)
fib 10 -- 89

-- 바인딩 이름에 신경 쓰지 않는 경우 밑줄을 사용하여 무엇이든 일치시킵니다
isZero 0 = true
isZero _ = false
isZero 9 -- false

-- 레코드에 대한 패턴 매칭
ecoTitle {author: "Umberto Eco", title: t} = Just t
ecoTitle _ = Nothing

ecoTitle {title: "Foucault's pendulum", author: "Umberto Eco"} -- (Just "Foucault's pendulum")
ecoTitle {title: "The Quantum Thief", author: "Hannu Rajaniemi"} -- Nothing
-- ecoTitle은 유형 검사를 위해 두 필드 모두 필요:
ecoTitle {title: "The Quantum Thief"} -- 객체에 필요한 속성 "author"가 없음

-- 람다 표현식
(\x -> x*x) 3 -- 9
(\x y -> x*x + y*y) 4 5 -- 41
sqr = \x -> x*x

-- 커링
myAdd x y = x + y -- 와 동일
myAdd' = \x -> \y -> x + y
add3 = myAdd 3
:t add3 -- Int -> Int

-- 순방향 및 역방향 함수 구성
-- drop 3 다음에 take 5
(drop 3 >>> take 5) (1..20) -- [4,5,6,7,8]
-- take 5 다음에 drop 3
(drop 3 <<< take 5) (1..20) -- [4,5]

-- 고차 함수를 사용한 연산
even x = x `mod` 2 == 0
filter even (1..10) -- [2,4,6,8,10]
map (\x -> x + 11) (1..5) -- [12,13,14,15,16]

-- purescript-foldable-traversable (Data.Foldable) 필요

foldr (+) 0 (1..10) -- 55
sum (1..10) -- 55
product (1..10) -- 3628800

-- 술어로 테스트
any even [1,2,3] -- true
all even [1,2,3] -- false
```
