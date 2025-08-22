---
name: Haskell
filename: learnhaskell.hs
contributors:
    - ["Adit Bhargava", "http://adit.io"]
    - ["Stanislav Modrak", "https://stanislav.gq"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Haskell은 실용적이고 순수한 함수형 프로그래밍 언어로 설계되었습니다. 모나드와 타입 시스템으로 유명하지만, 저는 그 우아함 때문에 계속해서 돌아옵니다. Haskell은 코딩을 정말 즐겁게 만듭니다.

```haskell
-- 한 줄 주석은 두 개의 대시로 시작합니다.
{- 여러 줄 주석은
이와 같이 블록으로 묶을 수 있습니다.
-}

----------------------------------------------------
-- 1. 기본 데이터 유형 및 연산자
----------------------------------------------------

-- 숫자가 있습니다.
3 -- 3

-- 수학은 예상대로입니다.
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- 나눗셈은 기본적으로 정수 나눗셈이 아닙니다.
35 / 4 -- 8.75

-- 정수 나눗셈
35 `div` 4 -- 8

-- 부울 값은 기본형입니다.
True
False

-- 부울 연산
not True -- False
not False -- True
True && False -- False
True || False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- 위 예제에서 `not`은 하나의 값을 받는 함수입니다.
-- Haskell은 함수 호출에 괄호가 필요하지 않습니다... 모든 인수는 함수 뒤에 나열됩니다. 따라서 일반적인 패턴은 다음과 같습니다:
-- func arg1 arg2 arg3...
-- 자신만의 함수를 작성하는 방법에 대한 정보는 함수 섹션을 참조하십시오.

-- 문자열 및 문자
"This is a string."
'a' -- 문자
'You cant use single quotes for strings.' -- 오류!

-- 문자열을 연결할 수 있습니다.
"Hello " ++ "world!" -- "Hello world!"

-- 문자열은 문자 목록입니다.
['H', 'e', 'l', 'l', 'o'] -- "Hello"

-- 목록은 `!!` 연산자와 인덱스를 사용하여 인덱싱할 수 있습니다.
-- 하지만 목록은 연결 리스트이므로 이것은 O(n) 연산입니다.
"This is a string" !! 0 -- 'T'


----------------------------------------------------
-- 2. 목록 및 튜플
----------------------------------------------------

-- 목록의 모든 요소는 동일한 유형이어야 합니다.
-- 이 두 목록은 동일합니다:
[1, 2, 3, 4, 5]
[1..5]

-- 범위는 다재다능합니다.
['A'..'F'] -- "ABCDEF"

-- 범위에서 단계를 만들 수 있습니다.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- [] (Haskell은 기본적으로 증가합니다)
[5,4..1] -- [5, 4, 3, 2, 1]

-- 목록 인덱싱
[1..10] !! 3 -- 4 (0부터 시작하는 인덱싱)

-- Haskell에서 무한 목록을 가질 수도 있습니다!
[1..] -- 모든 자연수의 목록

-- 무한 목록은 Haskell이 "지연 평가"를 하기 때문에 작동합니다. 이것은 Haskell이 필요할 때만 평가한다는 것을 의미합니다. 따라서 목록의 1000번째 요소를 요청하면 Haskell이 제공합니다:

[1..] !! 999 -- 1000

-- 그리고 이제 Haskell은 이 목록의 1-1000 요소를 평가했습니다... 하지만 이 "무한" 목록의 나머지 요소는 아직 존재하지 않습니다! Haskell은 필요할 때까지 실제로 평가하지 않습니다.

-- 두 목록 결합
[1..5] ++ [6..10]

-- 목록의 머리에 추가
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- 더 많은 목록 연산
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- 목록 이해
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- 조건부
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- 튜플의 모든 요소는 다른 유형일 수 있지만 튜플은 고정된 길이를 가집니다.
-- 튜플:
("haskell", 1)

-- 쌍의 요소 액세스 (즉, 길이가 2인 튜플)
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

-- 쌍 요소 액세스는 n-튜플(즉, 삼중, 사중 등)에서 작동하지 않습니다.
snd ("snd", "can't touch this", "da na na na") -- 오류! 아래 함수 참조

----------------------------------------------------
-- 3. 함수
----------------------------------------------------
-- 두 변수를 사용하는 간단한 함수
add a b = a + b

-- ghci(Haskell 인터프리터)를 사용하는 경우
-- `let`을 사용해야 합니다. 즉,
-- let add a b = a + b

-- 함수 사용
add 1 2 -- 3

-- 함수 이름을 두 인수 사이에 넣을 수도 있습니다.
-- 백틱 사용:
1 `add` 2 -- 3

-- 문자가 아닌 함수를 정의할 수도 있습니다! 이것은 자신만의 연산자를 정의할 수 있게 합니다! 다음은 정수 나눗셈을 수행하는 연산자입니다.
(//) a b = a `div` b
35 // 4 -- 8

-- 가드: 함수에서 분기하는 쉬운 방법
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- 패턴 매칭은 비슷합니다. 여기서는 fib를 정의하는 세 가지 다른 방정식을 제공했습니다. Haskell은 왼쪽 패턴이 값과 일치하는 첫 번째 방정식을 자동으로 사용합니다.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- 튜플에서 패턴 매칭
sndOfTriple (_, y, _) = y -- 와일드카드(_)를 사용하여 사용하지 않는 값의 이름 지정을 건너뜁니다.

-- 목록에서 패턴 매칭. 여기서 `x`는 목록의 첫 번째 요소이고 `xs`는 목록의 나머지 부분입니다. 자신만의 map 함수를 작성할 수 있습니다:
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- 익명 함수는 백슬래시와 모든 인수를 사용하여 생성됩니다.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- 익명 함수와 함께 fold(일부 언어에서는 `inject`라고 함) 사용. foldl1은 왼쪽으로 접는 것을 의미하며, 목록의 첫 번째 값을 누산기의 초기 값으로 사용합니다.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. 더 많은 함수
----------------------------------------------------

-- 부분 적용: 함수에 모든 인수를 전달하지 않으면 "부분적으로 적용"됩니다. 즉, 나머지 인수를 받는 함수를 반환합니다.

add a b = a + b
foo = add 10 -- foo는 이제 숫자를 받아 10을 더하는 함수입니다.
foo 5 -- 15

-- 동일한 것을 작성하는 또 다른 방법
foo = (10+)
foo 5 -- 15

-- 함수 구성
-- 연산자 `.`는 함수를 함께 연결합니다.
-- 예를 들어, 여기서 foo는 값을 받는 함수입니다. 10을 더하고, 그 결과에 4를 곱한 다음 최종 값을 반환합니다.
foo = (4*) . (10+)

-- 4*(10+5) = 60
foo 5 -- 60

-- 우선 순위 수정
-- Haskell에는 `$`라는 연산자가 있습니다. 이 연산자는 지정된 매개변수에 함수를 적용합니다. 가능한 가장 높은 우선 순위 10을 가지며 왼쪽 결합인 표준 함수 적용과 달리 `$` 연산자는 우선 순위 0을 가지며 오른쪽 결합입니다. 이러한 낮은 우선 순위는 오른쪽 표현식이 왼쪽 함수의 매개변수로 적용됨을 의미합니다.

-- 이전
even (fib 7) -- false

-- 동일하게
even $ fib 7 -- false

-- 함수 구성
even . fib $ 7 -- false


----------------------------------------------------
-- 5. 타입 서명
----------------------------------------------------

-- Haskell은 매우 강력한 타입 시스템을 가지고 있으며, 모든 유효한 표현식에는 타입이 있습니다.

-- 일부 기본 타입:
5 :: Integer
"hello" :: String
True :: Bool

-- 함수에도 타입이 있습니다.
-- `not`은 부울을 받아 부울을 반환합니다:
-- not :: Bool -> Bool

-- 다음은 두 개의 인수를 받는 함수입니다:
-- add :: Integer -> Integer -> Integer

-- 값을 정의할 때 그 위에 타입을 작성하는 것이 좋습니다:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. 제어 흐름 및 If 표현식
----------------------------------------------------

-- if-표현식
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if-표현식은 여러 줄일 수도 있으며, 들여쓰기가 중요합니다.
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case 표현식: 다음은 명령줄 인수를 구문 분석하는 방법입니다.
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell에는 루프가 없습니다. 대신 재귀를 사용합니다.
-- map은 목록의 모든 요소에 함수를 적용합니다.

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- map을 사용하여 for 함수를 만들 수 있습니다.
for list func = map func list

-- 그런 다음 사용합니다.
for [0..5] $ \i -> show i

-- 다음과 같이 작성할 수도 있습니다:
for [0..5] show

-- filter는 조건을 만족하는 목록의 요소만 유지합니다.
filter even [1..10] -- [2, 4, 8, 10]

-- foldl 또는 foldr을 사용하여 목록을 줄일 수 있습니다.
-- foldl <fn> <초기 값> <목록>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- 이것은 다음과 같습니다.
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl은 왼쪽 결합이고, foldr은 오른쪽 결합입니다.
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- 이것은 이제 다음과 같습니다.
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. 데이터 유형
----------------------------------------------------

-- 데이터 유형은 왼쪽에 '타입 생성자'와 오른쪽에 하나 이상의 '데이터 생성자'로 선언되며, 파이프 | 기호로 구분됩니다. 이것은 합계/공용체 유형입니다. 각 데이터 생성자는 타입 생성자가 명명한 유형의 객체를 생성하는 (아마도 0항) 함수입니다.

-- 이것은 본질적으로 열거형입니다.

data Color = Red | Blue | Green

-- 이제 함수에서 사용할 수 있습니다:

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"

-- 타입 생성자는 타입 서명에 사용되고 데이터 생성자는 함수 본문에 사용됩니다. 데이터 생성자는 주로 패턴 매칭에 사용됩니다.

-- 다음은 두 개의 필드를 가진 전통적인 컨테이너 유형입니다.
-- 타입 선언에서 데이터 생성자는 매개변수로 타입을 받습니다.
-- 데이터 생성자는 타입 생성자와 동일한 이름을 가질 수 있습니다.
-- 이것은 타입에 단일 데이터 생성자만 있는 경우 일반적입니다.

data Point = Point Float Float

-- 이것은 다음과 같은 함수에서 사용할 수 있습니다:

distance :: Point -> Point -> Float
distance (Point x y) (Point x' y') = sqrt $ dx + dy
    where dx = (x - x') ** 2
          dy = (y - y') ** 2

-- 타입에는 인수가 있는 여러 데이터 생성자가 있을 수도 있습니다.

data Name = Mononym String
          | FirstLastName String String
          | FullName String String String

-- 더 명확하게 하기 위해 레코드 구문을 사용할 수 있습니다.

data Point2D = CartesianPoint2D { x :: Float, y :: Float }
             | PolarPoint2D { r :: Float, theta :: Float }

myPoint = CartesianPoint2D { x = 7.0, y = 10.0 }

-- 레코드 구문을 사용하면 접근자 함수(필드 이름)가 자동으로 생성됩니다.

xOfMyPoint = x myPoint

-- xOfMyPoint는 7.0과 같습니다.

-- 레코드 구문은 또한 간단한 업데이트 형식을 허용합니다.

myPoint' = myPoint { x = 9.0 }

-- myPoint'는 CartesianPoint2D { x = 9.0, y = 10.0 }입니다.

-- 타입이 레코드 구문으로 정의되었더라도 간단한 데이터 생성자처럼 선언할 수 있습니다. 이것은 괜찮습니다:

myPoint'2 = CartesianPoint2D 3.3 4.0

-- `case` 표현식에서 데이터 생성자를 패턴 매칭하는 것이 유용합니다.

distanceFromOrigin x =
    case x of (CartesianPoint2D x y) -> sqrt $ x ** 2 + y ** 2
              (PolarPoint2D r _) -> r

-- 데이터 유형에는 타입 매개변수도 있을 수 있습니다:

data Maybe a = Nothing | Just a

-- 이것들은 모두 Maybe 유형입니다.
Just "hello"    -- `Maybe String` 유형
Just 1          -- `Maybe Int` 유형
Nothing         -- 모든 `a`에 대한 `Maybe a` 유형

-- 편의를 위해 'type' 키워드로 타입 동의어를 만들 수도 있습니다.

type String = [Char]

-- `data` 유형과 달리 타입 동의어에는 생성자가 필요하지 않으며, 동의어 데이터 유형을 사용할 수 있는 모든 곳에서 사용할 수 있습니다. 다음과 같은 타입 동의어와 다음 타입 서명이 있는 항목이 있다고 가정해 보겠습니다.

type Weight = Float
type Height = Float
type Point = (Float, Float)
getMyHeightAndWeight :: Person -> (Height, Weight)
findCenter :: Circle -> Point
somePerson :: Person
someCircle :: Circle
distance :: Point -> Point -> Float

-- 다음은 의미적으로 의미가 없더라도 컴파일되고 문제 없이 실행됩니다.
-- 타입 동의어가 동일한 기본 타입으로 축소되기 때문입니다.

distance (getMyHeightAndWeight somePerson) (findCenter someCircle)

----------------------------------------------------
-- 8. 타입 클래스
----------------------------------------------------

-- 타입 클래스는 Haskell이 다형성을 수행하는 한 가지 방법입니다.
-- 다른 언어의 인터페이스와 유사합니다.
-- 타입 클래스는 해당 타입 클래스에 있는 모든 타입에서 작동해야 하는 함수 집합을 정의합니다.

-- Eq 타입 클래스는 인스턴스를 서로 같음을 테스트할 수 있는 타입에 대한 것입니다.

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

-- 이것은 두 개의 함수 (==) 및 (/=)를 필요로 하는 타입 클래스를 정의합니다.
-- 또한 한 함수가 다른 함수로 정의될 수 있음을 선언합니다.
-- 따라서 (==) 함수 또는 (/=) 함수 중 하나만 정의하면 충분합니다.
-- 그리고 다른 하나는 타입 클래스 정의에 따라 '채워집니다'.

-- 타입을 타입 클래스의 멤버로 만들려면 instance 키워드를 사용합니다.

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- 이제 TrafficLight 객체와 함께 (==) 및 (/=)를 사용할 수 있습니다.

canProceedThrough :: TrafficLight -> Bool
canProceedThrough t = t /= Red

-- 타입 동의어에 대한 인스턴스 정의를 만들 수 없습니다.

-- 함수는 타입 클래스에 타입 매개변수를 사용하여 작성할 수 있습니다.
-- 타입 대신, 함수가 타입 클래스의 기능에만 의존한다고 가정합니다.

isEqual :: (Eq a) => a -> a -> Bool
isEqual x y = x == y

-- x와 y는 모두 타입 매개변수 'a'로 정의되었으므로 동일한 타입이어야 합니다.
-- 타입 클래스는 타입 클래스의 다른 타입이 함께 혼합될 수 있다고 명시하지 않습니다.
-- 따라서 `isEqual Red 2`는 2가 Eq의 인스턴스인 Int이고 Red가 Eq의 인스턴스인 TrafficLight이더라도 유효하지 않습니다.

-- 다른 일반적인 타입 클래스는 다음과 같습니다:
-- Ord는 정렬할 수 있는 타입에 대한 것으로, >, <= 등을 사용할 수 있습니다.
-- Read는 문자열 표현에서 만들 수 있는 타입에 대한 것입니다.
-- Show는 표시를 위해 문자열로 변환할 수 있는 타입에 대한 것입니다.
-- Num, Real, Integral, Fractional은 수학을 할 수 있는 타입에 대한 것입니다.
-- Enum은 단계를 거칠 수 있는 타입에 대한 것입니다.
-- Bounded는 최대 및 최소가 있는 타입에 대한 것입니다.

-- Haskell은 타입 선언 끝에 `deriving` 키워드를 사용하여 타입을 Eq, Ord, Read, Show, Enum 및 Bounded의 일부로 자동으로 만들 수 있습니다.

data Point = Point Float Float deriving (Eq, Read, Show)

-- 이 경우 'instance' 정의를 만들 필요가 없습니다.

----------------------------------------------------
-- 9. Haskell IO
----------------------------------------------------

-- IO는 모나드를 설명하지 않고는 완전히 설명할 수 없지만, 시작하기에 충분히 설명하는 것은 어렵지 않습니다.

-- Haskell 프로그램이 실행되면 `main`이 호출됩니다. 일부 타입 `a`에 대해 `IO a` 타입의 값을 반환해야 합니다. 예를 들어:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn은 String -> IO () 타입을 가집니다.

-- 프로그램을 String에서 String으로의 함수로 구현할 수 있다면 IO를 수행하는 것이 가장 쉽습니다. 함수
--    interact :: (String -> String) -> IO ()
-- 일부 텍스트를 입력하고, 함수를 실행하고, 출력을 인쇄합니다.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- `IO ()` 타입의 값은 명령형 언어로 작성된 컴퓨터 프로그램과 마찬가지로 컴퓨터가 수행할 일련의 작업을 나타내는 것으로 생각할 수 있습니다. `do` 표기법을 사용하여 작업을 함께 연결할 수 있습니다. 예를 들어:

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- 이것은 줄을 가져와 "name"이라는 이름을 부여합니다.
   putStrLn $ "Hello, " ++ name

-- 연습: 한 줄의 입력만 읽는 자신만의 `interact` 버전을 작성하십시오.

-- `sayHello`의 코드는 절대 실행되지 않습니다. 실행되는 유일한 작업은 `main`의 값입니다.
-- `sayHello`를 실행하려면 위의 `main` 정의를 주석 처리하고 다음으로 바꾸십시오:
--   main = sayHello

-- 방금 사용한 `getLine` 함수가 어떻게 작동하는지 더 잘 이해해 봅시다. 해당 타입은 다음과 같습니다:
--    getLine :: IO String
-- `IO a` 타입의 값은 실행될 때 `a` 타입의 값을 생성하는 컴퓨터 프로그램을 나타내는 것으로 생각할 수 있습니다(그 외에 수행하는 모든 작업 외에). `<-`를 사용하여 이 값을 명명하고 재사용할 수 있습니다. 또한 `IO String` 타입의 자신만의 작업을 만들 수도 있습니다:

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- `do` 문의 타입은 마지막 줄의 타입입니다.
   -- `return`은 키워드가 아니라 단지 함수입니다.
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- 이것을 `getLine`을 사용한 것처럼 사용할 수 있습니다:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- `IO` 타입은 "모나드"의 예입니다. Haskell이 IO를 수행하는 데 모나드를 사용하는 방식은 순수한 함수형 언어가 될 수 있게 합니다. 외부 세계와 상호 작용하는 모든 함수(즉, IO를 수행하는 함수)는 타입 서명에 `IO`로 표시됩니다. 이를 통해 어떤 함수가 "순수"(외부 세계와 상호 작용하거나 상태를 수정하지 않음)이고 어떤 함수가 그렇지 않은지 추론할 수 있습니다.

-- 이것은 순수 함수를 동시에 실행하기 쉽기 때문에 강력한 기능입니다. 따라서 Haskell의 동시성은 매우 쉽습니다.


----------------------------------------------------
-- 10. Haskell REPL
----------------------------------------------------

-- `ghci`를 입력하여 repl을 시작합니다.
-- 이제 Haskell 코드를 입력할 수 있습니다. 모든 새 값은 `let`으로 만들어야 합니다:

let foo = 5

-- `:t`로 모든 값이나 표현식의 타입을 볼 수 있습니다:

> :t foo
foo :: Integer

-- `+`, `:` 및 `$`와 같은 연산자는 함수입니다.
-- 괄호 안에 연산자를 넣어 타입을 검사할 수 있습니다:

> :t (:)
(:) :: a -> [a] -> [a]

-- `:i`를 사용하여 모든 `name`에 대한 추가 정보를 얻을 수 있습니다:

> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
    -- ‘GHC.Num’에 정의됨
infixl 6 +

-- `IO ()` 타입의 모든 작업을 실행할 수도 있습니다.

> sayHello
What is your name?
Friend!
Hello, Friend!
```

모나드와 타입 클래스를 포함하여 Haskell에는 훨씬 더 많은 것이 있습니다. 이것들은 Haskell에서 코딩하는 것을 매우 재미있게 만드는 큰 아이디어입니다. 마지막 Haskell 예제로 Haskell의 퀵 정렬 변형 구현을 남겨두겠습니다:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell을 설치하는 두 가지 인기 있는 방법이 있습니다: 전통적인 [Cabal 기반 설치](http://www.haskell.org/platform/)와 최신 [Stack 기반 프로세스](https://www.stackage.org/install)입니다.

훌륭한 [Learn you a Haskell](http://learnyouahaskell.com/) (또는 [최신 커뮤니티 버전](https://learnyouahaskell.github.io/)), [Happy Learn Haskell Tutorial](http://www.happylearnhaskelltutorial.com/) 또는 [Real World Haskell](http://book.realworldhaskell.org/)에서 훨씬 더 부드러운 소개를 찾을 수 있습니다.

```