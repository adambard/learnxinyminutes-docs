---
name: Elm
contributors:
    - ["Max Goldstein", "http://maxgoldste.in/"]
filename: learnelm.elm
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Elm은 (클라이언트 측) JavaScript로 컴파일되는 함수형 반응형 프로그래밍 언어입니다. Elm은 정적으로 유형이 지정되므로 컴파일러가 대부분의 오류를 즉시 포착하고 명확하고 이해하기 쉬운 오류 메시지를 제공합니다. Elm은 웹용 사용자 인터페이스 및 게임을 설계하는 데 적합합니다.


```haskell
-- 한 줄 주석은 두 개의 대시로 시작합니다.
{- 여러 줄 주석은 이와 같이 블록으로 묶을 수 있습니다.
{- 중첩될 수 있습니다. -}
-}

{-- 기본 사항 --}

-- 산술
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20

-- 소수점이 없는 모든 숫자 리터럴은 Int 또는 Float일 수 있습니다.
33 / 2 -- 부동 소수점 나눗셈으로 16.5
33 // 2 -- 정수 나눗셈으로 16

-- 지수
5 ^ 2 -- 25

-- 부울
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- 문자열 및 문자
"This is a string because it uses double quotes."
'a' -- 작은따옴표 안의 문자

-- 문자열을 추가할 수 있습니다.
"Hello " ++ "world!" -- "Hello world!"

{-- 목록, 튜플 및 레코드 --}

-- 목록의 모든 요소는 동일한 유형이어야 합니다.
["the", "quick", "brown", "fox"]
[1, 2, 3, 4, 5]
-- 두 번째 예제는 두 개의 점으로도 작성할 수 있습니다.
List.range 1 5

-- 문자열처럼 목록을 추가합니다.
List.range 1 5 ++ List.range 6 10 == List.range 1 10 -- True

-- 항목 하나를 추가하려면 "cons"를 사용하십시오.
0 :: List.range 1 5 -- [0, 1, 2, 3, 4, 5]

-- 목록의 머리와 꼬리는 Maybe로 반환됩니다. 모든 값을 확인하여 null인지 확인하는 대신 누락된 값을 명시적으로 처리합니다.
List.head (List.range 1 5) -- Just 1
List.tail (List.range 1 5) -- Just [2, 3, 4, 5]
List.head [] -- Nothing
-- List.functionName은 함수가 List 모듈에 있음을 의미합니다.

-- 튜플의 모든 요소는 다른 유형일 수 있지만 튜플은 고정된 길이를 가집니다.
("elm", 42)

-- 첫 번째 및 두 번째 함수로 쌍의 요소에 액세스합니다.
-- (이것은 바로 가기입니다. 잠시 후에 "실제 방법"을 설명하겠습니다.)
Tuple.first ("elm", 42) -- "elm"
Tuple.second ("elm", 42) -- 42

-- 빈 튜플 또는 "단위"는 때때로 자리 표시자로 사용됩니다.
-- 해당 유형의 유일한 값이며 "Unit"이라고도 합니다.
()

-- 레코드는 튜플과 같지만 필드에 이름이 있습니다. 필드 순서는 중요하지 않습니다. 레코드 값은 콜론이 아닌 등호를 사용합니다.
{ x = 3, y = 7 }

-- 점과 필드 이름으로 필드에 액세스합니다.
{ x = 3, y = 7 }.x -- 3

-- 또는 자체적으로 점과 필드 이름인 접근자 함수를 사용합니다.
.y { x = 3, y = 7 } -- 7

-- 레코드의 필드를 업데이트합니다. (이미 필드가 있어야 합니다.)
{ person |
  name = "George" }

-- 현재 값을 사용하여 한 번에 여러 필드를 업데이트합니다.
{ particle |
  position = particle.position + particle.velocity,
  velocity = particle.velocity + particle.acceleration }

{-- 제어 흐름 --}

-- If 문에는 항상 else가 있으며 분기는 동일한 유형이어야 합니다.
if powerLevel > 9000 then
  "WHOA!"
else
  "meh"

-- If 문을 연결할 수 있습니다.
if n < 0 then
  "n is negative"
else if n > 0 then
  "n is positive"
else
  "n is zero"

-- case 문을 사용하여 다른 가능성에 대해 패턴 일치
case aList of
  [] -> "matches the empty list"
  [x]-> "matches a list of exactly one item, " ++ toString x
  x::xs -> "matches a list of at least one item whose head is " ++ toString x
-- 패턴 일치는 순서대로 진행됩니다. [x]를 마지막에 넣으면 x::xs도 일치하므로(xs는 빈 목록이 됨) 절대 일치하지 않습니다. 일치는 "폴스루"되지 않습니다.
-- 컴파일러는 누락되거나 추가된 경우를 알려줍니다.

-- Maybe에서 패턴 일치.
case List.head aList of
  Just x -> "The head is " ++ toString x
  Nothing -> "The list was empty."

{-- 함수 --}

-- Elm의 함수 구문은 매우 최소화되어 있으며 괄호와 중괄호 대신 대부분 공백에 의존합니다. "return" 키워드는 없습니다.

-- 이름, 인수, 등호 및 본문으로 함수를 정의합니다.
multiply a b =
  a * b

-- 인수를 전달하여 함수를 적용(호출)합니다(쉼표 필요 없음).
multiply 7 6 -- 42

-- 일부 인수만 전달하여 함수를 부분적으로 적용합니다.
-- 그런 다음 해당 함수에 새 이름을 지정합니다.
double =
  multiply 2

-- 상수는 비슷하지만 인수가 없습니다.
answer =
  42

-- 다른 함수에 인수로 함수를 전달합니다.
List.map double (List.range 1 4) -- [2, 4, 6, 8]

-- 또는 익명 함수를 작성합니다.
List.map (\a -> a * 2) (List.range 1 4) -- [2, 4, 6, 8]

-- 한 가지 경우만 있는 경우 함수 정의에서 패턴 일치할 수 있습니다.
-- 이 함수는 두 개의 인수가 아닌 하나의 튜플을 사용합니다.
-- 이것이 일반적으로 튜플에서 값을 풀거나 추출하는 방법입니다.
area (width, height) =
  width * height

area (6, 7) -- 42

-- 중괄호를 사용하여 레코드 필드 이름과 패턴 일치합니다.
-- let을 사용하여 중간 값을 정의합니다.
volume {width, height, depth} =
  let
    area = width * height
  in
    area * depth

volume { width = 3, height = 2, depth = 7 } -- 42

-- 함수는 재귀적일 수 있습니다.
fib n =
  if n < 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

List.map fib (List.range 0 8) -- [1, 1, 2, 3, 5, 8, 13, 21, 34]

-- 또 다른 재귀 함수 (실제 코드에서는 List.length 사용).
listLength aList =
  case aList of
    [] -> 0
    x::xs -> 1 + listLength xs

-- 함수 호출은 모든 중위 연산자보다 먼저 발생합니다. 괄호는 우선 순위를 나타냅니다.
cos (degrees 30) ^ 2 + sin (degrees 30) ^ 2 -- 1
-- 먼저 degrees가 30에 적용되고, 결과가 삼각 함수에 전달되고, 제곱되고, 마지막으로 덧셈이 발생합니다.

{-- 유형 및 유형 주석 --}

-- 컴파일러는 프로그램의 모든 값의 유형을 추론합니다.
-- 유형은 항상 대문자입니다. x : T를 "x는 유형 T를 가짐"으로 읽습니다.
-- 몇 가지 일반적인 유형, Elm의 REPL에서 볼 수 있습니다.
5 : Int
6.7 : Float
"hello" : String
True : Bool

-- 함수에도 유형이 있습니다. ->를 "goes to"로 읽습니다. 가장 오른쪽 유형을 반환 값의 유형으로 생각하고 다른 유형을 인수로 생각하십시오.
not : Bool -> Bool
round : Float -> Int

-- 값을 정의할 때 그 위에 유형을 작성하는 것이 좋습니다.
-- 주석은 컴파일러에서 확인하는 문서 형식입니다.
double : Int -> Int
double x = x * 2

-- 함수 인수는 괄호로 전달됩니다.
-- 소문자 유형은 유형 변수입니다. 각 호출이 일관된 한 모든 유형이 될 수 있습니다.
List.map : (a -> b) -> List a -> List b
-- "List dot map has type a-goes-to-b, goes to list of a, goes to list of b."

-- 세 가지 특수 소문자 유형이 있습니다: number, comparable 및 appendable.
-- 숫자는 Int 및 Float에 대한 산술을 사용할 수 있게 합니다.
-- comparable은 a < b와 같이 숫자와 문자열을 정렬할 수 있게 합니다.
-- appendable은 a ++ b로 결합할 수 있습니다.

{-- 유형 별칭 및 사용자 지정 유형 --}

-- 레코드나 튜플을 작성할 때 해당 유형이 이미 존재합니다.
-- (레코드 유형은 콜론을 사용하고 레코드 값은 등호를 사용합니다.)
origin : { x : Float, y : Float, z : Float }
origin =
  { x = 0, y = 0, z = 0 }

-- 유형 별칭으로 기존 유형에 멋진 이름을 지정할 수 있습니다.
type alias Point3D =
  { x : Float, y : Float, z : Float }

-- 레코드에 별칭을 지정하면 이름을 생성자 함수로 사용할 수 있습니다.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0

-- 하지만 여전히 동일한 유형이므로 동일하게 취급할 수 있습니다.
origin == otherOrigin -- True

-- 반면에 사용자 지정 유형을 정의하면 이전에 존재하지 않았던 유형이 생성됩니다.
-- 사용자 지정 유형은 여러 가능성 중 하나일 수 있기 때문에 그렇게 불립니다.
-- 각 가능성은 "유형 변형"으로 표시됩니다.
type Direction =
  North | South | East | West

-- 유형 변형은 알려진 유형의 다른 값을 가질 수 있습니다. 이것은 재귀적으로 작동할 수 있습니다.
type IntTree =
  Leaf | Node Int IntTree IntTree
-- "Leaf"와 "Node"는 유형 변형입니다. 유형 변형 뒤에 오는 모든 것은 유형입니다.

-- 유형 변형은 값이나 함수로 사용할 수 있습니다.
root : IntTree
root =
  Node 7 Leaf Leaf

-- 사용자 지정 유형(및 유형 별칭)은 유형 변수를 사용할 수 있습니다.
type Tree a =
  Leaf | Node a (Tree a) (Tree a)
-- "The type tree-of-a is a leaf, or a node of a, tree-of-a, and tree-of-a."

-- 사용자 지정 유형에서 패턴 일치 변형. 대문자 변형은 정확히 일치합니다. 소문자 변수는 무엇이든 일치합니다. 밑줄도 무엇이든 일치하지만 사용하지 않음을 의미합니다.
leftmostElement : Tree a -> Maybe a
leftmostElement tree =
  case tree of
    Leaf -> Nothing
    Node x Leaf _ -> Just x
    Node _ subtree _ -> leftmostElement subtree

-- 이것이 언어 자체에 대한 거의 전부입니다. 이제 코드를 구성하고 실행하는 방법을 살펴보겠습니다.

{-- 모듈 및 가져오기 --}

-- 핵심 라이브러리는 모듈로 구성되며, 사용할 수 있는 모든 타사 라이브러리도 마찬가지입니다. 대규모 프로젝트의 경우 자신만의 모듈을 정의할 수 있습니다.

-- 파일 상단에 이것을 넣으십시오. 생략하면 Main에 있습니다.
module Name where

-- 기본적으로 모든 것이 내보내집니다. 내보내기를 명시적으로 지정할 수 있습니다.
module Name (MyType, myValue) where

-- 한 가지 일반적인 패턴은 사용자 지정 유형을 내보내지만 유형 변형은 내보내지 않는 것입니다. 이것은 "불투명 유형"으로 알려져 있으며 라이브러리에서 자주 사용됩니다.

-- 다른 모듈에서 코드를 가져와 이 모듈에서 사용합니다.
-- Dict를 범위에 배치하므로 Dict.insert를 호출할 수 있습니다.
import Dict

-- Dict 모듈과 Dict 유형을 가져오므로 주석에 Dict.Dict라고 말할 필요가 없습니다. 여전히 Dict.insert를 사용할 수 있습니다.
import Dict exposing (Dict)

-- 가져오기 이름 바꾸기.
import Graphics.Collage as C

{-- 포트 --}

-- 포트는 외부 세계와 통신할 것임을 나타냅니다.
-- 포트는 Main 모듈에서만 허용됩니다.

-- 들어오는 포트는 유형 서명일 뿐입니다.
port clientID : Int

-- 나가는 포트에는 정의가 있습니다.
port clientOrders : List String
port clientOrders = ["Books", "Groceries", "Furniture"]

-- 자세한 내용은 다루지 않겠지만, 들어오는 포트에서 보내고 나가는 포트에서 받도록 JavaScript에서 콜백을 설정합니다.

{-- 명령줄 도구 --}

-- 파일 컴파일.
$ elm make MyFile.elm

-- 처음 이 작업을 수행하면 Elm은 핵심 라이브러리를 설치하고 프로젝트에 대한 정보가 보관되는 elm-package.json을 생성합니다.

-- 리액터는 파일을 컴파일하고 실행하는 서버입니다.
-- 파일 이름 옆에 있는 렌치를 클릭하여 시간 여행 디버거를 시작하십시오!
$ elm reactor

-- 읽기-평가-인쇄 루프에서 간단한 표현식으로 실험합니다.
$ elm repl

-- 패키지는 GitHub 사용자 이름과 리포지토리 이름으로 식별됩니다.
-- 새 패키지를 설치하고 elm-package.json에 기록합니다.
$ elm package install elm-lang/html

-- 패키지 버전 간에 변경된 내용을 확인합니다.
$ elm package diff elm-lang/html 1.1.0 2.0.0
-- Elm의 패키지 관리자는 의미 체계 버전 관리를 적용하므로 부 버전 범프는 빌드를 절대 중단하지 않습니다!
```

Elm 언어는 놀라울 정도로 작습니다. 이제 거의 모든 Elm 소스 코드를 살펴보고 무슨 일이 일어나고 있는지 대략적으로 알 수 있습니다. 그러나 오류에 강하고 리팩토링하기 쉬운 코드의 가능성은 무한합니다!

다음은 몇 가지 유용한 자료입니다.

* [Elm 웹사이트](http://elm-lang.org/). 포함:
  * [설치 프로그램](http://elm-lang.org/install) 링크
  * [문서 가이드](http://elm-lang.org/docs), [구문 참조](http://elm-lang.org/docs/syntax) 포함
  * 많은 유용한 [예제](http://elm-lang.org/examples)

* [Elm의 핵심 라이브러리](http://package.elm-lang.org/packages/elm-lang/core/latest/) 문서. 다음 사항에 유의하십시오:
  * 기본적으로 가져오는 [기본 사항](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics)
  * 누락된 값이나 오류 처리에 일반적으로 사용되는 [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) 및 그 사촌 [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result)
  * [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List), [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array), [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) 및 [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set)과 같은 데이터 구조
  * JSON [인코딩](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode) 및 [디코딩](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode)

* [Elm 아키텍처](https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture). 코드를 구성 요소로 구성하는 방법에 대한 예제가 포함된 Elm 제작자의 에세이.

* [Elm 메일링 리스트](https://groups.google.com/forum/#!forum/elm-discuss). 모두 친절하고 도움이 됩니다.

* [Elm의 범위](https://github.com/elm-guides/elm-for-js/blob/master/Scope.md#scope-in-elm) 및 [유형 주석 읽는 방법](https://github.com/elm-guides/elm-for-js/blob/master/How%20to%20Read%20a%20Type%20Annotation.md#how-to-read-a-type-annotation). JavaScript 개발자를 위해 작성된 Elm의 기본 사항에 대한 더 많은 커뮤니티 가이드.

나가서 Elm을 작성하십시오!