---
name: LiveScript
filename: learnLivescript.ls
contributors:
    - ["Christina Whyte", "http://github.com/kurisuwhyte/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

LiveScript는 함수형 자바스크립트 컴파일 언어로, 호스트 언어와 대부분의 기본 의미를 공유합니다. 커링, 함수 합성, 패턴 매칭 및 Haskell, F#, Scala와 같은 언어에서 많이 차용한 다른 좋은 기능들이 추가되었습니다.

LiveScript는 [Coco](https://github.com/satyr/coco)의 포크이며, Coco 자체는 [CoffeeScript](https://coffeescript.org/)의 포크입니다.

```livescript
# CoffeeScript 사촌과 마찬가지로 LiveScript는 한 줄 주석에 숫자 기호를 사용합니다.

/*
 여러 줄 주석은 C 스타일로 작성됩니다. 주석을 자바스크립트 출력에
 보존하고 싶다면 사용하십시오.
 */

# 구문 측면에서 LiveScript는 블록을 구분하기 위해 중괄호 대신
# 들여쓰기를 사용하고, 함수를 적용하기 위해 괄호 대신 공백을 사용합니다.


########################################################################
## 1. 기본 값
########################################################################

# 값의 부재는 `undefined` 키워드 대신 `void`로 정의됩니다.
void            # `undefined`와 같지만 더 안전합니다 (재정의할 수 없음)

# 유효하지 않은 값은 Null로 표시됩니다.
null


# 가장 기본적인 실제 값은 논리 타입입니다:
true
false

# 그리고 같은 의미를 가진 많은 별칭이 있습니다:
on; off
yes; no


# 그 다음은 숫자입니다. JS에서와 같이 배정밀도 부동 소수점입니다.
10
0.4     # 참고: 앞에 `0`이 필요합니다.

# 가독성을 위해 숫자에 밑줄과 문자 접미사를 사용할 수 있으며,
# 컴파일러는 이를 무시합니다.
12_344km


# 문자열은 JS에서와 같이 불변의 문자 시퀀스입니다:
"Christina"             # 작은따옴표도 괜찮습니다!
"""여러 줄
   문자열도
   괜찮습니다."""

# 때로는 키워드를 인코딩하고 싶을 때가 있는데, 백슬래시 표기법을 사용하면
# 쉽게 할 수 있습니다:
\keyword                # => 'keyword'


# 배열은 순서가 있는 값의 컬렉션입니다.
fruits =
  * \apple
  * \orange
  * \pear

# 대괄호를 사용하여 더 간결하게 표현할 수 있습니다:
fruits = [ \apple, \orange, \pear ]

# 공백을 사용하여 항목을 구분하는 편리한 방법으로
# 문자열 목록을 만들 수도 있습니다.
fruits = <[ apple orange pear ]>

# 0부터 시작하는 인덱스로 항목을 검색할 수 있습니다:
fruits[0]       # => "apple"

# 객체는 순서가 없는 키/값 쌍의 컬렉션이며, 몇 가지 다른
# 기능도 있습니다 (나중에 자세히 설명).
person =
  name: "Christina"
  likes:
    * "kittens"
    * "and other cute stuff"

# 다시 말하지만, 중괄호로 간결하게 표현할 수 있습니다:
person = {name: "Christina", likes: ["kittens", "and other cute stuff"]}

# 키로 항목을 검색할 수 있습니다:
person.name     # => "Christina"
person["name"]  # => "Christina"


# 정규 표현식은 자바스크립트와 동일한 구문을 사용합니다:
trailing-space = /\s$/          # 대시-단어는 대시단어(dashedWords)가 됩니다.

# 단, 여러 줄 표현식도 가능합니다!
# (주석과 공백은 무시됩니다)
funRE = //
        function\s+(.+)         # 이름
        \s* \((.*)\) \s*        # 인수
        { (.*) }                # 본문
        //


########################################################################
## 2. 기본 연산
########################################################################

# 산술 연산자는 자바스크립트와 동일합니다:
1 + 2   # => 3
2 - 1   # => 1
2 * 3   # => 6
4 / 2   # => 2
3 % 2   # => 1


# 비교도 대부분 동일하지만, `==`는 JS의 `===`와 같고,
# JS의 `==`는 LiveScript에서 `~=`이며, `===`는
# 객체 및 배열 비교와 더 엄격한 비교를 가능하게 합니다:
2 == 2          # => true
2 == "2"        # => false
2 ~= "2"        # => true
2 === "2"       # => false

[1,2,3] == [1,2,3]        # => false
[1,2,3] === [1,2,3]       # => true

+0 == -0     # => true
+0 === -0    # => false

# 다른 관계 연산자에는 <, <=, > 및 >=가 포함됩니다.

# 논리 값은 `or`, `and`, `not` 논리 연산자를 통해 결합할 수 있습니다.
true and false  # => false
false or true   # => true
not false       # => true


# 컬렉션에는 몇 가지 멋진 추가 연산자도 있습니다.
[1, 2] ++ [3, 4]                # => [1, 2, 3, 4]
'a' in <[ a b c ]>              # => true
'name' of { name: 'Chris' }     # => true


########################################################################
## 3. 함수
########################################################################

# LiveScript는 함수형이므로 함수가 멋지게 처리될 것으로 기대할 수 있습니다.
# LiveScript에서는 함수가 일급이라는 것이 더욱 분명합니다:
add = (left, right) -> left + right
add 1, 2        # => 3

# 인수가 없는 함수는 느낌표로 호출됩니다!
two = -> 2
two!

# LiveScript는 자바스크립트와 마찬가지로 함수 범위를 사용하며 적절한
# 클로저도 있습니다. 자바스크립트와 달리 `=`는 선언 연산자로
# 작동하며 항상 왼쪽 변수를 선언합니다.

# `:=` 연산자는 부모 범위의 이름을 *재사용*하는 데 사용할 수 있습니다.


# 복잡한 데이터 구조 내의 흥미로운 값에 빠르게 접근하기 위해
# 함수의 인수를 분해할 수 있습니다:
tail = ([head, ...rest]) -> rest
tail [1, 2, 3]  # => [2, 3]

# 이항 또는 단항 연산자를 사용하여 인수를 변환할 수도 있습니다.
# 기본 인수도 가능합니다.
foo = (a = 1, b = 2) -> a + b
foo!    # => 3

# 예를 들어 부작용을 피하기 위해 특정 인수를 복제하는 데 사용할 수 있습니다:
copy = (^^target, source) ->
  for k,v of source => target[k] = v
  target
a = { a: 1 }
copy a, { b: 2 }        # => { a: 1, b: 2 }
a                       # => { a: 1 }


# 짧은 화살표 대신 긴 화살표를 사용하여 함수를 커링할 수 있습니다:
add = (left, right) --> left + right
add1 = add 1
add1 2          # => 3

# 함수는 선언하지 않아도 암시적인 `it` 인수를 갖습니다.
identity = -> it
identity 1      # => 1

# 연산자는 LiveScript에서 함수가 아니지만 쉽게 함수로 바꿀 수 있습니다!
# 연산자 섹셔닝을 입력하십시오:
divide-by-two = (/ 2)
[2, 4, 8, 16].map(divide-by-two) .reduce (+)


# LiveScript는 함수 적용뿐만 아니라, 좋은 함수형 언어에서처럼
# 함수를 합성하는 기능을 제공합니다:
double-minus-one = (- 1) . (* 2)

# 일반적인 `f . g` 수학 공식 외에도 `>>` 및 `<<` 연산자를 사용하여
# 함수를 통한 값의 흐름을 설명할 수 있습니다.
double-minus-one = (* 2) >> (- 1)
double-minus-one = (- 1) << (* 2)


# 값의 흐름에 대해 말하자면, LiveScript는 값을 함수에 적용하는
# `|>` 및 `<|` 연산자를 사용합니다:
map = (f, xs) --> xs.map f
[1 2 3] |> map (* 2)            # => [2 4 6]

# 밑줄(_)로 위치를 표시하여 값을 배치할 위치를 선택할 수도 있습니다:
reduce = (f, xs, initial) --> xs.reduce f, initial
[1 2 3] |> reduce (+), _, 0     # => 6


# 밑줄은 일반 부분 적용에도 사용되며, 모든 함수에 사용할 수 있습니다:
div = (left, right) -> left / right
div-by-two = div _, 2
div-by-two 4      # => 2


# 마지막으로, LiveScript에는 백-콜(back-calls)이 있어 일부 콜백 기반 코드에
# 도움이 될 수 있습니다(그러나 Promises와 같은 더 기능적인 접근 방식을
# 시도해야 합니다):
readFile = (name, f) -> f name
a <- readFile 'foo'
b <- readFile 'bar'
console.log a + b

# 다음과 동일:
readFile 'foo', (a) -> readFile 'bar', (b) -> console.log a + b


########################################################################
## 4. 패턴, 가드 및 제어 흐름
########################################################################

# `if...else` 표현식으로 계산을 분기할 수 있습니다:
x = if n > 0 then \positive else \negative

# `then` 대신 `=>`를 사용할 수 있습니다.
x = if n > 0 => \positive
    else        \negative

# 복잡한 조건은 `switch` 표현식으로 더 잘 표현됩니다:
y = {}
x = switch
  | (typeof y) is \number => \number
  | (typeof y) is \string => \string
  | 'length' of y         => \array
  | otherwise             => \object      # `otherwise`와 `_`는 항상 일치합니다.

# 함수 본문, 선언 및 할당은 무료 `switch`를 얻으므로
# 다시 입력할 필요가 없습니다:
take = (n, [x, ...xs]) -->
                        | n == 0 => []
                        | _      => [x] ++ take (n - 1), xs


########################################################################
## 5. 컴프리헨션
########################################################################

# 목록 및 객체를 다루기 위한 함수형 도우미는 자바스크립트의 표준 라이브러리에
# 바로 있으며(그리고 LiveScript의 "표준 라이브러리"인 prelude-ls에서 보완됨),
# 컴프리헨션을 사용하면 일반적으로 이러한 작업을 더 빠르고 멋진 구문으로
# 수행할 수 있습니다:
oneToTwenty = [1 to 20]
evens       = [x for x in oneToTwenty when x % 2 == 0]

# `when`과 `unless`는 컴프리헨션에서 필터로 사용할 수 있습니다.

# 객체 컴프리헨션은 배열 대신 객체를 반환한다는 점을 제외하고는
# 동일한 방식으로 작동합니다:
copy = { [k, v] for k, v of source }


########################################################################
## 4. OOP
########################################################################

# LiveScript는 대부분의 측면에서 함수형 언어이지만, 명령형 및
# 객체 지향 프로그래밍을 위한 몇 가지 좋은 기능도 있습니다. 그 중 하나는
# CoffeeScript에서 상속받은 클래스 구문과 클래스 슈가입니다:
class Animal
  (@name, kind) ->
    @kind = kind
  action: (what) -> "*#{@name} (a #{@kind}) #{what}*"

class Cat extends Animal
  (@name) -> super @name, 'cat'
  purr: -> @action 'purrs'

kitten = new Cat 'Mei'
kitten.purr!      # => "*Mei (a cat) purrs*"

# 고전적인 단일 상속 패턴 외에도 클래스에 원하는 만큼 많은
# 믹스인을 제공할 수 있습니다. 믹스인은 일반 객체일 뿐입니다:
Huggable =
  hug: -> @action 'is hugged'

class SnugglyCat extends Cat implements Huggable

kitten = new SnugglyCat 'Purr'
kitten.hug!     # => "*Mei (a cat) is hugged*"
```

## 더 읽을거리

LiveScript에 대해 더 많은 것이 있지만, 이것으로 작은 기능적인 것들을
작성하기 시작하기에 충분할 것입니다.
[공식 웹사이트](http://livescript.net/)에는 언어에 대한 많은 정보와
시도해 볼 수 있는 멋진 온라인 컴파일러가 있습니다!

[prelude.ls](http://gkz.github.io/prelude-ls/)를 사용해보고, Freenode
네트워크의 `#livescript` 채널을 확인해 볼 수도 있습니다.
