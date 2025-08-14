---
name: Nim
filename: learnNim.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
    - ["Dennis Felsing", "https://dennis.felsing.org"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Nim (이전 Nimrod)은 정적으로 타입이 지정된 명령형 프로그래밍 언어로,
런타임 효율성을 타협하지 않으면서 프로그래머에게 강력한 기능을 제공합니다.

Nim은 효율적이고 표현력이 풍부하며 우아합니다.

```nim
# 한 줄 주석은 #으로 시작합니다.

#[
  이것은 여러 줄 주석입니다.
  Nim에서 여러 줄 주석은 #[로 시작하여
  ... ]#로 끝나는 중첩이 가능합니다.
]#

discard """
이것은 여러 줄 주석으로도 작동할 수 있습니다.
또는 구문 분석할 수 없거나 깨진 코드에 대해서도 마찬가지입니다.
"""

var                     # 변수 선언 (및 할당),
  letter: char = 'n'    # 타입 주석 포함 또는 미포함
  lang = "N" & "im"
  nLength: int = len(lang)
  boat: float
  truth: bool = false

let            # let을 사용하여 변수를 *한 번* 선언하고 바인딩합니다.
  legs = 400   # legs는 불변입니다.
  arms = 2_000 # _는 무시되며 긴 숫자에 유용합니다.
  aboutPi = 3.15

const            # 상수는 컴파일 타임에 계산됩니다. 이는
  debug = true   # 성능을 제공하며 컴파일 타임 표현식에 유용합니다.
  compileBadCode = false

when compileBadCode:            # `when`은 컴파일 타임 `if`입니다.
  legs = legs + 1               # 이 오류는 절대 컴파일되지 않습니다.
  const input = readline(stdin) # const 값은 컴파일 타임에 알려져 있어야 합니다.

discard 1 > 2 # 참고: 컴파일러는 표현식의 결과가 사용되지 않으면
              # 불평합니다. `discard`는 이것을 우회합니다.


#
# 데이터 구조
#

# 튜플

var
  child: tuple[name: string, age: int]   # 튜플은 *필드 이름과*
  today: tuple[sun: string, temp: float] # *순서를* 모두 가집니다.

child = (name: "Rudiger", age: 2) # 리터럴 ()로 한 번에 모두 할당
today.sun = "Overcast"            # 또는 개별 필드.
today[1] = 70.1                   # 또는 인덱스로.

let impostor = ("Rudiger", 2) # 두 튜플은 동일한 타입과 동일한 내용을
assert child == impostor      # 가지는 한 동일합니다.

# 시퀀스

var
  drinks: seq[string]

drinks = @["Water", "Juice", "Chocolate"] # @[V1,..,Vn]은 시퀀스 리터럴입니다.

drinks.add("Milk")

if "Milk" in drinks:
  echo "We have Milk and ", drinks.len - 1, " other drinks"

let myDrink = drinks[2]

#
# 타입 정의
#

# 자신만의 타입을 정의하면 컴파일러가 당신을 위해 일하게 됩니다. 이것이
# 정적 타이핑을 강력하고 유용하게 만드는 것입니다.

type
  Name = string # 타입 별칭은 이전 타입과 상호 교환 가능하지만
  Age = int     # 더 설명적인 새 타입을 제공합니다.
  Person = tuple[name: Name, age: Age] # 데이터 구조도 정의합니다.
  AnotherSyntax = tuple
    fieldOne: string
    secondField: int

var
  john: Person = (name: "John B.", age: 17)
  newage: int = 18 # int보다 Age를 사용하는 것이 더 좋습니다.

john.age = newage # 하지만 int와 Age는 동의어이므로 여전히 작동합니다.

type
  Cash = distinct int    # `distinct`는 기본 타입과 호환되지 않는 새 타입을
  Desc = distinct string # 만듭니다.

var
  money: Cash = 100.Cash # `.Cash`는 int를 우리 타입으로 변환합니다.
  description: Desc  = "Interesting".Desc

when compileBadCode:
  john.age  = money        # 오류! age는 int 타입이고 money는 Cash입니다.
  john.name = description  # 컴파일러가 "안돼!"라고 말합니다.

#
# 더 많은 타입 및 데이터 구조
#

# 객체는 튜플과 유사하지만 필드 이름이 *필수*입니다.

type
  Room = ref object # 객체에 대한 참조, 큰 객체나
    windows: int    # 객체 내부의 객체에 유용합니다.
    doors: int = 1  # 필드의 기본값 변경 (Nim 2.0부터)
  House = object
    address: string
    rooms: seq[Room]

var
  defaultHouse = House() # 기본값으로 초기화
  defaultRoom = Room() # 기본값으로 ref 객체의 새 인스턴스 생성

  # 주어진 값으로 인스턴스 생성 및 초기화
  sesameRoom = Room(windows: 4, doors: 2)
  sesameHouse = House(address: "123 Sesame St.", rooms: @[sesameRoom])

# 열거형은 타입이 제한된 수의 값 중 하나를 갖도록 허용합니다.

type
  Color = enum cRed, cBlue, cGreen
  Direction = enum # 대체 서식
    dNorth
    dWest
    dEast
    dSouth
var
  orient = dNorth # `orient`는 Direction 타입이며 값은 `dNorth`입니다.
  pixel = cGreen # `pixel`은 Color 타입이며 값은 `cGreen`입니다.

discard dNorth > dEast # 열거형은 보통 "순서" 타입입니다.

# 서브레인지는 제한된 유효 범위를 지정합니다.

type
  DieFaces = range[1..20] # 1에서 20까지의 int만 유효한 값입니다.
var
  my_roll: DieFaces = 13

when compileBadCode:
  my_roll = 23 # 오류!

# 배열

type
  RollCounter = array[DieFaces, int]  # 배열은 고정 길이이며
  DirNames = array[Direction, string] # 모든 순서 타입으로 인덱싱됩니다.
  Truths = array[42..44, bool]
var
  counter: RollCounter
  directions: DirNames
  possible: Truths

possible = [false, false, false] # 리터럴 배열은 [V1,..,Vn]으로 생성됩니다.
possible[42] = true

directions[dNorth] = "Ahh. The Great White North!"
directions[dWest] = "No, don't go there."

my_roll = 13
counter[my_roll] += 1
counter[my_roll] += 1

var anotherArray = ["Default index", "starts at", "0"]

# 테이블, 세트, 리스트, 큐, 크릿 비트 트리 등 더 많은 데이터 구조를
# 사용할 수 있습니다.
# http://nim-lang.org/docs/lib.html#collections-and-algorithms

#
# IO 및 제어 흐름
#

# `case`, `readLine()`

echo "Read any good books lately?"
case readLine(stdin)
of "no", "No":
  echo "Go to your local library."
of "yes", "Yes":
  echo "Carry on, then."
else:
  echo "That's great; I assume."

# `while`, `if`, `continue`, `break`

import strutils as str # http://nim-lang.org/docs/strutils.html
echo "I'm thinking of a number between 41 and 43. Guess which!"
let number: int = 42
var
  raw_guess: string
  guess: int # Nim의 변수는 항상 0 값으로 초기화됩니다.
while guess != number:
  raw_guess = readLine(stdin)
  if raw_guess == "": continue # 이 반복 건너뛰기
  guess = str.parseInt(raw_guess)
  if guess == 1001:
    echo("AAAAAAGGG!")
    break
  elif guess > number:
    echo("Nope. Too high.")
  elif guess < number:
    echo(guess, " is too low")
  else:
    echo("Yeeeeeehaw!")

#
# 반복
#

for i, elem in ["Yes", "No", "Maybe so"]: # 또는 그냥 `for elem in`
  echo(elem, " is at index: ", i)

for k, v in items(@[(person: "You", power: 100), (person: "Me", power: 9000)]):
  echo v

let myString = """
an <example>
`string` to
play with
""" # 여러 줄 원시 문자열

for line in splitLines(myString):
  echo(line)

for i, c in myString:       # 인덱스와 문자. 또는 그냥 문자에 대해 `for j in`
  if i mod 2 == 0: continue # 간결한 `if` 형식
  elif c == 'X': break
  else: echo(c)

#
# 프로시저
#

type Answer = enum aYes, aNo

proc ask(question: string): Answer =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes":
      return Answer.aYes  # 열거형은 한정될 수 있습니다.
    of "n", "N", "no", "No":
      return Answer.aNo
    else: echo("Please be clear: yes or no")

proc addSugar(amount: int = 2) = # 기본 양은 2이며 아무것도 반환하지 않습니다.
  assert(amount > 0 and amount < 9000, "Crazy Sugar")
  for a in 1..amount:
    echo(a, " sugar...")

case ask("Would you like sugar in your tea?")
of aYes:
  addSugar(3)
of aNo:
  echo "Oh do take a little!"
  addSugar()
# 여기서 `else`는 필요 없습니다. `yes`와 `no`만 가능합니다.

#
# FFI
#

# Nim은 C로 컴파일되므로 FFI가 쉽습니다.

proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}

let cmp = strcmp("C?", "Easy!")
```

또한 Nim은 메타프로그래밍, 성능 및 컴파일 타임 기능으로
동료들과 차별화됩니다.

## 더 읽을거리

* [홈페이지](http://nim-lang.org)
* [다운로드](http://nim-lang.org/download.html)
* [커뮤니티](http://nim-lang.org/community.html)
* [FAQ](http://nim-lang.org/question.html)
* [문서](http://nim-lang.org/documentation.html)
* [매뉴얼](http://nim-lang.org/docs/manual.html)
* [표준 라이브러리](http://nim-lang.org/docs/lib.html)
* [로제타 코드](http://rosettacode.org/wiki/Category:Nim)
