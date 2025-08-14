---
name: Self
contributors:
    - ["Russell Allen", "http://github.com/russellallen"]
filename: learnself.self
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Self는 자체 JIT VM에서 실행되는 빠른 프로토타입 기반 객체 지향 언어입니다. 대부분의 개발은 통합된 브라우저와 디버거가 있는 *morphic*이라는 시각적 개발 환경을 통해 라이브 객체와 상호 작용하여 수행됩니다.

Self의 모든 것은 객체입니다. 모든 계산은 객체에 메시지를 보내는 것으로 수행됩니다. Self의 객체는 키-값 슬롯의 집합으로 이해할 수 있습니다.

# 객체 생성

내장된 Self 파서는 메서드 객체를 포함한 객체를 생성할 수 있습니다.

```
"이것은 주석입니다"

"문자열:"
'이스케이프 문자가 있는 문자열입니다.\n'

"30비트 정수"
23

"30비트 부동 소수점"
3.2

"-20"
-14r16

"하나의 메시지 'x'만 이해하고 20을 반환하는 객체"
(|
  x = 20.
|)

"x 슬롯을 설정하는 'x:'도 이해하는 객체"
(|
  x <- 20.
|)

"x의 값을 두 배로 늘린 다음 객체를 반환하는 'doubleX' 메서드를 이해하는 객체"
(|
  x <- 20.
  doubleX = (x: x * 2. self)
|)

"'traits point'가 이해하는 모든 메시지를 이해하는 객체". 파서는 'lobby'라는 알려진 객체에 'traits' 다음 'point' 메시지를 보내 'traits point'를 찾습니다. 또한 로비에 'true' 메시지를 보내 'true' 객체를 찾습니다."
(|     parent* = traits point.
       x = 7.
       y <- 5.
       isNice = true.
|)
```

# 객체에 메시지 보내기

메시지는 단항, 이항 또는 키워드일 수 있습니다. 우선순위는 그 순서대로입니다. Smalltalk와 달리 이항 메시지의 우선순위는 지정해야 하며, 첫 번째 키워드 다음의 모든 키워드는 대문자로 시작해야 합니다. 메시지는 공백으로 대상과 구분됩니다.

```
"단항 메시지, '23' 객체에 'printLine'을 보내 stdout에 '23' 문자열을 출력하고 수신 객체(즉, 23)를 반환합니다"
23 printLine

"'23'에 '7'과 함께 '+' 메시지를 보낸 다음 결과에 '8'과 함께 '*' 메시지를 보냅니다"
(23 + 7) * 8

"'2'에 '8'과 함께 'power:'를 보내 256을 반환합니다"
2 power: 8

"'hello'에 인수 'e'와 '-1'을 사용하여 'keyOf:IfAbsent:'를 보냅니다. 'hello'에서 'e'의 인덱스인 1을 반환합니다."
'hello' keyOf: 'e' IfAbsent: -1
```

# 블록

Self는 Smalltalk 및 Ruby와 같이 블록을 통해 제어 흐름을 정의합니다. 블록은 다음과 같은 형식의 지연된 계산입니다:

```
[|:x. localVar| x doSomething with: localVar]
```

블록 사용 예:

```
"returns 'HELLO'"
'hello' copyMutable mapBy: [|:c| c capitalize]

"returns 'Nah'"
'hello' size > 5 ifTrue: ['Yay'] False: ['Nah']

"returns 'HaLLO'"
'hello' copyMutable mapBy: [|:c|
   c = 'e' ifTrue: [c capitalize]
            False: ['a']]
```

여러 표현식은 마침표로 구분됩니다. ^는 즉시 반환됩니다.

```
"returns An 'E'! How icky!"
'hello' copyMutable mapBy: [|:c. tmp <- ''|
   tmp: c capitalize.
   tmp = 'E' ifTrue: [^ 'An \'E\'! How icky!'].
   c capitalize
   ]
```

블록은 'value' 메시지를 보내 수행되며 컨텍스트를 상속(위임)합니다:

```
"returns 0"
[|x|
    x: 15.
    "두 번째 블록에 'value'를 보낸 결과가 'true' 객체인 동안 첫 번째 블록에 'value'를 반복적으로 보냅니다"
    [x > 0] whileTrue: [x: x - 1].
    x
] value
```

# 메서드

메서드는 블록과 같지만 컨텍스트 내에 있지 않고 대신 슬롯의 값으로 저장됩니다. Smalltalk와 달리 메서드는 기본적으로 'self'가 아닌 최종 값을 반환합니다.

```
"여기 할당 가능한 슬롯 'x'와 메서드 'reduceXTo: y'가 있는 객체가 있습니다. 이 객체에 'reduceXTo: 10' 메시지를 보내면 'x' 슬롯에 '10' 객체를 넣고 원래 객체를 반환합니다"
(|
    x <- 50.
    reduceXTo: y = (
        [x > y] whileTrue: [x: x - 1].
        self)
|)
.
```

# 프로토타입

Self에는 클래스가 없습니다. 객체를 얻는 방법은 프로토타입을 찾아 복사하는 것입니다.

```
| d |
d: dictionary copy.
d at: 'hello' Put: 23 + 8.
d at: 'goodbye' Put: 'No!.
"Prints No!"
( d at: 'goodbye' IfAbsent: 'Yes! ) printLine.
"Prints 31"
( d at: 'hello' IfAbsent: -1 ) printLine.
```

# 추가 정보

[Self 핸드북](http://handbook.selflanguage.org)에는 훨씬 더 많은 정보가 있으며, [홈페이지](http://www.selflanguage.org)에서 Self를 다운로드하여 직접 경험하는 것보다 더 좋은 것은 없습니다.
