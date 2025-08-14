# smalltalk.md (번역)

---
name: Smalltalk
filename: smalltalk.st
contributors:
    - ["Jigyasa Grover", "https://jigyasa-grover.github.io"]
    - ["tim Rowledge", "tim@rowledge.org"]
---

- Smalltalk는 '비객체' 타입이 없는 완전한 객체 지향, 동적 타입, 반사적 프로그래밍 언어입니다.
- Smalltalk는 "인간-컴퓨터 공생"으로 대표되는 "새로운 컴퓨팅 세계"를 뒷받침하는 언어로 만들어졌습니다.
- 1970년대 Xerox PARC의 학습 연구 그룹(LRG)에서 Alan Kay, Dan Ingalls, Adele Goldberg, Ted Kaehler, Scott Wallace 등이 교육용, 특히 구성주의 학습을 위해 부분적으로 설계하고 만들었습니다.

## 기본

### 모든 것은 객체입니다
네, 모든 것입니다. 정수는 숫자 클래스 중 하나의 인스턴스입니다. 클래스는 Metaclass 클래스의 인스턴스이며 다른 객체와 마찬가지로 조작할 수 있습니다. 모든 클래스는 단일 클래스 트리의 일부이며, 분리된 클래스 트리는 없습니다. 스택 프레임은 객체이며 조작할 수 있으며, 이것이 디버거가 작동하는 방식입니다. 역참조하여 조작할 수 있는 메모리 위치에 대한 포인터는 없습니다.

### 함수는 호출되지 않고, 메시지가 객체로 전송됩니다
- 작업은 객체에 메시지를 보내는 것으로 수행되며, 객체는 해당 메시지에 응답하는 방법을 결정하고 그 결과로 메서드를 실행하며, 결국 원래 메시지를 보낸 코드로 일부 객체를 반환합니다.
- 시스템은 메시지를 수신하는 객체의 클래스를 알고 해당 클래스의 메서드 목록에서 메시지를 찾습니다. 찾지 못하면 상위 클래스에서 조회를 계속하며, 찾거나 클래스의 루트에 도달하고 여전히 관련 메서드가 없을 때까지 계속됩니다.
- 적절한 메서드를 찾으면 코드가 실행되고, 해당 메서드가 보낸 모든 메서드와 함께 동일한 프로세스가 계속됩니다.
- 적절한 메서드를 찾지 못하면 예외가 발생하며, 일반적으로 사용자에게 메시지를 이해할 수 없다는 것을 알리는 사용자 인터페이스 알림이 표시됩니다. 예외를 잡아 문제를 해결하기 위해 '무시'에서 '이 클래스에 대한 새 패키지를 로드하고 다시 시도'에 이르기까지 무언가를 수행하는 것이 전적으로 가능합니다.
- 메서드(더 엄격하게는 CompiledMethod 클래스의 인스턴스)는 바이트코드로 컴파일된 Smalltalk 코드 덩어리입니다. 실행 메서드는 처음부터 시작하여 반환이 발생하면(다음에 오는 객체를 반환하기 위해 ^를 사용함) 또는 코드 끝에 도달하면 보낸 사람에게 반환되며, 이 경우 코드를 실행하는 현재 객체가 반환됩니다.

### 간단한 구문
Smalltalk는 규칙이 거의 없는 간단한 구문을 가지고 있습니다.
가장 기본적인 작업은 객체에 메시지를 보내는 것입니다.
`anObject aMessage`

세 가지 종류의 메시지가 있습니다.

- 단항 - 인수가 없는 카멜케이스 형식으로 여러 단어가 결합된 단일 기호입니다. 예를 들어 'size', 'reverseBytes', 'convertToLargerFormatPixels'
- 이항 - 대부분의 언어에서 산술 연산에 자주 사용되는 작은 기호 집합으로, 단일 인수가 필요합니다. 예를 들어 '+', '//', '@'. 전통적인 산술 우선순위를 사용하지 않으므로 주의해야 합니다.
- 키워드 - 여러 인수를 전달할 수 있는 일반적인 형식입니다. 단항 형식과 마찬가지로 카멜케이스를 사용하여 단어를 결합하지만 인수는 콜론을 사용하여 어휘적으로 구분하여 메시지 중간에 삽입됩니다. 예를 들어 'setTemperature:', 'at:put:', 'drawFrom:to:lineWidth:fillColor:'

#### 예시
`result := myObject doSomethingWith: thatObject`
myObject에 'doSomethingWith:' 메시지를 보냅니다. 이것은 단일 인수가 있는 메시지이지만 아직 중요하지 않습니다.
'myObject'는 'MyExampleClass' 인스턴스이므로 시스템은 MyExampleClass가 이해하는 메시지 목록을 살펴봅니다.

- beClever
- doWeirdThing:
- doSomethingWith

검색에서 처음에는 일치하는 것처럼 보이지만 - 아니요, 마지막 콜론이 없습니다. 그래서 MyExampleClass의 상위 클래스인 BigExampleClass를 찾습니다. 여기에는 자체적으로 알려진 메시지 목록이 있습니다.

- beClever
- doSomethingWith:
- buildCastleInAir
- annoyUserByDoing:

정확히 일치하는 것을 찾아 코드를 실행하기 시작합니다:

```smalltalk
doSomethingWith: argumentObject
    self size > 4 ifTrue: [^argumentObject sizeRelatingTo: self].
```

여기서 `^`를 제외한 모든 것은 더 많은 메시지를 보내는 것을 포함합니다. 언어 제어 구조라고 생각할 수 있는 `ifTrue:`조차도 Smalltalk 코드일 뿐입니다.

`self`에 `size`를 보내는 것으로 시작합니다. `self`는 현재 코드를 실행하는 객체이므로 이 경우 우리가 시작한 myObject입니다. `size`는 객체의 크기에 대해 알려주는 매우 일반적인 메시지입니다. Smalltalk 도구를 사용하여 매우 간단하게 찾아볼 수 있습니다. 얻은 결과는 정수 4와 함께 `>` 메시지를 받으며, 이것도 객체입니다. 여기에는 기본 타입이 없습니다. `>`는 true 또는 false를 응답하는 비교입니다. 해당 부울(실제로 Smalltalk의 Boolean 객체임)은 `[]` 사이의 코드 블록을 인수로 사용하여 `ifTrue:` 메시지를 받습니다. true 부울은 해당 코드 블록을 실행하고 false는 무시할 것으로 예상됩니다.

블록이 실행되면 인수 객체에 더 많은 메시지를 보내고 `^`를 기록하여 답을 시작 지점으로 반환하고 `result`에 할당됩니다. 블록이 무시되면 코드가 다 떨어져서 `self`가 반환되고 `result`에 할당됩니다.

## Smalltalk 빠른 참조 치트 시트
[Smalltalk 치트 시트](http://www.angelfire.com/tx4/cus/notes/smalltalk.html)에서 가져옴

#### 허용되는 문자:
- a-z
- A-Z
- 0-9
- .+/\*~<>@%|&?
- 공백, 탭, cr, ff, lf

#### 변수:
- 변수 이름은 사용하기 전에 선언해야 하지만 타입이 지정되지 않았습니다.
- 공유 변수(전역, 클래스 변수)는 관례적으로 대문자로 시작합니다(아래에 표시된 예약어 제외).
- 지역 변수(인스턴스 변수, 임시 변수, 메서드 및 블록 인수)는 관례적으로 소문자로 시작합니다.
- 예약어: `nil`, `true`, `false`, `self`, `super`, `thisContext`

#### 변수 범위:
- 전역: 'Smalltalk'라는 Dictionary에 정의되어 있으며 시스템의 모든 객체에서 액세스할 수 있습니다.
- 특수: (예약됨) `Smalltalk`, `super`, `self`, `true`, `false`, `nil`
- 메서드 임시: 메서드에 로컬
- 블록 임시: 블록에 로컬
- 풀: Dictionary 객체의 변수로, 상속으로 직접 관련되지 않은 클래스와 공유될 수 있습니다.
- 메서드 매개변수: 들어오는 매개변수의 이름을 지정하는 자동 메서드 임시 변수입니다. 할당할 수 없습니다.
- 블록 매개변수: 들어오는 매개변수의 이름을 지정하는 자동 블록 임시 변수입니다. 할당할 수 없습니다.
- 클래스: 클래스 및 하위 클래스의 모든 인스턴스와 공유됩니다.
- 클래스 인스턴스: 클래스의 각 인스턴스에 고유합니다. 클래스 변수와 너무 흔하게 혼동됩니다.
- 인스턴스 변수: 클래스의 각 인스턴스에 고유합니다.

`"주석은 따옴표로 묶이며 임의의 길이일 수 있습니다"`

`"마침표(.)는 문장 구분 기호입니다. 메서드의 마지막 줄에는 필요하지 않습니다"`

#### 트랜스크립트:
```smalltalk
Transcript clear.                        "트랜스크립트 창 지우기"
Transcript show: 'Hello World'.          "트랜스크립트 창에 문자열 출력"
Transcript nextPutAll: 'Hello World'.    "트랜스크립트 창에 문자열 출력"
Transcript nextPut: $A.                  "트랜스크립트 창에 문자 출력"
Transcript space.                        "트랜스크립트 창에 공백 문자 출력"
Transcript tab.                          "트랜스크립트 창에 탭 문자 출력"
Transcript cr.                           "캐리지 리턴 / 줄 바꿈"
'Hello' printOn: Transcript.             "창에 인쇄 문자열 추가"
'Hello' storeOn: Transcript.             "창에 저장 문자열 추가"
Transcript endEntry.                     "출력 버퍼 플러시"
```

#### 할당:
```smalltalk
| x y |
x _ 4.                            "할당 (Squeak) <-"
x := 5.                           "할당"
x := y := z := 6.                 "복합 할당"
x := (y := 6) + 1.
x := Object new.                  "클래스의 할당된 인스턴스에 바인딩"
```

#### 상수:
```smalltalk
| b |
b := true.                "true 상수"
b := false.               "false 상수"
x := nil.                 "nil 객체 상수"
x := 1.                   "정수 상수"
x := 3.14.                "부동 소수점 상수"
x := 2e-2.                "분수 상수"
x := 16r0F.               "16진수 상수".
x := -1.                  "음수 상수"
x := 'Hello'.             "문자열 상수"
x := 'I''m here'.         "작은따옴표 이스케이프"
x := $A.                  "문자 상수"
x := $ .                  "문자 상수 (공백)"
x := #aSymbol.            "심볼 상수"
x := #(3 2 1).            "배열 상수"
x := #('abc' 2 $a).       "타입 혼합 허용"
```

#### 불리언:
```smalltalk
| b x y |
x := 1. y := 2.
b := (x = y).                         "같음"
b := (x ~= y).                         "같지 않음"
b := (x == y).                         "동일함"
b := (x ~~ y).                         "동일하지 않음"
b := (x > y).                          "보다 큼"
b := (x < y).                          "보다 작음"
b := (x >= y).                         "크거나 같음"
b := (x <= y).                         "작거나 같음"
b := b not.                            "불리언 부정"
b := (x < 5) & (y > 1).                "불리언 and"
b := (x < 5) | (y > 1).                "불리언 or"
b := (x < 5) and: [y > 1].             "불리언 and (단락 회로)"
b := (x < 5) or: [y > 1].              "불리언 or (단락 회로)"
b := (x < 5) eqv: (y > 1).             "둘 다 참이거나 둘 다 거짓인지 테스트"
b := (x < 5) xor: (y > 1).             "하나가 참이고 다른 하나가 거짓인지 테스트"
b := 5 between: 3 and: 12.             "사이 (포함)"
b := 123 isKindOf: Number.             "객체가 클래스 또는 하위 클래스인지 테스트"
b := 123 isMemberOf: SmallInteger.     "객체가 클래스 타입인지 테스트"
b := 123 respondsTo: #sqrt.             "객체가 메시지에 응답하는지 테스트"
b := x isNil.                          "객체가 nil인지 테스트"
b := x isZero.                         "숫자가 0인지 테스트"
b := x positive.                       "숫자가 양수인지 테스트"
b := x strictlyPositive.               "숫자가 0보다 큰지 테스트"
b := x negative.                       "숫자가 음수인지 테스트"
b := x even.                           "숫자가 짝수인지 테스트"
b := x odd.                            "숫자가 홀수인지 테스트"
b := x isLiteral.                      "리터럴 상수인지 테스트"
b := x isInteger.                      "객체가 정수인지 테스트"
b := x isFloat.                        "객체가 부동 소수점인지 테스트"
b := x isNumber.                       "객체가 숫자인지 테스트"
b := $A isUppercase.                   "대문자인지 테스트"
b := $A isLowercase.                   "소문자인지 테스트"
```

#### 산술 표현식:
```smalltalk
| x |
x := 6 + 3.                             "덧셈"
x := 6 - 3.                             "뺄셈"
x := 6 * 3.                             "곱셈"
x := 1 + 2 * 3.                         "평가는 항상 왼쪽에서 오른쪽으로 (1 + 2) * 3"
x := 5 / 3.                             "분수 결과가 있는 나눗셈"
x := 5.0 / 3.0.                         "부동 소수점 결과가 있는 나눗셈"
x := 5.0 // 3.0.                        "정수 나눗셈"
x := 5.0 \\ 3.0.                        "정수 나머지"
x := -5.                                "단항 빼기"
x := 5 sign.                            "숫자 부호 (1, -1 또는 0)"
x := 5 negated.                         "수신자 부정"
x := 1.2 integerPart.                   "숫자의 정수 부분 (1.0)"
x := 1.2 fractionPart.                  "숫자의 분수 부분 (0.2)"
x := 5 reciprocal.                      "역수 함수"
x := 6 * 3.1.                           "부동 소수점으로 자동 변환"
x := 5 squared.                         "제곱 함수"
x := 25 sqrt.                           "제곱근"
x := 5 raisedTo: 2.                     "거듭제곱 함수"
x := 5 raisedToInteger: 2.              "정수 거듭제곱 함수"
x := 5 exp.                             "지수"
x := -5 abs.                            "절대값"
x := 3.99 rounded.                      "반올림"
x := 3.99 truncated.                    "버림"
x := 3.99 roundTo: 1.                   "지정된 소수 자릿수로 반올림"
x := 3.99 truncateTo: 1.                "지정된 소수 자릿수로 버림"
x := 3.99 floor.                        "버림"
x := 3.99 ceiling.                      "올림"
x := 5 factorial.                       "팩토리얼"
x := -5 quo: 3.                         "0을 향해 반올림하는 정수 나눗셈"
x := -5 rem: 3.                         "0을 향해 반올림하는 정수 나머지"
x := 28 gcd: 12.                        "최대공약수"
x := 28 lcm: 12.                        "최소공배수"
x := 100 ln.                            "자연 로그"
x := 100 log.                           "밑이 10인 로그"
x := 100 log: 10.                       "로그의 바닥"
x := 180 degreesToRadians.              "도를 라디안으로 변환"
x := 3.14 radiansToDegrees.             "라디안을 도로 변환"
x := 0.7 sin.                           "사인"
x := 0.7 cos.                           "코사인"
x := 0.7 tan.                           "탄젠트"
x := 0.7 arcSin.                        "아크사인"
x := 0.7 arcCos.                        "아크코사인"
x := 0.7 arcTan.                        "아크탄젠트"
x := 10 max: 20.                        "두 숫자 중 최대값 가져오기"
x := 10 min: 20.                        "두 숫자 중 최소값 가져오기"
x := Float pi.                          "파이"
x := Float e.                           "지수 상수"
x := Float infinity.                    "무한대"
x := Float nan.                         "숫자 아님"
x := Random new next; yourself. x next. "난수 스트림 (0.0에서 1.0)"
x := 100 atRandom.                      "빠른 난수"
```

#### 비트 조작:
```smalltalk
| b x |
x := 16rFF bitAnd: 16r0F.           "비트 and"
x := 16rF0 bitOr: 16r0F.            "비트 or"
x := 16rFF bitXor: 16r0F.           "비트 xor"
x := 16rFF bitInvert.               "비트 반전"
x := 16r0F bitShift: 4.             "왼쪽 시프트"
x := 16rF0 bitShift: -4.            "오른쪽 시프트"
"x := 16r80 bitAt: 7."              "위치의 비트 (0|1) [!Squeak]"
x := 16r80 highbit.                 "가장 높은 비트의 위치"
b := 16rFF allMask: 16r0F.          "마스크에 설정된 모든 비트가 수신자에 설정되었는지 테스트"
b := 16rFF anyMask: 16r0F.          "마스크에 설정된 비트 중 하나라도 수신자에 설정되었는지 테스트"
b := 16rFF noMask: 16r0F.           "마스크에 설정된 모든 비트가 수신자에서 지워졌는지 테스트"
```

#### 변환:
```smalltalk
| x |
x := 3.99 asInteger.               "숫자를 정수로 변환 (Squeak에서는 버림)"
x := 3.99 asFraction.              "숫자를 분수로 변환"
x := 3 asFloat.                    "숫자를 부동 소수점으로 변환"
x := 65 asCharacter.               "정수를 문자로 변환"
x := $A asciiValue.                "문자를 정수로 변환"
x := 3.99 printString.             "printOn:을 통해 객체를 문자열로 변환"
x := 3.99 storeString.             "storeOn:을 통해 객체를 문자열로 변환"
x := 15 radix: 16.                 "지정된 기수로 문자열로 변환"
x := 15 printStringBase: 16.
x := 15 storeStringBase: 16.
```

#### 블록:
- 블록은 객체이며 변수에 할당될 수 있습니다.
- 값은 명시적 반환이 없는 한 마지막으로 평가된 표현식입니다.
- 블록은 중첩될 수 있습니다.
- 사양 [ 인수 | | 지역 변수 | 표현식 ]
- Squeak는 현재 블록의 지역 변수를 지원하지 않습니다.
- 최대 세 개의 인수가 허용됩니다.
- `^`표현식은 블록 및 메서드를 종료합니다(모든 중첩 블록 종료).
- 장기 저장을 위한 블록에는 `^`가 포함되어서는 안 됩니다.

```smalltalk
| x y z |
x := [ y := 1. z := 2. ]. x value.                          "간단한 블록 사용"
x := [ :argOne :argTwo |   argOne, ' and ' , argTwo.].      "인수 전달로 블록 설정"
Transcript show: (x value: 'First' value: 'Second'); cr.    "인수 전달로 블록 사용"

"x := [ | z | z := 1.]. *** Squeak 블록에서는 지역 변수를 사용할 수 없습니다"
```

#### 메서드 호출:
- 단항 메서드는 인수가 없는 메시지입니다.
- 이항 메서드
- 키워드 메서드는 콜론을 포함하는 선택자가 있는 메시지입니다. 표준 범주/프로토콜:
- initialize-release    (새 인스턴스에 대해 호출되는 메서드)
- accessing             (get/set 메서드)
- testing               (불리언 테스트 - is)
- comparing             (매개변수가 있는 불리언 테스트)
- displaying            (gui 관련 메서드)
- printing              (인쇄용 메서드)
- updating              (변경 알림 수신)
- private               (클래스에 비공개인 메서드)
- instance-creation     (인스턴스 생성을 위한 클래스 메서드)

```smalltalk
| x |
x := 2 sqrt.                                  "단항 메시지"
x := 2 raisedTo: 10.                          "키워드 메시지"
x := 194 * 9.                                 "이항 메시지"
Transcript show: (194 * 9) printString; cr.   "조합 (연쇄)"
x := 2 perform: #sqrt.                        "간접 메서드 호출"
Transcript                                    "연쇄 - 수신자에게 여러 메시지 보내기"
   show: 'hello ';
   show: 'world';
   cr.
x := 3 + 2; * 100.                            "결과=300. 동일한 수신자(3)에게 메시지 보내기"
```

#### 조건문:
```smalltalk
| x |
x > 10 ifTrue: [Transcript show: 'ifTrue'; cr].     "if then"
x > 10 ifFalse: [Transcript show: 'ifFalse'; cr].   "if else"

"if then else"
x > 10
   ifTrue: [Transcript show: 'ifTrue'; cr]
   ifFalse: [Transcript show: 'ifFalse'; cr].

"if else then"
x > 10
   ifFalse: [Transcript show: 'ifFalse'; cr]
   ifTrue: [Transcript show: 'ifTrue'; cr].
Transcript
   show:
      (x > 10
         ifTrue: ['ifTrue']
         ifFalse: ['ifFalse']);
   cr.

"중첩된 if then else"
Transcript
   show:
      (x > 10
         ifTrue: [x > 5
            ifTrue: ['A']
            ifFalse: ['B']]
         ifFalse: ['C']);
   cr.

"switch 기능"
switch := Dictionary new.
switch at: $A put: [Transcript show: 'Case A'; cr].
switch at: $B put: [Transcript show: 'Case B'; cr].
switch at: $C put: [Transcript show: 'Case C'; cr].
result := (switch at: $B) value.
```

#### 반복문:
```smalltalk
| x y |
x := 4. y := 1.
[x > 0] whileTrue: [x := x - 1. y := y * 2].     "while true 루프"
[x >= 4] whileFalse: [x := x + 1. y := y * 2].   "while false 루프"
x timesRepeat: [y := y * 2].                     "times repeat 루프 (i := 1 to x)"
1 to: x do: [:a | y := y * 2].                   "for 루프"
1 to: x by: 2 do: [:a | y := y / 2].             "지정된 증분이 있는 for 루프"
#(5 4 3) do: [:a | x := x + a].                  "배열 요소를 반복"
```

#### 문자:
```smalltalk
| x y |
x := $A.                         "문자 할당"
y := x isLowercase.              "소문자인지 테스트"
y := x isUppercase.              "대문자인지 테스트"
y := x isLetter.                 "문자인지 테스트"
y := x isDigit.                  "숫자인지 테스트"
y := x isAlphaNumeric.           "영숫자인지 테스트"
y := x isSeparator.              "구분 문자인지 테스트"
y := x isVowel.                  "모음인지 테스트"
y := x digitValue.               "숫자 값으로 변환"
y := x asLowercase.              "소문자로 변환"
y := x asUppercase.              "대문자로 변환"
y := x asciiValue.               "숫자 ascii 값으로 변환"
y := x asString.                 "문자열로 변환"
b := $A <= $B.                   "비교"
y := $A max: $B.
```

#### 심볼:
```smalltalk
| b x y |
x := #Hello.                                      "심볼 할당"
y := #Symbol, #Concatenation.                     "심볼 연결 (결과는 문자열)"
b := x isEmpty.                                   "심볼이 비어 있는지 테스트"
y := x size.                                      "문자열 크기"
y := x at: 2.                                     "위치의 문자"
y := x copyFrom: 2 to: 4.                         "부분 문자열"
y := x indexOf: $e ifAbsent: [0].                 "문자열 내 문자의 첫 번째 위치"
x do: [:a | Transcript show: a printString; cr].  "문자열 반복"
b := x conform: [:a | (a >= $a) & (a <= $z)].     "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > $a].                     "조건을 충족하는 모든 요소 반환"
y := x asString.                                  "심볼을 문자열로 변환"
y := x asText.                                    "심볼을 텍스트로 변환"
y := x asArray.                                   "심볼을 배열로 변환"
y := x asOrderedCollection.                       "심볼을 순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                        "심볼을 정렬된 컬렉션으로 변환"
y := x asBag.                                     "심볼을 백 컬렉션으로 변환"
y := x asSet.                                     "심볼을 세트 컬렉션으로 변환"
```

#### 문자열:
```smalltalk
| b x y |
x := 'This is a string'.                           "문자열 할당"
x := 'String', 'Concatenation'.                    "문자열 연결"
b := x isEmpty.                                    "문자열이 비어 있는지 테스트"
y := x size.                                       "문자열 크기"
y := x at: 2.                                      "위치의 문자"
y := x copyFrom: 2 to: 4.                          "부분 문자열"
y := x indexOf: $a ifAbsent: [0].                  "문자열 내 문자의 첫 번째 위치"
x := String new: 4.                                "문자열 객체 할당"
x                                                  "문자열 요소 설정"
   at: 1 put: $a;
   at: 2 put: $b;
   at: 3 put: $c;
   at: 4 put: $e.
x := String with: $a with: $b with: $c with: $d.  "한 번에 최대 4개 요소 설정"
x do: [:a | Transcript show: a printString; cr].  "문자열 반복"
b := x conform: [:a | (a >= $a) & (a <= $z)].     "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > $a].                     "조건을 충족하는 모든 요소 반환"
y := x asSymbol.                                  "문자열을 심볼로 변환"
y := x asArray.                                   "문자열을 배열로 변환"
x := 'ABCD' asByteArray.                          "문자열을 바이트 배열로 변환"
y := x asOrderedCollection.                       "문자열을 순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                        "문자열을 정렬된 컬렉션으로 변환"
y := x asBag.                                     "문자열을 백 컬렉션으로 변환"
y := x asSet.                                     "문자열을 세트 컬렉션으로 변환"
y := x shuffled.                                  "문자열 무작위로 섞기"
```

#### 배열:
고정 길이 컬렉션
- ByteArray:     바이트 요소로 제한된 배열 (0-255)
- WordArray:     워드 요소로 제한된 배열 (0-2^32)

```smalltalk
| b x y z sum max |
x := #(4 3 2 1).                                 "상수 배열"
z := #(1 2 3 'hi').                              "혼합 타입 배열"
x := Array with: 5 with: 4 with: 3 with: 2.      "최대 4개 요소로 배열 생성"
x := Array new: 4.                               "지정된 크기로 배열 할당"
x                                                "배열 요소 설정"
   at: 1 put: 5;
   at: 2 put: 4;
   at: 3 put: 3;
   at: 4 put: 2.
b := x isEmpty.                                  "배열이 비어 있는지 테스트"
y := x size.                                     "배열 크기"
y := x at: 4.                                    "인덱스에서 배열 요소 가져오기"
b := x includes: 3.                              "배열에 요소가 있는지 테스트"
y := x copyFrom: 2 to: 4.                        "부분 배열"
y := x indexOf: 3 ifAbsent: [0].                 "배열 내 요소의 첫 번째 위치"
y := x occurrencesOf: 3.                         "컬렉션에서 객체 수"
x do: [:a | Transcript show: a printString; cr]. "배열 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].      "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                     "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                     "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                    "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].          "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.      "배열 요소 합계"
sum := 0. 1 to: (x size)
            do: [:a | sum := sum + (x at: a)].   "배열 요소 합계"
sum := x inject: 0 into: [:a :c | a + c].        "배열 요소 합계"
max := x inject: 0 into: [:a :c | (a > c)        "배열에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x shuffled.                                 "컬렉션 무작위로 섞기"
y := x asArray.                                  "배열로 변환"
"y := x asByteArray."                            "참고: 이 지침은 Squeak에서 사용할 수 없습니다"
y := x asWordArray.                              "워드 배열로 변환"
y := x asOrderedCollection.                      "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                       "정렬된 컬렉션으로 변환"
y := x asBag.                                    "백 컬렉션으로 변환"
y := x asSet.                                    "세트 컬렉션으로 변환"
```

#### OrderedCollection:
확장 가능한 배열처럼 작동합니다

```smalltalk
| b x y sum max |
x := OrderedCollection
     with: 4 with: 3 with: 2 with: 1.            "최대 4개 요소로 컬렉션 생성"
x := OrderedCollection new.                      "컬렉션 할당"
x add: 3; add: 2; add: 1; add: 4; yourself.      "컬렉션에 요소 추가"
y := x addFirst: 5.                              "컬렉션 시작 부분에 요소 추가"
y := x removeFirst.                              "컬렉션에서 첫 번째 요소 제거"
y := x addLast: 6.                               "컬렉션 끝에 요소 추가"
y := x removeLast.                               "컬렉션에서 마지막 요소 제거"
y := x addAll: #(7 8 9).                         "컬렉션에 여러 요소 추가"
y := x removeAll: #(7 8 9).                      "컬렉션에서 여러 요소 제거"
x at: 2 put: 3.                                  "인덱스에 요소 설정"
y := x remove: 5 ifAbsent: [].                   "컬렉션에서 요소 제거"
b := x isEmpty.                                  "비어 있는지 테스트"
y := x size.                                     "요소 수"
y := x at: 2.                                    "인덱스에서 요소 검색"
y := x first.                                    "컬렉션에서 첫 번째 요소 검색"
y := x last.                                     "컬렉션에서 마지막 요소 검색"
b := x includes: 5.                              "컬렉션에 요소가 있는지 테스트"
y := x copyFrom: 2 to: 3.                        "부분 컬렉션"
y := x indexOf: 3 ifAbsent: [0].                 "컬렉션 내 요소의 첫 번째 위치"
y := x occurrencesOf: 3.                         "컬렉션에서 객체 수"
x do: [:a | Transcript show: a printString; cr]. "컬렉션 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].      "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                     "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                     "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                    "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].          "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.      "요소 합계"
sum := 0. 1 to: (x size)
            do: [:a | sum := sum + (x at: a)].   "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].        "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)        "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x shuffled.                                 "컬렉션 무작위로 섞기"
y := x asArray.                                  "배열로 변환"
y := x asOrderedCollection.                      "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                       "정렬된 컬렉션으로 변환"
y := x asBag.                                    "백 컬렉션으로 변환"
y := x asSet.                                    "세트 컬렉션으로 변환"
```

#### SortedCollection:
요소 순서가 정렬 기준에 따라 결정된다는 점을 제외하고 OrderedCollection과 같습니다.

```smalltalk
| b x y sum max |
x := SortedCollection
     with: 4 with: 3 with: 2 with: 1.              "최대 4개 요소로 컬렉션 생성"
x := SortedCollection new.                         "컬렉션 할당"
x := SortedCollection sortBlock: [:a :c | a > c].  "정렬 기준 설정"
x add: 3; add: 2; add: 1; add: 4; yourself.        "컬렉션에 요소 추가"
y := x addFirst: 5.                                "컬렉션 시작 부분에 요소 추가"
y := x removeFirst.                                "컬렉션에서 첫 번째 요소 제거"
y := x addLast: 6.                                 "컬렉션 끝에 요소 추가"
y := x removeLast.                                 "컬렉션에서 마지막 요소 제거"
y := x addAll: #(7 8 9).                           "컬렉션에 여러 요소 추가"
y := x removeAll: #(7 8 9).                        "컬렉션에서 여러 요소 제거"
y := x remove: 5 ifAbsent: [].                     "컬렉션에서 요소 제거"
b := x isEmpty.                                    "비어 있는지 테스트"
y := x size.                                       "요소 수"
y := x at: 2.                                      "인덱스에서 요소 검색"
y := x first.                                      "컬렉션에서 첫 번째 요소 검색"
y := x last.                                       "컬렉션에서 마지막 요소 검색"
b := x includes: 4.                                "컬렉션에 요소가 있는지 테스트"
y := x copyFrom: 2 to: 3.                          "부분 컬렉션"
y := x indexOf: 3 ifAbsent: [0].                   "컬렉션 내 요소의 첫 번째 위치"
y := x occurrencesOf: 3.                           "컬렉션에서 객체 수"
x do: [:a | Transcript show: a printString; cr].   "컬렉션 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].        "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                       "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                       "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                      "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].            "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.        "요소 합계"
sum := 0. 1 to: (x size)
            do: [:a | sum := sum + (x at: a)].     "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].          "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)          "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x asArray.                                     "배열로 변환"
y := x asOrderedCollection.                         "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                          "정렬된 컬렉션으로 변환"
y := x asBag.                                       "백 컬렉션으로 변환"
y := x asSet.                                       "세트 컬렉션으로 변환"
```

#### Bag:
요소가 특정 순서가 아니라는 점을 제외하고 OrderedCollection과 같습니다.

```smalltalk
| b x y sum max |
x := Bag with: 4 with: 3 with: 2 with: 1.        "최대 4개 요소로 컬렉션 생성"
x := Bag new.                                    "컬렉션 할당"
x add: 4; add: 3; add: 1; add: 2; yourself.      "컬렉션에 요소 추가"
x add: 3 withOccurrences: 2.                     "컬렉션에 여러 복사본 추가"
y := x addAll: #(7 8 9).                         "컬렉션에 여러 요소 추가"
y := x removeAll: #(7 8 9).                      "컬렉션에서 여러 요소 제거"
y := x remove: 4 ifAbsent: [].                   "컬렉션에서 요소 제거"
b := x isEmpty.                                  "비어 있는지 테스트"
y := x size.                                     "요소 수"
b := x includes: 3.                              "컬렉션에 요소가 있는지 테스트"
y := x occurrencesOf: 3.                         "컬렉션에서 객체 수"
x do: [:a | Transcript show: a printString; cr]. "컬렉션 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].      "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                     "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                     "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                    "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].          "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.      "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].        "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)        "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x asOrderedCollection.                       "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                        "정렬된 컬렉션으로 변환"
y := x asBag.                                     "백 컬렉션으로 변환"
y := x asSet.                                     "세트 컬렉션으로 변환"
```

#### Set:
중복이 허용되지 않는다는 점을 제외하고 Bag과 같습니다.

#### IdentitySet:
동일성 테스트를 사용합니다(== 대신 =)

```smalltalk
| b x y sum max |
x := Set with: 4 with: 3 with: 2 with: 1.        "최대 4개 요소로 컬렉션 생성"
x := Set new.                                    "컬렉션 할당"
x add: 4; add: 3; add: 1; add: 2; yourself.      "컬렉션에 요소 추가"
y := x addAll: #(7 8 9).                         "컬렉션에 여러 요소 추가"
y := x removeAll: #(7 8 9).                      "컬렉션에서 여러 요소 제거"
y := x remove: 4 ifAbsent: [].                   "컬렉션에서 요소 제거"
b := x isEmpty.                                  "비어 있는지 테스트"
y := x size.                                     "요소 수"
x includes: 4.                                   "컬렉션에 요소가 있는지 테스트"
x do: [:a | Transcript show: a printString; cr]. "컬렉션 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].      "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                     "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                     "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                    "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].          "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.      "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].        "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)        "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x asArray.                                  "배열로 변환"
y := x asOrderedCollection.                      "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                       "정렬된 컬렉션으로 변환"
y := x asBag.                                    "백 컬렉션으로 변환"
y := x asSet.                                    "세트 컬렉션으로 변환"
```

#### Interval:
```smalltalk
| b x y sum max |
x := Interval from: 5 to: 10.                     "간격 객체 생성"
x := 5 to: 10.
x := Interval from: 5 to: 10 by: 2.               "지정된 증분이 있는 간격 객체 생성"
x := 5 to: 10 by: 2.
b := x isEmpty.                                   "비어 있는지 테스트"
y := x size.                                      "요소 수"
x includes: 9.                                    "컬렉션에 요소가 있는지 테스트"
x do: [:k | Transcript show: k printString; cr].  "간격 반복"
b := x conform: [:a | (a >= 1) & (a <= 4)].       "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 7].                      "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                      "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                     "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].           "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.       "요소 합계"
sum := 0. 1 to: (x size)
            do: [:a | sum := sum + (x at: a)].    "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].         "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)         "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x asArray.                                   "배열로 변환"
y := x asOrderedCollection.                       "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                        "정렬된 컬렉션으로 변환"
y := x asBag.                                     "백 컬렉션으로 변환"
y := x asSet.                                     "세트 컬렉션으로 변환"
```

#### 연관:
```smalltalk
| x y |
x := #myVar->'hello'.
y := x key.
y := x value.
```

#### 딕셔너리:
#### IdentityDictionary:
동일성 테스트를 사용합니다(== 대신 =)

```smalltalk
| b x y |
x := Dictionary new.                   "컬렉션 할당"
x add: #a->4;
  add: #b->3;
  add: #c->1;
  add: #d->2; yourself.                "컬렉션에 요소 추가"
x at: #e put: 3.                       "인덱스에 요소 설정"
b := x isEmpty.                        "비어 있는지 테스트"
y := x size.                           "요소 수"
y := x at: #a ifAbsent: [].            "인덱스에서 요소 검색"
y := x keyAtValue: 3 ifAbsent: [].     "오류 블록이 있는 주어진 값에 대한 키 검색"
y := x removeKey: #e ifAbsent: [].     "컬렉션에서 요소 제거"
b := x includes: 3.                    "값 컬렉션에 요소가 있는지 테스트"
b := x includesKey: #a.                "키 컬렉션에 요소가 있는지 테스트"
y := x occurrencesOf: 3.               "컬렉션에서 객체 수"
y := x keys.                           "키 집합"
y := x values.                         "값 백"
x do: [:a | Transcript show: a printString; cr].            "값 컬렉션 반복"
x keysDo: [:a | Transcript show: a printString; cr].        "키 컬렉션 반복"
x associationsDo: [:a | Transcript show: a printString; cr]."연관 반복"
x keysAndValuesDo: [:aKey :aValue | Transcript              "키와 값 반복"
   show: aKey printString; space;
   show: aValue printString; cr].
b := x conform: [:a | (a >= 1) & (a <= 4)].      "모든 요소가 조건을 충족하는지 테스트"
y := x select: [:a | a > 2].                     "테스트를 통과하는 요소의 컬렉션 반환"
y := x reject: [:a | a < 2].                     "테스트에 실패하는 요소의 컬렉션 반환"
y := x collect: [:a | a + a].                    "새 컬렉션을 위해 각 요소 변환"
y := x detect: [:a | a > 3] ifNone: [].          "테스트를 통과하는 첫 번째 요소의 위치 찾기"
sum := 0. x do: [:a | sum := sum + a]. sum.      "요소 합계"
sum := x inject: 0 into: [:a :c | a + c].        "요소 합계"
max := x inject: 0 into: [:a :c | (a > c)        "컬렉션에서 최대 요소 찾기"
   ifTrue: [a]
   ifFalse: [c]].
y := x asArray.                                   "배열로 변환"
y := x asOrderedCollection.                       "순서 있는 컬렉션으로 변환"
y := x asSortedCollection.                        "정렬된 컬렉션으로 변환"
y := x asBag.                                     "백 컬렉션으로 변환"
y := x asSet.                                     "세트 컬렉션으로 변환"

Smalltalk at: #CMRGlobal put: 'CMR entry'.        "Smalltalk 딕셔너리에 전역 넣기"
x := Smalltalk at: #CMRGlobal.                    "Smalltalk 딕셔너리에서 전역 읽기"
Transcript show: (CMRGlobal printString).         "항목은 이름으로 직접 액세스 가능"
Smalltalk keys do: [ :k |                         "모든 클래스 출력"
   ((Smalltalk at: k) isKindOf: Class)
      ifFalse: [Transcript show: k printString; cr]].
Smalltalk at: #CMRDictionary put: (Dictionary new). "사용자 정의 딕셔너리 설정"
CMRDictionary at: #MyVar1 put: 'hello1'.            "딕셔너리에 항목 넣기"
CMRDictionary add: #MyVar2->'hello2'.               "키->값 조합을 사용하여 딕셔너리에 항목 추가"
CMRDictionary size.                                 "딕셔너리 크기"
CMRDictionary keys do: [ :k |                       "딕셔너리에서 키 출력"
   Transcript show: k printString; cr].
CMRDictionary values do: [ :k |                     "딕셔너리에서 값 출력"
   Transcript show: k printString; cr].
CMRDictionary keysAndValuesDo: [:aKey :aValue |     "키와 값 출력"
   Transcript
      show: aKey printString;
      space;
      show: aValue printString;
      cr].
CMRDictionary associationsDo: [:aKeyValue |           "키 값 인쇄를 위한 또 다른 반복자"
   Transcript show: aKeyValue printString; cr].
Smalltalk removeKey: #CMRGlobal ifAbsent: [].         "Smalltalk 딕셔너리에서 항목 제거"
Smalltalk removeKey: #CMRDictionary ifAbsent: [].     "Smalltalk 딕셔너리에서 사용자 딕셔너리 제거"
```

#### 내부 스트림:
```smalltalk
| b x ios |
ios := ReadStream on: 'Hello read stream'.
ios := ReadStream on: 'Hello read stream' from: 1 to: 5.
[(x := ios nextLine) notNil] whileTrue: [Transcript show: x; cr].
ios position: 3.
ios position.
x := ios next.
x := ios peek.
x := ios contents.
b := ios atEnd.

ios := ReadWriteStream on: 'Hello read stream'.
ios := ReadWriteStream on: 'Hello read stream' from: 1 to: 5.
ios := ReadWriteStream with: 'Hello read stream'.
ios := ReadWriteStream with: 'Hello read stream' from: 1 to: 10.
ios position: 0.
[(x := ios nextLine) notNil] whileTrue: [Transcript show: x; cr].
ios position: 6.
ios position.
ios nextPutAll: 'Chris'.
x := ios next.
x := ios peek.
x := ios contents.
b := ios atEnd.
```

#### 파일 스트림:
```smalltalk
| b x ios |
ios := FileStream newFileNamed: 'ios.txt'.
ios nextPut: $H; cr.
ios nextPutAll: 'Hello File'; cr.
'Hello File' printOn: ios.
'Hello File' storeOn: ios.
ios close.

ios := FileStream oldFileNamed: 'ios.txt'.
[(x := ios nextLine) notNil] whileTrue: [Transcript show: x; cr].
ios position: 3.
x := ios position.
x := ios next.
x := ios peek.
b := ios atEnd.
ios close.
```

#### 날짜:
```smalltalk
| x y |
x := Date today.                                "오늘 날짜 생성"
x := Date dateAndTimeNow.                       "현재 시간/날짜로 날짜 생성"
x := Date readFromString: '01/02/1999'.         "형식화된 문자열에서 날짜 생성"
x := Date newDay: 12 month: #July year: 1999    "부분에서 날짜 생성"
x := Date fromDays: 36000.                      "1901년 1월 1일 이후 경과 일수에서 날짜 생성"
y := Date dayOfWeek: #Monday.                   "요일을 정수로 (1-7)"
y := Date indexOfMonth: #January.               "연중 월을 정수로 (1-12)"
y := Date daysInMonth: 2 forYear: 1996.         "월의 일을 정수로 (1-31)"
y := Date daysInYear: 1996.                     "연중 일수 (365|366)"
y := Date nameOfDay: 1                          "요일 이름 (#Monday,...)"
y := Date nameOfMonth: 1.                       "월 이름 (#January,...)"
y := Date leapYear: 1996.                       "윤년이면 1; 아니면 0"
y := x weekday.                                 "요일 (#Monday,...)"
y := x previous: #Monday.                       "이전 요일의 날짜"
y := x dayOfMonth.                              "월의 일 (1-31)"
y := x day.                                     "연중 일 (1-366)"
y := x firstDayOfMonth.                         "월의 첫째 날에 대한 연중 일"
y := x monthName.                               "연중 월 (#January,...)"
y := x monthIndex.                              "연중 월 (1-12)"
y := x daysInMonth.                             "월의 일수 (1-31)"
y := x year.                                    "연도 (19xx)"
y := x daysInYear.                              "연중 일수 (365|366)"
y := x daysLeftInYear.                          "연중 남은 일수 (364|365)"
y := x asSeconds.                               "1901년 1월 1일 이후 경과 초"
y := x addDays: 10.                             "날짜 객체에 일 추가"
y := x subtractDays: 10.                        "날짜 객체에서 일 빼기"
y := x subtractDate: (Date today).              "날짜 빼기 (결과는 일 단위)"
y := x printFormat: #(2 1 3 $/ 1 1).            "형식화된 날짜 인쇄"
b := (x <= Date today).                         "비교"
```

#### 시간:
```smalltalk
| x y |
x := Time now.                                      "현재 시간으로 시간 생성"
x := Time dateAndTimeNow.                           "현재 시간/날짜로 시간 생성"
x := Time readFromString: '3:47:26 pm'.             "형식화된 문자열에서 시간 생성"
x := Time fromSeconds: (60 * 60 * 4).               "자정 이후 경과 시간으로 시간 생성"
y := Time millisecondClockValue.                    "자정 이후 밀리초"
y := Time totalSeconds.                             "1901년 1월 1일 이후 총 초"
y := x seconds.                                     "분을 지난 초 (0-59)"
y := x minutes.                                     "시간을 지난 분 (0-59)"
y := x hours.                                       "자정을 지난 시간 (0-23)"
y := x addTime: (Time now).                         "시간 객체에 시간 추가"
y := x subtractTime: (Time now).                    "시간 객체에서 시간 빼기"
y := x asSeconds.                                   "시간을 초로 변환"
x := Time millisecondsToRun: [                      "타이밍 기능"
   1 to: 1000 do: [:index | y := 3.14 * index]].
b := (x <= Time now).                               "비교"
```

#### 점:
```smalltalk
| x y |
x := 200@100.                            "새 점 얻기"
y := x x.                                "x 좌표"
y := x y.                                "y 좌표"
x := 200@100 negated.                    "x와 y 부정"
x := (-200@-100) abs.                    "x와 y의 절대값"
x := (200.5@100.5) rounded.              "x와 y 반올림"
x := (200.5@100.5) truncated.            "x와 y 버림"
x := 200@100 + 100.                      "x와 y에 스케일 추가"
x := 200@100 - 100.                      "x와 y에서 스케일 빼기"
x := 200@100 * 2.                        "x와 y에 스케일 곱하기"
x := 200@100 / 2.                        "x와 y를 스케일로 나누기"
x := 200@100 // 2.                       "x와 y를 스케일로 나누기"
x := 200@100 \\ 3.                       "x와 y를 스케일로 나눈 나머지"
x := 200@100 + 50@25.                    "점 더하기"
x := 200@100 - 50@25.                    "점 빼기"
x := 200@100 * 3@4.                      "점 곱하기"
x := 200@100 // 3@4.                     "점 나누기"
x := 200@100 max: 50@200.                "x와 y의 최대값"
x := 200@100 min: 50@200.                "x와 y의 최소값"
x := 20@5 dotProduct: 10@2.              "곱의 합 (x1*x2 + y1*y2)"
```

#### 사각형:
```smalltalk
Rectangle fromUser.
```

#### 펜:
```smalltalk
| myPen |
Display restoreAfter: [
   Display fillWhite.

myPen := Pen new.                            "그래픽 펜 얻기"
myPen squareNib: 1.
myPen color: (Color blue).                   "펜 색상 설정"
myPen home.                                  "디스플레이 중앙에 펜 위치"
myPen up.                                    "펜촉을 그릴 수 없게 만듦"
myPen down.                                  "펜촉을 그릴 수 있게 함"
myPen north.                                 "방향을 위쪽으로 가리킴"
myPen turn: -180.                            "방향에 지정된 각도 추가"
myPen direction.                             "펜의 현재 각도 가져오기"
myPen go: 50.                                "지정된 픽셀 수만큼 펜 이동"
myPen location.                              "펜 위치 가져오기"
myPen goto: 200@200.                         "지정된 지점으로 이동"
myPen place: 250@250.                        "그리지 않고 지정된 지점으로 이동"
myPen print: 'Hello World'
      withFont: (TextStyle default fontAt: 1).
Display extent.                              "디스플레이 너비@높이 가져오기"
Display width.                               "디스플레이 너비 가져오기"
Display height.                              "디스플레이 높이 가져오기"

].
```

#### 동적 메시지 호출/컴파일:
```smalltalk
| receiver message result argument keyword1 keyword2 argument1 argument2 |

"단항 메시지"
receiver := 5.
message := 'factorial' asSymbol.
result := receiver perform: message.
result := Compiler evaluate: ((receiver storeString), ' ', message).
result := (Message new setSelector: message arguments: #()) sentTo: receiver.

"이항 메시지"
receiver := 1.
message := '+' asSymbol.
argument := 2.
result := receiver perform: message withArguments: (Array with: argument).
result := Compiler evaluate: ((receiver storeString), ' ', message, ' ', (argument storeString)).
result := (Message new setSelector: message arguments: (Array with: argument)) sentTo: receiver.

"키워드 메시지"
receiver := 12.
keyword1 := 'between:' asSymbol.
keyword2 := 'and:' asSymbol.
argument1 := 10.
argument2 := 20.

result := receiver
   perform: (keyword1, keyword2) asSymbol
   withArguments: (Array with: argument1 with: argument2).

result := Compiler evaluate:
   ((receiver storeString), ' ', keyword1, (argument1 storeString) , ' ', keyword2, (argument2 storeString)).

result := (Message
   new
      setSelector: (keyword1, keyword2) asSymbol
      arguments: (Array with: argument1 with: argument2))
   sentTo: receiver.
```

#### 클래스/메타 클래스:
```smalltalk
| b x |
x := String name.                     "클래스 이름"
x := String category.                 "조직 범주"
x := String comment.                  "클래스 주석"
x := String kindOfSubclass.           "하위 클래스 타입 - subclass: variableSubclass 등"
x := String definition.               "클래스 정의"
x := String instVarNames.             "직접 인스턴스 변수 이름"
x := String allInstVarNames.          "누적 인스턴스 변수 이름"
x := String classVarNames.            "직접 클래스 변수 이름"
x := String allClassVarNames.         "누적 클래스 변수 이름"
x := String sharedPools.              "공유 풀로 사용되는 직접 딕셔너리"
x := String allSharedPools.           "공유 풀로 사용되는 누적 딕셔너리"
x := String selectors.                "클래스에 대한 메시지 선택자"
x := String sourceCodeAt: #size.      "지정된 메서드의 소스 코드"
x := String allInstances.             "클래스의 모든 인스턴스 컬렉션"
x := String superclass.               "직접 상위 클래스"
x := String allSuperclasses.          "누적 상위 클래스"
x := String withAllSuperclasses.      "수신자 클래스 및 누적 상위 클래스"
x := String subclasses.               "직접 하위 클래스"
x := String allSubclasses.            "누적 하위 클래스"
x := String withAllSubclasses.        "수신자 클래스 및 누적 하위 클래스"
b := String instSize.                 "명명된 인스턴스 변수 수"
b := String isFixed.                  "인덱싱된 인스턴스 변수가 없으면 true"
b := String isVariable.               "인덱싱된 인스턴스 변수가 있으면 true"
b := String isPointers.               "인덱스 인스턴스 변수에 객체가 포함되어 있으면 true"
b := String isBits.                   "인덱스 인스턴스 변수에 바이트/워드가 포함되어 있으면 true"
b := String isBytes.                  "인덱스 인스턴스 변수에 바이트가 포함되어 있으면 true"
b := String isWords.                  "인덱스 인스턴스 변수에 워드가 포함되어 있으면 true"
Object withAllSubclasses size.        "총 클래스 항목 수 가져오기"
```

#### 디버깅:
```smalltalk
| a b x |
x yourself.                             "수신자 반환"
String browse.                          "지정된 클래스 탐색"
x inspect.                              "객체 검사기 창 열기"
x confirm: 'Is this correct?'.
x halt.                                 "디버거 창을 열기 위한 중단점"
x halt: 'Halt message'.
x notify: 'Notify text'.
x error: 'Error string'.                "제목이 있는 오류 창 열기"
x doesNotUnderstand: #cmrMessage.       "메시지가 처리되지 않음을 플래그"
x shouldNotImplement.                   "메시지가 구현되어서는 안 됨을 플래그"
x subclassResponsibility.               "메시지를 추상으로 플래그"
x errorImproperStore.                   "인덱싱 가능한 객체에 대한 부적절한 저장을 플래그"
x errorNonIntegerIndex.                 "정수만 인덱스로 사용해야 함을 플래그"
x errorSubscriptBounds.                 "경계 밖의 첨자를 플래그"
x primitiveFailed.                      "시스템 기본 실패"

a := 'A1'. b := 'B2'. a become: b.      "두 객체 교환"
Transcript show: a, b; cr.
```

#### 기타
```smalltalk
| x |
x := 1.2 hash.                                  "객체에 대한 해시 값"
y := x copy.                                    "객체 복사"
y := x shallowCopy.                             "객체 복사 (재정의되지 않음)"
y := x deepCopy.                                "객체 및 인스턴스 변수 복사"
y := x veryDeepCopy.                            "딕셔너리를 사용한 전체 트리 복사"
"Smalltalk condenseChanges."                    "변경 파일 압축"
x := FillInTheBlank request: 'Prompt Me'.       "사용자에게 입력 프롬프트"
Utilities openCommandKeyHelp
```

## 더 알아보기

### 온라인 Smalltalk 시스템
대부분의 Smalltalk는 OSS로 무료이거나 상업적 사용에 일부 지불이 필요한 무료 다운로드 버전이 있습니다.
* [Squeak](https://www.squeak.org)
* [Pharo](http://pharo.org)
* [Smalltalk/X](https://www.exept.de/en/smalltalk-x.html)
* [Gemstone](http://gemtalksystems.com/)
* [VA Smalltalk](http://www.instantiations.com/products/vasmalltalk/)
* [VisualWorks Smalltalk](http://www.cincomsmalltalk.com/)

### 온라인 Smalltalk 책 및 기사
* [Smalltalk 치트 시트](http://www.angelfire.com/tx4/cus/notes/smalltalk.html)
* [Smalltalk-72 매뉴얼](http://www.bitsavers.org/pdf/xerox/parc/techReports/Smalltalk-72_Instruction_Manual_Mar76.pdf)
* [GNU Smalltalk 사용자 가이드](https://www.gnu.org/software/smalltalk/manual/html_node/Tutorial.html)

#### 역사적 문서
* [BYTE: Smalltalk 특별호](https://archive.org/details/byte-magazine-1981-08)
* [Smalltalk-72 매뉴얼](http://www.bitsavers.org/pdf/xerox/parc/techReports/Smalltalk-72_Instruction_Manual_Mar76.pdf)
* [Smalltalk, 객체 및 디자인](https://books.google.co.in/books?id=W8_Une9cbbgC&printsec=frontcover&dq=smalltalk&hl=en&sa=X&ved=0CCIQ6AEwAWoVChMIw63Vo6CpyAIV0HGOCh3S2Alf#v=onepage&q=smalltalk&f=false)
* [Smalltalk: VisualWorks를 사용한 애플리케이션 개발 소개](https://books.google.co.in/books?id=zalQAAAAMAAJ&q=smalltalk&dq=smalltalk&hl=en&sa=X&ved=0CCgQ6AEwAmoVChMIw63Vo6CpyAIV0HGOCh3S2Alf/)
