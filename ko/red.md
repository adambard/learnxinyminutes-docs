---
name: Red
filename: learnred.red
contributors:
    - ["Arnold van Hofwegen", "https://github.com/iArnold"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Red는 작업을 완료해야 할 필요성에서 만들어졌으며, 저자가 사용하고 싶었던 도구인 REBOL 언어에는 몇 가지 단점이 있었습니다.
그 당시에는 오픈 소스가 아니었고, 컴파일된 언어에 비해 평균적으로 느린 해석 언어였습니다.

Red는 C 수준의 방언인 Red/System과 함께 프로그래밍에 필요한 전체 프로그래밍 공간을 다루는 언어를 제공합니다.
Red는 REBOL 언어를 기반으로 하는 언어입니다. Red 자체는 REBOL 언어의 유연성을 재현하며, Red가 기반으로 구축될 기본 언어인 Red/System은 C와 같이 금속에 더 가까운 프로그래밍의 더 기본적인 요구 사항을 다룹니다.

Red는 세계 최초의 풀 스택 프로그래밍 언어가 될 것입니다. 이는 다른 스택 도구의 도움 없이 금속에서 메타까지 모든 수준에서 (거의) 모든 프로그래밍 작업을 수행할 수 있는 효과적인 도구가 될 것임을 의미합니다.
또한 Red는 GCC와 같은 도구 체인 없이 모든 플랫폼에서 다른 모든 플랫폼으로 Red 소스 코드를 교차 컴파일할 수 있습니다. 그리고 이 모든 것을 1MB 미만의 바이너리 실행 파일에서 수행할 것입니다.

첫 번째 Red를 배울 준비가 되셨습니까?

```
헤더 앞의 모든 텍스트는 주석으로 처리됩니다. 단, 이 헤더 앞 텍스트에서 대문자 "R"로 시작하는 "red"라는 단어를 사용하지 않는 한 말이죠.
이것은 사용된 렉서의 일시적인 단점이지만, 대부분의 경우 스크립트나 프로그램을 헤더 자체로 시작합니다.

빨간색 스크립트의 헤더는 대문자 "red" 뒤에 공백 문자, 그 뒤에 대괄호 블록 []이 옵니다. 대괄호 블록은 이 스크립트 또는 프로그램에 대한 유용한 정보로 채울 수 있습니다:
작성자 이름, 파일 이름, 버전, 라이선스, 프로그램 요약 또는 필요한 기타 파일. red/System 헤더는 red 헤더와 동일하며, "red/System"이라고만 표시하고 "red"라고 표시하지 않습니다.
```

```red
Red []

;이것은 주석 처리된 줄입니다.

print "Hello Red World"    ; 이것은 또 다른 주석입니다.

comment {
    이것은 여러 줄 주석입니다.
    방금 Red 버전의 "Hello World" 프로그램을 보셨습니다.
}

; 프로그램의 진입점은 발견된 첫 번째 실행 가능한 코드입니다.
; 'main' 함수로 제한할 필요가 없습니다.

; 유효한 변수 이름은 문자로 시작하며 숫자를 포함할 수 있습니다.
; 대문자 A부터 F와 숫자만 포함하고 'h'로 끝나는 변수는 금지됩니다.
; 이는 Red 및 Red/System에서 16진수를 표현하는 방식이기 때문입니다.

; 콜론 ":"을 사용하여 변수에 값을 할당합니다.
my-name: "Red"
reason-for-using-the-colon: {콜론을 사용하여 값을 할당하면
 등호 "="는 비교 목적으로만 사용할 수 있게 됩니다.
 "="가 원래 의도했던 바와 정확히 일치합니다!
 학교에서 배웠던 y = x + 1 및 x = 1 => y = 2와 같은 것을 기억하십니까?
}
is-this-name-valid?: true

; print를 사용하여 출력을 인쇄하거나, 인쇄된 텍스트 끝에 줄 바꿈이나 줄 바꿈 없이 인쇄하려면 prin을 사용합니다.

prin " My name is " print my-name
My name is Red

print ["My name is " my-name lf]
My name is Red

; 이미 눈치채셨겠지만: 문장은 세미콜론으로 끝나지 않습니다 ;-)

;
; 데이터 유형
;
; Rebol을 아신다면 아마도 많은 데이터 유형이 있다는 것을 눈치채셨을 겁니다. Red는 아직 모든 유형을 가지고 있지 않지만, Rebol에 가깝기를 원하므로 많은 데이터 유형을 가질 것입니다.
; 느낌표로 유형을 인식할 수 있습니다. 하지만 이름이 느낌표로 끝나는 것도 허용됩니다.
; 사용 가능한 유형 중 일부는 integer! string! block!입니다.

; 사용하기 전에 변수를 선언해야 합니까?
; Red는 사용하려는 데이터에 가장 적합한 변수를 스스로 알고 있습니다.
; 변수 선언은 항상 필요한 것은 아닙니다.
; 변수를 선언하고 유형을 지정하는 것이 좋은 코딩 관행으로 간주되지만,
; Red는 이를 강요하지 않습니다.
; 변수를 선언하고 유형을 지정할 수 있습니다. 변수의 유형은
; 바이트 단위의 크기를 결정합니다.

; integer! 유형의 변수는 일반적으로 4바이트 또는 32비트입니다.
my-integer: 0
; Red의 정수는 부호가 있습니다. 현재는 부호 없는 정수를 지원하지 않지만, 나중에 지원될 예정입니다.

; type?를 사용하여 변수의 유형을 확인합니다.
type? my-integer
integer!

; 동시에 초기화되는 다른 변수를 사용하여 변수를 초기화할 수 있습니다.
; 여기서 초기화는 변수를 선언하고 값을 할당하는 것을 의미합니다.
i2: 1 + i1: 1

; 산술은 간단합니다.
i1 + i2 ; 결과 3
i2 - i1 ; 결과 1
i2 * i1 ; 결과 2
i1 / i2 ; 결과 0 (0.5이지만 0으로 잘림)

; 비교 연산자는 아마도 익숙할 것입니다. 다른 언어와 달리
; 비교에는 단일 '=' 기호만 필요합니다. 부등호는 Pascal과 같이 '<>'입니다.
; Red에는 부울과 같은 유형이 있습니다. true 및 false 값을 가지지만,
; on/off 또는 yes/no 값도 사용할 수 있습니다.

3 = 2 ; 결과 false
3 <> 2 ; 결과 true
3 > 2 ; 결과 true
3 < 2 ; 결과 false
2 <= 2 ; 결과 true
2 >= 2 ; 결과 true

;
; 제어 구조
;
; if
; 주어진 조건이 참이면 코드 블록을 평가합니다. IF는
; 블록의 결과 값을 반환하거나 조건이 거짓이면 'none'을 반환합니다.
if a < 0 [print "a is negative"]

; either
; 주어진 조건이 참이면 코드 블록을 평가하고, 그렇지 않으면
; 대체 코드 블록을 평가합니다. 두 블록의 마지막 표현식이
; 동일한 유형이면 EITHER를 표현식 내에서 사용할 수 있습니다.
either a > 0 [
   msg: "positive"
][
   either a = 0 [
       msg: "zero"
   ][
       msg: "negative"
   ]
]

print ["a is " msg lf]

; 이것을 작성하는 다른 방법이 있습니다.
; (모든 코드 경로가 동일한 유형의 값을 반환하므로 허용됩니다):

msg: either a > 0 [
   "positive"
][
   either a = 0 [
       "zero"
   ][
       "negative"
   ]
]
print ["a is " msg lf]

; until
; 블록 끝의 조건이 충족될 때까지 코드 블록을 반복합니다.
; UNTIL은 항상 마지막 표현식의 최종 평가에서 'true' 값을 반환합니다.
c: 5
until [
   prin "o"
   c: c - 1
   c = 0    ; until 루프를 끝내는 조건
]
;   출력:
ooooo
; 조건이 처음부터 충족되지 않더라도 루프는 항상 최소 한 번 평가됩니다.

; while
; 주어진 조건이 충족되는 동안 코드 블록을 평가합니다.
; WHILE은 값을 반환하지 않으므로 표현식에서 사용할 수 없습니다.
c: 5
while [c > 0][
   prin "o"
   c: c - 1
]
; 출력:
ooooo

;
; 함수
;
; 함수 예제
twice: function [a [integer!] /one return: [integer!]][
        c: 2
        a: a * c
        either one [a + 1][a]
]
b: 3
print twice b   ; 6을 출력합니다.

; 외부 파일을 #include로 가져오고 파일 이름은 % 기호로 시작합니다.
#include %includefile.red
; 이제 포함된 파일의 함수도 사용할 수 있습니다.
```

## 더 읽을거리

Red에 대한 주요 정보 출처는 [Red 언어 홈페이지](http://www.red-lang.org)입니다.

소스는 [GitHub](https://github.com/red/red)에서 찾을 수 있습니다.

Red/System 언어 사양은 [여기](http://static.red-lang.org/red-system-specs-light.html)에서 찾을 수 있습니다.

Rebol과 Red에 대해 더 자세히 알아보려면 [Gitter 채팅](https://gitter.im/red/red)에 참여하십시오. 그리고 그것이 작동하지 않는다면 [Red 메일링 리스트](mailto: red-langNO_SPAM@googlegroups.com)로 메일을 보내십시오(NO_SPAM 제거).

[Stack Overflow](https://stackoverflow.com/questions/tagged/red)에서 질문을 찾아보거나 질문하십시오.

Red를 바로 사용해보고 싶으십니까? [try Rebol and Red 사이트](http://tryrebol.esperconsultancy.nl)에서 가능합니다.

[Rebol](http://www.rebol.com/docs.html)을 배우면서 Red를 배울 수도 있습니다.