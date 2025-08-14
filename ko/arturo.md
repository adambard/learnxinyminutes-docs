---
name: Arturo
filename: learnarturo.art
contributors:
  - ["Dr.Kameleon", "https://github.com/drkameleon"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```red
; 이것은 주석입니다
; 이것은 또 다른 주석입니다

;---------------------------------
; 변수 및 값
;---------------------------------

; 숫자
a1: 2
a2: 3.14
a3: to :complex [1 2.0]     ; 1.0+2.0i

; 문자열
c1: "이것은 문자열입니다"
c2: {
    이것은 여러 줄 문자열입니다
    들여쓰기에 구애받지 않습니다
}
c3: {:
    이것은
        있는 그대로의
            여러 줄 문자열입니다
                정확히
                    원본과 같이 유지됩니다
:}

; 문자
ch: `c`

; 블록/배열
d: [1 2 3]

; 사전
e: #[
    name: "John"
    surname: "Doe"
    age: 34
    likes: [pizza spaghetti]
]

; 예, 함수도 값입니다
f: function [x][
    2 * x
]

; 날짜
g: now              ; 2021-05-03T17:10:48+02:00

; 부울
h1: true
h2: false

;---------------------------------
; 기본 연산자
;---------------------------------

; 간단한 산술
1 + 1       ; => 2
8 - 1       ; => 7
4.2 - 1.1   ; => 3.1
10 * 2      ; => 20
35 / 4      ; => 8
35 // 4     ; => 8.75
2 ^ 5       ; => 32
5 % 3       ; => 2

; 비트 연산자
and 3 5     ; => 1
or 3 5      ; => 7
xor 3 5     ; => 6

; 미리 정의된 상수
pi          ; => 3.141592653589793
epsilon     ; => 2.718281828459045
null        ; => null
true        ; => true
false       ; => false

;---------------------------------
; 비교 연산자
;---------------------------------

; 같음
1 = 1       ; => true
2 = 1       ; => false

; 같지 않음
1 <> 1      ; => false
2 <> 1      ; => true

; 더 많은 비교
1 < 10      ; => true
1 =< 10     ; => true
10 =< 10    ; => true
1 > 10      ; => false
1 >= 10     ; => false
11 >= 10    ; => true

;---------------------------------
; 조건문
;---------------------------------

; 논리 연산자
and? true true      ; => true
and? true false     ; => false
or? true false      ; => true
or? false false     ; => false

and? [1=2][2<3]     ; => false
                    ; (두 번째 블록은 평가되지 않습니다)

; 간단한 if 문
if 2 > 1 [ print "yes!"]    ; yes!
if 3 <> 2 -> print "true!"  ; true!

; if/else 문
if? 2 > 3 -> print "2 is greater than 3"
else -> print "2 is not greater than 3"         ; 2 is not greater than 3

; switch 문
switch 2 > 3 -> print "2 is greater than 3"
             -> print "2 is not greater than 3" ; 2 is not greater than 3

a: (2 > 3)["yes"]["no"]         ; a: "no"
a: (2 > 3)? -> "yes" -> "no"    ; a: "no" (위와 정확히 동일)

; case/when 문
case [1]
    when? [>2] -> print "1 is greater than 2. what?!"
    when? [<0] -> print "1 is less than 0. nope..."
    else -> print "here we are!"                ; here we are!

;---------------------------------
; 루프
;---------------------------------

; `loop` 사용
arr: [1 4 5 3]
loop arr 'x [
    print ["x =" x]
]
; x = 1
; x = 4
; x = 5
; x = 3

; loop와 사용자 정의 인덱스 사용
loop.with:'i arr 'x [
    print ["item at position" i "=>" x]
]
; item at position 0 => 1
; item at position 1 => 4
; item at position 2 => 5
; item at position 3 => 3

; 범위 사용
loop 1..3 'x ->         ; 단일 문장이므로
    print x             ; [block] 표기법이 필요 없습니다
                        ; `->` 구문 설탕을 사용하여 묶을 수 있습니다

loop `a`..`c` 'ch ->
    print ch
; a
; b
; c

; 여러 항목 선택
loop 1..10 [x y] ->
    print ["x =" x ", y =" y]
; x = 1 , y = 2
; x = 3 , y = 4
; x = 5 , y = 6
; x = 7 , y = 8
; x = 9 , y = 10

; 사전을 통한 루핑
dict: #[
    name: "John"
    surname: "Doe"
    age: 34
]
loop dict [key value][
    print [key "->" value]
]
; name -> John
; surname -> Doe
; age -> 34

; while 루프
i: new 0
while [i<3][
    print ["i =" i]
    inc 'i
]
; i = 0
; i = 1
; i = 2

;---------------------------------
; 문자열
;---------------------------------

; 대소문자
a: "tHis Is a stRinG"
print upper a               ; THIS IS A STRING
print lower a               ; this is a string
print capitalize a          ; tHis Is a stRinG

; 연결
a: "Hello " ++ "World!"     ; a: "Hello World!"

; 배열로서의 문자열
split "hello"               ; => [h e l l o]
split.words "hello world"   ; => [hello world]

print first "hello"         ; h
print last "hello"          ; o

; 변환
to :string 123              ; => "123"
to :integer "123"           ; => 123

; 문자열 결합
join ["hello" "world"]              ; => "helloworld"
join.with:"-" ["hello" "world"]     ; => "hello-world"

; 문자열 보간
x: 2
print ~"x = |x|"            ; x = 2

; `print`를 사용한 보간
print ["x =" x]             ; x = 2
                            ; (`print`는 주어진 블록을 계산하고
                            ;  다른 값들을 문자열로 결합하여
                            ;  사이에 단일 공백을 둡니다)

; 템플릿
print render.template {
    <||= switch x=2 [ ||>
        Yes, x = 2
    <||][||>
        No, x is not 2
    <||]||>
} ; Yes, x = 2

; 일치
prefix? "hello" "he"        ; => true
suffix? "hello" "he"        ; => false

contains? "hello" "ll"      ; => true
contains? "hello" "he"      ; => true
contains? "hello" "x"       ; => false

in? "ll" "hello"            ; => true
in? "x" "hello"             ; => false

;---------------------------------
; 블록
;---------------------------------

; 블록 계산
arr: [1 1+1 1+1+1]
@arr                        ; => [1 2 3]

; 블록 실행
sth: [print "Hello world"]  ; 이것은 완벽하게 유효하며,
                            ; *무엇이든* 포함할 수 있습니다
                            ; 그리고 실행되지 않습니다...

do sth                      ; Hello world
                            ; (...우리가 그렇게 하라고 말할 때까지)

; 배열 인덱싱
arr: ["zero" "one" "two" "three"]
print first arr             ; zero
print arr\0                 ; zero
print last arr              ; three
print arr\3                 ; three

x: 2
print get arr x             ; two
print arr \ 2               ; two
                            ; (`get`에 대한 `\` 중위 별칭 사용 -
                            ;  피연산자 사이에 공백을 주목하십시오!
                            ;  그렇지 않으면 경로로 구문 분석됩니다)

; 배열 요소 설정
arr\0: "nada"
set arr 2 "dos"
print arr                   ; nada one dos three

; 배열에 요소 추가
arr: new []
'arr ++ "one"
'arr ++ "two"
print arr                   ; one two

; 배열에서 요소 제거
arr: new ["one" "two" "three" "four"]
'arr -- "two"               ; arr: ["one" "three" "four"]
remove 'arr .index 0        ; arr: ["three" "four"]

; 배열 크기 가져오기
arr: ["one" 2 "three" 4]
print size arr              ; 4

; 배열의 일부 가져오기
print slice ["one" "two" "three" "four"] 0 1        ; one two

; 배열에 특정 요소가 포함되어 있는지 확인
print contains? arr "one"   ; true
print contains? arr "five"  ; false

; 배열 정렬
arr: [1 5 3 2 4]
sort arr                    ; => [1 2 3 4 5]
sort.descending arr         ; => [5 4 3 2 1]

; 값 매핑
map 1..10 [x][2*x]          ; => [2 4 6 8 10 12 14 16 18 20]
map 1..10 'x -> 2*x         ; 위와 동일
map 1..10 => [2*&]          ; 위와 동일
map 1..10 => [2*]           ; 위와 동일

; 배열 값 선택/필터링
select 1..10 [x][odd? x]    ; => [1 3 5 7 9]
select 1..10 => odd?        ; 위와 동일

filter 1..10 => odd?        ; => [2 4 6 8 10]
                            ; (이제 모든 홀수를 제외합니다 -
                            ;  select는 유지하는 반면)

; 기타 작업
arr: ["one" 2 "three" 4]
reverse arr                 ; => [4 "three" 2 "one"]
shuffle arr                 ; => [2 4 "three" "one"]
unique [1 2 3 2 3 1]        ; => [1 2 3]
permutate [1 2 3]           ; => [[1 2 3] [1 3 2] [3 1 2] [2 1 3] [2 3 1] [3 2 1]]
take 1..10 3                ; => [1 2 3]
repeat [1 2] 3              ; => [1 2 1 2 1 2]

;---------------------------------
; 함수
;---------------------------------

; 함수 선언
f: function [x][ 2*x ]
f: function [x]-> 2*x       ; 위와 동일
f: $[x]->2*x                ; 위와 동일 (단지 `function`에 대한 `$` 별칭 사용
                            ;  ... 함수)

; 함수 호출
f 10                        ; => 20

; 값 반환
g: function [x][
    if x < 2 -> return 0

    res: 0
    loop 0..x 'z [
        res: res + z
    ]
    return res
]

;---------------------------------
; 사용자 정의 유형
;---------------------------------

; 사용자 정의 유형 정의
define :person [
    name                                    ; 필드 포함: name, surname, age
    surname
    age
][
    ; 사용자 정의 사후 생성 초기화 프로그램 포함
    init: [
        this\name: capitalize this\name
    ]

    ; 사용자 정의 인쇄 함수
    print: [
        render "NAME: |this\name|, SURNAME: |this\surname|, AGE: |this\age|"
    ]

    ; 사용자 정의 비교 연산자
    compare: 'age
]

; 사용자 정의 유형에 대한 메서드 생성
sayHello: function [this][
    ensure -> is? :person this

    print ["Hello" this\name]
]

; 사용자 정의 유형의 새 객체 생성
a: to :person ["John" "Doe" 34]                 ; 2개의 "Person"을 생성해 보겠습니다
b: to :person ["jane" "Doe" 33]                 ; 그리고 또 하나

; 의사 내부 메서드 호출
sayHello a                                      ; Hello John
sayHello b                                      ; Hello Jane

; 객체 필드 접근
print ["The first person's name is:" a\name]    ; The first person's name is: John
print ["The second person's name is:" b\name]   ; The second person's name is: Jane

; 객체 필드 변경
a\name: "Bob"
sayHello a                                      ; Hello Bob

; 객체 유형 확인
print type a                                    ; :person
print is? :person a                             ; true

; 객체 인쇄
print a                                         ; NAME: John, SURNAME: Doe, AGE: 34

; 사용자 객체 정렬 (사용자 정의 비교기 사용)
sort @[a b]                                     ; Jane..., John...
sort.descending @[a b]                          ; John..., Jane...
```

## 추가 자료

- [공식 문서](https://arturo-lang.io/documentation/) - Arturo 공식 문서 및 참조.
- [온라인 놀이터](https://arturo-lang.io/playground/) - Arturo 프로그래밍 언어를 위한 온라인 REPL.
