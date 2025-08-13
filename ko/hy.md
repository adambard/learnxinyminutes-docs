---
name: Hy
filename: learnhy.hy
contributors:
    - ["Abhishek L", "http://twitter.com/abhishekl"]
    - ["Zirak", "http://zirak.me"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Hy는 Python 위에 구축된 Lisp 방언입니다. 이것은 Hy 코드를 Python의 추상 구문 트리(AST)로 변환하여 달성됩니다. 이를 통해 Hy는 네이티브 Python 코드를 호출하거나 Python이 네이티브 Hy 코드를 호출할 수 있습니다.

```hylang
; 다른 Lisp와 마찬가지로 세미콜론 주석

;; S-표현식 기본
; Lisp 프로그램은 기호 표현식 또는 sexps로 구성됩니다.
(some-function args)
; 이제 전형적인 hello world
(print "hello world")

;; 간단한 데이터 유형
; 모든 간단한 데이터 유형은 Python의 해당 유형과 동일합니다.
42 ; => 42
3.14 ; => 3.14
True ; => True
4+10j ; => (4+10j) 복소수

; 간단한 산술부터 시작하겠습니다.
(+ 4 1) ;=> 5
; 다른 Lisp와 마찬가지로 연산자는 모든 인수에 적용됩니다.
(+ 4 1 2 3) ;=> 10
(- 2 1) ;=> 1
(* 4 2) ;=> 8
(/ 4 1) ;=> 4
(% 4 2) ;=> 0 모듈로 연산자
; 거듭제곱은 Python과 같이 ** 연산자로 표시됩니다.
(** 3 2) ;=> 9
; 중첩된 형식은 예상대로 작동합니다.
(+ 2 (* 4 2)) ;=> 10
; 또한 논리 연산자 and or not 및 등호 등은 예상대로 작동합니다.
(= 5 4) ;=> False
(not (= 5 4)) ;=> True

;; 변수
; 변수는 setv를 사용하여 설정되며, 변수 이름은 ()[]{}",'`;#|를 제외한 모든 utf-8을 사용할 수 있습니다.
(setv a 42)
(setv π 3.14159)
(def *foo* 42)
;; 다른 컨테이너 데이터 유형
; 문자열, 목록, 튜플 및 사전
; 이것들은 Python의 컨테이너 유형과 정확히 동일합니다.
"hello world" ;=> "hello world"
; 문자열 연산은 Python과 유사하게 작동합니다.
(+ "hello " "world") ;=> "hello world"
; 목록은 []를 사용하여 생성되며, 인덱싱은 0부터 시작합니다.
(setv mylist [1 2 3 4])
; 튜플은 불변 데이터 구조입니다.
(setv mytuple (, 1 2))
; 사전은 키-값 쌍입니다.
(setv dict1 {"key1" 42 "key2" 21})
; :name은 Hy에서 키워드를 정의하는 데 사용할 수 있으며, 키에 사용할 수 있습니다.
(setv dict2 {:key1 41 :key2 20})
; `get`을 사용하여 인덱스/키에서 요소를 가져옵니다.
(get mylist 1) ;=> 2
(get dict1 "key1") ;=> 42
; 또는 키워드가 사용된 경우 직접 호출할 수 있습니다.
(:key1 dict2) ;=> 41

;; 함수 및 기타 프로그램 구성
; 함수는 defn을 사용하여 정의되며, 마지막 sexp는 기본적으로 반환됩니다.
(defn greet [name]
  "A simple greeting" ; 선택적 문서 문자열
  (print "hello " name))

(greet "bilbo") ;=> "hello bilbo"

; 함수는 선택적 인수와 키워드 인수를 사용할 수 있습니다.
(defn foolists [arg1 &optional [arg2 2]]
  [arg1 arg2])

(foolists 3) ;=> [3 2]
(foolists 10 3) ;=> [10 3]

; 나머지 인수와 kwargs도 사용할 수 있습니다:
(defn something-fancy [wow &rest descriptions &kwargs props]
  (print "Look at" wow)
  (print "It's" descriptions)
  (print "And it also has:" props))

(something-fancy "My horse" "amazing" :mane "spectacular")

; 스플랫 연산자 대신 apply를 사용합니다:
(apply something-fancy ["My horse" "amazing"] { "mane" "spectacular" })

; 익명 함수는 `fn` 또는 `lambda` 구성을 사용하여 생성되며, `defn`과 유사합니다.
(map (fn [x] (* x x)) [1 2 3 4]) ;=> [1 4 9 16]

;; 시퀀스 연산
; Hy에는 시퀀스 연산 등을 위한 몇 가지 내장 유틸리티가 있습니다.
; `first` 또는 `car`를 사용하여 첫 번째 요소를 검색합니다.
(setv mylist [1 2 3 4])
(setv mydict {"a" 1 "b" 2})
(first mylist) ;=> 1

; cut을 사용하여 목록 슬라이스
(cut mylist 1 3) ;=> [2 3]

; `get`을 사용하여 목록 또는 사전에서 요소 가져오기
(get mylist 1) ;=> 2
(get mydict "b") ;=> 2
; 목록 인덱싱은 Python과 마찬가지로 0부터 시작합니다.
; assoc은 키/인덱스에서 요소를 설정할 수 있습니다.
(assoc mylist 2 10) ; mylist를 [1 2 10 4]로 만듭니다.
(assoc mydict "c" 3) ; mydict를 {"a" 1 "b" 2 "c" 3}으로 만듭니다.
; 시퀀스 작업을 재미있게 만드는 다른 많은 핵심 함수가 있습니다.

;; Python 상호 운용성
;; import는 Python과 똑같이 작동합니다.
(import datetime)
(import functools [partial reduce]) ; functools에서 partial 및 reduce 가져오기
(import matplotlib.pyplot :as plt) ; foo를 bar로 가져오기
; 모든 내장 Python 메서드 등은 Hy에서 액세스할 수 있습니다.
; a.foo(arg)는 (.foo a arg)로 호출됩니다.
(.split (.strip "hello world  ")) ;=> ["hello" "world"]

; 값을 여러 함수에 실행하는 바로 가기인 "스레딩 매크로"가 있으며, 화살표로 표시됩니다:
(-> "hello world  " (.strip) (.split)) ;=> ["hello" "world"]
; 화살표는 값을 첫 번째 인수로 호출을 따라 전달합니다. 예를 들어:
(-> 4 (* 3) (+ 2))
; 다음과 같습니다:
(+ (* 4 3) 2)

; "스레딩 꼬리 매크로"도 있으며, 대신 값을 두 번째 인수로 전달합니다. 비교:
(-> 4 (- 2) (+ 1)) ;=> 3
(+ (- 4 2) 1) ;=> 3
; ~에:
(->> 4 (- 2) (+ 1)) ;=> -1
(+ 1 (- 2 4)) ;=> -1

;; 조건문
; (if condition (body-if-true) (body-if-false)
(if (= passcode "moria")
  (print "welcome")
  (print "Speak friend, and Enter!"))

; cond로 여러 if else if 절 중첩
(cond
  (= someval 42) (print "Life, universe and everything else!")
  (> someval 42) (print "val too large")
  (< someval 42) (print "val too small"))

; do로 문 그룹화, 순차적으로 실행됩니다.
; defn과 같은 형식에는 암시적 do가 있습니다.
(do
  (setv someval 10)
  (print "someval is set to " someval)) ;=> 10

; `let`으로 어휘 바인딩 만들기, 이렇게 정의된 모든 변수는 로컬 범위를 가집니다.
(let [nemesis {"superman" "lex luther"
                "sherlock" "moriarty"
                "seinfeld" "newman"}]
  (for [[h v] (.items nemesis)]
    (print (.format "{0}'s nemesis was {1}" h v))))

;; 클래스
; 클래스는 다음과 같이 정의됩니다.
(defclass Wizard [object]
  (defn __init__ [self spell]
    (setv self.spell spell))

  (defn get-spell [self]
    self.spell))
```

### 추가 자료

이 튜토리얼은 Hy/Lisp/Python에 대한 기본적인 소개일 뿐입니다.

Hy 문서는 여기에 있습니다: [https://hylang.org/hy/doc](https://hylang.org/hy/doc)

Hy의 GitHub 리포지토리: [https://github.com/hylang/hy](https://github.com/hylang/hy)

freenode IRC `#hy`, 트위터 해시태그 #hylang