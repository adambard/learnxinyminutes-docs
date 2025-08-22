---
name: "Lisp Flavoured Erlang (LFE)"
filename: lispflavourederlang.lfe
contributors:
  - ["Pratik Karki", "https://github.com/prertik"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Lisp Flavoured Erlang(LFE)는 함수형, 동시성, 범용 프로그래밍 언어이자 코어 얼랭(Core Erlang) 및 얼랭 가상 머신(BEAM) 위에 구축된 리스프 방언(Lisp-2)입니다.

LFE는 [LFE](https://github.com/rvirding/lfe)에서 얻을 수 있습니다.
고전적인 시작점은 [LFE 문서](http://docs.lfe.io)입니다.

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. 구문
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 일반적인 형식.

;; 리스프는 ATOM과 S-표현식이라는 두 가지 구문으로 구성됩니다.
;; `form`은 그룹화된 S-표현식으로 알려져 있습니다.

8  ; 아톰; 자기 자신으로 평가됩니다.

:ERLANG ; 아톰; 심볼 :ERLANG으로 평가됩니다.

t  ; true를 나타내는 또 다른 아톰입니다.

(* 2 21) ; S-표현식입니다.

'(8 :foo t)  ; 또 다른 예시입니다.


;;; 주석

;; 한 줄 주석은 세미콜론으로 시작합니다. 일반 주석에는 두 개,
;; 섹션 주석에는 세 개, 파일 수준 주석에는 네 개를 사용하십시오.

;; 블록 주석

   #| 주석 텍스트 |#

;;; 환경

;; LFE는 사실상의 표준입니다.

;; 라이브러리는 얼랭 생태계에서 직접 사용할 수 있습니다. Rebar3가 빌드 도구입니다.

;; LFE는 보통 텍스트 편집기(가급적 이맥스)와 REPL(Read Evaluate Print Loop)을
;; 동시에 실행하여 개발합니다. REPL을 사용하면 시스템에서 "실시간"으로
;; 실행 중인 프로그램을 대화식으로 탐색할 수 있습니다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. 리터럴 및 특수 구문 규칙
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 정수

1234 -123           ; 일반 10진수 표기법
#b0 #b10101         ; 2진수 표기법
#0 #10101           ; 2진수 표기법 (대체 형식)
#o377 #o-111        ; 8진수 표기법
#d123456789 #d+123  ; 명시적 10진수 표기법
#xc0ffe 0x-01       ; 16진수 표기법
#2r1010 #8r377      ; 명시적 밑을 사용한 표기법 (최대 36)
#\a #$ #\ä #\🐭     ; 문자 표기법 (값은 해당 문자의 유니코드 코드 포인트입니다)
#\x1f42d;           ; 16진수 값을 사용한 문자 표기법

;;; 부동 소수점 숫자
1.0 +2.0 -1.5 1.0e10 1.111e-10

;;; 문자열

"큰따옴표 사이의 모든 텍스트. \"나 다른 특수 문자(\n 등)는 이스케이프될 수 있습니다."
; 리스트 문자열
"Cat: \x1f639;" ; 일반 글꼴의 문자열에 유니코드를 작성하고 세미콜론으로 끝냅니다.

#"이것은 일부 \"이스케이프된\" 및 인용된 (\x1f639;) 문자가 있는 바이너리 문자열입니다 \n"
; 바이너리 문자열은 그냥 문자열이지만 VM에서 다르게 작동합니다.
; 다른 작성 방법으로는 #B("a"), #"a", #B(97)이 있습니다.


;;; 문자 이스케이프

\b  ; => 백스페이스
\t  ; => 탭
\n  ; => 개행
\v  ; => 수직 탭
\f  ; => 폼 피드
\r  ; => 캐리지 리턴
\e  ; => 이스케이프
\s  ; => 공백
\d  ; => 삭제

;;; 바이너리
;; 어떤 내용이든 바이너리를 만드는 데 사용됩니다.
#B((#"a" binary) (#"b" binary))                 ; #"ab" (평가된 형식)

;;; 리스트: () 또는 (foo bar baz)

;;; 튜플은 #(value1 value2 ...) 형식으로 작성됩니다. 빈 튜플 #()도 유효합니다.

;;; 맵은 #M(key1 value1 key2 value2 ...) 형식으로 작성됩니다. 빈 맵 #M()도 유효합니다.

;;; 심볼: 파싱할 수 없는 것들. 예: foo, Foo, foo-bar, :foo
| foo | ; 수직 막대로 감싸서 심볼을 명시적으로 생성합니다.

;;; 평가

;; #.(... 어떤 표현식 ...). 예: '#.(+ 1 1)은 표현식을 읽는 동안 (+ 1 1)을
;; 평가하여 효과적으로 '2가 됩니다.

;; LFE REPL에서의 리스트 컴프리헨션

lfe> (list-comp
          ((<- x '(0 1 2 3)))
          (trunc (math:pow 3 x)))
       (1 3 9 27)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. 핵심 형식
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 이 형식들은 커먼 리스프(Common Lisp)와 스킴(Scheme)에서 볼 수 있는 것들과 동일합니다.

(quote e)
(cons head tail)
(car e)
(cdr e)
(list e ... )
(tuple e ... )
(binary seg ... )
(map key val ...), (map-get m k), (map-set m k v ...), (map-update m k v ...)

(lambda (arg ...) ...)
  (match-lambda
    ((arg ... ) {{(when e ...)}} ...) ; 절 일치
    ... )
(let ((pat {{(when e ...)}} e)
      ...)
  ... )
(let-function ((name lambda|match-lambda) ; 지역 함수만 정의
               ... )
  ... )
(letrec-function ((name lambda|match-lambda) ; 지역 함수만 정의
                  ... )
  ... )
(let-macro ((name lambda-match-lambda) ; 지역 매크로만 정의
            ...)
  ...)
(progn ... )
(if test true-expr {{false-expr}})
(case e
  (pat {{(when e ...)}} ...)
   ... ))
(receive
  (pat {{(when e ...)}} ... )
  ...
  (after timeout ... ))
(catch ... )
(try
  e
  {{(case ((pat {{(when e ...)}} ... )
          ... ))}}
  {{(catch
     ; 다음은 반드시 길이가 3인 튜플이어야 합니다!
     (((tuple type value ignore) {{(when e ...)}}
      ... )
     ... )}}
  {{(after ... )}})

(funcall func arg ... )
(call mod func arg ... ) - 얼랭 Mod:Func(Arg, ... ) 호출
(define-module name declaration ... )
(extend-module declaration ... ) - 모듈 및 선언 정의/확장
(define-function name lambda|match-lambda)
(define-macro name lambda|match-lambda) - 최상위 수준에서 함수/매크로 정의

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 매크로
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 매크로는 언어의 일부이며, 핵심 언어와 표준 라이브러리 위에
;; 추상화를 생성하여 표현하고자 하는 것을 더 직접적으로
;; 표현할 수 있도록 해줍니다.

;; 최상위 함수

(defun name (arg ...) ...)

;; 함수에 주석 추가하기

(defun name
  "패턴 매칭 인수를 사용하는 최상위 함수"
  ((argpat ...) ...)
  ...)

;; 최상위 매크로

(defmacro name (arg ...) ...)
(defmacro name arg ...)

;; 패턴 매칭 인수를 사용하는 최상위 매크로

(defmacro name
  ((argpat ...) ...)
  ...)

;; 스킴에서 영감을 받은 syntax-rules 형식을 사용하는 최상위 매크로

(defsyntax name
  (pat exp)
  ...)

;;; 매크로 또는 syntax-rule 형식의 지역 매크로

(macrolet ((name (arg ... ) ... )
            ... )
    ... )

(syntaxlet ((name (pat exp) ...)
             ...)
 ...)

;; CLISP와 유사

(prog1 ...)
(prog2 ...)

;; 얼랭 LFE 모듈

(defmodule name ...)

;; 얼랭 LFE 레코드

(defrecord name ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 패턴과 가드
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 얼랭과 비교한 LFE에서의 패턴 사용

;; 얼랭                     ;; LFE
;; {ok, X}                       (tuple 'ok x)
;; error                         'error
;; {yes, [X|Xs]}                 (tuple 'yes (cons x xs))
;; <<34,F/float>>                (binary 34 (f float))
;; [P|Ps]=All                    (= (cons p ps) all)

  _    ; => 패턴 매칭 시 신경 쓰지 않음

  (= pattern1 pattern2)     ; => 더 쉽고 나은 버전의 패턴 매칭

;; 가드

;; 패턴이 나타날 때마다 (let, case, receive, lc 등) 선택적으로
;; (when test ...) 형식의 가드를 뒤따를 수 있습니다.

(progn gtest ...)             ;; => 가드 테스트의 시퀀스
(if gexpr gexpr gexpr)
(type-test e)
(guard-bif ...)               ;; => 가드 BIF, 산술, 불리언 및 비교 연산자

;;; REPL

lfe>(set (tuple len status msg) #(8 ok "Trillian"))
    #(8 ok "Trillian")
lfe>msg
    "Trillian"

;;; 가드 사용을 보여주는 프로그램

(defun right-number?
        ((x) (when (orelse (== x 42) (== x 276709)))
          'true)
        ((_) 'false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. 함수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if를 사용하는 간단한 함수.

(defun max (x y)
  "max 함수."
  (if (>= x y) x y))

;; 더 많은 절을 사용하는 동일한 함수

(defun max
  "max 함수."
  ((x y) (when (>= x y)) x)
  ((x y) y))

;; 비슷한 스타일이지만 flet 또는 fletrec으로 정의된 지역 함수를 사용하는 동일한 함수

(defun foo (x y)
  "max 함수."
  (flet ((m (a b) "지역 주석."
            (if (>= a b) a b)))
    (m x y)))

;; LFE는 Lisp-2이므로 변수와 함수에 대해 별도의 네임스페이스를 가집니다.
;; 변수와 함수/매크로는 모두 어휘적으로 범위가 지정됩니다.
;; 변수는 lambda, match-lambda 및 let에 의해 바인딩됩니다.
;; 함수는 최상위 defun, flet 및 fletrec에 의해 바인딩됩니다.
;; 매크로는 최상위 defmacro/defsyntax 및 macrolet/syntaxlet에 의해 바인딩됩니다.

;; (funcall func arg ...)은 CL처럼 람다/매치-람다를 호출하는 데 사용됩니다.
;; 변수에 바인딩된 (funs)가 사용됩니다.

;; apply를 위한 별도의 바인딩 및 특수.
apply _F (...),
apply _F/3 ( a1, a2, a3 )

;; 함수 헤드에서의 Cons'ing
(defun sum (l) (sum l 0))
  (defun sum
    (('() total) total)
    (((cons h t) total) (sum t (+ h total))))

;; 생성자 형식 대신 cons 리터럴
      (defun sum (l) (sum l 0))
      (defun sum
        (('() total) total)
        ((`(,h . ,t) total) (sum t (+ h total))))

;; 함수 헤드에서 레코드 매칭

(defun handle_info
  (('ping (= (match-state remote-pid 'undefined) state))
    (gen_server:cast (self) 'ping)
    `#(noreply ,state))
  (('ping state)
   `#(noreply ,state)))

;; 메시지 수신
      (defun universal-server ()
        (receive
          ((tuple 'become func)
           (funcall func))))

;; 메시지를 수신하는 또 다른 방법

 (defun universal-server ()
        (receive
          (`#(become ,func)
            (funcall func))))

;; 특정 작업을 위한 완전한 함수 작성

(defun compose (f g)
  (lambda (x)
   (funcall f
     (funcall g x))))

(defun check ()
  (let* ((sin-asin (compose #'sin/1 #'asin/1))
         (expected (sin (asin 0.5)))
         (compose-result (funcall sin-asin 0.5)))
    (io:format "Expected answer: ~p~n" (list expected))
    (io:format "Answer with compose: ~p~n" (list compose-result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. 동시성
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 얼랭의 경량 "프로세스"에 의해 수행되는 메시지 전달.

(defmodule messenger-back
 (export (print-result 0) (send-message 2)))

(defun print-result ()
  (receive
    ((tuple pid msg)
      (io:format "Received message: '~s'~n" (list msg))
      (io:format "Sending message to process ~p ...~n" (list pid))
      (! pid (tuple msg))
      (print-result))))

(defun send-message (calling-pid msg)
  (let ((spawned-pid (spawn 'messenger-back 'print-result ())))
    (! spawned-pid (tuple calling-pid msg))))

;; 다중 동시 HTTP 요청:

(defun parse-args (flag)
  "하나 이상의 명령줄 인수가 주어지면 전달된 값을 추출합니다.

  예를 들어, 명령줄을 통해 다음이 전달된 경우:

    $ erl -my-flag my-value-1 -my-flag my-value-2

  그런 다음 LFE 프로그램에서 이 함수를 호출하여 추출할 수 있습니다:

    (let ((args (parse-args 'my-flag)))
      ...
      )
  이 예에서 arg 변수에 할당된 값은 my-value-1 및 my-value-2 값을
  포함하는 리스트가 됩니다."
  (let ((`#(ok ,data) (init:get_argument flag)))
    (lists:merge data)))

(defun get-pages ()
  "인수가 없으면 'url 매개변수가 명령줄을 통해 전달되었다고 가정합니다."
  (let ((urls (parse-args 'url)))
    (get-pages urls)))

(defun get-pages (urls)
  "inets를 시작하고 (잠재적으로 많은) HTTP 요청을 만듭니다."
  (inets:start)
  (plists:map
    (lambda (x)
      (get-page x)) urls))

(defun get-page (url)
  "단일 HTTP 요청을 만듭니다."
  (let* ((method 'get)
         (headers '())
         (request-data `#(,url ,headers))
         (http-options ())
         (request-options '(#(sync false))))
    (httpc:request method request-data http-options request-options)
    (receive
      (`#(http #(,request-id #(error ,reason)))
       (io:format "Error: ~p~n" `(,reason)))
      (`#(http #(,request-id ,result))
       (io:format "Result: ~p~n" `(,result))))))
```

## 더 읽을거리

* [LFE DOCS](http://docs.lfe.io)
* [LFE GitBook](https://lfe.gitbooks.io/reference-guide/index.html)
* [LFE Wiki](https://en.wikipedia.org/wiki/LFE_(programming_language))

## 추가 정보

* [LFE PDF](http.www.erlang-factory.com/upload/presentations/61/Robertvirding-LispFlavouredErlang.pdf)
* [LFE mail](https://groups.google.com/d/msg/lisp-flavoured-erlang/XA5HeLbQQDk/TUHabZCHXB0J)
