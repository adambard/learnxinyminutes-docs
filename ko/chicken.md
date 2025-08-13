---
name: "CHICKEN"
filename: CHICKEN.scm
contributors:
  - ["Diwakar Wagle", "https://github.com/deewakar"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

CHICKEN은 Scheme 프로그래밍 언어의 구현으로, Scheme 프로그램을 C 코드로 컴파일하고 해석할 수 있습니다. CHICKEN은 R5RS 및 R7RS(작업 중) 표준과 많은 확장을 지원합니다.


```scheme
;; #!/usr/bin/env csi -s

;; 다음과 같이 명령줄에서 CHICKEN REPL을 실행합니다:
;; $ csi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 0. 구문
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 한 줄 주석은 세미콜론으로 시작합니다.

#| 블록 주석
   여러 줄에 걸쳐 작성할 수 있으며...
   #| 중첩될 수 있습니다.
   |#
|#

;; S-표현식 주석은 표현식을 주석 처리하는 데 사용됩니다.
#; (display "nothing")    ; 이 표현식을 버립니다.

;; CHICKEN에는 두 가지 기본 구문이 있습니다: 원자(Atom)와 S-표현식(S-expression)
;; 원자는 그 자체로 평가되는 것입니다.
;; 숫자, 문자, 부울, 문자열 등 모든 내장 데이터 유형은 원자입니다.
;; 또한 원자는 기호, 식별자, 키워드, 프로시저
;; 또는 빈 목록(null이라고도 함)일 수 있습니다.
'athing              ;; => athing
'+'                   ;; => +
+
;; => <procedure C_plus>

;; S-표현식(기호 표현식의 약자)은 하나 이상의 원자로 구성됩니다.
(quote +)            ;; => + ; '+'를 쓰는 또 다른 방법
(+ 1 2 3)            ;; => 6 ; 이 S-표현식은 함수 호출로 평가됩니다.
'(+ 1 2 3)           ;; => (+ 1 2 3) ; 목록으로 평가됩니다.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. 기본 데이터 유형 및 연산자
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 숫자
99999999999999999999 ;; 정수
#b1010               ;; 이진수 ; => 10
#o10                 ;; 8진수  ; => 8
#x8ded               ;; 16진수 ; => 36333
3.14                 ;; 실수
6.02e+23
3/4                  ;; 유리수

;;문자 및 문자열
#\A                  ;; 문자 A
"Hello, World!"      ;; 문자열은 고정 길이의 문자 배열입니다.

;; 부울
#t                  ;; 참
#f                  ;; 거짓

;; 함수 호출은 (f x y z ...)로 작성됩니다.
;; 여기서 f는 함수이고 x,y,z, ...는 인수입니다.
(print "Hello, World!")    ;; => Hello, World!
;; 형식화된 출력
(printf "Hello, ~a.\n" "World")  ;; => Hello, World.

;; 명령줄 인수 인쇄
(map print (command-line-arguments))

(list 'foo 'bar 'baz)          ;; => (foo bar baz)
(string-append "pine" "apple") ;; => "pineapple"
(string-ref "tapioca" 3)       ;; => #\i;; 문자 'i'는 인덱스 3에 있습니다.
(string->list "CHICKEN")       ;; => (#\C #\H #\I #\C #\K #\E #\N)
(string-intersperse '("1" "2") ":") ;; => "1:2"
(string-split "1:2:3" ":")     ;; => ("1" "2" "3")


;; 술어는 부울 값을 반환하는 특수 함수입니다.
(atom? #t)                ;; => #t

(symbol? #t)              ;; => #f

(symbol? '+')              ;; => #t

(procedure? +)            ;; => #t

(pair? '(1 2))            ;; => #t

(pair? '(1 2 . 3))        ;; => #t

(pair? '())               ;; => #f

(list? '())               ;; => #t


;; 일부 산술 연산

(+ 1 1)                   ;; => 2
(- 8 1)                   ;; => 7
(* 10 2)                  ;; => 20
(expt 2 3)                ;; => 8
(remainder 5 2)           ;; => 1
(/ 35 5)                  ;; => 7
(/ 1 3)                   ;; => 0.333333333333333

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. 변수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define으로 변수를 만들 수 있습니다.
;; 변수 이름은 ()[]{}\",'`;#\를 제외한 모든 문자를 사용할 수 있습니다.
(define myvar 5)
myvar        ;; => 5

;; 프로시저에 대한 별칭
(define ** expt)
(** 2 3)     ;; => 8

;; 정의되지 않은 변수에 액세스하면 예외가 발생합니다.
s            ;; => 오류: 바인딩되지 않은 변수: s

;; 지역 바인딩
(let ((me "Bob"))
  (print me))     ;; => Bob

(print me)        ;; => 오류: 바인딩되지 않은 변수: me

;; 이전에 정의된 변수에 새 값을 할당합니다.
(set! myvar 10)
myvar             ;; => 10


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. 컬렉션
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 쌍
;; 'cons'는 쌍을 구성하고,
;; 'car'는 첫 번째 요소를 추출하고, 'cdr'은 나머지 요소를 추출합니다.
(cons 'subject 'verb)       ;; => '(subject . verb)
(car (cons 'subject 'verb)) ;; => subject
(cdr (cons 'subject 'verb)) ;; => verb

;; 목록
;; 두 번째 항목이 목록인 경우 cons는 새 목록을 만듭니다.
(cons 0 '())         ;; => (0)
(cons 1 (cons 2  (cons 3 '())))
;; => (1 2 3)
;; 'list'는 목록에 대한 편리한 가변 생성자입니다.
(list 1 2 3)    ;; => (1 2 3)


;; 'append'를 사용하여 목록을 함께 추가합니다.
(append '(1 2) '(3 4)) ;; => (1 2 3 4)

;; 목록에 대한 일부 기본 작업
(map add1 '(1 2 3))    ;; => (2 3 4)
(reverse '(1 3 4 7))   ;; => (7 4 3 1)
(sort '(11 22 33 44) >)   ;; => (44 33 22 11)

(define days '(SUN MON FRI))
(list-ref days 1)      ;; => MON
(set! (list-ref days 1) 'TUE)
days                   ;; => (SUN TUE FRI)

;; 벡터
;; 벡터는 요소가 정수로 인덱싱되는 이기종 구조입니다.
;; 벡터는 일반적으로 동일한 길이의 목록보다 공간을 덜 차지합니다.
;; 벡터의 요소에 대한 임의 액세스는 목록보다 빠릅니다.
#(1 2 3)                     ;; => #(1 2 3) ;; 리터럴 구문
(vector 'a 'b 'c)            ;; => #(a b c)
(vector? #(1 2 3))           ;; => #t
(vector-length #(1 (2) "a")) ;; => 3
(vector-ref #(1 (2) (3 3)) 2);; => (3 3)

(define vec #(1 2 3))
(vector-set! vec 2 4)
vec                         ;; => #(1 2 4)

;; 벡터는 목록에서 만들 수 있으며 그 반대도 가능합니다.
(vector->list #(1 2 4))     ;; => '(1 2 4)
(list->vector '(a b c))     ;; => #(a b c)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. 함수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'lambda'를 사용하여 함수를 만듭니다.
;; 함수는 항상 마지막 표현식의 값을 반환합니다.
(lambda () "Hello World")   ;; => #<procedure (?)>

;; 함수 정의 주위에 추가 괄호를 사용하여 실행합니다.
((lambda () "Hello World")) ;; => Hello World ;; 인수 목록이 비어 있습니다.

;; 인수가 있는 함수
((lambda (x) (* x x)) 3)           ;; => 9
;; 인수가 두 개인 함수
((lambda (x y) (* x y)) 2 3)       ;; => 6

;; 변수에 함수 할당
(define sqr (lambda (x) (* x x)))
sqr                        ;; => #<procedure (sqr x)>
(sqr 3)                    ;; => 9

;; 함수 정의 구문 설탕을 사용하여 이것을 단축할 수 있습니다.
(define (sqr x) (* x x))
(sqr 3)                    ;; => 9

;; 기존 프로시저를 재정의할 수 있습니다.
(foldl cons '() '(1 2 3 4 5)) ;; => (((((() . 1) . 2) . 3) . 4) . 5)
(define (foldl func accu alist)
  (if (null? alist)
    accu
    (foldl func (func (car alist) accu) (cdr alist))))

(foldl cons '() '(1 2 3 4 5))   ;; => (5 4 3 2 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5. 같음
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 숫자의 경우 '='를 사용합니다.
(= 3 3.0)                  ;; => #t
(= 2 1)                    ;; => #f

;; 'eq?'는 두 인수가 메모리에서 동일한 객체를 참조하는 경우 #t를 반환합니다.
;; 즉, 간단한 포인터 비교입니다.
(eq? '() '())
;; => #t ;; 메모리에 빈 목록이 하나만 있습니다.
(eq? (list 3) (list 3))    ;; => #f ;; 동일한 객체가 아닙니다.
(eq? 'yes 'yes)            ;; => #t
(eq? 3 3)                  ;; => #t ;; 이 경우 작동하더라도 이렇게 하지 마십시오.
(eq? 3 3.0)                ;; => #f ;; 숫자 비교에는 '='를 사용하는 것이 좋습니다.
(eq? "Hello" "Hello")      ;; => #f

;; 'eqv?'는 숫자와 문자를 제외한 모든 데이터 유형에 대해 'eq?'와 동일합니다.
(eqv? 3 3.0)               ;; => #f
(eqv? (expt 2 3) (expt 2 3)) ;; => #t
(eqv? 'yes 'yes)           ;; => #t

;; 'equal?'은 쌍, 벡터 및 문자열의 내용을 재귀적으로 비교하고
;; 숫자 및 기호와 같은 다른 객체에 eqv?를 적용합니다.
;; 일반적으로 객체가 동일하게 인쇄되면 equal?인 것이 경험 법칙입니다.

(equal? '(1 2 3) '(1 2 3)) ;; => #t
(equal? #(a b c) #(a b c)) ;; => #t
(equal? 'a 'a)             ;; => #t
(equal? "abc" "abc")       ;; => #t

;; 요약:
;; eq?는 객체가 동일한지 테스트합니다.
;; eqv?는 객체가 작동적으로 동등한지 테스트합니다.
;; equal?은 객체가 동일한 구조와 내용을 가지고 있는지 테스트합니다.

;; 문자열의 같음 비교
(string=? "Hello" "Hello") ;; => #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6. 제어 흐름
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 조건문
(if #t                     ;; 테스트 표현식
  "True"                   ;; then 표현식
  "False")                 ;; else 표현식
                           ;; => "True"

(if (> 3 2)
  "yes"
  "no")                    ;; => "yes"

;; 조건문에서 '#f'가 아닌 모든 값은 참으로 처리됩니다.
;; 0, '(), #() ""는 모두 참 값입니다.
(if 0
  "0 is not false"
  "0 is false")            ;; => "0 is not false"

;; 'cond'는 일련의 테스트를 연결하고 참 조건을 만나자마자 반환합니다.
;; 'cond'는 'if/elseif/else' 문을 시뮬레이션하는 데 사용할 수 있습니다.
(cond ((> 2 2) "not true so don't return this")
      ((< 2 5) "true, so return this")
      (else "returning default"))    ;; => "true, so return this"


;; case 표현식은 다음과 같이 평가됩니다:
;; 키가 평가되고 'eqv?'의 의미에서 각 데이터와 비교됩니다.
;; 일치하는 데이터의 해당 절이 평가되고 결과로 반환됩니다.
(case (* 2 3)              ;; 키는 6입니다.
  ((2 3 5 7) 'prime)       ;; 데이터 1
  ((1 4 6 8) 'composite))  ;; 데이터 2; 일치!
                           ;; => composite

;; else 절이 있는 case
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))       ;; =>  consonant

;; 부울 표현식
;; 'and'는 #f로 평가되는 첫 번째 표현식을 반환합니다.
;; 그렇지 않으면 마지막 표현식의 결과를 반환합니다.
(and #t #f (= 2 2.0))                ;; => #f
(and (< 2 5) (> 2 0) "0 < 2 < 5")    ;; => "0 < 2 < 5"

;; 'or'는 #t로 평가되는 첫 번째 표현식을 반환합니다.
;; 그렇지 않으면 마지막 표현식의 결과가 반환됩니다.
(or #f #t #f)                        ;; => #t
(or #f #f #f)                        ;; => #f

;; 'when'은 else 표현식이 없는 'if'와 같습니다.
(when (positive? 5) "I'm positive")  ;; => "I'm positive"

;; 'unless'는 (when (not <test>) <expr>)와 동일합니다.
(unless (null? '(1 2 3)) "not null") ;; => "not null"


;; 루프
;; 루프는 꼬리 재귀의 도움으로 만들 수 있습니다.
(define (loop count)
  (unless (= count 0)
    (print "hello")
    (loop (sub1 count))))
(loop 4)                             ;; => hello, hello ...

;; 또는 명명된 let으로
(let loop ((i 0) (limit 5))
  (when (< i limit)
    (printf "i = ~a\n" i)
    (loop (add1 i) limit)))          ;; => i = 0, i = 1....

;; 'do'는 또 다른 반복 구문입니다.
;; 변수 집합을 초기화하고 각 반복에서 업데이트합니다.
;; 종료 조건이 충족된 후 최종 표현식이 평가됩니다.
(do ((x 0 (add1 x )))            ;; x = 0을 초기화하고 각 반복에서 1을 더합니다.
  ((= x 10) (print "done"))      ;; 종료 조건 및 최종 표현식
  (print x))                     ;; 각 단계에서 실행할 명령
                                 ;; => 0,1,2,3....9,done

;; 목록 반복
(for-each (lambda (a) (print (* a a)))
          '(3 5 7))                  ;; => 9, 25, 49

;; 'map'은 for-each와 같지만 목록을 반환합니다.
(map add1 '(11 22 33))               ;; => (12 23 34)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7. 확장
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CHICKEN 코어는 매우 최소화되어 있지만, Eggs라는 라이브러리 확장을 통해 추가 기능이 제공됩니다.
;; 'chicken-install <eggname>' 명령으로 Eggs를 설치할 수 있습니다.

;; 복소수
3+4i                               ;; => 3+2i
;; 부정확한 부동 소수점으로 대체되지 않고 분수를 지원합니다.
1/3                                ;; => 1/3
;; bignum을 통해 큰 정수를 지원합니다.
(expt 9 20)                        ;; => 12157665459056928801
;; 및 기타 '확장' 함수
(log 10 (exp 1))                   ;; => 2.30258509299405
(numerator 2/3)                    ;; => 2

;; 'utf8'은 유니코드 지원을 제공합니다.
(import utf8)
"\u03BBx:(\u03BC\u0251.\u0251\u2192\u0251).xx" ;; => "λx:(μɑ.ɑ→ɑ).xx"

;; 'posix'는 파일 I/O 및 유닉스 계열 운영 체제를 위한 기타 여러 서비스를 제공합니다.
;; 일부 함수는 Windows 시스템에서 사용할 수 없습니다.
;; 자세한 내용은 http://wiki.call-cc.org/man/5/Module%20(chicken%20file%20posix)를 참조하십시오.

;; 추가할 파일을 열고, "쓰기 전용"으로 열고, 존재하지 않으면 파일을 만듭니다.
(define outfn (file-open "chicken-hen.txt" (+ open/append open/wronly open/creat)))
;; 파일에 일부 텍스트를 씁니다.
(file-write outfn "Did chicken came before hen?")
;; 파일을 닫습니다.
(file-close outfn)
;; 파일을 "읽기 전용"으로 엽니다.
(define infn (file-open "chicken-hen.txt" open/rdonly))
;; 파일에서 일부 텍스트를 읽습니다.
(file-read infn 30)         ;; => ("Did chicken came before hen?  ", 28)
(file-close infn)

;; CHICKEN은 SRFI(Scheme Requests For Implementation) 확장도 지원합니다.
;; CHICKEN에서 지원하는 srfi를 보려면 'http://srfi.schemers.org/srfi-implementers.html"을 참조하십시오.
(import srfi-1)                    ;; 목록 라이브러리
(filter odd? '(1 2 3 4 5 6 7))     ;; => (1 3 5 7)
(count even? '(1 2 3 4 5))         ;; => 2
(take '(12 24 36 48 60) 3)         ;; => (12 24 36)
(drop '(12 24 36 48 60) 2)         ;; => (36 48 60)
(circular-list 'z 'q)              ;; => z q z q ...

(import srfi-13)                   ;; 문자열 라이브러리
(string-reverse "pan")             ;; => "nap"
(string-index "Turkey" #\k)        ;; => 3
(string-every char-upper-case? "CHICKEN") ;; => #t
(string-join '("foo" "bar" "baz") ":")    ;; => "foo:bar:baz"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8. 매크로
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 파이썬과 같은 'for .. in ..' 반복, 목록용
(define-syntax for
  (syntax-rules (in)
                ((for elem in alist body ...)
                 (for-each (lambda (elem) body ...) alist))))

(for x in '(2 4 8 16)
     (print x))          ;; => 2, 4, 8, 16

(for chr in (string->list "PENCHANT")
     (print chr))        ;; => P, E, N, C, H, A, N, T

;; While 루프
(define-syntax while
  (syntax-rules ()
                ((while cond body ...)
                 (let loop ()
                   (when cond
                     body ...
                     (loop))))))

(let ((str "PENCHANT") (i 0))
  (while (< i (string-length str))     ;; while (조건)
         (print (string-ref str i))    ;; 본문
         (set! i (add1 i))))
                                       ;; => P, E, N, C, H, A, N, T

;; 고급 구문 규칙 입문 -> http://petrofsky.org/src/primer.txt
;; chicken의 매크로 시스템 -> http://lists.gnu.org/archive/html/chicken-users/2008-04/msg00013.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9. 모듈
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://wiki.call-cc.org/man/5/Modules도 참조하십시오.

;; 'test' 모듈은 'hello'라는 값과 'greet'라는 매크로를 내보냅니다.
(module test (hello greet)
  (import scheme)

  (define-syntax greet
    (syntax-rules ()
      ((_ whom)
       (begin
         (display "Hello, ")
         (display whom)
         (display " !\n") ) ) ) )

  (define (hello)
    (greet "world") )  )

;; 별도의 파일(예: test.scm)에 모듈을 정의하고 인터프리터에 로드할 수 있습니다.
;;         (load "test.scm")

;; 모듈 가져오기
(import test)
(hello)                ;; => Hello, world !
(greet "schemers")     ;; => Hello, schemers !

;; 다음 명령을 사용하여 모듈 파일을 공유 라이브러리로 컴파일할 수 있습니다.
;;         csc -s test.scm
;;         (load "test.so")

;; 펑터
;; 펑터는 다른 모듈로 매개변수화할 수 있는 상위 수준 모듈입니다.
;; 다음 펑터는 'multiply'라는 함수를 제공하는 'M'이라는 다른 모듈이 필요합니다.
;; 펑터 자체는 제네릭 함수 'square'를 내보냅니다.
(functor (squaring-functor (M (multiply))) (square)
         (import scheme M)
         (define (square x) (multiply x x)))

;; 모듈 'nums'는 'squaring-functor'에 매개변수로 전달될 수 있습니다.
(module nums (multiply)
        (import scheme)     ;; 미리 정의된 모듈
        (define (multiply x y) (* x y)))
;; 최종 모듈은 프로그램에서 가져와 사용할 수 있습니다.
(module number-squarer = (squaring-functor nums))

(import number-squarer)
(square 3)              ;; => 9

;; 다른 입력에 대해 펑터를 인스턴스화할 수 있습니다.
;; 다음은 squaring-functor에 전달할 수 있는 또 다른 예제 모듈입니다.
(module stars (multiply)
        (import chicken scheme)  ;; 'use' 키워드에 대한 chicken 모듈
        (use srfi-1)             ;; 모듈에서 외부 라이브러리를 사용할 수 있습니다.
        (define (multiply x y)
          (list-tabulate x (lambda _ (list-tabulate y (lambda _ '*))))))
(module star-squarer = (squaring-functor stars))

(import star-squarer)
(square 3)              ;; => ((* * *)(* * *)(* * *))
```

## 더 읽을거리
* [CHICKEN 사용자 설명서](https://wiki.call-cc.org/manual).
* [R5RS 표준](http://www.schemers.org/Documents/Standards/R5RS)


## 추가 정보

* [다른 언어 프로그래머를 위해](https://wiki.call-cc.org/chicken-for-programmers-of-other-languages)
* [CHICKEN 구문을 다른 언어와 비교](http://plr.sourceforge.net/cgi-bin/plr/launch.py)

```