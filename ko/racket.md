---
name: Paren
filename: learnparen.paren
contributors:
  - ["KIM Taegyoon", "https://github.com/kimtg"]
  - ["Claudson Martins", "https://github.com/claudsonm"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Paren](https://bitbucket.org/ktg/paren)은 Lisp의 방언입니다. 임베디드 언어로 설계되었습니다.

일부 예제는 [Racket](../racket/)에서 가져왔습니다.

```scheme
;;; 주석
# 주석

;; 한 줄 주석은 세미콜론 또는 샵 기호로 시작합니다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. 기본 데이터 유형 및 연산자
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 숫자
123 ; 정수
3.14 ; 더블
6.02e+23 ; 더블
(int 3.14) ; => 3 : int
(double 123) ; => 123 : double

;; 함수 적용은 (f x y z ...)로 작성됩니다.
;; 여기서 f는 함수이고 x, y, z는 피연산자입니다.
;; 글자 그대로의 데이터 목록을 만들려면 (quote)를 사용하여 평가를 중지하십시오.
(quote (+ 1 2)) ; => (+ 1 2)
;; 이제 몇 가지 산술 연산
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(^ 2 3) ; => 8
(/ 5 2) ; => 2
(% 5 2) ; => 1
(/ 5.0 2) ; => 2.5

;;; 부울
true ; 참
false ; 거짓 -- #f가 아니면 참
(! true) ; => false
(&& true false (prn "doesn't get here")) ; => false
(|| false true (prn "doesn't get here")) ; => true

;;; 문자는 int입니다.
(char-at "A" 0) ; => 65
(chr 65) ; => "A"

;;; 문자열은 고정 길이의 문자 배열입니다.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; 백슬래시는 이스케이프 문자입니다.
"Foo\tbar\r\n" ; C 이스케이프 포함: \t \r \n

;; 문자열도 추가할 수 있습니다!
(strcat "Hello " "world!") ; => "Hello world!"

;; 문자열은 문자 목록처럼 처리될 수 있습니다.
(char-at "Apple" 0) ; => 65

;; 인쇄는 매우 쉽습니다.
(pr "I'm" "Paren. ") (prn "Nice to meet you!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. 변수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set)을 사용하여 변수를 만들거나 설정할 수 있습니다.
;; 변수 이름은 ();#"를 제외한 모든 문자를 사용할 수 있습니다.
(set some-var 5) ; => 5
some-var ; => 5

;; 이전에 할당되지 않은 변수에 접근하면 예외가 발생합니다.
; x ; => 알 수 없는 변수: x : nil

;; 지역 바인딩: 'a'와 'b'는 (fn ...) 내에서만 '1'과 '2'에 바인딩됩니다.
((fn (a b) (+ a b)) 1 2) ; => 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 컬렉션
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 목록

;; 목록은 벡터와 같은 데이터 구조입니다. (임의 접근은 O(1)입니다.)
(cons 1 (cons 2 (cons 3 (list)))) ; => (1 2 3)
;; 'list'는 목록에 대한 편리한 가변 생성자입니다.
(list 1 2 3) ; => (1 2 3)
;; 그리고 인용 부호도 리터럴 목록 값에 사용할 수 있습니다.
(quote (+ 1 2)) ; => (+ 1 2)

;; 'cons'를 사용하여 목록 시작 부분에 항목을 추가할 수 있습니다.
(cons 0 (list 1 2 3)) ; => (0 1 2 3)

;; 목록은 매우 기본적인 유형이므로, 목록에 대한 많은 기능이 있습니다.
;; 몇 가지 예:
(map inc (list 1 2 3))          ; => (2 3 4)
(filter (fn (x) (== 0 (% x 2))) (list 1 2 3 4))    ; => (2 4)
(length (list 1 2 3 4))     ; => 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 함수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'fn'을 사용하여 함수를 만듭니다.
;; 함수는 항상 마지막 표현식의 값을 반환합니다.
(fn () "Hello World") ; => (fn () Hello World) : fn

;; 모든 함수를 호출할 때는 괄호를 사용합니다. 람다 표현식도 포함합니다.
((fn () "Hello World")) ; => "Hello World"

;; 변수에 함수를 할당
(set hello-world (fn () "Hello World"))
(hello-world) ; => "Hello World"

;; 함수 정의 구문 설탕을 사용하여 이것을 단축할 수 있습니다:
(defn hello-world2 () "Hello World")

;; 위의 ()는 함수의 인수 목록입니다.
(set hello
  (fn (name)
    (strcat "Hello " name)))
(hello "Steve") ; => "Hello Steve"

;; ... 또는 동일하게, 설탕이 첨가된 정의를 사용하여:
(defn hello2 (name)
  (strcat "Hello " name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 같음
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 숫자의 경우 '=='를 사용합니다.
(== 3 3.0) ; => true
(== 2 1) ; => false

;; 객체 동등성의 경우 'eq?'를 사용합니다.
(eq? 3 3) ; => true
(eq? 3 3.0) ; => false
(eq? (list 3) (list 3)) ; => false

;; 컬렉션의 경우 'equal?'을 사용합니다.
(equal? (list 'a 'b) (list 'a 'b)) ; => true
(equal? (list 'a 'b) (list 'b 'a)) ; => false

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. 제어 흐름
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 조건문

(if true               ; 테스트 표현식
    "this is true"   ; then 표현식
    "this is false") ; else 표현식
; => "this is true"

;; 조건에서 #f가 아니면 참으로 처리됩니다.
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; `cond`는 연속하여 조사하여 값을 선택합니다.
(cond [(> 2 2) (error "wrong!")]
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

;;; 루프

;; for 루프는 숫자에 대한 것입니다.
;; (for SYMBOL START END STEP EXPR ..)
(for i 0 10 2 (pr i "")) ; => 0 2 4 6 8 10 인쇄
(for i 0.0 10 2.5 (pr i "")) ; => 0 2.5 5 7.5 10 인쇄

;; while 루프
((fn (i)
  (while (< i 10)
    (pr i)
    (++ i))) 0) ; => 0123456789 인쇄

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. 변경
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'set'을 사용하여 변수 또는 위치에 새 값을 할당합니다.
(set n 5) ; => 5
(set n (inc n)) ; => 6
n ; => 6
(set a (list 1 2)) ; => (1 2)
(set (nth 0 a) 3) ; => 3
a ; => (3 2)

;; 명시적인 가변 값을 사용하려면 box 사용 (다른 언어의 포인터나 참조와 비슷함)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*) ; => 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. 매크로
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 매크로는 언어의 구문을 확장할 수 있게 합니다.
;; Paren 매크로는 쉽습니다.
;; 사실, (defn)은 매크로입니다.
(defmacro setfn (name ...) (set name (fn ...)))
(defmacro defn (name ...) (def name (fn ...)))

;; 중위 표기법 추가
(defmacro infix (a op ...) (op a ...))
(infix 1 + 2 (infix 3 * 4)) ; => 15

;; 매크로는 위생적이지 않습니다. 기존 변수를 침범할 수 있습니다!
;; 코드 변환입니다.
(define-syntax-rule (swap! x y) ; -!는 변경의 관용구
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
;; `tmp` 변수는 이름 충돌을 피하기 위해 `tmp_1`로 이름이 변경됩니다.
;; (let ([tmp_1 tmp])
;;   (set! tmp other)
;;   (set! other tmp_1))

;; 하지만 그것들은 단지 코드 변형일 뿐입니다. 예를 들어:
(define-syntax-rule (bad-while condition body ...) 
  (when condition
    body ...
    (bad-while condition body ...)))
;; 이 매크로는 엉터리다: 무한 코드를 생성하며,
;; 이것을 사용하려고 하면 컴파일러가 무한 반복에 빠진다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. 클래스와 개체
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 클래스 fish%를 생성한다. (-%는 클래스 정의에 쓰이는 관용구)
(define fish%
  (class object%
    (init size) ; 초기화 인자
    (super-new) ; 상위 클래스 초기화
    ;; 필드
    (define current-size size)
    ;; 공용 메서드
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

;; fish%의 인스턴스를 생성한다.
(define charlie
  (new fish% [size 10]))

;; 개체의 메서드를 호출하기 위해 `send'를 사용한다.
(send charlie get-size) ; => 10
(send charlie grow 6)
(send charlie get-size) ; => 16

;; `fish%'는 보통의 "일급" 값이며, mixin을 줄 수 있다.
(define (add-color c%)
  (class c%
    (init color)
    (super-new)
    (define my-color color)
    (define/public (get-color) my-color)))
(define colored-fish% (add-color fish%))
(define charlie2 (new colored-fish% [size 10] [color 'red]))
(send charlie2 get-color)
;; 또는, 이름 없이:
(send (new (add-color fish%) [size 10] [color 'red]) get-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. 매크로
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 매크로는 언어의 문법을 확장할 수 있게 한다.

;; while 반복문을 추가하자.
(define-syntax-rule (while condition body ...) 
  (let loop () 
    (when condition 
      body ... 
      (loop))))

(let ([i 0])
  (while (< i  10)
    (displayln i)
    (set! i (add1 i))))

;; 매크로는 위생적이다. 즉, 기존 변수를 침범할 수 없다.
(define-syntax-rule (swap! x y) ; -!는 변경의 관용구
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
;; `tmp` 변수는 이름 충돌을 피하기 위해 `tmp_1`로 이름이 변경된다.
;; (let ([tmp_1 tmp])
;;   (set! tmp other)
;;   (set! other tmp_1))

;; 하지만 그것들은 단지 코드 변형일 뿐이다. 예를 들어:
(define-syntax-rule (bad-while condition body ...) 
  (when condition
    body ...
    (bad-while condition body ...)))
;; 이 매크로는 엉터리다: 무한 코드를 생성하며,
;; 이것을 사용하려고 하면 컴파일러가 무한 반복에 빠진다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. 계약(Contract)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 계약은 모듈에서 노출된 값에 대해 제약을 부여한다.

(module bank-account racket
  (provide (contract-out
            [deposit (-> positive? any)] ; 값은 양수여야 함
            [balance (-> positive?)]))

  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount)
  )

(require 'bank-account)
(deposit 5)

(balance) ; => 5

;; 양수가 아닌 값을 예치하려고 하는 고객은 비난받는다.
;; (deposit -5) ; => deposit: contract violation
;; expected: positive?
;; given: -5
;; more details....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. 입력과 출력
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Racket은 이 "port"라는 개념이 있다. 이것은 다른 언어의
;; 파일 서술자 (file descriptor)와 매우 비슷하다.

;; "/tmp/tmp.txt"를 열고 "Hello World"를 기록한다.
;; 그 파일이 이미 있다면 에러를 발생시킨다.
(define out-port (open-output-file "/tmp/tmp.txt"))
(displayln "Hello World" out-port)
(close-output-port out-port)

;; "/tmp/tmp.txt"에 붙이기
(define out-port (open-output-file "/tmp/tmp.txt"
                                   #:exists 'append))
(displayln "Hola mundo" out-port)
(close-output-port out-port)

;; 파일에서 다시 읽기
(define in-port (open-input-file "/tmp/tmp.txt"))
(displayln (read-line in-port))
; => "Hello World"
(displayln (read-line in-port))
; => "Hola mundo"
(close-input-port in-port)

;; 다르게, call-with-output-file을 사용하면, 명시적으로 파일을 닫지 않아도 된다.
(call-with-output-file "/tmp/tmp.txt"
  #:exists 'update ; 내용을 다시 쓴다.
  (λ (out-port)
    (displayln "World Hello!" out-port)))

;; call-with-input-file은 입력에 대해 같은 방식으로 작동한다.
(call-with-input-file "/tmp/tmp.txt"
  (λ (in-port)
    (displayln (read-line in-port))))
```

## 더 읽을거리

더 배우고 싶으면, [Getting Started with Racket](http://docs.racket-lang.org/getting-started/)도 보시오.