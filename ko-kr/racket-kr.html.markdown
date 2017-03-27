---

language: racket
filename: learnracket-kr.rkt
contributors:
  - ["th3rac25", "https://github.com/voila"]
  - ["Eli Barzilay", "https://github.com/elibarzilay"]
  - ["Gustavo Schmidt", "https://github.com/gustavoschmidt"]
  - ["Duong H. Nguyen", "https://github.com/cmpitg"]
translators:
  - ["KIM Taegyoon", "https://github.com/kimtg"]
lang: ko-kr
---

Racket 은 Lisp/Scheme 계열의 일반 목적의, 다중 패러다임 프로그래밍 언어이다.

```racket
#lang racket ; 우리가 사용하는 언어를 정의한다.

;;; 주석

;; 한 줄 주석은 세미콜론으로 시작한다.

#| 블록 주석
   은 여러 줄에 걸칠 수 있으며...
    #|
       중첩될 수 있다!
    |#
|#

;; S-expression 주석은 아래 식을 버리므로,
;; 디버깅할 때 식을 주석화할 때 유용하다.
#; (이 식은 버려짐)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. 근본 자료형과 연산자
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 숫자
9999999999999999999999 ; 정수
#b111                  ; 이진수 => 7
#o111                  ; 팔진수 => 73
#x111                  ; 16진수 => 273
3.14                   ; 실수
6.02e+23
1/2                    ; 분수
1+2i                   ; 복소수

;; 함수 적용은 이렇게 쓴다: (f x y z ...)
;; 여기에서 f는 함수이고 x, y, z는 피연산자이다.
;; 글자 그대로의 데이터 리스트를 만들고 싶다면 평가를 막기 위해 '를 쓰시오.
'(+ 1 2) ; => (+ 1 2)
;; 이제, 산술 연산 몇 개
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(expt 2 3) ; => 8
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(/ 35 5) ; => 7
(/ 1 3) ; => 1/3
(exact->inexact 1/3) ; => 0.3333333333333333
(+ 1+2i  2-3i) ; => 3-1i

;;; 불린
#t ; 참
#f ; 거짓 -- #f가 아닌 것은 참
(not #t) ; => #f
(and 0 #f (error "doesn't get here")) ; => #f
(or #f 0 (error "doesn't get here"))  ; => 0

;;; 문자
#\A ; => #\A
#\λ ; => #\λ
#\u03BB ; => #\λ

;;; 문자열은 고정 길이의 문자 배열이다.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; 백슬래시는 탈출 문자이다.
"Foo\tbar\41\x21\u0021\a\r\n" ; C 탈출 문자, 유니코드 포함
"λx:(μα.α→α).xx"              ; 유니코드 문자 포함 가능

;; 문자열은 붙여질 수 있다!
(string-append "Hello " "world!") ; => "Hello world!"

;; 문자열은 문자의 리스트처럼 취급될 수 있다.
(string-ref "Apple" 0) ; => #\A

;; format은 문자열을 형식화하기 위해 사용된다:
(format "~a can be ~a" "strings" "formatted")

;; 인쇄는 쉽다.
(printf "I'm Racket. Nice to meet you!\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. 변수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define으로 변수를 만든다.
;; 변수명으로 다음 문자를 사용할 수 없다: ()[]{}",'`;#|\
(define some-var 5)
some-var ; => 5

;; 유니코드 문자도 사용 가능하다.
(define ⊆ subset?)
(⊆ (set 3 2) (set 1 2 3)) ; => #t

;; 앞에서 정의되지 않은 변수에 접근하면 예외가 발생한다.
; x ; => x: undefined ...

;; 지역 변수: `me'는 (let ...) 안에서만 "Bob"이다.
(let ([me "Bob"])
  "Alice"
  me) ; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 구조체(Struct)와 모음(Collection)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 구조체
(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;;; 쌍 (불변)
;; `cons'는 쌍을 만들고, `car'와 `cdr'는 첫번째와
;; 두번째 원소를 추출한다.
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;;; 리스트

;; 리스트는 연결-리스트 데이터 구조이며, `cons' 쌍으로 만들어지며
;; `null' (또는 '()) 로 리스트의 끝을 표시한다.
(cons 1 (cons 2 (cons 3 null))) ; => '(1 2 3)
;; `list'는 편리한 가변인자 리스트 생성자이다.
(list 1 2 3) ; => '(1 2 3)
;; 글자 그대로의 리스트 값에는 인용부호를 쓴다.
'(1 2 3) ; => '(1 2 3)

;; 리스트의 앞에 항목을 추가하기 위하여 `cons'를 사용한다.
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; 리스트들을 붙이기 위해 `append'를 사용한다.
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; 리스트는 매우 기본적인 자료형이기 때문에, 리스트에 대해 적용되는 많은 기능들이 있다.
;; 예를 들어:
(map add1 '(1 2 3))          ; => '(2 3 4)
(map + '(1 2 3) '(10 20 30)) ; => '(11 22 33)
(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; 벡터

;; 벡터는 고정 길이의 배열이다.
#(1 2 3) ; => '#(1 2 3)

;; `vector-append'를 사용하여 벡터들을 붙인다.
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; 집합

;; 리스트로부터 집합 만들기
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

;; 원소를 추가하려면 `set-add'를 사용한다.
;; (함수적: 확장된 집합을 반환하며, 원래의 입력을 변경하지 않는다.)
(set-add (set 1 2 3) 4) ; => (set 1 2 3 4)

;; 원소를 삭제하려면 `set-remove'
(set-remove (set 1 2 3) 1) ; => (set 2 3)

;; 존재 여부를 조사하려면 `set-member?'
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; 해시

;; 불변의 해시 테이블을 만든다. (가변 예제는 아래에)
(define m (hash 'a 1 'b 2 'c 3))

;; 값 꺼내기
(hash-ref m 'a) ; => 1

;; 없는 값을 꺼내는 것은 예외를 발생시킨다.
; (hash-ref m 'd) => no value found

;; 키가 없을 때 반환할 기본값을 지정할 수 있다.
(hash-ref m 'd 0) ; => 0

;; `hash-set'을 사용하여 불변의 해시 테이블을 확장
;; (원래 것을 변경하지 않고 확장된 해시를 반환한다.)
(define m2 (hash-set m 'd 4))
m2 ; => '#hash((b . 2) (a . 1) (d . 4) (c . 3))

;; 이 해시들은 불변이라는 점을 기억하시오!
m ; => '#hash((b . 2) (a . 1) (c . 3))  <-- no `d'

;; `hash-remove'로 키를 삭제 (이것도 함수적)
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 함수
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `lambda'로 함수를 만든다.
;; 함수는 항상 마지막 식을 반환한다.
(lambda () "Hello World") ; => #<procedure>
;; 유니코드 `λ'도 사용 가능
(λ () "Hello World")     ; => same function

;; 모든 함수를 호출할 때는 괄호를 쓴다, lambda 식도 포함하여.
((lambda () "Hello World")) ; => "Hello World"
((λ () "Hello World"))      ; => "Hello World"

;; 변수에 함수를 할당
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

;; 문법적 설탕을 사용하여 함수 정의를 더 짧게할 수 있다:
(define (hello-world2) "Hello World")

;; 위에서 ()는 함수의 인자 리스트이다.
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"
;; ... 또는, 설탕 친 정의로:
(define (hello2 name)
  (string-append "Hello " name))

;; 가변인자 함수에는 `case-lambda'를 사용한다.
(define hello3
  (case-lambda
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"
;; ... 또는 선택적 인자에 기본값 지정
(define (hello4 [name "World"])
  (string-append "Hello " name))

;; 함수는 추가 인자를 리스트에 포장할 수 있다.
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"
;; ... 설탕 안 친 `lambda' 형식으로는:
(define count-args2
  (lambda args
    (format "You passed ~a args: ~a" (length args) args)))

;; 일반 인자와 포장된 인자를 섞을 수 있다.
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"
;; ... 설탕 안 친 것:
(define hello-count2
  (lambda (name . args)
    (format "Hello ~a, you passed ~a extra args" name (length args))))

;; 키워드 인자
(define (hello-k #:name [name "World"] #:greeting [g "Hello"] . args)
  (format "~a ~a, ~a extra args" g name (length args)))
(hello-k)                 ; => "Hello World, 0 extra args"
(hello-k 1 2 3)           ; => "Hello World, 3 extra args"
(hello-k #:greeting "Hi") ; => "Hi World, 0 extra args"
(hello-k #:name "Finn" #:greeting "Hey") ; => "Hey Finn, 0 extra args"
(hello-k 1 2 3 #:greeting "Hi" #:name "Finn" 4 5 6)
                                         ; => "Hi Finn, 6 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 동등성
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 숫자에는 `='를 사용하시오.
(= 3 3.0) ; => #t
(= 2 1) ; => #f

;; 개체의 동등성에는 `eq?'를 사용하시오.
(eq? 3 3) ; => #t
(eq? 3 3.0) ; => #f
(eq? (list 3) (list 3)) ; => #f

;; 모음에는 `equal?'을 사용하시오.
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. 흐름 제어하기
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 조건

(if #t               ; 조사 식
    "this is true"   ; 그러면 식
    "this is false") ; 아니면 식
; => "this is true"

;; 조건에서는 #f가 아니면 참으로 취급된다.
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; `cond'는 연속하여 조사하여 값을 선택한다.
(cond [(> 2 2) (error "wrong!")]
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

;;; 양식 맞춤

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;;; 반복

;; 반복은 (꼬리-) 재귀로 한다.
(define (loop i)
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i))))
(loop 5) ; => i=5, i=6, ...

;; 이름 있는 let으로도...
(let loop ((i 0))
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i)))) ; => i=0, i=1, ...

;; Racket은 매우 유연한 `for' 형식을 가지고 있다:
(for ([i 10])
  (printf "i=~a\n" i)) ; => i=0, i=1, ...
(for ([i (in-range 5 10)])
  (printf "i=~a\n" i)) ; => i=5, i=6, ...

;;; 다른 Sequence들을 순회하는 반복
;; `for'는 여러 가지의 sequence를 순회할 수 있다:
;; 리스트, 벡터, 문자열, 집합, 해시 테이블 등...

(for ([i (in-list '(l i s t))])
  (displayln i))

(for ([i (in-vector #(v e c t o r))])
  (displayln i))

(for ([i (in-string "string")])
  (displayln i))

(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))

(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3 ))])
  (printf "key:~a value:~a\n" k v))

;;; 더 복잡한 반복

;; 여러 sequence에 대한 병렬 순회 (가장 짧은 것 기준으로 중단)
(for ([i 10] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x 1:y 2:z

;; 중첩 반복
(for* ([i 2] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x, 0:y, 0:z, 1:x, 1:y, 1:z

;; 조건
(for ([i 1000]
      #:when (> i 5)
      #:unless (odd? i)
      #:break (> i 10))
  (printf "i=~a\n" i))
; => i=6, i=8, i=10

;;; 함축
;; `for' 반복과 비슷하며, 결과만 수집한다.

(for/list ([i '(1 2 3)])
  (add1 i)) ; => '(2 3 4)

(for/list ([i '(1 2 3)] #:when (even? i))
  i) ; => '(2)

(for/list ([i 10] [j '(x y z)])
  (list i j)) ; => '((0 x) (1 y) (2 z))

(for/list ([i 1000] #:when (> i 5) #:unless (odd? i) #:break (> i 10))
  i) ; => '(6 8 10)

(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
; => '#hash((1 . "1") (2 . "2") (3 . "3"))

;; 반복의 값을 수집하는 여러 가지 방법이 있다:
(for/sum ([i 10]) (* i i)) ; => 285
(for/product ([i (in-range 1 11)]) (* i i)) ; => 13168189440000
(for/and ([i 10] [j (in-range 10 20)]) (< i j)) ; => #t
(for/or ([i 10] [j (in-range 0 20 2)]) (= i j)) ; => #t
;; 임의의 조합을 사용하려면 `for/fold'를 사용:
(for/fold ([sum 0]) ([i '(1 2 3 4)]) (+ sum i)) ; => 10
;; (이것은 명령형 반복문을 대체하기도 한다.)

;;; 예외

;; 예외를 잡으려면 `with-handlers' 형식을 사용
(with-handlers ([exn:fail? (lambda (exn) 999)])
  (+ 1 "2")) ; => 999
(with-handlers ([exn:break? (lambda (exn) "no time")])
  (sleep 3)
  "phew") ; => "phew", but if you break it => "no time"

;; 예외나 다른 값을 던지려면 `raise'를 사용
(with-handlers ([number?    ; catch numeric values raised
                 identity]) ; return them as plain values
  (+ 1 (raise 2))) ; => 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. 변경
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 기존 변수에 새 값을 할당하려면 `set!'을 사용한다.
(define n 5)
(set! n (add1 n))
n ; => 6

;; 명시적인 가변 값을 사용하려면 box 사용 (다른 언어의 포인터나 참조와 비슷함)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*) ; => 6

;; 많은 Racket 자료형은 불변이다 (쌍, 리스트 등). 그러나 어떤 것들은
;; 가변과 불변형이 둘 다 있다. (string, vector, hash table 등)

;; `vector'나 `make-vector'로 가변 벡터를 생성한다.
(define vec (vector 2 2 3 4))
(define wall (make-vector 100 'bottle-of-beer))
;; 칸을 변경하려면 vector-set!을 사용한다.
(vector-set! vec 0 1)
(vector-set! wall 99 'down)
vec ; => #(1 2 3 4)

;; 비어 있는 가변 해시 테이블을 만들고 조작한다.
(define m3 (make-hash))
(hash-set! m3 'a 1)
(hash-set! m3 'b 2)
(hash-set! m3 'c 3)
(hash-ref m3 'a)   ; => 1
(hash-ref m3 'd 0) ; => 0
(hash-remove! m3 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. 모듈
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 모듈은 코드를 여러 파일과 재사용 가능한 라이브러리로 조직하게 한다.
;; 여기서 우리는 서브-모듈을 사용한다. 이 글이 만드는 전체 모듈("lang" 줄 부터 시작)에 포함된 모듈이다.

(module cake racket/base ; racket/base 기반의 `cake' 모듈 정의

  (provide print-cake) ; 모듈이 노출(export)시키는 함수

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch) ; 내부 함수
    (printf fmt (make-string n ch))
    (newline)))

;; `require'를 사용하여 모듈에서 모든 `provide'된 이름을 사용한다.
(require 'cake) ; '는 지역 지역 서브-모듈을 위한 것이다.
(print-cake 3)
; (show "~a" 1 #\A) ; => 에러, `show'가 export되지 않았음

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
