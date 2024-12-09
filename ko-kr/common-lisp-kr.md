---
language: "Common Lisp"
category: language
filename: commonlisp-kr.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
  - ["Rommel Martinez", "https://ebzzry.io"]
translators:
  - ["Eunpyoung Kim", "https://github.com/netpyoung"]
lang: ko-kr
---

커먼 리스프(Common Lisp, CL)는 다양한 산업 어플리케이션에 적합한 범용적인 멀티페러다임 언어입니다.
 프로그래밍할 수 있는 프로그래밍 언어로서 자주 언급되곤 합니다.

처음 접하기 좋은책은 [Practical Common Lisp](http://www.gigamonkeys.com/book/)입니다.
 또 다른 유명한 책으로는 최근에 나온 [Land of Lisp](http://landoflisp.com/)입니다.
 베스트 프렉틱스에 관한 [Common Lisp Recipes](http://weitz.de/cl-recipes/) 책도 최근에 출판되었습니다.

```lisp
;;;-----------------------------------------------------------------------------
;;; 0. 구문 (Syntax, 신택스)
;;;-----------------------------------------------------------------------------

;;; 일반적인 폼(form)

;;; CL은 2개의 근본이 되는 구문 요소를 가집니다: ATOM 과 S-EXPRESSION.
;;; 일반적으로, 괄호로 묶인 S-expressions를 `폼(form)`이라고 부릅니다.

10            ; atom; 그 자체로 평가됩니다
:thing        ; atom; 심볼 :thing로 평가됩니다
t             ; atom, 참을 나타냅니다
(+ 1 2 3 4)   ; s-expression
'(4 :foo t)   ; s-expression


;;; 주석

;;; 한줄주석은 세미콜론으로 시작합니다( ; )
;;; 파일 단위로는 4개, 구획(section) 설명으로는 3개, 정의(definition) 안에서는 2개,
;;; 한 라인에 대해서는 1개를 사용합니다. 예를들면,

;;;; life.lisp

;;; Foo bar baz, because quu quux. Optimized for maximum krakaboom and umph.
;;; Needed by the function LINULUKO.

(defun meaning (life)
  "LIFE의 의미를 계산하여 반환합니다."
  (let ((meh "abc"))
    ;; Invoke krakaboom
    (loop :for x :across meh
       :collect x)))                    ; 값을 x에 저장한 다음, 이를 반환합니다.


;;; 반면, 블록주석은 자유롭게 쓸 수 있습니다.
;;; #| 와 |# 로 구역을 나눌 수 있습니다.

#| 이것은 블록 주석이며,
   여러 줄로 쓸 수 있으며
    #|
       중첩하여 사용할 수 있습니다!
    |#
|#


;;; 환경

;;; 여러 종류의 구현체들이 있습니다;
;;; 대부분 표준을 따르지만, SBCL이 처음 시작하기에 좋습니다.
;;; Quicklisp를 이용하여 서드파티 라이브러리들을 쉽게 설치할 수 있습니다.

;;; CL을 개발할때 텍스트 편집기와
;;; 읽고(Read) 평가하고(Eval) 출력(Print)을 반복(Loop)하는 REPL을 동시에 활용하여 개발합니다.
;;; REPL은 프로그램이 "실행되고 있는 중(live)"에 프로그램과 상호작용을 할 수 있도록 만들어 줍니다.



;;;-----------------------------------------------------------------------------
;;; 1. 주요 데이터 타입 및 연산자
;;;-----------------------------------------------------------------------------

;;; 심볼

'foo ; => FOO  자동으로 심볼이 대문자로 된 것을 주목하시기 바랍니다.

;;; INTERN은 문자열을 심볼로 만들어 줍니다.

(intern "AAAA")        ; => AAAA
(intern "aaa")         ; => |aaa|

;;; 숫자

9999999999999999999999 ; 정수
#b111                  ; 2진수 => 7
#o111                  ; 8진수 => 73
#x111                  ; 16진수 => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; 분수
#C(1 2)                ; 복소수

;;; 함수 적용
;;; (f x y z ...)에서 f는 함수이며, x, y, z, ... 인자입니다.

(+ 1 2)                ; => 3

;;; 데이터 그 자체를 만들기 원한다면,
;;; QUOTE를 이용하여 평가를 막을 수 있습니다.

(quote (+ 1 2))        ; => (+ 1 2)
(quote a)              ; => A

;;; QUOTE을 짧게 쓰면 ( ' )입니다.

'(+ 1 2)               ; => (+ 1 2)
'a                     ; => A

;;; 기본 산술 연산자

(+ 1 1)                ; => 2
(- 8 1)                ; => 7
(* 10 2)               ; => 20
(expt 2 3)             ; => 8   ;; exponentiation: 제곱
(mod 5 2)              ; => 1   ;; modulo: 나머지 연산
(/ 35 5)               ; => 7
(/ 1 3)                ; => 1/3
(+ #C(1 2) #C(6 -4))   ; => #C(7 -2)

;;; 불리언(Boolean)

t                      ; 참  ; NIL이 아니면 참.
nil                    ; 거짓; 빈 리스트: () 역시 거짓.
(not nil)              ; => T
(and 0 t)              ; => T
(or 0 nil)             ; => 0

;;; 문자

#\A                    ; => #\A
#\λ                    ; => #\GREEK_SMALL_LETTER_LAMDA
#\u03BB                ; => #\GREEK_SMALL_LETTER_LAMDA

;;; 문자열은 고정된 길이의 배열속에 문자들이 들어있는 것입니다.

"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; 역슬래쉬(\)는 이스케이프 문자입니다.

;;; 문자열을 연결(concatenate)시킬 수 도 있습니다.

(concatenate 'string "Hello, " "world!") ; => "Hello, world!"

;;; 문자열을 마치 문자들이 나열된것처럼 취급할 수도 있습니다.

(elt "Apple" 0) ; => #\A

;;; FORMAT은 형식화된 출력을 만들기 위해 사용됩니다.
;;; 간단한 스트링 인터폴레이션(string interpolation)부터 반복문이나 조건문까지 다양한 기능을 제공합니다.
;;; FORMAT의 첫번째 인자는 포맷팅된 문자열이 어디로 갈지 결정합니다
;;; 만약 NIL이라면, FORMAT은 포맷팅된 문자열을 반환합니다;
;;; 만약 T라면, FORMAT은 표준 출력, 일반적으로 스크린에 출력한 다음 NIL을 반환합니다.

(format nil "~A, ~A!" "Hello" "world")   ; => "Hello, world!"
(format t "~A, ~A!" "Hello" "world")     ; => NIL


;;;-----------------------------------------------------------------------------
;;; 2. 변수
;;;-----------------------------------------------------------------------------

;;; DEFVAR와 DEFPARAMETER를 이용하여 전역 (동적 스코프) 변수를 만들 수 있습니다.
;;; 변수 이름은 다음을 제외한 모든 문자를 사용할 수 있습니다: ()",'`;#|\

;;; DEFVAR와 DEFPARAMETER의 차이점으로는, DEFVAR 표현식을 다시 평가하더라도 변수의 값이 변경되지 않는다는 것입니다.
;;; 반면 DEFPARAMETER는 변경됩니다.

;;; 관례상, 동적 스코프 변수는 이름에는 귀마개(earmuffs)를 씌워줍니다.

(defparameter *some-var* 5)
*some-var* ; => 5

;;; 유니코드 문자 역시 사용할 수 있습니다.
(defparameter *AΛB* nil)

;;; 이전에 바인딩되지 않은 변수에 접근하면 UNBOUND-VARIABLE 에러가 발생하지만, 이것은 정의된 동작입니다.
;;; 바인딩되지 않은 변수에는 접근하지 마세요.

;;; LET 으로 지역 바인딩을 만들 수 있습니다.
;;; 다음 코드에서 (let ...) 안에서 "dance with you"는 `me`로 바인딩됩니다.
;;; LET은 항상 LET 폼의 마지막 `form`의 값을 반환합니다.

(let ((me "dance with you")) me) ; => "dance with you"


;;;-----------------------------------------------------------------------------;
;;; 3. 구조체와 컬렉션
;;;-----------------------------------------------------------------------------;


;;; 구조체

(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover*            ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)
(dog-p *rover*)    ; => T
(dog-name *rover*) ; => "rover"

;;; DOG-P, MAKE-DOG, DOG-NAME은 모두 DEFSTRUCT에 의해 자동으로 생성됩니다.


;;; 페어(쌍, Pair)

;;; CONS는 페어를 만듭니다. CAR와 CDR은 페어의 head와 tail을 반환합니다.

(cons 'SUBJECT 'VERB)         ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB))   ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB))   ; => VERB


;;; 리스트

;;; 리스트는 CONS 페어로 만들어진 링크드 리스트 데이터 구조입니다.
;;; 리스트의 끝은 NIL (또는 '())로 표시됩니다.

(cons 1 (cons 2 (cons 3 nil)))     ; => '(1 2 3)

;;; LIST는 리스트를 위한 가변인자 생성자입니다.

(list 1 2 3)                       ; => '(1 2 3)

;;; CONS에 첫번째 인자가 원자이고 두번째 인자는 리스트일때,
;;; CONS는 새로운 CONS-페어를 반환하게 되는데,
;;; 첫번째 인자를 첫번째 아이템으로, 두번째 인자를 CONS-페어의 나머지로 하게됩니다.

(cons 4 '(1 2 3))                  ; => '(4 1 2 3)

;;; APPEND를 사용하여 리스트를 연결할 수 있습니다.

(append '(1 2) '(3 4))             ; => '(1 2 3 4)

;;; 또는 CONCATENATE

(concatenate 'list '(1 2) '(3 4))  ; => '(1 2 3 4)

;;; 리스트는 매우 중요한 타입이며, 다양한 기능들이 있습니다.
;;; 몇 가지 예를 들어보겠습니다:

(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => NIL
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; 벡터

;;; 벡터는 고정길이 배열입니다.

#(1 2 3) ; => #(1 2 3)

;;; CONCATENATE를 사용하여 벡터를 연결할 수 있습니다.

(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)


;;; 배열

;;; 벡터와 스트링은 배열의 특이 케이스입니다.

;;; 2차원 배열

(make-array (list 2 2))         ; => #2A((0 0) (0 0))
(make-array '(2 2))             ; => #2A((0 0) (0 0))
(make-array (list 2 2 2))       ; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;;; 주의: MAKE-ARRAY의 기본 초기값은 구현체에 따라 다릅니다.
;;; 명시적으로 지정하려면 다음과 같이하면 됩니다:

(make-array '(2) :initial-element 'unset)  ; => #(UNSET UNSET)

;;; 1, 1, 1에 있는 요소에 접근하기:

(aref (make-array (list 2 2 2)) 1 1 1)     ;  => 0
;;; 반환되는 값은 구현체에 따라 다릅니다:
;;; SBCL과 CCL에서는 0, ECL에서는 NIL

;;; 조절 가능한 벡터(adjustable vector)

;;; 조절 가능한 벡터는 고정길이 벡터와 동일한 출력 결과를 갖습니다.

(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
                                   :adjustable t :fill-pointer t))
*adjvec* ; => #(1 2 3)

;;; 새로운 요소 추가하기

(vector-push-extend 4 *adjvec*)   ; => 3
*adjvec*                          ; => #(1 2 3 4)


;;; 셋(Set)은 단순히 리스트입니다:

(set-difference '(1 2 3 4) '(4 5 6 7))   ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7))     ; => 4
(union '(1 2 3 4) '(4 5 6 7))            ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))                    ; => (1 2 3 4)

;;; 하지만, 많은 데이터 셋을 다룰 경우,
;;; 링크드리스트 보다 더 나은 데이터 구조를 필요로 할 것입니다.

;;; 딕션어리는 해쉬태이블로 만들어졌습니다.

;;; 해쉬 테이블 만들기

(defparameter *m* (make-hash-table))

;;; 값 설정

(setf (gethash 'a *m*) 1)

;;; 값 받아오기

(gethash 'a *m*) ; => 1, T

;;; CL의 표현식은 여러개의 값을 반환 할 수 있습니다.

(values 1 2) ; => 1, 2

;;; MULTIPLE-VALUE-BIND로 연결(bind)지을 수 있습니다.

(multiple-value-bind (x y)
    (values 1 2)
  (list y x))

; => '(2 1)

;;; GETHASH는 여러 값을 반환하는 함수의 예입니다.
;;; 첫번째 반환값은 해쉬 테이블의 키의 값입니다;
;;; 만약 키가 발견되지 않으면 NIL을 반환합니다.

;;; 두번째 반환값은 키가 해쉬 테이블에 존재하는지 여부를 결정합니다.
;;; 테이블에서 키를 찾지 못하면 NIL을 반환합니다.
;;; 이러한 동작은 키의 값이 실제로 NIL인지 확인할 수 있도록 해줍니다.

(gethash 'd *m*) ;=> NIL, NIL

;;; 키가 없을때를 대비한 기본값을 설정할 수 있습니다;

(gethash 'd *m* :not-found) ; => :NOT-FOUND

;;; 반환된 값들을 처리해보겠습니다.

(multiple-value-bind (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)


;;;-----------------------------------------------------------------------------
;;; 3. 함수
;;;-----------------------------------------------------------------------------

;;; 익명 함수를 만들기 위해 LAMBDA를 사용합니다.
;;; 함수는 항상 마지막 표현식의 값을 반환합니다.
;;; 함수의 출력방식은 구현체마다 다릅니다.

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;;; 익명 함수를 호출하기 위해 FUNCALL을 사용합니다.

(funcall (lambda () "Hello World"))   ; => "Hello World"
(funcall #'+ 1 2 3)                   ; => 6

;;; 리스트의 앞에 lambda표현식을 넣으면, 암시적으로 FUNCALL을 호출합니다.

((lambda () "Hello World"))           ; => "Hello World"
((lambda (val) val) "Hello World")    ; => "Hello World"

;;; 인자가 미리 주어져 있으면 FUNCALL을 사용하고, 그렇지 않으면 APPLY를 사용합니다.

(apply #'+ '(1 2 3))   ; => 6
(apply (lambda () "Hello World") nil) ; => "Hello World"

;;; 함수에 이름을 붙이려면 DEFUN을 사용합니다.

(defun hello-world () "Hello World")
(hello-world) ; => "Hello World"

;;; 위 정의에서 ()는 인자 리스트입니다.

(defun hello (name) (format nil "Hello, ~A" name))
(hello "Steve") ; => "Hello, Steve"

;;; 함수는 선택적(optional) 인자를 가질 수 있습니다; 기본값은 NIL입니다.

(defun hello (name &optional from)
  (if from
      (format t "Hello, ~A, from ~A" name from)
      (format t "Hello, ~A" name)))

(hello "Jim" "Alpacas")       ; => Hello, Jim, from Alpacas

;;; 기본값을 다음과 같이 지정할 수도 있습니다.

(defun hello (name &optional (from "The world"))
   (format nil "Hello, ~A, from ~A" name from))

(hello "Steve")               ; => Hello, Steve, from The world
(hello "Steve" "the alpacas") ; => Hello, Steve, from the alpacas

;;; 함수는 키워드 인자를 이용하여 위치와 상관없는 인자를 가질 수도 있습니다.

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format t "Hello, ~A ~A, from ~A" honorific name from))

(generalized-greeter "Jim")
; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer


;;;-----------------------------------------------------------------------------
;;; 4. 동등성(Equality)
;;;-----------------------------------------------------------------------------

;;; CL은 정교한 동등성 시스템을 가지고 있습니다.
;;; 그 중 일부를 여기서 다뤄보도록 하겠습니다.

;;; 숫자에 대해서는 ( = )를 사용합니다.
(= 3 3.0)               ; => T
(= 2 1)                 ; => NIL

;;; 객체 식별에 대해서는 EQL을 사용합니다.
(eql 3 3)               ; => T
(eql 3 3.0)             ; => NIL
(eql (list 3) (list 3)) ; => NIL

;;; 리스트, 스트링, 비트-벡터에 대해서는 EQUAL을 사용합니다.
(equal (list 'a 'b) (list 'a 'b)) ; => T
(equal (list 'a 'b) (list 'b 'a)) ; => NIL


;;;-----------------------------------------------------------------------------
;;; 5. 제어 흐름(Control Flow)
;;;-----------------------------------------------------------------------------

;;; 조건문

(if t             ; 구문: 조건
    "참입니다"    ; 구문: 그러면
    "거짓입니다") ; 구문: 그렇지 않으면
; => "참입니다"

;;; 조건문에서, NIL이 아닌 모든 값은 참으로 취급됩니다.

(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;;; COND는 일련의 테스트를 실행하며, 결과를 선택합니다.
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;;; TYPECASE는 값의 타입에 따라 분기합니다.
(typecase 1
  (string :string)
  (integer :int))
; => :int


;;; 루프(loop)

;;; 재귀(recursion)

(defun fact (n)
  (if (< n 2)
      1
    (* n (fact(- n 1)))))

(fact 5) ; => 120

;;; 반복(iteration)

(defun fact (n)
  (loop :for result = 1 :then (* result i)
     :for i :from 2 :to n
     :finally (return result)))

(fact 5) ; => 120

(loop :for x :across "abcd" :collect x)
; => (#\a #\b #\c #\d)

(dolist (i '(1 2 3 4))
  (format t "~A" i))
; => 1234


;;;-----------------------------------------------------------------------------
;;; 6. 변경(Mutation)
;;;-----------------------------------------------------------------------------

;;; 기존 변수에 새 값을 할당하기 위해선 SETF를 사용합니다.
;;; 해쉬 테이블 예제에서도 한번 나왔었습니다.

(let ((variable 10))
    (setf variable 2))
; => 2

;;; 좋은 리스프 스타일은 파괴적인 함수의 사용을 최소화하고,
;;; 변경을 되도록 피하는 것입니다.

;;;-----------------------------------------------------------------------------
;;; 7. 클래스와 객체
;;;-----------------------------------------------------------------------------

;;; 더 이상 animal 클래스는 없습니다.
;;; 인간을 동력수단으로 삼는 운송기계
;;; (Human-Powered Mechanical Conveyances)를 만들어보겠습니다.

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;;; DEFCLASS의 인자의 순서는 다음과 같습니다:
;;; 1. 클래스 이름
;;; 2. 슈퍼클래스 목록
;;; 3. 슬롯 목록
;;; 4. 선택적 지정자

;;; 이 때 슈퍼클래스 목록이 설정되지 않으면, 빈 목록이 표준 객체 클래스로 기본 설정됩니다.
;;; 이것은 변경할 수도 있지만, 어떻게 돌아가는지 알기전에는 변경하지 않습니다.
;;; 그러면, 메타오브젝트 프로토콜의 예술(Art of the Metaobject Protocol)에 대해 좀 더 살펴보도록 하겠습니다.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

;;; REPL에서 HUMAN-POWERED-CONVEYANCE 클래스에 대해 DESCRIBE를 호출하면 다음과 같은 결과를 얻게됩니다:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;;; 주목할 점은 리플렉션이 가능하다는 것입니다.
;;; CL은 대화형 시스템으로 설계되었습니다.

;;; 메서드를 정의하기 앞서, 자전거 바퀴의 둘레가 얼마나 되는 공식을 살펴봅시다:
;;; C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;;; PI는 CL에 미리 정의되어 있습니다.



;;; 카누에 있는 노 젓는 사람들의 수의 효율성 값이 대략 로그함수적이라는 것을 안다고 가정해봅시다.
;;; 이와 같은 정보는 생성자/초기화자에서 설정하는 것이 좋습니다.

;;; CL이 인스턴스를 생성한 다음에(after), 인스턴스를 초기화하기 위해선:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;;; 그런 다음 인스턴스를 생성하고, 평균 효율을 확인합니다...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887


;;;-----------------------------------------------------------------------------
;;; 8. 매크로
;;;-----------------------------------------------------------------------------

;;; 매크로는 언어의 구문을 확장할 수 있게 해줍니다.
;;; CL에는 WHILE 루프가 없지만, 새로 작성하는 것은 간단합니다.
;;; 어셈블러의 명령어를 따라가자면, 다음과 같이 될 것입니다:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        `(tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))

;;; 좀 더 고수준 버전을 살펴보겠습니다:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;;; 하지만, 현대의 컴파일러에서는 이것이 필요하지 않습니다;
;;; LOOP 폼은 동일하게 잘 컴파일되며 읽기 쉽습니다.

;;; Note that ``` is used, as well as `,` and `@`. ``` is a quote-type operator
;;; known as quasiquote; it allows the use of `,` . `,` allows "unquoting"
;;; variables. @ interpolates lists.

;;; GEMSYM은 다른 곳에서 사용되지 않는 것이 보장된 시스템에서 유일한 심볼을 생성합니다.
;;; 컴파일 타임에 매크로가 확장되는데, 매크로에서 선언된 변수가
;;; 일반 코드에서 사용되는 변수와 충돌할 가능성이 있기 때문입니다.

;;; 매크로에 대해 더 자세한 정보를 얻고 싶으시다면, Practical Common Lisp와 On Lisp를 살펴보시기 바랍니다.
```

## 더 읽어볼거리

- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/book.pdf)

## 추가 정보

- [CLiki](http://www.cliki.net/)
- [common-lisp.net](https://common-lisp.net/)
- [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl)
- [Lisp Lang](http://lisp-lang.org/)

## 크레딧

Scheme 사용자들의 노고에 큰 감사를 드립니다. 좋은 시작점을 만들어 주신 덕분에 쉽게 Common Lisp로 옮길 수 있었습니다.

- 훌륭한 리뷰를 해주신 [Paul Khuong](https://github.com/pkhuong)
