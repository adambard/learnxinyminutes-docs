---
language: "clojure macros"
filename: learnclojuremacros-kr.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Eunpyoung Kim", "https://github.com/netpyoung"]
lang: ko-kr
---

다른 모든 Lisp와 마찬가지로, Clojure가 가진 [동형성(homoiconicity)](https://en.wikipedia.org/wiki/Homoiconic)은
 "매크로"라고 불리는 코드 생성 루틴을 작성할 수 있도록 언어의 전체적인 범위에 접근할 수 있게 해줍니다.
 매크로는 필요에 맞게 언어를 바꿀 수 있는 강력한 방법을 제공합니다.

주의하시기 바랍니다. 함수로도 충분히 해결할 수 있는 문제를 매크로로 작성하게 된다면, 좋은 코드라고 할 수 없습니다.
인자가 평가되는 시점을 제어해야 할 때만 매크로를 사용하는게 좋습니다.

Clojure랑 친해지면 쉽게 따라갈 수 있습니다. [Clojure in Y Minutes](/docs/ko-kr/clojure-kr/)를 한번 읽어보세요.

```clojure
;; defmacro로 매크로를 정의합니다.
;; 매크로는 clojure 코드로 평가될 수 있는 리스트를 반환해야 합니다.
;;
;; 다음 매크로는 (reverse "Hello World") 라고 쓴 것과 같습니다.
(defmacro my-first-macro []
  (list reverse "Hello World"))

;; macroexpand나 macroexpand-1을 사용해서 매크로의 결과를 확인할 수 있습니다.
;;
;; 호출하는 부분이 '(quote)된 것을 주목합니다.
(macroexpand '(my-first-macro))
;; -> (#<core$reverse clojure.core$reverse@xxxxxxxx> "Hello World")

;; macroexpand의 결과를 바로 평가할 수 있습니다.
(eval (macroexpand '(my-first-macro)))
; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; 하지만, 함수와 같이 좀 더 간결한 구문을 이용하는게 좋습니다:
(my-first-macro)  ; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; 더 간결한 quote 구문 ( ' )을 이용하여, 보다 편리하게 매크로 안에서 리스트를 만들 수 있습니다:
(defmacro my-first-quoted-macro []
  '(reverse "Hello World"))

(macroexpand '(my-first-quoted-macro))
;; -> (reverse "Hello World")
;; reverse는 더 이상 함수 객체가 아니라 심볼이라는 것에 주목하세요.

;; 매크로는 인자를 받을 수 있습니다.
(defmacro inc2 [arg]
  (list + 2 arg))

(inc2 2) ; -> 4

;; 하지만, quote된 리스트를 사용하면 에러가 발생합니다.
;; 인자도 quote되기 때문입니다.
;; 이를 해결하기 위해, clojure는 매크로를 quote할 수 있는 방법을 제공합니다: `.
;; ` 안에서 ~를 사용하면 외부 스코프에 접근할 수 있습니다.
(defmacro inc2-quoted [arg]
  `(+ 2 ~arg))

(inc2-quoted 2)

;; destructuring args도 사용할 수 있습니다. ~@를 사용하여 리스트 변수를 확장할 수 있습니다.
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do ~@body))) ; do를 빼먹지 마세요!

(macroexpand '(unless true (reverse "Hello World")))
;; ->
;; (if (clojure.core/not true) (do (reverse "Hello World")))

;; (unless)는 첫 번째 인자가 false일 때, body를 평가하고 반환합니다.
;; 그렇지않으면, nil을 반환합니다.
(unless true "Hello") ; -> nil
(unless false "Hello") ; -> "Hello"

;; 주의하지 않으면, 매크로는 변수를 덮어쓰는 등 큰 문제를 일으킬 수 있습니다.
(defmacro define-x []
  '(do
     (def x 2)
     (list x)))

(def x 4)
(define-x) ; -> (2)
(list x) ; -> (2)

;; 이를 피하기 위해, gensym을 이용하여 고유한 식별자를 얻을 수 있습니다.
(gensym 'x) ; -> x1281 (혹은 다른 식별자)

(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(def x 4)
(define-x-safely) ; -> (2)
(list x) ; -> (4)

;; ` 안에서 #를 사용하면 자동으로 각 심볼에 대한 gensym을 생성할 수 있습니다.
(defmacro define-x-hygienically []
  `(do
     (def x# 2)
     (list x#)))

(def x 4)
(define-x-hygienically) ; -> (2)
(list x) ; -> (4)

;; 매크로를 만들 때는 보통 헬퍼 함수를 많이 이용합니다.
;; 인라인 산술 문법을 지원하는 몇 개의 헬퍼 함수를 만들어 봅시다.
(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Given args [x (+ y)], return (+ x y)"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

;; 매크로를 만들지 않고, 바로 테스트해볼 수 있습니다.
(inline-2-helper '(a + (b - 2) - (c * 5))) ; -> (- (+ a (- b 2)) (* c 5))

; 하지만, 이 함수를 컴파일 타임에 실행하려면 매크로로 만들어야 합니다.
(defmacro inline-2 [form]
  (inline-2-helper form))

(macroexpand '(inline-2 (1 + (3 / 2) - (1 / 2) + 1)))
; -> (+ (- (+ 1 (/ 3 2)) (/ 1 2)) 1)

(inline-2 (1 + (3 / 2) - (1 / 2) + 1))
; -> 3 (실제로는 3N이라는 결과가 나옵니다. / 연산자를 사용하면 숫자가 유리수로 캐스팅되기 때문입니다.)
```

### 더 읽어볼거리

- [Writing Macros](http://www.braveclojure.com/writing-macros/)
- [Official docs](http://clojure.org/macros)
- [When to use macros?](https://lispcast.com/when-to-use-a-macro/)
