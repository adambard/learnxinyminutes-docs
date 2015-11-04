---
language: clojure
filename: learnclojure-kr.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["netpyoung", "http://netpyoung.github.io/"]
lang: ko-kr
---

Clojure는 Java 가상머신을 위해 개발된 Lisp 계통의 언어입니다
이는 Common Lisp보다 순수 [함수형 프로그래밍](https://en.wikipedia.org/wiki/Functional_programming)을 더욱 강조했으며, 
상태를 있는 그대로 다루기 위해 다양한 [STM](https://en.wikipedia.org/wiki/Software_transactional_memory) 을 지원하는 프로그램들을 갖췄습니다.

이를 조합하여, 병행처리(concurrent processing)를 매우 단순하게 처리할 수 있으며,
대게 자동으로 처리될 수 있도록 만들 수 있습니다.

(Clojure 1.2 이상의 버전이 필요로 합니다.)


```clojure
; 주석은 세미콜론(;)으로 시작합니다.

; Clojure는 "폼(forms)"으로 구성되었으며,
; 폼은 괄호로 감싸져있으며, 공백으로 구분된 것들이 나열된 것입니다.
;
; clojure의 reader는 첫번째로 오는 것을
; 함수 혹은 매크로를 호출하는 것, 그리고 나머지를 인자라고 가정합니다.

; namespace를 지정하기 위해, 파일에서 우선적으로 호출해야될 것은 ns입니다.
(ns learnclojure)

; 간단한 예제들:

; str 은 인자로 받은 것들을 하나의 문자열로 만들어줍니다.
(str "Hello" " " "World") ; => "Hello World"

; 직관적인 수학 함수들을 갖고 있습니다.
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; = 로 동일성을 판별할 수 있습니다.
(= 1 1) ; => true
(= 2 1) ; => false

; 논리연산을 위한 not 역시 필요합니다.
(not true) ; => false

; 중첩된 폼(forms)은 기대한대로 동작합니다.
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; 타입
;;;;;;;;;;;;;

; Clojure는 부울(boolean), 문자열, 숫자를 위해 Java의 object 타입을 이용합니다.
; `class` 를 이용하여 이를 확인할 수 있습니다.
(class 1) ; 정수는 기본적으로 java.lang.Long입니다.
(class 1.); 소수는 java.lang.Double입니다.
(class ""); 문자열은 쌍따옴표로 감싸져 있으며, java.lang.String입니다.
(class false) ; 부울값은 java.lang.Boolean입니다.
(class nil); nil은 "null"값입니다.

; 데이터 리스트 자체를 만들고자 한다면,
; '를 이용하여 평가(evaluate)되지 않도록 막아야 합니다.
'(+ 1 2) ; => (+ 1 2)
; (quote (+ 1 2)) 를 줄여서 쓴것

; quote 가 된 리스트를 평가할 수 도 있습니다.
(eval '(+ 1 2)) ; => 3

; 컬렉션(Collections) & 시퀀스(Sequences)
;;;;;;;;;;;;;;;;;;;

; 리스트(List)는 연결된(linked-list) 자료구조이며, 벡터(Vector)는 배열이 뒤로붙는(array-backed) 자료구조입니다.
; 리스트와 벡터 모두 java 클래스입니다!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; 간단하게 (1 2 3)로 리스트를 나타낼 수 있지만,
; reader가 함수라고 여기지 못하게 quote(')를 해줘야 합니다.
; 따라서, (list 1 2 3)는 '(1 2 3)와 같습니다.

; "컬렉션"은 단순하게 데이터의 그룹입니다.
; 리스트와 벡터 모두 컬렉션입니다:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "시퀀스" (seq) 는 데이터 리스트를 추상적으로 기술한 것입니다.
; 리스트는 시퀀스입니다.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; 시퀀스는 접근하고자 하는 항목만 제공해주면 됩니다.
; 따라서, 시퀀스는 lazy 할 수 있습니다 -- 무한하게 늘어나는 것을 정의할 수 있습니다:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (an infinite series)
(take 4 (range)) ;  (0 1 2 3)

; cons 를 이용하여 리스트나 벡터의 시작부에 항목을 추가할 수 있습니다.
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; conj 는 컬렉션에 가장 효율적인 방식으로 항목을 추가합니다.
; 리스트는 시작부분에 삽입하고, 벡터는 끝부분에 삽입합니다.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; concat 을 이용하여 리스트와 벡터를 서로 합칠 수 있습니다.
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; filter, map 을 이용하여 컬렉션을 다룰 수 있습니다.
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; reduce 를 이용하여 줄여나갈 수 있습니다.
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; reduce 는 초기 값을 인자로 취할 수 도 있습니다.
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; 함수
;;;;;;;;;;;;;;;;;;;;;

; fn 을 이용하여 함수를 만들 수 있습니다 .
; 함수는 항상 마지막 문장을 반환합니다.
(fn [] "Hello World") ; => fn

; (정의한 것을 호출하기 위해선, 괄호가 더 필요합니다.)
((fn [] "Hello World")) ; => "Hello World"

; def 를 이용하여 var 를 만들 수 있습니다.
(def x 1)
x ; => 1

; var 에 함수를 할당시켜보겠습니다.
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; defn 을 이용하여 짧게 쓸 수 도 있습니다.
(defn hello-world [] "Hello World")

; [] 는 함수의 인자 목록을 나타냅니다.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; 약자(shorthand)를 써서 함수를 만들 수 도 있습니다:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; 함수가 다양한 인자를 받도록 정의할 수 도 있습니다.
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; 함수는 여러 인자를 시퀀스로 취할 수 있습니다.
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; 개별적으로 받는 것과, 시퀀스로 취하는 것을 같이 쓸 수 도 있습니다.
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; 맵(Maps)
;;;;;;;;;;

; 해쉬맵(hash map)과 배열맵(array map)은 공통된 인터페이스를 공유합니다.
; 해쉬맵은 찾기가 빠르지만, 키의 순서가 유지되지 않습니다.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; 배열맵은 여러 연산을 거쳐 자연스레 해쉬맵이 됩니다.
; 만일 이게 커진다 하더라도, 걱정할 필요가 없습니다.

; 맵은 해쉬가 가능한 타입이라면 어떠한 것이든 키로써 활용이 가능하지만, 보통 키워드를 이용하는 것이 가장 좋습니다.
; 키워드(Keyword)는 문자열과 비슷하지만, 보다 효율적인 면이 있습니다.
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; 여기서, 쉽표가 공백으로 취급되며, 아무 일도 하지 않는다는 것을 주목하시기 바랍니다.

; 맵에서 값을 얻어오기 위해선, 함수로써 맵을 호출해야 합니다.
(stringmap "a") ; => 1
(keymap :a) ; => 1

; 키워드 역시 맵에서 함수를 얻어올 때 사용할 수 있습니다!
(:b keymap) ; => 2

; 하지만, 문자열로는 하면 안됩니다.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; 없는 값을 얻어오고자 하면, nil이 반환됩니다.
(stringmap "d") ; => nil

; assoc 를 이용하여 해쉬맵에 새로운 키를 추가할 수 있습니다.
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; 하지만, 변경할 수 없는(immutable) clojure 타입이라는 것을 기억해야 합니다!
keymap ; => {:a 1, :b 2, :c 3}

; dissoc 를 이용하여 키를 제거할 수 있습니다.
(dissoc keymap :a :b) ; => {:c 3}

; 쎗(Set:집합)
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; conj 로 항목을 추가할 수 있습니다.
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; disj 로 제거할 수 도 있습니다.
(disj #{1 2 3} 1) ; => #{2 3}

; 존재하는지 확인할 목적으로, 쎗을 함수로 사용할 수 도 있습니다.
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; clojure.sets 네임스페이스(namespace)에는 더 많은 함수들이 있습니다.

; 유용한 폼(forms)
;;;;;;;;;;;;;;;;;

; clojure에선, if 와 매크로(macro)를 가지고,
; 다른 여러 논리 연산들을 만들 수 있습니다.
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; let 을 이용하여 임시적으로 바인딩(binding)을 구축할 수 있습니다.
(let [a 1 b 2]
  (> a b)) ; => false

; do 로 문단을 묶을 수 도 있습니다.
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; 함수는 암시적으로 do 를 가지고 있습니다.
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; let 역시 그러합니다.
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; 모듈(Modules)
;;;;;;;;;;;;;;;

; "use" 를 이용하여 module에 있는 모든 함수들을 얻어올 수 있습니다.
(use 'clojure.set)

; 이제 쎗(set:집합)연산을 사용 할 수 있습니다.
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; 함수들 중에 일 부분만을 가져올 수 도 있습니다.
(use '[clojure.set :only [intersection]])

; require 를 이용하여 모듈을 import할 수 있습니다.
(require 'clojure.string)

; / 를 이용하여 모듈에 있는 함수를 호출 할 수 있습니다.
; 여기, clojure.string 라는 모듈에, blank? 라는 함수가 있습니다.
(clojure.string/blank? "") ; => true

; import시, 모듈에 짧은 이름을 붙여줄 수 있습니다.
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" denotes a regular expression literal)

; :require 를 이용하여, 네임스페이스에서 require 를 사용할 수 있습니다.
; 아레와 같은 방법을 이용하면, 모듈을 quote하지 않아도 됩니다.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java는 유용한 많은 표준 라이브러리를 가지고 있으며,
; 이를 어떻게 활용할 수 있는지 알아보도록 하겠습니다.

; import 로 java 모듈을 불러올 수 있습니다.
(import java.util.Date)

; ns 와 함께 import 를 할 수 도 있습니다.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; 새로운 인스턴스를 만들기 위해선, 클래스 이름 끝에 "."을 찍습니다.
(Date.) ; <a date object>

; . 을 이용하여 메소드를 호출할 수 있습니다.
; 아니면, 줄여서 ".메소드"로도 호출 할 수 있습니다.
(. (Date.) getTime) ; <a timestamp>
(.getTime (Date.)) ; exactly the same thing.

; / 를 이용하여 정적메소드를 호출 할 수 있습니다.
(System/currentTimeMillis) ; <a timestamp> (system is always present)

; doto 를 이용하여 상태가 변하는(mutable) 클래스들을 좀 더 편하게(tolerable) 다룰 수 있습니다.
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory 는 clojure가 영구적인(persistent) 상태를 다루는 방식입니다.
; clojure가 이용하는 몇몇 자료형(construct)이 있습니다.

; 가장 단순한 것은 atom 입니다. 초기 값을 넣어보도록 하겠습니다.
(def my-atom (atom {}))

; swap! 으로 atom을 갱신(update)할 수 있습니다!
; swap! 은 함수를 인자로 받아, 그 함수에 대해 현재 atom에 들어있는 값을 첫번째 인자로,
; 나머지를 두번째 인자로 하여 호출합니다.
(swap! my-atom assoc :a 1) ; Sets my-atom to the result of (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Sets my-atom to the result of (assoc {:a 1} :b 2)

; '@' 를 이용하여 atom을 역참조(dereference)하여 값을 얻을 수 있습니다.
my-atom  ;=> Atom<#...> (atom 객체가 반환됩니다.)
@my-atom ; => {:a 1 :b 2}

; 여기 atom을 이용한 단순한 카운터가 있습니다.
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; STM을 구성하는 다른 것들에는 ref 와 agent 가 있습니다.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### 읽어볼거리

부족한 것이 많았지만, 다행히도 채울 수 있는 것들이 많이 있습니다.

Clojure.org에 많은 문서들이 보관되어 있습니다:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org는 core 함수들에 대해 다양한 예제와 문서를 보유하고 있습니다:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure는 clojure/FP 스킬을 올릴 수 있는 좋은 길입니다:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org는 많고 많은 문서들을 보유하고 있습니다:
[http://clojure-doc.org/](http://clojure-doc.org/)
