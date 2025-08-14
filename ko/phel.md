---
name: Phel
filename: learnphel.phel
contributors:
    - ["Chemaclass", "https://github.com/Chemaclass"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Phel](https://phel-lang.org/)은 PHP로 컴파일되는 함수형 프로그래밍 언어입니다.
Clojure와 Janet에서 영감을 받은 Lisp의 방언입니다.

## 특징
- PHP 생태계 기반
- 우수한 오류 보고
- 영속적인 자료구조 (리스트, 벡터, 맵, 세트)
- 매크로
- 재귀 함수
- 강력하지만 간단한 구문
- REPL

```newlisp
# 주석은 # 문자로 시작하여 줄 끝까지 이어집니다. 여러 줄 주석은 없습니다.

# Phel은 "형식(form)"으로 작성되며, 이는 공백으로 구분된
# 괄호 안의 목록일 뿐입니다.

# 파일의 첫 번째 호출은 ns여야 하며, 네임스페이스를 설정합니다.
(ns learn-phel)

# 더 기본적인 예제:

# str은 모든 인수를 사용하여 문자열을 생성합니다
(str "Hello" " " "World") #=> "Hello World"

# 수학은 간단합니다
(+ 1 1) #=> 2
(- 2 1) #=> 1
(* 1 2) #=> 2
(/ 2 1) #=> 2

# 등호는 = 입니다
(= 1 1) #=> true
(= 2 1) #=> false

# 논리에도 not이 필요합니다
(not true) #=> false

# 중첩 형식은 예상대로 작동합니다
(+ 1 (- 3 2)) # = 1 + (3 - 2) => 2

# Phel은 내부적으로 PHP를 상속하므로, `php/` 접두사를 모든 PHP 네이티브 함수에
# 사용하여 추가 비용 없이 네이티브 PHP(함수 및 클래스)를 사용할 수 있습니다.

# 타입
#############

# 불리언은 네이티브 PHP와 유사합니다

nil
true
false

# 심볼은 Phel에서 함수와 변수의 이름을 지정하는 데 사용됩니다
# 예: symbol, snake_case_symbol, my-module/my-function

# 키워드는 콜론 문자로 시작하는 심볼과 같습니다. 그러나 무엇인가의
# 이름이 아닌 상수로 사용됩니다.

:keyword
:0x0x0x
::

# Phel의 숫자는 PHP의 숫자와 동일합니다

1337 # 정수
+1337 # 양의 정수
-1337 # 음의 정수

1.234 # 부동 소수점
+1.234 # 양의 부동 소수점
-1.234 # 음의 부동 소수점
1.2e3 # 부동 소수점
7E-10 # 부동 소수점

# 문자열은 큰따옴표로 묶습니다. PHP 큰따옴표 문자열과 거의 동일하게 작동합니다.
# 문자열은 여러 줄로 작성할 수 있습니다. 그러면 줄 바꿈 문자는 리더에 의해 무시됩니다.

"hello world"

"this is\na\nstring"

"this
is
a
string."

"use backslack to escape \" string"

"the dollar must not be escaped: $ or $abc just works"


# 컬렉션 및 시퀀스
#############

# 리스트는 연결 리스트 자료구조이며, 벡터는 배열 기반입니다.
(type '(1 2 3)) #=> :list
(type [1 2 3])  #=> :vector

# 리스트는 (1 2 3)으로 작성되지만, 리더가 함수로
# 생각하지 않도록 따옴표로 묶어야 합니다.
# 또한, (list 1 2 3)은 '(1 2 3)과 같습니다.

# 범위 사이의 (지연되지 않는) 시퀀스를 생성할 수 있습니다.
(range 1 10 2) #=> (range from to step)
(take 4 (range 10))

# cons를 사용하여 리스트 시작 부분에 항목을 추가합니다.
(cons 4 '(1 2 3)) #=> (4 1 2 3)

# push를 사용하여 벡터에 항목을 추가하고 put을 사용하여 교체합니다.
(push [1 2 3] 4)  #=> (1 2 3 4)
(put [1 2 3] 1 4) #=> (1 4 3)

# concat을 사용하여 리스트나 벡터를 함께 추가합니다.
(concat [1 2] '(3 4)) #=> [1 2 3 4]

# filter, map을 사용하여 컬렉션과 상호 작용합니다.
(map inc [1 2 3])      #=> [2 3 4]
(filter even? [1 2 3]) #=> [2]

# reduce를 사용하여 축소합니다. 초기 값은 필수입니다.
(reduce + 0 [1 2 3 4])
#=> (+ (+ (+ 1 2) 3) 4)
#=> 10

(reduce push [] '(3 2 1))
#=> (push (push (push [] 3) 2) 1)
#=> [3 2 1]

# 함수
#############

# fn을 사용하여 새 함수를 만듭니다.
# 함수는 항상 마지막 문을 반환합니다.
(fn [] "Hello World") #=> <function>

# 호출하려면 추가 괄호가 필요합니다.
((fn [] "Hello World")) #=> "Hello World"

# def를 사용하여 심볼에 값을 바인딩할 수 있습니다.
(def x 1)
x #=> 1

# 변수는 변경 가능한 상태를 관리하는 방법을 제공합니다.
(def foo (var 10)) # 값 10으로 변수 정의

# 정의에 함수 할당
(def hello-world (fn [] "Hello World"))
(hello-world) #=> "Hello World"

# defn을 사용하여 이 프로세스를 단축할 수 있습니다.
(defn hello-world [] "Hello World")

# []는 함수의 인수 목록입니다.
(defn hello [name]
  (str "Hello " name))
(hello "Jens") #=> "Hello Jens"

# 이 약식 표기법을 사용하여 함수를 만들 수도 있습니다.
(def hello2 |(str "Hello " $1))
(hello2 "Anna") #=> "Hello Anna"

# 함수는 추가 인수를 seq에 묶을 수 있습니다.
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) #=> "You passed 3 args: @[1 2 3]"

# 일반 인수와 묶인 인수를 혼합할 수 있습니다.
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Jesus" 1 2) #=> "Hello Jesus, you passed 2 extra args"


# 맵
#############

# 해시 맵은 조회 속도가 빠르지만 키 순서를 유지하지 않습니다.
(type {:a 1 :b 2 :c 3})          #=> :hash-map
(type (hash-map :a 1 :b 2 :c 3)) #=> :hash-map

# 맵은 해시 가능한 모든 유형을 키로 사용할 수 있지만, 일반적으로 키워드가 가장 좋습니다.
# 키워드는 일부 효율성 보너스가 있는 문자열과 같으며 `:`으로 시작합니다.
(type :a) #=> :keyword

(def stringmap {"a" 1 "b" 2 "c" 3})
stringmap  #=> {"a" 1 "b" 2 "c" 3}

(def keymap {:a 1 :b 2 :c 3})
keymap  #=> {:a 1 :c 3 :b 2}

# 맵에서 값을 검색하려면 함수로 호출합니다.
(stringmap "a") #=> 1
(keymap :a)     #=> 1

# 키워드를 사용하여 맵에서 해당 값을 검색할 수도 있습니다!
(:b keymap) #=> 2

# 문자열로 시도하지 마십시오.
# ("a" stringmap)
# ...예외: 정의되지 않은 함수 a() 호출

# 존재하지 않는 키를 검색하면 nil이 반환됩니다.
(stringmap "d") #=> nil

# put을 사용하여 해시 맵에 새 키를 추가합니다.
(def newkeymap (put keymap :d 4))
newkeymap #=> {:a 1 :b 2 :c 3 :d 4}

# 하지만 phel 유형은 불변임을 기억하십시오!
keymap #=> {:a 1 :b 2 :c 3}

# unset을 사용하여 키를 제거합니다.
(unset keymap :a) #=> {:b 2 :c 3}

# 세트
#############

# 세트는 임의의 순서로 고유한 값을 포함합니다.

(type (set 1 2 3)) #=> :set
(set 1 2 3 1 2 3 3 2 1 3 2 1) #=> (set 1 2 3)

# push로 멤버 추가
(push (set 1 2 3) 4) #=> (set 1 2 3 4)

# unset으로 하나 제거
(unset (set 1 2 3) 1) #=> (set 2 3)

# 세트를 함수로 사용하여 존재 여부 테스트
((set 1 2 3) 1) #=> 1
((set 1 2 3) 4) #=> nil

# count, union, intersection, difference 등과 같은 더 많은 함수가 있습니다.


# 유용한 형식
#############

# phel의 `If` 조건문은 특수 형식입니다.
(if false "a" "b") #=> "b"
(if false "a") #=> nil

# let을 사용하여 임시 바인딩을 만듭니다.
(let [a 1 b 2]
  (> a b)) #=> false

# do를 사용하여 문을 함께 그룹화합니다.
(do
  (print "Hello")
  "World") #=> "World" ("Hello" 인쇄)

# 함수에는 암시적 do가 있습니다.
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") #=> "Hello Jeff" ("Saying hello to Jeff" 인쇄)

# let도 마찬가지입니다.
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) #=> "Hello Urkel" ("Saying hello to Urkel" 인쇄)

# 스레딩 매크로(-> 및 ->>)를 사용하여
# 데이터 변환을 더 명확하게 표현합니다.

# "스레드-우선" 매크로(->)는 각 형식에
# 이전 결과를 첫 번째 인수(두 번째 항목)로 삽입합니다.
(->
   {:a 1 :b 2}
   (put :c 3)  #=> (put {:a 1 :b 2} :c 3)
   (unset :b)) #=> (unset (put {:a 1 :b 2} :c 3) :b)


# 이중 화살표는 동일한 작업을 수행하지만, 각 줄의
# 결과를 형식의 *끝*에 삽입합니다. 이는 특히
# 컬렉션 작업에 유용합니다:
(->>
   (range 10)
   (map inc)      #=> (map inc (range 10))
   (filter odd?)) #=> (filter odd? (map inc (range 10)))
                  # 결과: [1 3 5 7 9]


# 이전 데이터 변환 결과를 표현식에
# 더 자유롭게 배치하려는 상황에서는 as-> 매크로를
# 사용할 수 있습니다. 이를 통해 변환 출력에 특정 이름을
# 할당하고 연결된 표현식에서 자리 표시자로 사용할 수 있습니다:

(as-> [1 2 3] input
  (map inc input)     #=> 마지막 변환의 출력을 마지막 위치에 사용할 수 있습니다.
  (get input 2)       #=> 그리고 동일한 표현식에서 두 번째 위치에,
  (push [4 5 6] input 8 9 10)) #=> 또는 중간에!
                               # 결과: [4 5 6 4 8 9 10]

# PHP
#################

# PHP에는 거대하고 유용한 표준 라이브러리가 있으며, `php/` 접두사를
# 사용하여 모든 네이티브 함수를 사용할 수 있습니다.
(php/+ 1 2 3)

# :use를 사용하면 다른 네임스페이스를 사용할 수 있습니다. PHP의 `use`와 유사합니다.
(ns my\module
  (:use \DateTimeImmutable))

# :require를 사용하여 다른 phel 파일에서 함수를 가져올 수 있습니다.
(ns my\module
  (:require phel\test :refer [deftest is]))

# 클래스 이름과 "php/new"를 사용하여 새 인스턴스를 만듭니다.
(php/new \DateTime) # <날짜-시간 객체>

# php/->를 사용하여 객체의 메서드를 호출합니다.
(def d (php/new \DateTime))
(php/-> d (getTimestamp)) # <타임스탬프>

# 한 줄로도 할 수 있습니다.
(php/-> (php/new \DateTime) (getTimestamp))

# php/::를 사용하여 정적 메서드를 호출합니다.
(php/:: \DateTimeImmutable ATOM) # <타임스탬프>
```

### 더 읽을거리

이것은 완전하지는 않지만, 시작하기에는 충분할 것입니다.

웹사이트에서 전체 문서를 읽으십시오: [https://phel-lang.org/](https://phel-lang.org/documentation/getting-started/)
