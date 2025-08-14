---
name: LB Stanza
filename: learn-stanza.stanza
contributors:
  - ["Mike Hilgendorf", "https://github.com/m-hilgendorf"]
---

LB Stanza(또는 줄여서 Stanza)는 캘리포니아 대학교 버클리에서 만든 새로운 선택적 타입 범용 프로그래밍 언어입니다. Stanza는 프로그래머가 대규모 프로그램 아키텍처의 복잡성을 해결하고 전체 소프트웨어 개발 수명 주기 동안 애플리케이션 프로그래머의 생산성을 크게 향상시키는 데 도움이 되도록 설계되었습니다.


```
; 이것은 주석입니다.
;<A>
이것은 블록 주석입니다.
    ;<B>
        블록 주석은 선택적 태그로 중첩될 수 있습니다.
    ;<B>
;<A>
defpackage learn-stanza-in-y:
  import core
  import collections

;==============================================================================
; 대부분의 프로그래밍 언어에서 볼 수 있는 기본 사항
;==============================================================================


; 변수는 가변(var) 또는 불변(val)일 수 있습니다.
val immutable = "this string can't be changed"
var mutable = "this one can be"
mutable = "like this"

; 기본 데이터 타입 (주석은 선택 사항)
val an-int: Int = 12345
val a-long: Long = 12345L
val a-float: Float = 1.2345f
val a-double: Double = 3.14159
val a-string: String = "this is a string"
val a-multiline-string = \<tag>
    this is a "raw" string literal
\<tag>

; println과 "..." % [...]를 사용하여 서식화된 문자열 출력
println("this is a formatted string %_ %_" % [mutable, immutable])

; Stanza는 선택적 타입이며, ? (any) 타입을 가집니다.
var anything:? = 0
anything = 3.14159
anything = "a string"

; Stanza에는 튜플, 배열, 벡터 및 해시 테이블과 같은 기본 컬렉션이 있습니다.
val tuple: Tuple<?> = [mutable, immutable]

val array = Array<?>(3)
array[0] = "string"
array[1] = 1
array[2] = 1.23455
; array[3] = "out-of-bounds" ; 배열은 경계 검사를 합니다.

val vector = Vector<?>()
vector[0] = "string"
vector[1] = 1
vector[2] = 3.14159

val hash-table = HashTable<String, ?>()
hash-table["0"] = 0
hash-table["1"] = 1
hash-table["2"] = 1


;==============================================================================
; 함수
;==============================================================================
; 함수는 `defn` 키워드로 선언됩니다.
defn my-function (arg:?) : ; 식별자와 인수 목록 사이에 공백이 있습니다.
  println("called my-function with %_" % [arg])

my-function("arg")  ; 함수를 호출할 때 공백이 없습니다.

; 함수는 다른 함수 내부에 선언될 수 있으며,
; 주변 환경의 변수를 캡처할 수 있습니다.
defn outer (arg):
  defn inner ():
    println("outer had arg: %_" % [arg])
  inner()

outer("something")

; 함수는 stanza에서 "일급"입니다. 즉, 변수를
; 함수에 할당하고 함수를 다른 함수에 인수로 전달할 수 있습니다.
val a-function = outer
defn do-n-times (arg, func, n:Int):
  for i in 0 to n do :
    func(arg)
do-n-times("argument", a-function, 3)

; 때로는 함수를 인라인으로 정의하거나 익명 함수를 사용하고 싶을 때가 있습니다.
; 이를 위해 다음 구문을 사용할 수 있습니다.
;   fn (args):
;       ...
do-n-times("hello", fn (arg): println(arg), 2)

; 익명 함수를 작성하는 약식 구문이 있습니다.
do-n-times("hello", { println(_) }, 2)

; 약식 구문은 여러 인수에도 작동합니다.
val multi-lambda = { println(_ + 2 * _) }
multi-lambda(1, 2)

;==============================================================================
; 사용자 정의 타입
;==============================================================================
; 구조체는 `defstruct` 키워드로 선언됩니다.
defstruct MyStruct:
  field

; 생성자는 자동으로 파생됩니다.
val my-struct = MyStruct("field:value")

; 필드는 함수 호출 구문을 사용하여 액세스됩니다.
println(field(my-struct))

; Stanza는 메서드 오버로딩을 기반으로 한 "다중 메서드" 시스템으로
; 서브타이핑을 지원합니다.
deftype MyType
defmulti a-method (m:MyType)

defstruct Foo <: MyType
defstruct Bar <: MyType
defmethod a-method (a-foo: Foo):
  println("called a-method on a Foo")

defmethod a-method (a-foo: Bar):
  println("called a-method on a Bar")

;==============================================================================
; 타입 시스템
;==============================================================================
; True와 False는 단일 값을 가진 타입입니다.
val a-true: True = true
val a-false: False = false

; 유니온 타입을 선언할 수 있습니다. 즉, 여러 타입 중 하나의 값을 가질 수 있습니다.
val a-boolean: True|False = true
val another-boolean: True|False = false

; 타입에 대해 패턴 매칭을 할 수 있습니다.
match(a-boolean):
  (t:True): println("is true")
  (f:False): println("is false")

; 단일 가능한 타입에 대해 매칭할 수 있습니다.
match(a-boolean:True):
  println("is still true")
else:
  println("is not true")

; 변수의 타입을 중심으로 프로그램 로직을 구성할 수 있습니다.
if anything is Float :
  println("anything is a float")
else if anything is-not String :
  println("anything is not an int")
else :
  println("I don't know what anything is")

;==============================================================================
; 제어 흐름
;==============================================================================
; stanza에는 표준 기본 제어 흐름이 있습니다.
val condition = [false, false]
if condition[0] :
  ; 무언가 수행
  false
else if condition[1] :
  ; 다른 작업 수행
  false
else :
  ; 그 외 모든 것
  false

; 값에 대해 패턴 매칭하는 데 사용할 수 있는 switch 문도 있습니다.
; (타입과 반대)
switch(anything):
  "this": false
  "that": false
  "the-other-thing": false
  else: false

; for 및 while 루프가 지원됩니다.
while condition[0]:
  println("do stuff")

for i in 0 to 10 do:
  vector[i] = i

; stanza는 break 또는 return 문으로 작동할 수 있는
; 명명된 레이블도 지원합니다.
defn another-fn ():
  label<False> return:
    label<False> break:
      while true:
        if condition[0] is False:
            break(false)
    return(false)

; Stanza의 고급 제어 흐름에 대한 포괄적인 가이드는
; Stanza-by-Example의 이 페이지를 확인하십시오: http://lbstanza.org/chapter9.html

;==============================================================================
; 시퀀스
;==============================================================================
; "for" 루프는 더 강력한 구문의 설탕입니다.
val xs = [1, 2, 3]
val ys = ['a', 'b', 'c']
val zs = ["foo", "bar", "baz"]

for (x in xs, y in ys, z in zs) do :
  println("x:%_, y:%_, z:%_" % [x, y, z])


;xs, ys, zs는 모두 "Seqable"입니다. 즉, Seq 타입(시퀀스)입니다.
; `do` 식별자는 for 루프의 본문을 시퀀스의 각 요소에
; 적용하는 특수 함수입니다.
;
; 일반적인 시퀀스 작업은 시퀀스를 연결하는 것입니다. 이것은
; `seq-cat` 함수를 사용하여 수행됩니다. 이것은 반복자를 "평탄화"하는 것과
; 유사합니다.
val concat = to-tuple $
  for sequence in [xs, ys, zs] seq-cat:
    sequence

; 여러 시퀀스의 요소를 인터리브하는 변형을 사용할 수도 있습니다.
val interleaved = to-tuple $
  for (x in xs, y in ys, z in zs) seq-cat :
    [x, y, z]

println("[%,] [%,]" % [concat, interleaved])

; 또 다른 일반적인 작업은 시퀀스를 다른 시퀀스에 매핑하는 것입니다. 예를 들어
; 숫자 목록의 모든 요소를 상수로 곱하는 것입니다. 이를 위해 `seq`를 사용합니다.
var numbers = [1.0, 2.0, 3.0, 4.0]
numbers = to-tuple $
  for n in numbers seq :
    2.0 * n
println("%," % [numbers])

if find({_ == 2.0}, numbers) is-not False :
  println("found it!")

; 또는 시퀀스에 무언가 있는지 알고 싶을 수도 있습니다.
var is-there =
  for n in numbers any? :
    n == 2.0

; 이것은 "구문 설탕"이므로
; 익명 함수를 사용하여 명시적으로 작성할 수 있습니다.
is-there = any?({_ == 2.0}, numbers)

; 시퀀스 라이브러리 및 다양한 어댑터에 대한 자세한 참조는
; 여기에서 찾을 수 있습니다: http://lbstanza.org/reference.html#anchor439


=========================================================================
; 문서
;=========================================================================
;
; 최상위 문에는 문자열 값을 사용하는 "doc" 필드가 접두사로 붙을 수 있으며,
; 패키지에 대한 문서를 자동으로 생성하는 데 사용됩니다.
doc: \<doc>
    # 문서 문자열

    ```
    val you-can = "include code snippets, too"
    ```

    문서를 마크다운(mdbook과 호환)으로 렌더링하려면

    ```bash
    stanza doc source.stanza -o docs
    ```
\<doc>
defn docfn () : false
```
