---
name: "Standard ML"
filename: standardml.sml
contributors:
    - ["Simon Shine", "https://simonshine.dk/"]
    - ["David Pedersen", "https://github.com/davidpdrsn"]
    - ["James Baker", "http://www.jbaker.io/"]
    - ["Leo Zovic", "http://langnostic.inaimathi.ca/"]
    - ["Chris Wilson", "http://sencjw.com/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Standard ML은 타입 추론과 일부 부수 효과가 있는 함수형 프로그래밍 언어입니다. Standard ML을 배우는 데 어려운 부분 중 일부는 재귀, 패턴 매칭, 타입 추론(올바른 타입을 추측하지만 암시적 타입 변환은 절대 허용하지 않음)입니다. Standard ML은 변수를 업데이트할 수 있는 참조를 포함하여 Haskell과 구별됩니다.

```ocaml
(* Standard ML의 주석은 (*로 시작하고 *)로 끝납니다. 주석은
   중첩될 수 있습니다. 즉, 모든 (* 태그는 *) 태그로 끝나야 합니다. 이 주석은
   예를 들어 두 개의 중첩된 주석을 포함합니다. *)

(* Standard ML 프로그램은 선언으로 구성됩니다. 예: 값 선언: *)
val rent = 1200
val phone_no = 5551337
val pi = 3.14159
val negative_number = ~15  (* 예, 단항 마이너스는 '물결표' 기호를 사용합니다 *)

(* 선택적으로 타입을 명시적으로 선언할 수 있습니다. ML이
   값의 타입을 자동으로 파악하므로 필수는 아닙니다. *)
val diameter = 7926 : int
val e = 2.718 : real
val name = "Bobby" : string

(* 그리고 마찬가지로 중요한 함수: *)
fun is_large(x : int) = if x > 37 then true else false

(* 부동 소수점 숫자는 "실수"라고 합니다. *)
val tau = 2.0 * pi         (* 두 실수를 곱할 수 있습니다 *)
val twice_rent = 2 * rent  (* 두 정수를 곱할 수 있습니다 *)
(* val meh = 1.25 * 10 *)  (* 하지만 정수와 실수를 곱할 수는 없습니다 *)
val yeh = 1.25 * (Real.fromInt 10) (* ... 명시적으로 변환하지 않는 한
                                      하나 또는 다른 것 *)

(* +, - 및 *는 오버로드되어 int와 real 모두에 대해 작동합니다. *)
(* 나눗셈은 별도의 연산자가 있으므로 동일하다고 말할 수 없습니다: *)
val real_division = 14.0 / 4.0  (* 3.5를 제공 *)
val int_division  = 14 div 4    (* 3을 제공, 버림 *)
val int_remainder = 14 mod 4    (* 2를 제공, 3*4 = 12이므로 *)

(* ~는 실제로 때때로 함수입니다(예: 변수 앞에 놓을 때) *)
val negative_rent = ~(rent)  (* rent가 "real"인 경우에도 작동했을 것입니다 *)

(* 불리언 및 불리언 연산자도 있습니다 *)
val got_milk = true
val got_bread = false
val has_breakfast = got_milk andalso got_bread  (* 'andalso'는 연산자입니다 *)
val has_something = got_milk orelse got_bread   (* 'orelse'는 연산자입니다 *)
val is_sad = not(has_something)                 (* not은 함수입니다 *)

(* 많은 값은 등호 연산자 = 및 <>를 사용하여 비교할 수 있습니다 *)
val pays_same_rent = (rent = 1300)  (* false *)
val is_wrong_phone_no = (phone_no <> 5551337)  (* false *)

(* 연산자 <>는 대부분의 다른 언어에서 !=라고 부르는 것입니다. *)
(* 'andalso'와 'orelse'는 많은 다른 언어에서 &&와 ||라고 합니다. *)

(* 실제로 위의 괄호 대부분은 불필요합니다. 위에서 언급한
   몇 가지를 다른 방식으로 말하는 몇 가지 방법이 있습니다: *)
fun is_large x = x > 37  (* 위의 괄호는 ': int' 때문에 필요했습니다 *)
val is_sad = not has_something
val pays_same_rent = rent = 1300  (* 혼란스러워 보이지만 작동합니다 *)
val is_wrong_phone_no = phone_no <> 5551337
val negative_rent = ~rent  (* ~ rent (공백 참고)도 작동합니다 *)

(* 괄호는 주로 그룹화할 때 필요합니다: *)
val some_answer = is_large (5 + 5)      (* 괄호가 없으면 이것은 깨집니다! *)
(* val some_answer = is_large 5 + 5 *)  (* (is_large 5) + 5로 읽습니다. 나쁨! *)


(* 불리언, 정수 및 실수 외에도 Standard ML에는 문자 및 문자열도 있습니다: *)
val foo = "Hello, World!\n"  (* \n은 줄 바꿈의 이스케이프 시퀀스입니다 *)
val one_letter = #"a"        (* 그 펑키한 구문은 단지 한 문자, a입니다 *)

val combined = "Hello " ^ "there, " ^ "fellow!\n"  (* 문자열 연결 *)

val _ = print foo       (* 인쇄할 수 있습니다. 우리는 이 계산의 결과에
                           관심이 없으므로 버립니다. *)
val _ = print combined
(* val _ = print one_letter *)  (* 이 방법으로는 문자열만 인쇄할 수 있습니다 *)


val bar = [ #"H", #"e", #"l", #"l", #"o" ]  (* SML에는 리스트도 있습니다! *)
(* val _ = print bar *)  (* 불행히도 리스트는 문자열과 같지 않습니다 *)

(* 다행히 변환할 수 있습니다. String은 라이브러리이고 implode와 size는
   해당 라이브러리에서 사용할 수 있는 함수이며 문자열을 인수로 사용합니다. *)
val bob = String.implode bar          (* "Hello"를 제공 *)
val bob_char_count = String.size bob  (* 5를 제공 *)
val _ = print (bob ^ "\n")            (* 좋은 측정을 위해 줄 바꿈 추가 *)

(* 모든 종류의 리스트를 가질 수 있습니다 *)
val numbers = [1, 3, 3, 7, 229, 230, 248]  (* : int list *)
val names = [ "Fred", "Jane", "Alice" ]    (* : string list *)

(* 리스트의 리스트도 가능 *)
val groups = [ [ "Alice", "Bob" ],
               [ "Huey", "Dewey", "Louie" ],
               [ "Bonnie", "Clyde" ] ]     (* : string list list *)

val number_count = List.length numbers     (* 7을 제공 *)

(* :: 연산자("cons 연산자"라고 함, Lisp에서 알려짐)를 사용하여
   동일한 종류의 리스트 앞에 단일 값을 넣을 수 있습니다. *)
val more_numbers = 13 :: numbers  (* [13, 1, 3, 3, 7, ...]을 제공 *)
val more_groups  = ["Batman","Superman"] :: groups

(* 동일한 종류의 리스트는 @ ("append") 연산자를 사용하여 추가할 수 있습니다 *)
val guest_list = [ "Mom", "Dad" ] @ [ "Aunt", "Uncle" ]

(* 이것은 "cons" 연산자로 수행할 수 있었습니다. 왼쪽 피연산자는
   요소여야 하고 오른쪽 피연산자는 해당 요소의 리스트여야 하므로
   까다롭습니다. *)
val guest_list = "Mom" :: "Dad" :: [ "Aunt", "Uncle" ]
val guest_list = "Mom" :: ("Dad" :: ("Aunt" :: ("Uncle" :: [])))

(* 동일한 종류의 리스트가 많은 경우 모두 연결할 수 있습니다 *)
val everyone = List.concat groups  (* [ "Alice", "Bob", "Huey", ... ] *)

(* 리스트는 (유한한) 수의 값을 포함할 수 있습니다 *)
val lots = [ 5, 5, 5, 6, 4, 5, 6, 5, 4, 5, 7, 3 ]  (* 여전히 int list일 뿐입니다 *)

(* 리스트는 한 종류의 것만 포함할 수 있습니다... *)
(* val bad_list = [ 1, "Hello", 3.14159 ] : ??? list *)


(* 반면에 튜플은 고정된 수의 다른 것을 포함할 수 있습니다 *)
val person1 = ("Simon", 28, 3.14159)  (* : string * int * real *)

(* 리스트 안에 튜플을, 튜플 안에 리스트를 가질 수도 있습니다 *)
val likes = [ ("Alice", "ice cream"),
              ("Bob",   "hot dogs"),
              ("Bob",   "Alice") ]     (* : (string * string) list *)

val mixup = [ ("Alice", 39),
              ("Bob",   37),
              ("Eve",   41) ]  (* : (string * int) list *)

val good_bad_stuff =
  (["ice cream", "hot dogs", "chocolate"],
   ["liver", "paying the rent" ])           (* : string list * string list *)


(* 레코드는 이름 있는 슬롯이 있는 튜플입니다 *)

val rgb = { r=0.23, g=0.56, b=0.91 } (* : {b:real, g:real, r:real} *)

(* 미리 슬롯을 선언할 필요가 없습니다. 슬롯 이름이 다른 레코드는
   슬롯 값 유형이 일치하더라도 다른 유형으로 간주됩니다.
   예를 들어... *)

val Hsl = { H=310.3, s=0.51, l=0.23 } (* : {H:real, l:real, s:real} *)
val Hsv = { H=310.3, s=0.51, v=0.23 } (* : {H:real, s:real, v:real} *)

(* ...`Hsv = Hsl` 또는 `rgb = Hsl`을 평가하려고 하면 유형 오류가
   발생합니다. 모두 세 개의 슬롯으로 구성된 `real` 레코드이지만,
   각각 적어도 일부 슬롯에 대해 다른 이름을 가지고 있습니다. *)

(* 해시 표기법을 사용하여 튜플에서 값을 가져올 수 있습니다. *)

val H = #H Hsv (* : real *)
val s = #s Hsl (* : real *)

(* 함수! *)
fun add_them (a, b) = a + b    (* 두 숫자를 더하는 간단한 함수 *)
val test_it = add_them (3, 4)  (* 7을 제공 *)

(* 더 큰 함수는 일반적으로 가독성을 위해 여러 줄로 나뉩니다 *)
fun thermometer temp =
    if temp < 37
    then "Cold"
    else if temp > 37
         then "Warm"
         else "Normal"

val test_thermo = thermometer 40  (* "Warm"을 제공 *)

(* if-문장은 실제로는 문/선언이 아닌 표현식입니다.
   함수 본문은 하나의 표현식만 포함할 수 있습니다. 하지만 함수가
   한 가지 이상의 일을 하도록 하는 몇 가지 트릭이 있습니다. *)

(* 함수는 결과의 일부로 자신을 호출할 수 있습니다(재귀!) *)
fun fibonacci n =
    if n = 0 then 0 else                   (* 기본 사례 *)
    if n = 1 then 1 else                   (* 기본 사례 *)
    fibonacci (n - 1) + fibonacci (n - 2)  (* 재귀 사례 *)

(* 때로는 재귀를 직접 함수를 평가하여 가장 잘 이해할 수 있습니다:

 fibonacci 4
   ~> fibonacci (4 - 1) + fibonacci (4 - 2)
   ~> fibonacci 3 + fibonacci 2
   ~> (fibonacci (3 - 1) + fibonacci (3 - 2)) + fibonacci 2
   ~> (fibonacci 2 + fibonacci 1) + fibonacci 2
   ~> ((fibonacci (2 - 1) + fibonacci (2 - 2)) + fibonacci 1) + fibonacci 2
   ~> ((fibonacci 1 + fibonacci 0) + fibonacci 1) + fibonacci 2
   ~> ((1 + fibonacci 0) + fibonacci 1) + fibonacci 2
   ~> ((1 + 0) + fibonacci 1) + fibonacci 2
   ~> (1 + fibonacci 1) + fibonacci 2
   ~> (1 + 1) + fibonacci 2
   ~> 2 + fibonacci 2
   ~> 2 + (fibonacci (2 - 1) + fibonacci (2 - 2))
   ~> 2 + (fibonacci (2 - 1) + fibonacci (2 - 2))
   ~> 2 + (fibonacci 1 + fibonacci 0)
   ~> 2 + (1 + fibonacci 0)
   ~> 2 + (1 + 0)
   ~> 2 + 1
   ~> 3  이 정의에 따르면 4번째 피보나치 수입니다

 *)

(* 함수는 참조할 수 있는 변수를 변경할 수 없습니다.
   동일한 이름을 가진 새 변수로 일시적으로 가릴 수만 있습니다.
   이런 의미에서 변수는 실제로는 상수이며
   재귀를 다룰 때만 변수처럼 동작합니다.
   이러한 이유로 변수는 값 바인딩이라고도 합니다.
   예: *)

val x = 42
fun answer(question) =
    if question = "What is the meaning of life, the universe and everything?"
    then x
    else raise Fail "I'm an exception. Also, I don't know what the answer is."
val x = 43
val hmm = answer "What is the meaning of life, the universe and everything?"
(* 이제 hmm은 값 42를 가집니다. 이것은 함수 answer가
   자체 함수 정의 전에 표시되었던 x의 복사본을 참조하기 때문입니다. *)


(* 함수는 하나의 튜플을 인수로 사용하여 여러 인수를 받을 수 있습니다: *)
fun solve2 (a : real, b : real, c : real) =
    ((~b + Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a),
     (~b - Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a))

(* 때로는 동일한 계산이 여러 번 수행됩니다.
   결과를 저장하고 처음으로 재사용하는 것이 합리적입니다.
   "let-bindings"를 사용할 수 있습니다: *)
fun solve2 (a : real, b : real, c : real) =
    let val discr  = b * b - 4.0 * a * c
        val sqr = Math.sqrt discr
        val denom = 2.0 * a
    in ((~b + sqr) / denom,
        (~b - sqr) / denom)
    end


(* 패턴 매칭은 함수형 프로그래밍의 펑키한 부분입니다.
   if-문장의 대안입니다. 피보나치 함수는 다음과 같이 다시 작성할 수 있습니다: *)
fun fibonacci 0 = 0  (* 기본 사례 *)
  | fibonacci 1 = 1  (* 기본 사례 *)
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  (* 재귀 사례 *)

(* 패턴 매칭은 튜플, 리스트 및 레코드와 같은 복합 유형에서도 가능합니다.
   "fun solve2 (a, b, c) = ..."를 작성하는 것은 사실상
   solve2가 인수로 받는 하나의 세 튜플에 대한 패턴 매치입니다.
   마찬가지로, 덜 직관적이지만, 그 안에 있는 요소로 구성된 리스트에 대해
   (리스트의 시작부터만) 매치할 수 있습니다. *)
fun first_elem (x::xs) = x
fun second_elem (x::y::xs) = y
fun evenly_positioned_elems (odd::even::xs) = even::evenly_positioned_elems xs
  | evenly_positioned_elems [odd] = []  (* 기본 사례: 버리기 *)
  | evenly_positioned_elems []    = []  (* 기본 사례 *)

(* case 표현식은 패턴 매치하고 값을 반환하는 데에도 사용할 수 있습니다 *)
datatype temp =
      C of real
    | F of real

(*  새로운 C 온도 값 선언...
    val t: temp = C 45.0  *)

fun temp_to_f t =
    case t of
      C x => x * (9.0 / 5.0) + 32.0
    | F x => x

(* 레코드에 대해 매칭할 때 슬롯 이름을 사용해야 하며,
   레코드의 모든 슬롯을 바인딩해야 합니다. 슬롯의 순서는 중요하지 않습니다. *)

fun rgbToTup {r, g, b} = (r, g, b)    (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)
fun mixRgbToTup {g, b, r} = (r, g, b) (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)

(* {r=0.1, g=0.2, b=0.3}으로 호출하면 위의 함수 중 하나가
   (0.1, 0.2, 0.3)을 반환합니다. 그러나 {r=0.1, g=0.2, b=0.3, a=0.4}로
   호출하면 유형 오류가 발생합니다. *)

(* 고차 함수: 함수는 다른 함수를 인수로 받을 수 있습니다.
   함수는 다른 종류의 값일 뿐이며, 함수는 존재하기 위해
   이름이 필요하지 않습니다. 이름 없는 함수는 "익명 함수" 또는
   람다 표현식 또는 클로저(어휘적 범위를 가지므로)라고 합니다. *)
val is_large = (fn x => x > 37)
val add_them = fn (a,b) => a + b
val thermometer =
    fn temp => if temp < 37
               then "Cold"
               else if temp > 37
                    then "Warm"
                    else "Normal"

(* 다음은 익명 함수를 직접 사용하여 "ColdWarm"을 제공합니다 *)
val some_result = (fn x => thermometer (x - 5) ^ thermometer (x + 5)) 37

(* 다음은 리스트에서 작동하는 고차 함수입니다(리스트 조합기) *)
(* map f l
       l의 각 요소에 왼쪽에서 오른쪽으로 f를 적용하고,
       결과 목록을 반환합니다. *)
val readings = [ 34, 39, 37, 38, 35, 36, 37, 37, 37 ]  (* 먼저 int list *)
val opinions = List.map thermometer readings (* [ "Cold", "Warm", ... ]을 제공 *)

(* 그리고 다음은 리스트 필터링을 위한 또 다른 것입니다 *)
val warm_readings = List.filter is_large readings  (* [39, 38]을 제공 *)

(* 자신만의 고차 함수를 만들 수도 있습니다. 함수는
   "커링"하여 여러 인수를 받을 수도 있습니다. 구문적으로 이것은
   쉼표와 주변 괄호 대신 함수 인수 사이에 공백을 추가하는 것을
   의미합니다. *)
fun map f [] = []
  | map f (x::xs) = f(x) :: map f xs

(* map은 유형 ('a -> 'b) -> 'a list -> 'b list를 가지며 다형성이라고 합니다. *)
(* 'a는 유형 변수라고 합니다. *)


(* 함수를 중위로 선언할 수 있습니다 *)
val plus = add_them   (* plus는 이제 add_them과 동일한 함수와 같습니다 *)
infix plus            (* plus는 이제 중위 연산자입니다 *)
val seven = 2 plus 5  (* seven은 이제 7에 바인딩됩니다 *)

(* 함수는 선언되기 전에 중위로 만들 수도 있습니다 *)
infix minus
fun x minus y = x - y (* 인수가 무엇인지 보기가 조금 어려워집니다 *)
val four = 8 minus 4  (* four는 이제 4에 바인딩됩니다 *)

(* 중위 함수/연산자는 'op'로 접두사로 만들 수 있습니다 *)
val n = op + (5, 5)   (* n은 이제 10입니다 *)

(* 'op'는 고차 함수와 결합할 때 유용합니다. 왜냐하면 그들은
   연산자가 아닌 함수를 인수로 기대하기 때문입니다. 대부분의 연산자는
   실제로 중위 함수일 뿐입니다. *)
(* foldl f init [x1, x2, ..., xn]
       반환
       f(xn, ...f(x2, f(x1, init))...)
       또는 리스트가 비어 있으면 init. *)
val sum_of_numbers = foldl op+ 0 [1, 2, 3, 4, 5]


(* 데이터 유형은 간단하고 복잡한 구조를 만드는 데 유용합니다 *)
datatype color = Red | Green | Blue

(* 다음은 이들 중 하나를 인수로 받는 함수입니다 *)
fun say(col) =
    if col = Red then "You are red!" else
    if col = Green then "You are green!" else
    if col = Blue then "You are blue!" else
    raise Fail "Unknown color"

val _ = print (say(Red) ^ "\n")

(* 데이터 유형은 패턴 매칭과 함께 매우 자주 사용됩니다 *)
fun say Red   = "You are red!"
  | say Green = "You are green!"
  | say Blue  = "You are blue!"

(* 세 가지 색상을 모두 지정한 후 패턴이 완전하므로
   `say _ = raise Fail "Unknown color"` 일치 암을 포함하지 않았습니다.
   그리고 패턴 매칭에서는 중복이 허용되지 않습니다. *)


(* 다음은 이진 트리 데이터 유형입니다 *)
datatype 'a btree = Leaf of 'a
                  | Node of 'a btree * 'a * 'a btree (* 세 인수 생성자 *)

(* 다음은 이진 트리입니다 *)
val myTree = Node (Leaf 9, 8, Node (Leaf 3, 5, Leaf 7))

(* 그리면 다음과 같이 보일 수 있습니다...

           8
          / \
 leaf -> 9   5
            / \
   leaf -> 3   7 <- leaf
 *)

(* 이 함수는 트리의 모든 요소의 합을 계산합니다 *)
fun count (Leaf n) = n
  | count (Node (leftTree, n, rightTree)) = count leftTree + n + count rightTree

val myTreeCount = count myTree  (* myTreeCount는 이제 32에 바인딩됩니다 *)


(* 예외! *)
(* 예외는 예약어 'raise'를 사용하여 발생/던질 수 있습니다 *)
fun calculate_interest(n) = if n < 0.0
                            then raise Domain
                            else n * 1.04

(* 예외는 "handle"을 사용하여 잡을 수 있습니다 *)
val balance = calculate_interest ~180.0
              handle Domain => ~180.0    (* balance는 이제 ~180.0 값을 가집니다 *)

(* 일부 예외는 추가 정보를 포함합니다 *)
(* 다음은 내장 예외의 몇 가지 예입니다 *)
fun failing_function []    = raise Empty  (* 빈 리스트에 사용됨 *)
  | failing_function [x]   = raise Fail "This list is too short!"
  | failing_function [x,y] = raise Overflow  (* 산술에 사용됨 *)
  | failing_function xs    = raise Fail "This list is too long!"

(* 'handle'에서 패턴 매칭하여
   특정 예외가 발생했는지 확인하거나 메시지를 가져올 수 있습니다 *)
val err_msg = failing_function [1,2] handle Fail _ => "Fail was raised"
                                          | Domain => "Domain was raised"
                                          | Empty  => "Empty was raised"
                                          | _      => "Unknown exception"

(* err_msg는 이제 "Unknown exception" 값을 가집니다. Overflow가
   패턴 중 하나로 나열되지 않았기 때문입니다. 따라서 catch-all 패턴 _이
   사용됩니다. *)

(* 다음과 같이 자신만의 예외를 정의할 수 있습니다 *)
exception MyException
exception MyExceptionWithMessage of string
exception SyntaxError of string * (int * int)

(* 파일 I/O! *)
(* 파일에 멋진 시 쓰기 *)
fun writePoem(filename) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, "Roses are red,\nViolets are blue.\n")
        val _ = TextIO.output(file, "I have a gun.\nGet in the van.\n")
    in TextIO.closeOut(file)
    end

(* 파일에서 멋진 시를 문자열 목록으로 읽기 *)
fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

val _ = writePoem "roses.txt"
val test_poem = readPoem "roses.txt"  (* [ "Roses are red,",
                                                 "Violets are blue.",
                                                 "I have a gun.",
                                                 "Get in the van." ]을 제공 *)

(* 업데이트할 수 있는 데이터에 대한 참조를 만들 수 있습니다 *)
val counter = ref 0 (* ref 함수로 참조 생성 *)

(* 할당 연산자로 참조에 할당 *)
fun set_five reference = reference := 5

(* 역참조 연산자로 참조 읽기 *)
fun equals_five reference = !reference = 5

(* 재귀가 복잡할 때 while 루프를 사용할 수 있습니다 *)
fun decrement_to_zero r = if !r < 0
                          then r := 0
                          else while !r >= 0 do r := !r - 1

(* 이것은 단위 값을 반환합니다 (실질적으로는 아무것도 아님, 0-튜플) *)

(* 값을 반환하도록 허용하려면 세미콜론을 사용하여 평가를 시퀀싱할 수 있습니다 *)
fun decrement_ret x y = (x := !x - 1; y)
```

## 추가 학습

* 대화형 컴파일러(REPL) 설치, 예를 들어
  [Poly/ML](http://www.polyml.org/),
  [Moscow ML](http://mosml.org),
  [SML/NJ](http://smlnj.org/).
* Coursera 과정 [프로그래밍 언어](https://www.coursera.org/course/proglang) 수강.
* Larry C. Paulson의 *[ML for the Working Programmer](https://www.cl.cam.ac.uk/~lp15/MLbook/pub-details.html)* 읽기.
* [StackOverflow의 sml 태그](http://stackoverflow.com/questions/tagged/sml) 사용.
* [Exercism.io의 Standard ML 트랙](https://exercism.io/tracks/sml)에서 연습 문제 풀기.
