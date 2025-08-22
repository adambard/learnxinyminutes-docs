---
name: OCaml
filename: learnocaml.ml
contributors:
    - ["Daniil Baturin", "http://baturin.org/"]
    - ["Stanislav Modrak", "https://stanislav.gq/"]
    - ["Luke Tong", "https://lukert.me/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

OCaml은 일부 명령형 기능이 있는 엄격하게 평가되는 함수형 언어입니다.

Standard ML 및 그 방언과 함께 ML 언어 계열에 속합니다.
F#도 OCaml의 영향을 많이 받았습니다.

Standard ML과 마찬가지로 OCaml은 대화형으로 사용할 수 있는 인터프리터와 컴파일러를 모두 갖추고 있습니다.
인터프리터 바이너리는 일반적으로 `ocaml`이라고 하고 컴파일러는 `ocamlopt`입니다.
바이트코드 컴파일러인 `ocamlc`도 있지만 사용할 이유는 거의 없습니다.

또한 패키지 관리자인 `opam`과 빌드 시스템인 `dune`도 포함합니다.

강력하고 정적으로 유형이 지정되지만 수동으로 작성된 유형 주석을 사용하는 대신
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
알고리즘을 사용하여 표현식의 유형을 유추합니다.
대부분의 경우 유형 주석이 필요하지 않지만 초보자에게는 큰 혼란의 원인이 될 수 있습니다.

최상위 루프에 있을 때 OCaml은 표현식을 입력한 후 유추된 유형을 인쇄합니다.

```
# let inc x = x	+ 1 ;;
val inc : int -> int = <fun>
# let a = 99 ;;
val a : int = 99
```

소스 파일의 경우 `ocamlc -i /path/to/file.ml` 명령을 사용하여 모든 이름과 유형 서명을 인쇄할 수 있습니다.

```
$ cat sigtest.ml
let inc x = x + 1
let add x y = x + y

let a = 1

$ ocamlc -i ./sigtest.ml
val inc : int -> int
val add : int -> int -> int
val a : int
```

여러 인수를 갖는 함수의 유형 서명은
[커링된](https://en.wikipedia.org/wiki/Currying) 형식으로 작성됩니다.
여러 인수를 받는 함수는 단일 인수를 받는 함수의
구성으로 나타낼 수 있습니다.
위 예제의 `f(x,y) = x + y` 함수를
인수 2와 3에 적용하면 `f0(y) = 2 + y` 함수를 3에 적용한 것과 같습니다.
따라서 `int -> int -> int` 서명이 됩니다.

```ocaml
(*** 주석 ***)

(* 주석은 (*와 *)로 묶습니다. 주석을 중첩해도 괜찮습니다. *)

(* 한 줄 주석은 없습니다. *)


(*** 변수와 함수 ***)

(* 표현식은 이중 세미콜론 ";;"으로 구분할 수 있습니다.
   많은 경우 중복되지만, 이 튜토리얼에서는 인터프리터 셸에
   쉽게 붙여넣을 수 있도록 모든 표현식 뒤에 사용합니다.
   소스 코드 파일에서 표현식 구분자를 불필요하게 사용하는 것은
   종종 나쁜 스타일로 간주됩니다. *)

(* 변수 및 함수 선언은 "let" 키워드를 사용합니다. *)
(* OCaml에서 변수는 기본적으로 불변입니다. *)
let x = 10 ;;

(* OCaml은 식별자에 작은따옴표 문자를 허용합니다.
   이 경우 작은따옴표는 특별한 의미가 없으며, 다른 언어에서
   "foo_tmp"와 같은 이름을 사용하는 경우에 종종 사용됩니다. *)
let foo = 1 ;;
let foo' = foo * 2 ;;

(* OCaml 컴파일러는 유형을 자동으로 유추하므로 일반적으로
   인수 유형을 명시적으로 지정할 필요가 없습니다. 하지만
   원하거나 필요한 경우 그렇게 할 수 있습니다. *)
let inc_int (x: int) : int = x + 1 ;;

(* 명시적 유형 주석이 필요할 수 있는 경우 중 하나는
   이름이 같은 필드를 가진 두 레코드 유형 간의 모호성을
   해결하는 것입니다. 대안은 해당 유형을 모듈로 캡슐화하는 것이지만,
   두 주제 모두 이 튜토리얼의 범위를 약간 벗어납니다. *)

(* "rec" 키워드로 재귀 함수 정의를 표시해야 합니다. *)
let rec factorial n =
    if n = 0 then 1
    else n * factorial (n-1)
;;

(* 함수 적용은 일반적으로 인수 주위에 괄호가 필요하지 않습니다. *)
let fact_5 = factorial 5 ;;

(* ...인수가 표현식인 경우는 예외입니다. *)
let fact_4 = factorial (5-1) ;;
let sqr2 = sqr (-2) ;;

(* 모든 함수는 최소한 하나의 인수를 가져야 합니다.
   일부 함수는 자연스럽게 인수를 받지 않으므로,
   "()"로 작성된 유일한 값을 갖는 "unit" 유형이 있습니다. *)
let print_hello () = print_endline "hello world" ;;

(* 호출할 때 인수로 "()"를 지정해야 한다는 점에 유의하십시오. *)
print_hello () ;;

(* 불충분한 수의 인수로 함수를 호출해도
   오류가 발생하지 않으며, 새 함수가 생성됩니다. *)
let make_inc x y = x + y ;; (* make_inc는 int -> int -> int *)
let inc_2 = make_inc 2 ;;   (* inc_2는 int -> int *)
inc_2 3 ;; (* 5로 평가됨 *)

(* 함수 본문에 여러 표현식을 사용할 수 있습니다.
   마지막 표현식이 반환 값이 됩니다. 다른 모든
   표현식은 "unit" 유형이어야 합니다.
   이것은 명령형 스타일로 작성할 때 유용하며, 가장 간단한
   형태는 디버그 인쇄를 삽입하는 것입니다. *)
let print_and_return x =
    print_endline (string_of_int x);
    x
;;

(* OCaml은 함수형 언어이므로 "프로시저"가 없습니다.
   모든 함수는 무언가를 반환해야 합니다. 따라서
   print_endline처럼 실제로는 아무것도 반환하지 않고 부수 효과를
   위해서만 호출되는 함수는 "unit" 유형의 값을 반환합니다. *)


(* 정의는 "let ... in" 구문으로 연결할 수 있습니다.
   이것은 명령형 언어에서 표현식에서 사용하기 전에 여러
   변수에 값을 할당하는 것과 거의 같습니다. *)
let x = 10 in
let y = 20 in
x + y ;;

(* 또는 "let ... and ... in" 구문을 사용할 수 있습니다.
   이것은 상호 재귀 함수에 특히 유용하며,
   일반적인 "let ... in"을 사용하면 컴파일러가
   바인딩되지 않은 값에 대해 불평합니다. *)
let rec
  is_even = function
  | 0 -> true
  | n -> is_odd (n-1)
and
  is_odd = function
  | 0 -> false
  | n -> is_even (n-1)
;;

(* 익명 함수는 다음 구문을 사용합니다: *)
let my_lambda = fun x -> x * x ;;

(*** 연산자 ***)

(* 연산자와 함수 사이에는 거의 구별이 없습니다.
   모든 연산자는 함수로 호출할 수 있습니다. *)

(+) 3 4  (* 3 + 4와 동일 *)

(* 여러 내장 연산자가 있습니다. 한 가지 특이한 특징은
   OCaml이 정수와 부동 소수점 간의 암시적 변환을
   자제할 뿐만 아니라 부동 소수점에 대해 다른 연산자를
   사용한다는 것입니다. *)
12 + 3 ;; (* 정수 덧셈. *)
12.0 +. 3.0 ;; (* 부동 소수점 덧셈. *)

12 / 3 ;; (* 정수 나눗셈. *)
12.0 /. 3.0 ;; (* 부동 소수점 나눗셈. *)
5 mod 2 ;; (* 나머지. *)

(* 단항 마이너스는 주목할 만한 예외이며, 다형성입니다.
   그러나 "순수" 정수 및 부동 소수점 형식도 있습니다. *)
- 3 ;; (* 다형성, 정수 *)
- 4.5 ;; (* 다형성, 부동 소수점 *)
~- 3 (* 정수만 *)
~- 3.4 (* 유형 오류 *)
~-. 3.4 (* 부동 소수점만 *)

(* 자신만의 연산자를 정의하거나 기존 연산자를 재정의할 수 있습니다.
   Standard ML이나 Haskell과 달리 특정 기호만
   연산자 이름으로 사용할 수 있으며 연산자의 첫 번째 기호가
   결합성 및 우선 순위 규칙을 결정합니다. *)
let (+) a b = a - b ;; (* 유지보수 프로그래머를 놀라게 하십시오. *)

(* 더 유용한 예: 부동 소수점의 역수 연산자.
   단항 연산자는 "~"로 시작해야 합니다. *)
let (~/) x = 1.0 /. x ;;
~/4.0 (* = 0.25 *)


(*** 내장 데이터 구조 ***)

(* 리스트는 대괄호로 묶고, 항목은
   세미콜론으로 구분합니다. *)
let my_list = [1; 2; 3] ;; (* "int list" 유형을 가짐. *)

(* 튜플은 (선택적으로) 괄호로 묶고, 항목은
   쉼표로 구분합니다. *)
let first_tuple = 3, 4 ;; (* "int * int" 유형을 가짐. *)
let second_tuple = (4, 5) ;;

(* 결과적으로: 리스트 항목을 쉼표로 구분하려고 하면,
   튜플이 들어있는 리스트가 생성되는데, 아마 원하는 것이 아닐 것입니다. *)
let bad_list = [1, 2] ;; (* [(1, 2)]가 됨 *)

(* List.nth 함수로 개별 리스트 항목에 접근할 수 있습니다. *)
List.nth my_list 1 ;;

(* map 및 filter와 같은 리스트에 대한 고차 함수가 있습니다. *)
List.map (fun x -> x * 2) [1; 2; 3] ;;
List.filter (fun x -> x mod 2 = 0) [1; 2; 3; 4] ;;

(* "::" 생성자(종종 "cons"라고 함)를 사용하여 리스트 시작 부분에
   항목을 추가할 수 있습니다. *)
1 :: [2; 3] ;; (* [1; 2; 3]을 반환 *)

(* cons :: 생성자는 단일 항목만 리스트의 앞에 추가할 수 있다는 점을
   기억하십시오. 두 리스트를 결합하려면 append @ 연산자를 사용하십시오. *)
[1; 2] @ [3; 4] ;; (* [1; 2; 3; 4]를 반환 *)

(* 배열은 [| |]로 묶습니다. *)
let my_array = [| 1; 2; 3 |] ;;

(* 다음과 같이 배열 항목에 접근할 수 있습니다: *)
my_array.(0) ;;


(*** 문자열과 문자 ***)

(* 문자열 리터럴에는 큰따옴표를 사용합니다. *)
let my_str = "Hello world" ;;

(* 문자 리터럴에는 작은따옴표를 사용합니다. *)
let my_char = 'a' ;;

(* 작은따옴표와 큰따옴표는 서로 바꿔 쓸 수 없습니다. *)
let bad_str = 'syntax error' ;; (* 구문 오류. *)

(* 이것은 문자가 아닌 단일 문자 문자열을 제공합니다. *)
let single_char_str = "w" ;;

(* 문자열은 "^" 연산자로 연결할 수 있습니다. *)
let some_str = "hello" ^ "world" ;;

(* 문자열은 문자 배열이 아닙니다.
   표현식에서 문자와 문자열을 혼합할 수 없습니다.
   "String.make 1 my_char"를 사용하여 문자를 문자열로 변환할 수 있습니다.
   Core.Std와 같은 추가 라이브러리에는 이 목적을 위한 더 편리한
   함수가 있으며, 기본적으로 설치 및/또는 로드되지 않을 수 있습니다. *)
let ocaml = (String.make 1 'O') ^ "Caml" ;;

(* printf 함수가 있습니다. *)
Printf.printf "%d %s" 99 "bottles of beer" ;;

(* 서식 없는 읽기 및 쓰기 함수도 있습니다. *)
print_string "hello world\n" ;;
print_endline "hello world" ;;
let line = read_line () ;;


(*** 사용자 정의 데이터 유형 ***)

(* "type some_type =" 구문으로 유형을 정의할 수 있습니다. 이
   쓸모없는 유형 별칭처럼: *)
type my_int = int ;;

(* 더 흥미로운 유형에는 소위 유형 생성자가 포함됩니다.
   생성자는 대문자로 시작해야 합니다. *)
type ml = OCaml | StandardML ;;
let lang = OCaml ;;  (* "ml" 유형을 가짐. *)

(* 유형 생성자는 비어 있을 필요가 없습니다. *)
type my_number = PlusInfinity | MinusInfinity | Real of float ;;
let r0 = Real (-3.4) ;; (* "my_number" 유형을 가짐. *)

(* 다형성 산술을 구현하는 데 사용할 수 있습니다. *)
type number = Int of int | Float of float ;;

(* 평면 위의 점, 본질적으로 유형이 제한된 튜플 *)
type point2d = Point of float * float ;;
let my_point = Point (2.0, 3.0) ;;

(* 유형은 매개변수화될 수 있습니다. 이 유형은 "무엇이든
   리스트의 리스트"에 대한 것입니다. 'a는 모든 유형으로 대체될 수 있습니다. *)
type 'a list_of_lists = 'a list list ;;
type int_list_list = int list_of_lists ;;

(* 이러한 기능은 유용한 선택적 유형을 허용합니다. *)
type 'a option = Some of 'a | None ;;
let x = Some x ;;
let y = None ;;

(* 유형은 재귀적일 수도 있습니다. 이 유형은
   내장 정수 리스트와 유사합니다. *)
type my_int_list = EmptyList | IntList of int * my_int_list ;;
let l = IntList (1, EmptyList) ;;

(* 또는 트리 *)
type 'a tree =
   | Empty
   | Node of 'a tree * 'a * 'a tree

let example_tree: int tree =
   Node (
      Node (Empty, 7, Empty),
      5,
      Node (Empty, 9, Empty)
   )
(*
   5
  / \
 7   9
*)

(*** 레코드 ***)

(* 이름 있는 필드가 있는 값 모음 *)

type animal =
   {
      name: string;
      color: string;
      legs: int;
   }
;;

let cow =
   {  name = "cow";
      color = "black and white";
      legs = 4;
   }
;;
val cow : animal

cow.name ;;
- : string = "cow"

(*** 패턴 매칭 ***)

(* 패턴 매칭은 명령형 언어의 switch 문과 다소 유사하지만,
   훨씬 더 많은 표현력을 제공합니다.

   복잡해 보일 수 있지만, 실제로는 인수를 정확한 값,
   술어 또는 유형 생성자와 일치시키는 것으로 귀결됩니다.
   유형 시스템이 이를 매우 강력하게 만듭니다. *)

(** 정확한 값 일치.  **)

let is_zero x =
    match x with
    | 0 -> true
    | _ -> false  (* "_"는 "그 밖의 모든 것"을 의미합니다. *)
;;

(* 또는 "function" 키워드를 사용할 수 있습니다. *)
let is_one = function
| 1 -> true
| _ -> false
;;

(* 술어 일치, 즉 "가드된 패턴 매칭". *)
let abs x =
    match x with
    | x when x < 0 -> -x
    | _ -> x
;;

abs 5 ;; (* 5 *)
abs (-5) (* 다시 5 *)

(** 유형 생성자 일치 **)

type animal = Dog of string | Cat of string ;;

let say x =
    match x with
    | Dog x -> x ^ " says woof"
    | Cat x -> x ^ " says meow"
;;

say (Cat "Fluffy") ;; (* "Fluffy says meow". *)

(* 그러나 패턴 매칭은 철저해야 합니다. *)
type color = Red | Blue | Green ;;
let what_color x =
   match x with
   | Red -> "color is red"
   | Blue -> "color is blue"
   (* 컴파일되지 않음! 모든 가능성을 고려하도록 _ 케이스 또는 Green 케이스를
      추가해야 합니다. *)
;;
(* 또한 match 문은 각 케이스를 순서대로 확인합니다.
   따라서 _ 케이스가 먼저 나타나면
   다음 케이스는 도달하지 않습니다! *)

(** 패턴 매칭으로 데이터 구조 순회 **)

(* 재귀 유형은 패턴 매칭으로 쉽게 순회할 수 있습니다.
   내장 리스트 유형의 데이터 구조를 순회하는 방법을 살펴보겠습니다.
   내장 cons ("::")가 중위 연산자처럼 보이지만,
   실제로는 유형 생성자이며 다른 것과 같이 일치시킬 수 있습니다. *)
let rec sum_list l =
    match l with
    | [] -> 0
    | head :: tail -> head + (sum_list tail)
;;

sum_list [1; 2; 3] ;; (* 6으로 평가됨 *)

(* 내장 cons 구문은 구조를 약간 모호하게 하므로,
   데모용으로 우리만의 리스트를 만들겠습니다. *)

type int_list = Nil | Cons of int * int_list ;;
let rec sum_int_list l =
  match l with
      | Nil -> 0
      | Cons (head, tail) -> head + (sum_int_list tail)
;;

let t = Cons (1, Cons (2, Cons (3, Nil))) ;;
sum_int_list t ;;

(* 리스트가 정렬되었는지 확인하는 함수입니다. *)
let rec is_sorted l =
   match l with
   | x :: y :: tail -> x <= y && is_sorted (y :: tail)
   | _ -> true
;;

is_sorted [1; 2; 3] ;; (* True *)
(* OCaml의 강력한 유형 추론은 l의 요소에 <= 연산자가
   사용되므로 l이 int list 유형이라고 추측합니다. *)

(* 그리고 리스트를 뒤집는 또 다른 함수 *)
let rec rev (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | x::tl -> (rev tl) @ [x]
;;

rev [1; 2; 3] ;; (* [3; 2; 1]을 반환 *)
(* 이 함수는 모든 요소 유형의 리스트에서 작동합니다. *)

(*** 고차 함수 ***)

(* 함수는 OCaml에서 일급입니다. *)

let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | head :: tail -> (f head) :: transform f tail
;;

transform (fun x -> x + 1) [1; 2; 3] ;; (* [2; 3; 4]를 반환 *)

(** 배운 모든 것을 결합해 봅시다! **)
let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  begin match l with
  | [] -> []
  | x :: xs ->
     let rest = filter pred xs in
     if pred x then x :: rest else rest
  end
;;

filter (fun x -> x < 4) [3; 1; 4; 1; 5] ;; (* [3; 1; 1]을 반환) *)

(*** 가변성 ***)

(* 레코드와 변수는 불변입니다: 변수가 가리키는 위치를 변경할 수 없습니다. *)

(* 그러나 변경 가능한 다형성 필드를 만들 수 있습니다. *)
type counter = { mutable num : int } ;;

let c = { num = 0 } ;;
c.num ;; (* 0을 반환 *)
c.num <- 1 ;; (* <- 연산자는 변경 가능한 레코드 필드를 설정할 수 있습니다. *)
c.num ;; (* 1을 반환 *)

(* OCaml의 표준 라이브러리는 단일 필드 가변성을 더 쉽게 만들기 위해 ref 유형을 제공합니다. *)
type 'a ref = { mutable contents : 'a } ;;
let counter = ref 0 ;;
!counter ;; (* ! 연산자는 x.contents를 반환합니다. *)
counter := !counter + 1 ;; (* :=는 내용을 설정하는 데 사용할 수 있습니다. *)
```

## 더 읽을거리

* 공식 웹사이트를 방문하여 컴파일러를 받고 문서를 읽으십시오: [http://ocaml.org/](http://ocaml.org/)
* OCaml 빠른 튜토리얼: [https://ocaml.org/docs/up-and-running](https://ocaml.org/docs/up-and-running)
* 완전한 온라인 OCaml v5 놀이터: [https://ocaml.org/play](https://ocaml.org/play)
* 최신(2022년) 책(무료 온라인 버전 포함) "Real World OCaml": [https://www.cambridge.org/core/books/real-world-ocaml-functional-programming-for-the-masses/052E4BCCB09D56A0FE875DD81B1ED571](https://www.cambridge.org/core/books/real-world-ocaml-functional-programming-for-the-masses/052E4BCCB09D56A0FE875DD81B1ED571)
* 코넬 대학의 온라인 대화형 교과서 "OCaml Programming: Correct + Efficient + Beautiful": [https://cs3110.github.io/textbook/cover.html](https://cs3110.github.io/textbook/cover.html)
* OCaml Pro의 대화형 튜토리얼 및 웹 기반 인터프리터 시도: [http://try.ocamlpro.com/](http://try.ocamlpro.com/)
