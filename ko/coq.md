---
name: Coq
filename: learncoq.v
contributors:
    - ["Philip Zucker", "http://www.philipzucker.com/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Coq 시스템은 증명 보조기입니다. 수학적 증명을 구축하고 검증하도록 설계되었습니다. Coq 시스템에는 함수형 프로그래밍 언어 Gallina가 포함되어 있으며 이 언어로 작성된 프로그램의 속성을 증명할 수 있습니다.

Coq는 종속 타입 언어입니다. 이는 언어의 타입이 변수의 값에 따라 달라질 수 있음을 의미합니다. 이러한 점에서 Agda, Idris, F*, Lean 등과 같은 다른 관련 언어와 유사합니다. Curry-Howard 대응을 통해 프로그램, 속성 및 증명은 동일한 언어로 형식화됩니다.

Coq는 OCaml로 개발되었으며 OCaml과 일부 구문 및 개념적 유사성을 공유합니다. Coq는 매력적이지만 어려운 주제를 많이 포함하는 언어입니다. 이 튜토리얼은 증명보다는 Coq의 프로그래밍 측면에 초점을 맞출 것입니다. 특히 함수형 프로그래밍에 익숙하지 않은 경우 OCaml을 먼저 배우는 것이 도움이 될 수 있지만 필수는 아닙니다. 이 튜토리얼은 OCaml과 동일한 내용을 기반으로 합니다.

Coq의 표준 사용 모델은 강력한 REPL처럼 작동하는 대화형 도구 지원을 사용하여 작성하는 것입니다. 두 가지 일반적인 편집기는 CoqIDE 및 Proof General Emacs 모드입니다.

Proof General 내에서 `Ctrl+C Ctrl+<Enter>`는 커서까지 평가합니다.

```coq
(*** 주석 ***)

(* 주석은 (*와 *)로 묶입니다. 주석을 중첩해도 괜찮습니다. *)

(* 한 줄 주석은 없습니다. *)

(*** 변수 및 함수 ***)

(* Coq 증명 보조기는 Vernacular라는 명령 언어로 제어하고 쿼리할 수 있습니다. Vernacular 키워드는 대문자로 시작하고 명령은 마침표로 끝납니다. 변수 및 함수 선언은 Definition Vernacular로 형성됩니다. *)

Definition x := 10.

(* Coq는 때때로 인수의 유형을 추론할 수 있지만, 유형으로 주석을 다는 것이 일반적인 관행입니다. *)

Definition inc_nat (x : nat) : nat := x + 1.

(* 정보를 쿼리하기 위한 많은 Vernacular 명령이 존재합니다. 이것들은 매우 유용할 수 있습니다. *)

Compute (1 + 1). (* 2 : nat *) (* 결과 계산. *)

Check tt. (* tt : unit *) (* 표현식의 유형 확인 *)

About plus. (* 객체에 대한 정보 인쇄 *)

(* 정의를 포함한 정보 인쇄 *)
Print true. (* Inductive bool : Set := true : Bool | false : Bool *)

Search nat. (* nat 관련 값의 큰 목록 반환 *)
Search "_ + _". (* 패턴으로도 검색할 수 있습니다. *)
Search (?a -> ?a -> bool). (* 패턴은 명명된 매개변수를 가질 수 있습니다. *)
Search (?a * ?a).

(* Locate는 표기법이 어디에서 왔는지 알려줍니다. 새로운 표기법을 접할 때 매우 유용합니다. *)

Locate "+".

(* 함수를 충분한 수의 인수로 호출하지 않으면 오류가 발생하지 않고 새 함수가 생성됩니다. *)
Definition make_inc x y := x + y. (* make_inc는 nat -> nat -> nat입니다. *)
Definition inc_2 := make_inc 2.   (* inc_2는 nat -> nat입니다. *)
Compute inc_2 3. (* 5로 평가됩니다. *)


(* 정의는 "let ... in" 구문으로 연결할 수 있습니다. 이는 명령형 언어에서 표현식에 사용하기 전에 여러 변수에 값을 할당하는 것과 거의 동일합니다. *)

Definition add_xy : nat := let x := 10 in
                           let y := 20 in
                           x + y.

(* 패턴 매칭은 명령형 언어의 switch 문과 다소 유사하지만 훨씬 더 표현력이 뛰어납니다. *)

Definition is_zero (x : nat) :=
    match x with
    | 0 => true
    | _ => false  (* "_" 패턴은 "다른 모든 것"을 의미합니다. *)
    end.

(* Fixpoint Vernacular를 사용하여 재귀 함수 정의를 정의할 수 있습니다. *)

Fixpoint factorial n := match n with
                        | 0 => 1
                        | (S n') => n * factorial n'
                        end.

(* 함수 적용은 일반적으로 인수에 괄호가 필요하지 않습니다. *)
Compute factorial 5. (* 120 : nat *)

(* ...인수가 표현식인 경우를 제외하고. *)
Compute factorial (5-1). (* 24 : nat *)

(* "with"를 사용하여 상호 재귀 함수를 정의할 수 있습니다. *)
Fixpoint is_even (n : nat) : bool := match n with
  | 0 => true
  | (S n) => is_odd n
end with
  is_odd n := match n with
  | 0 => false
  | (S n) => is_even n
              end.

(* Coq는 완전한 프로그래밍 언어이므로, 프로그램이 종료됨을 이해할 수 있을 때만 프로그램을 허용합니다. 재귀 호출이 입력의 패턴 일치된 하위 부분에 있을 때 가장 쉽게 볼 수 있습니다. 그러면 입력 크기가 항상 감소하기 때문입니다. Coq가 함수가 종료됨을 이해하도록 하는 것은 항상 쉽지 않습니다. 이 주제에 대한 자세한 내용은 문서 끝의 참고 자료를 참조하십시오. *)

(* 익명 함수는 다음 구문을 사용합니다: *)

Definition my_square : nat -> nat := fun x => x * x.

Definition my_id (A : Type) (x : A) : A := x.
Definition my_id2 : forall A : Type, A -> A := fun A x => x.
Compute my_id nat 3. (* 3 : nat *)

(* Coq에게 밑줄로 용어를 추론하도록 요청할 수 있습니다. *)
Compute my_id _ 3.

(* 함수의 암시적 인수는 컨텍스트 지식에서 추론할 수 있는 인수입니다. {}로 묶인 매개변수는 기본적으로 암시적입니다. *)

Definition my_id3 {A : Type} (x : A) : A := x.
Compute my_id3 3. (* 3 : nat *)

(* 때로는 이것을 끄는 것이 필요할 수 있습니다. @로 모든 인수를 다시 명시적으로 만들 수 있습니다. *)

Compute @my_id3 nat 3.

(* 또는 이름으로 인수 제공 *)
Compute my_id3 (A:=nat) 3.

(* Coq는 OCaml, Haskell 및 Scheme으로 코드를 추출할 수 있습니다. *)
Require Extraction.
Extraction Language OCaml.
Extraction "factorial.ml" factorial.
(* 위는 다음을 포함하는 factorial.ml 및 factorial.mli 파일을 생성합니다:

type nat =
| O
| S of nat

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

(** val factorial : nat -> nat **)

let rec factorial n = match n with
| O -> S O
| S n' -> mul n (factorial n')
*)


(*** 표기법 ***)

(* Coq는 표현식을 더 자연스러운 형태로 작성하는 데 사용할 수 있는 매우 강력한 표기법 시스템을 가지고 있습니다. *)

Compute Nat.add 3 4. (* 7 : nat *)
Compute 3 + 4. (* 7 : nat *)

(* 표기법은 프로그램 텍스트에 적용된 구문 변환으로, 평가되기 전에 적용됩니다. 표기법은 표기법 범위로 구성됩니다. 다른 표기법 범위를 사용하면 오버로딩의 약한 개념을 사용할 수 있습니다. *)

(* 정수 Z와 관련된 정의를 포함하는 Zarith 모듈을 가져옵니다. *)

Require Import ZArith.

(* 표기법 범위는 열 수 있습니다. *)
Open Scope Z_scope.

(* 이제 숫자와 덧셈이 정수에 대해 정의됩니다. *)
Compute 1 + 7. (* 8 : Z *)

(* 정수 같음 확인 *)
Compute 1 =? 2. (* false : bool *)

(* Locate는 표기법의 출처와 정의를 찾는 데 유용합니다. *)
Locate "_ =? _". (* Z.eqb x y : Z_scope *)
Close Scope Z_scope.

(* 이제 nat이 "+"의 기본 해석입니다. *)
Compute 1 + 7. (* 8 : nat *)

(* 범위는 %를 사용하여 인라인으로 열 수도 있습니다. *)
Compute (3 * -7)%Z. (* -21%Z : Z *)

(* Coq는 기본적으로 다음 해석 범위를 선언합니다: core_scope, type_scope, function_scope, nat_scope, bool_scope, list_scope, int_scope, uint_scope. ZArith 및 QArith 모듈에 각각 포함된 숫자 범위 Z_scope(정수) 및 Q_scope(분수)도 원할 수 있습니다. *)

(* 범위 내용을 인쇄할 수 있습니다. *)
Print Scope nat_scope.
(*
Scope nat_scope
Delimiting key is nat
Bound to classes nat Nat.t
"x 'mod' y" := Nat.modulo x y
"x ^ y" := Nat.pow x y
"x ?= y" := Nat.compare x y
"x >= y" := ge x y
"x > y" := gt x y
"x =? y" := Nat.eqb x y
"x <? y" := Nat.ltb x y
"x <=? y" := Nat.leb x y
"x <= y <= z" := and (le x y) (le y z)
"x <= y < z" := and (le x y) (lt y z)
"n <= m" := le n m
"x < y <= z" := and (lt x y) (le y z)
"x < y < z" := and (lt x y) (lt y z)
"x < y" := lt x y
"x / y" := Nat.div x y
"x - y" := Init.Nat.sub x y
"x + y" := Init.Nat.add x y
"x * y" := Init.Nat.mul x y
*)

(* Coq는 QArith 모듈에서 Q 유형으로 정확한 분수를 사용할 수 있습니다.
   부동 소수점 숫자와 실수도 사용할 수 있지만, 속성을 증명하는 것이 다소 까다롭기 때문에 더 고급 주제입니다. *)

Require Import QArith.

Open Scope Q_scope.
Compute 1. (* 1 : Q *)

(* 1과 0만 Q_scope에 의해 분수로 해석됩니다. *)
Compute 2. (* 2 : nat *)
Compute (2 # 3). (* 분수 2/3 *)
Compute (1 # 3) ?= (2 # 6). (* Eq : 비교 *)
Close Scope Q_scope.

Compute ( (2 # 3) / (1 # 5) )%Q. (* 10 # 3 : Q *)


(*** 일반적인 데이터 구조 ***)

(* 표준 라이브러리에 많은 일반적인 데이터 유형이 포함되어 있습니다. *)

(* 단위 유형은 정확히 하나의 값, tt를 가집니다. *)
Check tt. (* tt : unit *)

(* 옵션 유형은 실패할 수 있는 계산을 표현하는 데 유용합니다. *)
Compute None. (* None : option ?A *)
Check Some 3. (* Some 3 : option nat *)

(* 유형 합계 A B는 유형 A 또는 유형 B의 값을 허용합니다. *)
Print sum.
Check inl 3. (* inl 3 : nat + ?B *)
Check inr true. (* inr true : ?A + bool *)
Check sum bool nat. (* (bool + nat)%type : Set *)
Check (bool + nat)%type. (* 합계에 대한 표기법 *)

(* 튜플은 (선택적으로) 괄호로 묶이며, 항목은 쉼표로 구분됩니다. *)
Check (1, true). (* (1, true) : nat * bool *)
Compute prod nat bool. (* (nat * bool)%type : Set *)

Definition my_fst {A B : Type} (x : A * B) : A := match x with
                                                  | (a,b) => a
                                                  end.

(* 패턴 일치가 반박할 수 없는 경우 구조 분해 let을 사용할 수 있습니다. *)
Definition my_fst2 {A B : Type} (x : A * B) : A := let (a,b) := x in
                                                   a.

(*** 목록 ***)

(* 목록은 cons 및 nil을 사용하거나 list_scope에서 사용할 수 있는 표기법을 사용하여 구축됩니다. *)
Compute cons 1 (cons 2 (cons 3 nil)). (*  (1 :: 2 :: 3 :: nil)%list : list nat *)
Compute (1 :: 2 :: 3 :: nil)%list.

(* ListNotations 모듈에서도 목록 표기법을 사용할 수 있습니다. *)
Require Import List.
Import ListNotations.
Compute [1 ; 2 ; 3]. (* [1; 2; 3] : list nat *)


(* 다음을 포함하여 많은 수의 목록 조작 함수를 사용할 수 있습니다:

• length
• head : 첫 번째 요소 (기본값 포함)
• tail : 첫 번째 요소를 제외한 모든 요소
• app : 추가
• rev : 역순
• nth : n번째 요소 액세스 (기본값 포함)
• map : 함수 적용
• flat_map : 목록을 반환하는 함수 적용
• fold_left : 반복자 (머리에서 꼬리까지)
• fold_right : 반복자 (꼬리에서 머리까지)

 *)

Definition my_list : list nat := [47; 18; 34].

Compute List.length my_list. (* 3 : nat *)

(* Coq의 모든 함수는 총체적이어야 하므로 인덱싱에는 기본값이 필요합니다. *)
Compute List.nth 1 my_list 0. (* 18 : nat *)
Compute List.map (fun x => x * 2) my_list. (* [94; 36; 68] : list nat *)
Compute List.filter (fun x => Nat.eqb (Nat.modulo x 2) 0) my_list.
                                               (* [18; 34] : list nat *)
Compute (my_list ++ my_list)%list. (* [47; 18; 34; 47; 18; 34] : list nat *)

(*** 문자열 ***)

Require Import Strings.String.

(* 문자열 리터럴에는 큰따옴표를 사용합니다. *)
Compute "hi"%string.

Open Scope string_scope.

(* 문자열은 "++" 연산자로 연결할 수 있습니다. *)
Compute String.append "Hello " "World". (* "Hello World" : string *)
Compute "Hello " ++ "World". (* "Hello World" : string *)

(* 문자열은 같음을 비교할 수 있습니다. *)
Compute String.eqb "Coq is fun!" "Coq is fun!". (* true : bool *)
Compute "no" =? "way". (* false : bool *)

Close Scope string_scope.

(*** 기타 모듈 ***)

(* 표준 라이브러리에서 관심 있을 만한 다른 모듈:

• Logic : 고전 논리 및 종속 같음
• Arith : 기본 Peano 산술
• PArith : 기본 양의 정수 산술
• NArith : 기본 이진 자연수 산술

• Numbers : 자연수, 정수 및 주기적 숫자에 대한 다양한 접근 방식
            (현재 공리적으로 및 2^31 이진 워드 위에)
• Bool : 부울 (기본 함수 및 결과)

• Lists : 단형 및 다형 목록 (기본 함수 및 결과),
          스트림 (공귀납적 유형으로 정의된 무한 시퀀스)
• Sets : 집합 (고전적, 구성적, 유한, 무한, 멱집합 등)
• FSets : 유한 집합 및 유한 맵의 사양 및 구현
          (목록 및 AVL 트리에 의해)
• Reals : 실수 공리화 (고전적, 기본 함수, 정수 부분, 소수 부분, 극한, 미분, 코시 급수, 멱급수 및 결과 등)
• Relations : 관계 (정의 및 기본 결과)
• Sorting : 정렬된 목록 (기본 정의 및 힙 정렬 정확성)
• Strings : 8비트 문자 및 문자열
• Wellfounded : 잘 정의된 관계 (기본 결과)
 *)

(*** 사용자 정의 데이터 유형 ***)

(* Coq는 종속적으로 유형이 지정되므로 유형 별칭을 정의하는 것은 값을 위한 별칭을 정의하는 것과 다르지 않습니다. *)

Definition my_three : nat := 3.
Definition my_nat : Type := nat.

(* Inductive Vernacular를 사용하여 더 흥미로운 유형을 정의할 수 있습니다.
   간단한 열거형은 다음과 같이 정의할 수 있습니다: *)

Inductive ml := OCaml | StandardML | Coq.
Definition lang := Coq.  (* 유형은 "ml"입니다. *)

(* 유형 생성자는 비어 있을 필요가 없습니다. *)
Inductive my_number := plus_infinity
                     | nat_value : nat -> my_number.
Compute nat_value 3. (* nat_value 3 : my_number *)


(* 레코드 구문은 튜플과 유사한 유형에 대한 설탕입니다. 구성 요소에 대한 명명된 접근자 함수를 정의합니다. 레코드 유형은 {...} 표기법으로 정의됩니다. *)

Record Point2d (A : Set) := mkPoint2d { x2 : A ; y2 : A }.
(* 레코드 값은 {|...|} 표기법으로 구성됩니다. *)
Definition mypoint : Point2d nat :=  {| x2 := 2 ; y2 := 3 |}.
Compute x2 nat mypoint. (* 2 : nat *)
Compute mypoint.(x2 nat). (* 2 : nat *)

(* 유형은 매개변수화될 수 있습니다. 예를 들어 "아무거나의 목록 목록" 유형에서와 같습니다. 'a는 모든 유형으로 대체될 수 있습니다. *)

Definition list_of_lists a := list (list a).
Definition list_list_nat := list_of_lists nat.

(* 유형은 재귀적일 수도 있습니다. 내장된 정수 목록과 유사한 유형에서와 같습니다. *)

Inductive my_nat_list := 
  EmptyList | NatList : nat -> my_nat_list -> my_nat_list.

Compute NatList 1 EmptyList. (*  NatList 1 EmptyList : my_nat_list *)

(** 유형 생성자 일치 **)

Inductive animal := Dog : string -> animal | Cat : string -> animal.

Definition say x :=
    match x with
    | Dog x => (x ++ " says woof")%string
    | Cat x => (x ++ " says meow")%string
    end.

Compute say (Cat "Fluffy"). (* "Fluffy says meow". *)

(** 패턴 일치를 사용한 데이터 구조 순회 **)

(* 재귀 유형은 패턴 일치를 사용하여 쉽게 순회할 수 있습니다.
   내장된 목록 유형의 데이터 구조를 순회하는 방법을 살펴보겠습니다.
   내장된 cons("::")는 중위 연산자처럼 보이지만,
   실제로는 유형 생성자이며 다른 유형과 마찬가지로 일치시킬 수 있습니다. *)
Fixpoint sum_list l :=
    match l with
    | [] => 0
    | head :: tail => head + (sum_list tail)
    end.

Compute sum_list [1; 2; 3]. (* 6으로 평가됩니다. *)


(*** 증명 맛보기 ***)

(* 증명 언어를 설명하는 것은 이 튜토리얼의 범위를 벗어나지만,
   식욕을 돋우기 위한 맛보기입니다. 자세한 내용은 아래 자료를 참조하십시오. *)

(* 종속 유형 기반 정리 증명기의 매력적인 기능은 증명 언어와 프로그래밍 기능에 동일한 기본 구성 요소가 있다는 것입니다. 예를 들어, 순수 Gallina에서 명제 A와 B가 A를 의미함을 작성하고 증명할 수 있습니다. *)

Definition my_theorem : forall A B, A /\ B -> A :=
  fun A B ab => match ab with
                  | (conj a b) => a
                end.

(* 또는 전술을 사용하여 증명할 수 있습니다. 전술은 증명 용어를 더 자연스러운 스타일로 구축하고 일부 지루한 작업을 자동화하는 데 도움이 되는 매크로 언어입니다. *)

Theorem my_theorem2 : forall A B, A /\ B -> A.
Proof.
  intros A B ab.  destruct ab as [ a b ]. apply a.
Qed.

(* 자동화된 전술 ring을 사용하여 간단한 다항식 등식을 쉽게 증명할 수 있습니다. *)

Require Import Ring.
Require Import Arith.
Theorem simple_poly : forall (x : nat), (x + 1) * (x + 2) = x * x + 3 * x + 2.
  Proof. intros. ring. Qed.

(* 여기서는 귀납법을 사용하여 1부터 n까지의 모든 숫자의 합에 대한 닫힌 형식을 증명합니다. *)

Fixpoint sumn (n : nat) : nat :=
  match n with
  | 0 => 0
  | (S n') => n + (sumn n')
  end.

Theorem sum_formula : forall n, 2 * (sumn n) = (n + 1) * n.
Proof. intros n. induction n.
       - reflexivity. (* 0 = 0 기본 사례 *)
       - simpl. ring [IHn]. (* 귀납 단계 *)
Qed.
```

이것은 단지 coq의 겉햝기 입니다. Coq는 흥미롭고 특이한 주제가 많아 현대 연구까지 이어지는 거대한 생태계입니다.

## 더 읽을거리

* [Coq 참조 매뉴얼](https://coq.inria.fr/refman/)
* [소프트웨어 기초](https://softwarefoundations.cis.upenn.edu/)
* [종속 유형을 사용한 인증 프로그래밍](http://adam.chlipala.net/cpdt/)
* [수학적 구성 요소](https://math-comp.github.io/mcb/)
* [Coq'Art: 귀납적 구성의 미적분학](http://www.cse.chalmers.se/research/group/logic/TypesSS05/resources/coq/CoqArt/)
* [FRAP](http://adam.chlipala.net/frap/)