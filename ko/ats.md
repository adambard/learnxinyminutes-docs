---
name: ATS
contributors:
  - ["Mark Barbone", "https://github.com/mb64"]
filename: learnats.dats
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

ATS는 저수준 함수형 프로그래밍 언어입니다. C와 동일한 수준의 제어 및 효율성으로 프로그램을 작성할 수 있는 강력한 타입 시스템을 갖추고 있지만, 메모리 안전 및 타입 안전 방식으로 작성할 수 있습니다.

ATS 타입 시스템은 다음을 지원합니다:

* 전체 타입 삭제: ATS는 효율적인 C로 컴파일됩니다.
* 종속 타입, [LF](http://twelf.org/wiki/LF) 및 메타정리 증명 포함
* 정제 타입
* 리소스 추적을 위한 선형성
* 예외, 변경, 종료 및 기타 부작용을 추적하는 효과 시스템

이 튜토리얼은 함수형 프로그래밍, 종속 타입 또는 선형 타입에 대한 소개가 아니라 ATS에서 이 모든 것이 어떻게 함께 작동하는지에 대한 것입니다. 즉, ATS는 매우 복잡한 언어이며 이 튜토리얼에서는 모든 것을 다루지 않습니다. ATS의 타입 시스템은 혼란스러운 기능을 광범위하게 자랑할 뿐만 아니라, 특이한 구문으로 인해 "간단한" 예제조차 이해하기 어려울 수 있습니다. 합리적인 길이를 유지하기 위해 이 문서는 가능한 것과 방법에 대한 높은 수준의 개요를 제공하여 ATS의 맛을 보여주는 것을 목표로 하며, 모든 것이 어떻게 작동하는지 완전히 설명하려고 시도하지는 않습니다.

[브라우저에서 ATS를 사용해 보거나](http://www.ats-lang.org/SERVER/MYCODE/Patsoptaas_serve.php) [http://www.ats-lang.org/](http://www.ats-lang.org/)에서 설치할 수 있습니다.


```ocaml
// 표준 라이브러리 포함
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// 컴파일하려면 다음 중 하나를 사용하십시오.
//   $ patscc -DATS_MEMALLOC_LIBC program.dats -o program
// 또는 ats-acc 래퍼 https://github.com/sparverius/ats-acc를 설치하고 다음을 사용하십시오.
//   $ acc pc program.dats

// C 스타일 줄 주석
/* 및 C 스타일 블록 주석 */
(* ML 스타일 블록 주석도 마찬가지입니다 *)

/*************** 1부: ML 조각 ****************/

val () = print "Hello, World!\n"

// 커링 없음
fn add (x: int, y: int) = x + y

// fn 대 fun은 OCaml/F#에서 let과 let rec의 차이와 같습니다.
fun fact (n: int): int = if n = 0 then 1 else n * fact (n-1)

// 다중 인수 함수는 호출할 때 괄호가 필요합니다. 단일 인수
// 함수는 괄호를 생략할 수 있습니다.
val forty_three = add (fact 4, 19)

// let은 SML의 let과 같습니다.
fn sum_and_prod (x: int, y: int): (int, int) =
  let
    val sum = x + y
    val prod = x * y
  in (sum, prod) end

// 'type'은 힙에 할당된 모든 비선형 타입의 타입입니다.
// 다형성 매개변수는 함수 이름 뒤에 {}로 들어갑니다.
fn id {a:type} (x: a) = x

// int는 힙에 할당되지 않으므로 'id'에 전달할 수 없습니다.
// val y: int = id 7 // 컴파일되지 않음

// 't@ype'은 잠재적으로 언박싱된 모든 비선형 타입의 타입입니다. 'type'의
// 상위 타입입니다.
// 템플릿 매개변수는 함수 이름 앞에 {}로 들어갑니다.
fn {a:t@ype} id2 (x: a) = x

val y: int = id2 7 // 작동함

// 다형성 t@ype 매개변수를 가질 수 없습니다.
// fn id3 {a:t@ype} (x: a) = x // 컴파일되지 않음

// 타입 매개변수 명시적 지정:
fn id4 {a:type} (x: a) = id {a} x // 비템플릿 매개변수에 대한 {}
fn id5 {a:type} (x: a) = id2<a> x // 템플릿 매개변수에 대한 <>
fn id6 {a:type} (x: a) = id {..} x // 명시적으로 추론하기 위한 {..}

// 힙 할당 공유 데이터 타입
// 데이터 타입을 사용하면 메모리가 누수됩니다.
datatype These (a:t@ype, b:t@ype) = This of a
                                  | That of b
                                  | These of (a, b)

// 'case'를 사용한 패턴 매칭
fn {a,b: t@ype} from_these (x: a, y: b, these: These(a,b)): (a, b) =
  case these of
  | This(x) => (x, y) // 변수 이름의 섀도잉은 괜찮습니다. 여기서 x는
                      // 매개변수 x를 섀도잉합니다.
  | That(y) => (x, y)
  | These(x, y) => (x, y)

// 'case-'를 사용한 부분 패턴 매칭
// This를 전달하면 예외가 발생합니다.
fn {a,b:t@ype} unwrap_that (these: These(a,b)): b =
  case- these of
  | That(y) => y
  | These(_, y) => y


/*************** 2부: 정제 ****************/

// 함수가 취하고 반환하는 값으로 함수를 매개변수화합니다.
fn cool_add {n:int} {m:int} (x: int n, y: int m): int (n+m) = x + y

// list(a, n)은 n개의 a로 구성된 목록입니다.
fun square_all {n:int} (xs: list(int, n)): list(int, n) =
  case xs of
  | list_nil() => list_nil()
  | list_cons(x, rest) => list_cons(x * x, square_all rest)

fn {a:t@ype} get_first {n:int | n >= 1} (xs: list(a, n)): a =
  case+ xs of // '+'는 ATS에게 전체임을 증명하도록 요청합니다.
  | list_cons(x, _) => x

// 길이가 0인 목록에서 get_first를 실행할 수 없습니다.
// val x: int = get_first (list_nil()) // 컴파일되지 않음

// stdlib에서:
// sortdef nat = {n:int | n >= 0}
// sortdef pos = {n:int | n >= 1}

fn {a:t@ype} also_get_first {n:pos} (xs: list(a, n)): a =
  let
    val+ list_cons(x, _) = xs // val+도 작동합니다.
  in x end

// 꼬리 재귀 역순
fun {a:t@ype} reverse {n:int} (xs: list(a, n)): list(a, n) =
  let
    // 지역 함수는 둘러싸는 범위의 타입 변수를 사용할 수 있습니다.
    // 이 함수는 'a'와 'n'을 모두 사용합니다.
    fun rev_helper {i:nat} (xs: list(a, n-i), acc: list(a, i)): list(a, n) =
      case xs of
      | list_nil() => acc
      | list_cons(x, rest) => rev_helper(rest, list_cons(x, acc))
  in rev_helper(xs, list_nil) end

// ATS에는 세 가지 컨텍스트 종속 네임스페이스가 있습니다.
// 이 예제에서 두 개의 'int'는 두 개의 'n'과 마찬가지로 다른 것을 의미합니다.
fn namespace_example {n:int} (n: int n): int n = n
//                      ^^^                         정렬 네임스페이스
//                    ^          ^^^ ^   ^^^ ^      정적 네임스페이스
// ^^^^^^^^^^^^^^^^^          ^                  ^  값 네임스페이스

// 종료 메트릭은 .< >.에 들어갈 수 있습니다.
// 각 재귀 호출에서 감소해야 합니다.
// 그러면 ATS는 무한 재귀가 아님을 증명합니다.
fun terminating_factorial {n:nat} .<n>. (n: int n): int =
  if n = 0 then 1 else n * terminating_factorial (n-1)


/*************** 3부: LF 조각 ****************/

// ATS는 LF(http://twelf.org/wiki/LF)에서 정리 증명을 지원합니다.

// 관계는 귀납적 타입으로 표현됩니다.

// n번째 피보나치 수가 f라는 증명
dataprop Fib(n:int, m:int) =
  | FibZero(0, 0)
  | FibOne(1, 1)
  | {n, f1, f2: int} FibInd(n, f1 + f2) of (Fib(n-1,f1), Fib(n-2,f2))

// 증명된 올바른 피보나치 구현
// [A] B는 실존 타입입니다: "B인 A가 존재합니다"
// (증명 | 값)
fun fib {n:nat} .<n>. (n: int n): [f:int] (Fib(n,f) | int f) =
  if n = 0 then (FibZero | 0) else
  if n = 1 then (FibOne | 1) else
  let
    val (proof1 | val1) = fib (n-1)
    val (proof2 | val2) = fib (n-2)
  // 실존 타입이 추론됩니다.
  in (FibInd(proof1, proof2) | val1 + val2) end

// 더 빠른 증명된 올바른 피보나치 구현
fn fib_tail {n:nat} (n: int n): [f:int] (Fib(n,f) | int f) =
  let
    fun loop {i:int | i < n} {f1, f2: int} .<n - i>.
          (p1: Fib(i,f1), p2: Fib(i+1,f2)
          | i: int i, f1: int f1, f2: int f2, n: int n
          ): [f:int] (Fib(n,f) | int f) =
      if i = n - 1
      then (p2 | f2)
      else loop (p2, FibInd(p2,p1) | i+1, f2, f1+f2, n)
  in if n = 0 then (FibZero | 0) else loop (FibZero, FibOne | 0, 0, 1, n) end

// 증명 수준의 int 목록, 타입 'sort'
datasort IntList = ILNil of ()
                 | ILCons of (int, IntList)

// ILAppend(x,y,z) iff x ++ y = z
dataprop ILAppend(IntList, IntList, IntList) =
  | {y:IntList} AppendNil(ILNil, y, y)
  | {a:int} {x,y,z: IntList}
    AppendCons(ILCons(a,x), y, ILCons(a,z)) of ILAppend(x,y,z)

// prfuns/prfns는 증명에 작용하는 컴파일 타임 함수입니다.

// 메타정리: append는 전체입니다.
prfun append_total {x,y: IntList} .<x>. (): [z:IntList] ILAppend(x,y,z)
  = scase x of // scase를 사용하면 정적 인수를 검사할 수 있습니다(prfun에서만).
  | ILNil() => AppendNil
  | ILCons(a,rest) => AppendCons(append_total())


/*************** 4부: 뷰 ****************/

// 뷰는 소품과 같지만 선형입니다. 즉, 정확히 한 번 소비되어야 합니다.
// 소품은 뷰의 하위 타입입니다.

// 'type @ address'는 가장 기본적인 뷰입니다.

fn {a:t@ype} read_ptr {l:addr} (pf: a@l | p: ptr l): (a@l | a) =
  let
    // !p는 해당 주소에 무언가가 있다는 사용 가능한 증명을 검색합니다.
    val x = !p
  in (pf | x) end

// 이런, 잠재적으로 유효하지 않은 포인터를 역참조하려고 했습니다.
// fn {a:t@ype} bad {l:addr} (p: ptr l): a = !p // 컴파일되지 않음

// 이런, 증명을 떨어뜨렸습니다(메모리 누수).
// fn {a:t@ype} bad {l:addr} (pf: a@l | p: ptr l): a = !p // 컴파일되지 않음

fn inc_at_ptr {l:addr} (pf: int@l | p: ptr l): (int@l | void) =
  let
    // !p := 값은 p의 위치에 값을 씁니다.
    // !p와 마찬가지로 범위에 있는 사용 가능한 증명을 암시적으로 검색합니다.
    val () = !p := !p + 1
  in (pf | ()) end

// 증명을 스레딩하는 것은 짜증납니다.
fn inc_three_times {l:addr} (pf: int@l | p: ptr l): (int@l | void) =
  let
    val (pf2 | ()) = inc_at_ptr (pf | p)
    val (pf3 | ()) = inc_at_ptr (pf2 | p)
    val (pf4 | ()) = inc_at_ptr (pf3 | p)
  in (pf4 | ()) end

// 그래서 증명을 소비하지 않을 때 특별한 구문 설탕이 있습니다.
fn dec_at_ptr {l:addr} (pf: !int@l | p: ptr l): void =
  !p := !p - 1           // ^ 느낌표를 주목하십시오.

fn dec_three_times {l:addr} (pf: !int@l | p: ptr l): void =
  let
    val () = dec_at_ptr (pf | p)
    val () = dec_at_ptr (pf | p)
    val () = dec_at_ptr (pf | p)
  in () end

// dataview는 dataprop과 같지만 선형입니다.
// 주소가 null이거나 값이 있다는 증명입니다.
dataview MaybeNull(a:t@ype, addr) =
  | NullPtr(a, null)
  | {l:addr | l > null} NonNullPtr(a, l) of (a @ l)

fn maybe_inc {l:addr} (pf: !MaybeNull(int, l) | p: ptr l): void =
  if ptr1_is_null p
  then ()
  else let
    // a @ l의 증명에 접근하기 위해 증명을 해체합니다.
    prval NonNullPtr(value_exists) = pf
    val () = !p := !p + 1
    // 호출자를 위해 다시 재구성합니다.
    prval () = pf := NonNullPtr(value_exists)
  in () end

// array_v(a,l,n)은 위치 l에 있는 n개의 a로 구성된 배열을 나타냅니다.
// 이것은 모든 증명이 삭제되므로 효율적인 for 루프로 컴파일됩니다.
fn sum_array {l:addr}{n:nat} (pf: !array_v(int,l,n) | p: ptr l, n: int n): int =
  let
    fun loop {l:addr}{n:nat} .<n>. (
      pf: !array_v(int,l,n)
    | ptr: ptr l,
      length: int n,
      acc: int
    ): int = if length = 0
      then acc
      else let
        prval (head, rest) = array_v_uncons(pf)
        val result = loop(rest | ptr_add<int>(ptr, 1), length - 1, acc + !ptr)
        prval () = pf := array_v_cons(head, rest)
      in result end
  in loop (pf | p, n, 0) end

// 'var'는 스택 할당(lvalue) 변수를 만드는 데 사용됩니다.
val seven: int = let
    var res: int = 3
    // 수정할 수 있습니다.
    val () = res := res + 1
    // addr@ res는 이에 대한 포인터이고, view@ res는 관련 증명입니다.
    val (pf | ()) = inc_three_times(view@ res | addr@ res)
    // 변수가 범위를 벗어나기 전에 뷰를 반환해야 합니다.
    prval () = view@ res := pf
  in res end

// 참조를 사용하면 C++에서와 같이 lvalue를 전달할 수 있습니다.
fn square (x: &int): void =
  x := x * x // 수정할 수 있습니다.

val sixteen: int = let
    var res: int = 4
    val () = square res
  in res end

fn inc_at_ref (x: &int): void =
  let
    // var와 마찬가지로 참조에는 뷰와 주소가 있습니다.
    val (pf | ()) = inc_at_ptr(view@ x | addr@ x)
    prval () = view@ x := pf
  in () end

// 뷰에 대한 !와 마찬가지로 & 참조는 인수 타입으로만 합법적입니다.
// fn bad (x: &int): &int = x // 컴파일되지 않음

// 이것은 증명 int n @ l을 취하지만 증명 int (n+1) @ l을 반환합니다.
// 다른 타입이므로 이전처럼 !int @ l을 사용할 수 없습니다.
fn refined_inc_at_ptr {n:int}{l:addr} (
  pf: int n @ l | p: ptr l
): (int (n+1) @ l | void) =
  let
    val () = !p := !p + 1
  in (pf | ()) end

// 다른 타입으로 증명을 반환하기 위한 특별한 구문 설탕
fn refined_dec_at_ptr {n:int}{l:addr} (
  pf: !int n @ l >> int (n-1) @ l | p: ptr l
): void =
  !p := !p - 1

// 합법적이지만 매우 나쁜 코드
prfn swap_proofs {v1,v2:view} (a: !v1 >> v2, b: !v2 >> v1): void =
  let
    prval tmp = a
    prval () = a := b
    prval () = b := tmp
  in () end

// 참조에서도 작동합니다.
fn refined_square {n:int} (x: &int n >> int (n*n)): void =
  x := x * x

fn replace {a,b:vtype} (dest: &a >> b, src: b): a =
  let
    val old = dest
    val () = dest := src
  in old end

// 값은 초기화되지 않을 수 있습니다.
fn {a:vt@ype} write (place: &a? >> a, value: a): void =
  place := value

val forty: int = let
    var res: int
    val () = write (res, 40)
  in res end

// viewtype: 뷰와 타입
viewtypedef MaybeNullPtr(a:t@ype) = [l:addr] (MaybeNull(a, l) | ptr l)
// MaybeNullPtr은 타입 'viewtype'(일명 'vtype')을 가집니다.
// 타입은 vtype의 하위 타입이고 t@ype는 vt@ype의 하위 타입입니다.

// 가장 일반적인 항등 함수:
fn {a:vt@ype} id7 (x: a) = x

// 뷰를 포함하므로 viewtype은 선형으로 사용해야 합니다.
// fn {a:vt@ype} duplicate (x: a) = (x, x) // 컴파일되지 않음
// fn {a:vt@ype} ignore (x: a) = () // 컴파일되지 않음

// arrayptr(a,l,n)은 편리한 내장 viewtype입니다.
fn easier_sum_array {l:addr}{n:nat} (p: !arrayptr(int,l,n), n: int n): int =
  let
    fun loop {i:nat | i <= n} (
      p: !arrayptr(int,l,n), n: int n, i: int i, acc: int
    ): int =
      if i = n
      then acc
      else loop(p, n, i+1, acc + p[i])
  in loop(p, n, 0, 0) end


/*************** 5부: dataviewtypes ****************/

// dataviewtype은 힙 할당 비공유 귀납적 타입입니다.

// stdlib에서:
// dataviewtype list_vt(a:vt@ype, int) =
//   | list_vt_nil(a, 0)
//   | {n:nat} list_vt_cons(a, n+1) of (a, list_vt(a, n))

fn {a:vt@ype} length {n:int} (l: !list_vt(a, n)): int n =
  let                         // ^ 소비하지 않으므로
    fun loop {acc:int} (l: !list_vt(a, n-acc), acc: int acc): int n =
      case l of
      | list_vt_nil() => acc
      | list_vt_cons(head, tail) => loop(tail, acc + 1)
  in loop (l, 0) end

//     vvvvv  vt@ype가 아닙니다. vt@ype를 쉽게 제거할 수 없기 때문입니다.
fun {a:t@ype} destroy_list {n:nat} (l: list_vt(a,n)): void =
  case l of
  // ~ 패턴 매치는 해당 노드를 소비하고 해제합니다.
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, tail) => destroy_list tail

// 데이터 타입과 달리 dataviewtype은 수정할 수 있습니다:
fun {a:vt@ype} push_back {n:nat} (
  x: a,
  l: &list_vt(a,n) >> list_vt(a,n+1)
): void =
  case l of
  | ~list_vt_nil() => l := list_vt_cons(x, list_vt_nil)
  // @ 패턴 매치는 datavtype의 뷰를 분해/"펼치"므로
  // 구성 요소를 수정할 수 있습니다.
  | @list_vt_cons(head, tail) => let
      val () = push_back (x, tail)
      // fold@로 다시 조립합니다.
      prval () = fold@ l
    in () end

fun {a:vt@ype} pop_last {n:pos} (l: &list_vt(a,n) >> list_vt(a,n-1)): a =
  let
    val+ @list_vt_cons(head, tail) = l
  in case tail of
    | list_vt_cons _ => let
        val res = pop_last tail
        prval () = fold@ l
      in res end
    | ~list_vt_nil() => let
        val head = head
        // free@로 빈 datavtype 노드를 할당 해제합니다.
        val () = free@{..}{0} l
        val () = l := list_vt_nil()
      in head end
 /** 동등하게:
  * | ~list_vt_nil() => let
  *     prval () = fold@ l
  *     val+ ~list_vt_cons(head, ~list_vt_nil()) = l
  *     val () = l := list_vt_nil()
  *   in head end
  */
  end

// "구멍"(즉, 초기화되지 않은 메모리)은 RHS에 _를 사용하여 만들 수 있습니다.
// 이 함수는 단일 꼬리 재귀 패스에서 목록을 복사하기 위해 대상 전달 스타일을 사용합니다.
fn {a:t@ype} copy_list {n:nat} (l: !list_vt(a, n)): list_vt(a, n) =
  let
    var res: ptr
    fun loop {k:nat} (l: !list_vt(a, k), hole: &ptr? >> list_vt(a, k)): void =
      case l of
      | list_vt_nil() => hole := list_vt_nil
      | list_vt_cons(first, rest) => let
          val () = hole := list_vt_cons{..}{k-1}(first, _)
          //                                            ^ RHS: 구멍
          val+list_vt_cons(_, new_hole) = hole
          //               ^ LHS: 와일드카드 패턴(구멍 아님)
          val () = loop (rest, new_hole)
          prval () = fold@ hole
        in () end
    val () = loop (l, res)
  in res end

// 연결 리스트를 *제자리에서* 뒤집습니다 -- 할당이나 해제가 없습니다.
fn {a:vt@ype} in_place_reverse {n:nat} (l: list_vt(a, n)): list_vt(a, n) =
  let
    fun loop {k:nat} (l: list_vt(a, n-k), acc: list_vt(a, k)): list_vt(a, n) =
      case l of
      | ~list_vt_nil() => acc
      | @list_vt_cons(x, tail) => let
          val rest = replace(tail, acc)
          // 노드 'l'은 이제 원래 목록 대신 acc의 일부입니다.
          prval () = fold@ l
        in loop (rest, l) end
  in loop (l, list_vt_nil) end


/*************** 6부: 기타 추가 사항 ****************/

// 레코드
// Point는 타입 't@ype'을 가집니다.
typedef Point = @{ x= int, y= int }
val origin: Point = @{ x= 0, y= 0 }

// 튜플과 레코드는 일반적으로 언박싱되지만, 박싱된 변형이 있습니다.
// BoxedPoint는 타입 'type'을 가집니다.
typedef BoxedPoint = '{ x= int, y= int }
val boxed_pair: '(int,int) = '(5, 3)

// 함수에 단일 인수로 쌍을 전달할 때, 다중 인수 함수와의 모호성을 피하기 위해
// @(a,b)로 작성해야 합니다.
val six_plus_seven = let
    fun sum_of_pair (pair: (int,int)) = pair.0 + pair.1
  in sum_of_pair @(6, 7) end

// 생성자에 관련 데이터가 없는 경우(예: None()), 표현식에서 괄호는
// 선택 사항입니다. 그러나 패턴에서는 필수입니다.
fn inc_option (opt: Option int) =
  case opt of
  | Some(x) => Some(x+1)
  | None() => None

// ATS는 간단한 FFI를 가지고 있습니다. C로 컴파일되고 (대부분) C ABI를 사용하기 때문입니다.
%{ 
// 인라인 C 코드
int scanf_wrapper(void *format, void *value) {
    return scanf((char *) format, (int *) value);
}
%}
// 원한다면 scanf 결과에 대해 더 정확하게 설명하는 사용자 정의 dataviewtype을 정의할 수 있습니다.
extern fn scanf (format: string, value: &int): int = "scanf_wrapper"

fn get_input_number (): Option int =
  let
    var x: int = 0
  in
    if scanf("%d\n", x) = 1
    then Some(x)
    else None
  end

// extern은 별도의 선언 및 정의에도 사용됩니다.
extern fn say_hi (): void
// 나중에 또는 다른 파일에서:
implement say_hi () = print "hi\n"

// main0을 구현하면 주 함수로 실행됩니다.
// implmnt는 implement의 별칭입니다.
implmnt main0 () = ()

// 공리에 대해서도 마찬가지입니다:
extern praxi view_id {a:view} (x: a): a
// 실제로 공리를 구현할 필요는 없지만, 할 수 있습니다.
primplmnt view_id x = x
// primplmnt는 primplement의 별칭입니다.

// 일부 표준 별칭은 다음과 같습니다:
// List0(a) = [n:nat] list(a,n) 및 List0_vt(a) = [n:nat] list_vt(a,n)
// t0p = t@ype 및 vt0p = vt@ype
fun {a:t0p} append (xs: List0 a, ys: List0 a): List0 a =
  case xs of
  | list_nil() => ys
  | list_cons(x, xs) => list_cons(x, append(xs, ys))

// 차례로 작업을 수행하는 방법은 여러 가지가 있습니다.
val let_in_example = let
    val () = print "thing one\n"
    val () = print "thing two\n"
  in () end

val parens_example = (print "thing one\n"; print "thing two\n")

val begin_end_example = begin
    print "thing one\n";
    print "thing two\n"; // 선택적 후행 세미콜론
  end

// 지역 변수를 사용하는 방법도 여러 가지가 있습니다.
fun times_four_let x =
  let
    fun times_two (x: int) = x * 2
  in times_two (times_two x) end

local
  fun times_two (x: int) = x * 2
in
  fun times_four_local x = times_two (times_two x)
end

fun times_four_where x = times_two (times_two x)
  where {
    fun times_two (x: int) = x * 2
  }

//// ATS의 마지막 주석 종류는 파일 끝 주석입니다.

네 개의 슬래시와 파일 끝 사이에 있는 모든 것은 무시됩니다.

ATS를 즐기세요!
```

## 더 알아보기

이것이 ATS의 전부는 아닙니다 -- 특히 클로저 및 효과 시스템과 같은 일부 핵심 기능은 생략되었으며, 모듈 및 빌드 시스템과 같은 덜 타입적인 내용도 생략되었습니다. 이 섹션을 직접 작성하고 싶다면 기여를 환영합니다!

ATS에 대해 더 자세히 알아보려면 [ATS 웹사이트](http://www.ats-lang.org/), 특히 [문서 페이지](http://www.ats-lang.org/Documents.html)를 방문하십시오.

```