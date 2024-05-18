---
language: ATS
contributors:
  - ["Mark Barbone", "https://github.com/mb64"]
filename: learnats.dats
---

ATS is a low-level functional programming language.  It has a powerful type
system which lets you write programs with the same level of control and
efficiency as C, but in a memory safe and type safe way.

The ATS type system supports:

* Full type erasure: ATS compiles to efficient C
* Dependent types, including [LF](http://twelf.org/wiki/LF) and proving
  metatheorems
* Refinement types
* Linearity for resource tracking
* An effect system that tracks exceptions, mutation, termination, and other
  side effects

This tutorial is not an introduction to functional programming, dependent types,
or linear types, but rather to how they all fit together in ATS.  That said, ATS
is a very complex language, and this tutorial doesn't cover it all.  Not only
does ATS's type system boast a wide array of confusing features, its
idiosyncratic syntax can make even "simple" examples hard to understand.  In the
interest of keeping it a reasonable length, this document is meant to give a
taste of ATS, giving a high-level overview of what's possible and how, rather
than attempting to fully explain how everything works.

You can [try ATS in your browser](http://www.ats-lang.org/SERVER/MYCODE/Patsoptaas_serve.php),
or install it from [http://www.ats-lang.org/](http://www.ats-lang.org/).


```ocaml
// Include the standard library
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// To compile, either use
//   $ patscc -DATS_MEMALLOC_LIBC program.dats -o program
// or install the ats-acc wrapper https://github.com/sparverius/ats-acc and use
//   $ acc pc program.dats

// C-style line comments
/* and C-style block comments */
(* as well as ML-style block comments *)

/*************** Part 1: the ML fragment ****************/

val () = print "Hello, World!\n"

// No currying
fn add (x: int, y: int) = x + y

// fn vs fun is like the difference between let and let rec in OCaml/F#
fun fact (n: int): int = if n = 0 then 1 else n * fact (n-1)

// Multi-argument functions need parentheses when you call them; single-argument
// functions can omit parentheses
val forty_three = add (fact 4, 19)

// let is like let in SML
fn sum_and_prod (x: int, y: int): (int, int) =
  let
    val sum = x + y
    val prod = x * y
  in (sum, prod) end

// 'type' is the type of all heap-allocated, non-linear types
// Polymorphic parameters go in {} after the function name
fn id {a:type} (x: a) = x

// ints aren't heap-allocated, so we can't pass them to 'id'
// val y: int = id 7 // doesn't compile

// 't@ype' is the type of all non-linear potentially unboxed types. It is a
// supertype of 'type'.
// Templated parameters go in {} before the function name
fn {a:t@ype} id2 (x: a) = x

val y: int = id2 7 // works

// can't have polymorphic t@ype parameters
// fn id3 {a:t@ype} (x: a) = x // doesn't compile

// explicity specifying type parameters:
fn id4 {a:type} (x: a) = id {a} x // {} for non-template parameters
fn id5 {a:type} (x: a) = id2<a> x // <> for template parameters
fn id6 {a:type} (x: a) = id {..} x // {..} to explicitly infer it

// Heap allocated shareable datatypes
// using datatypes leaks memory
datatype These (a:t@ype, b:t@ype) = This of a
                                  | That of b
                                  | These of (a, b)

// Pattern matching using 'case'
fn {a,b: t@ype} from_these (x: a, y: b, these: These(a,b)): (a, b) =
  case these of
  | This(x) => (x, y) // Shadowing of variable names is fine; here, x shadows
                      // the parameter x
  | That(y) => (x, y)
  | These(x, y) => (x, y)

// Partial pattern match using 'case-'
// Will throw an exception if passed This
fn {a,b:t@ype} unwrap_that (these: These(a,b)): b =
  case- these of
  | That(y) => y
  | These(_, y) => y


/*************** Part 2: refinements ****************/

// Parameterize functions by what values they take and return
fn cool_add {n:int} {m:int} (x: int n, y: int m): int (n+m) = x + y

// list(a, n) is a list of n a's
fun square_all {n:int} (xs: list(int, n)): list(int, n) =
  case xs of
  | list_nil() => list_nil()
  | list_cons(x, rest) => list_cons(x * x, square_all rest)

fn {a:t@ype} get_first {n:int | n >= 1} (xs: list(a, n)): a =
  case+ xs of // '+' asks ATS to prove it's total
  | list_cons(x, _) => x

// Can't run get_first on lists of length 0
// val x: int = get_first (list_nil()) // doesn't compile

// in the stdlib:
// sortdef nat = {n:int | n >= 0}
// sortdef pos = {n:int | n >= 1}

fn {a:t@ype} also_get_first {n:pos} (xs: list(a, n)): a =
  let
    val+ list_cons(x, _) = xs // val+ also works
  in x end

// tail-recursive reverse
fun {a:t@ype} reverse {n:int} (xs: list(a, n)): list(a, n) =
  let
    // local functions can use type variables from their enclosing scope
    // this one uses both 'a' and 'n'
    fun rev_helper {i:nat} (xs: list(a, n-i), acc: list(a, i)): list(a, n) =
      case xs of
      | list_nil() => acc
      | list_cons(x, rest) => rev_helper(rest, list_cons(x, acc))
  in rev_helper(xs, list_nil) end

// ATS has three context-dependent namespaces
// the two 'int's mean different things in this example, as do the two 'n's
fn namespace_example {n:int} (n: int n): int n = n
//                      ^^^                         sort namespace
//                    ^          ^^^ ^   ^^^ ^      statics namespace
// ^^^^^^^^^^^^^^^^^          ^                  ^  value namespace

// a termination metric can go in .< >.
// it must decrease on each recursive call
// then ATS will prove it doesn't infinitely recurse
fun terminating_factorial {n:nat} .<n>. (n: int n): int =
  if n = 0 then 1 else n * terminating_factorial (n-1)


/*************** Part 3: the LF fragment ****************/

// ATS supports proving theorems in LF (http://twelf.org/wiki/LF)

// Relations are represented by inductive types

// Proofs that the nth fibonacci number is f
dataprop Fib(n:int, m:int) =
  | FibZero(0, 0)
  | FibOne(1, 1)
  | {n, f1, f2: int} FibInd(n, f1 + f2) of (Fib(n-1,f1), Fib(n-2,f2))

// Proved-correct fibonacci implementation
// [A] B is an existential type: "there exists A such that B"
// (proof | value)
fun fib {n:nat} .<n>. (n: int n): [f:int] (Fib(n,f) | int f) =
  if n = 0 then (FibZero | 0) else
  if n = 1 then (FibOne | 1) else
  let
    val (proof1 | val1) = fib (n-1)
    val (proof2 | val2) = fib (n-2)
  // the existential type is inferred
  in (FibInd(proof1, proof2) | val1 + val2) end

// Faster proved-correct fibonacci implementation
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

// Proof-level lists of ints, of type 'sort'
datasort IntList = ILNil of ()
                 | ILCons of (int, IntList)

// ILAppend(x,y,z) iff x ++ y = z
dataprop ILAppend(IntList, IntList, IntList) =
  | {y:IntList} AppendNil(ILNil, y, y)
  | {a:int} {x,y,z: IntList}
    AppendCons(ILCons(a,x), y, ILCons(a,z)) of ILAppend(x,y,z)

// prfuns/prfns are compile-time functions acting on proofs

// metatheorem: append is total
prfun append_total {x,y: IntList} .<x>. (): [z:IntList] ILAppend(x,y,z)
  = scase x of // scase lets you inspect static arguments (only in prfuns)
  | ILNil() => AppendNil
  | ILCons(a,rest) => AppendCons(append_total())


/*************** Part 4: views ****************/

// views are like props, but linear; ie they must be consumed exactly once
// prop is a subtype of view

// 'type @ address' is the most basic view

fn {a:t@ype} read_ptr {l:addr} (pf: a@l | p: ptr l): (a@l | a) =
  let
    // !p searches for usable proofs that say something is at that address
    val x = !p
  in (pf | x) end

// oops, tried to dereference a potentially invalid pointer
// fn {a:t@ype} bad {l:addr} (p: ptr l): a = !p // doesn't compile

// oops, dropped the proof (leaked the memory)
// fn {a:t@ype} bad {l:addr} (pf: a@l | p: ptr l): a = !p // doesn't compile

fn inc_at_ptr {l:addr} (pf: int@l | p: ptr l): (int@l | void) =
  let
    // !p := value writes value to the location at p
    // like !p, it implicitly searches for usable proofs that are in scope
    val () = !p := !p + 1
  in (pf | ()) end

// threading proofs through gets annoying
fn inc_three_times {l:addr} (pf: int@l | p: ptr l): (int@l | void) =
  let
    val (pf2 | ()) = inc_at_ptr (pf | p)
    val (pf3 | ()) = inc_at_ptr (pf2 | p)
    val (pf4 | ()) = inc_at_ptr (pf3 | p)
  in (pf4 | ()) end

// so there's special syntactic sugar for when you don't consume a proof
fn dec_at_ptr {l:addr} (pf: !int@l | p: ptr l): void =
  !p := !p - 1           // ^ note the exclamation point

fn dec_three_times {l:addr} (pf: !int@l | p: ptr l): void =
  let
    val () = dec_at_ptr (pf | p)
    val () = dec_at_ptr (pf | p)
    val () = dec_at_ptr (pf | p)
  in () end

// dataview is like dataprop, but linear
// A proof that either the address is null, or there is a value there
dataview MaybeNull(a:t@ype, addr) =
  | NullPtr(a, null)
  | {l:addr | l > null} NonNullPtr(a, l) of (a @ l)

fn maybe_inc {l:addr} (pf: !MaybeNull(int, l) | p: ptr l): void =
  if ptr1_is_null p
  then ()
  else let
    // Deconstruct the proof to access the proof of a @ l
    prval NonNullPtr(value_exists) = pf
    val () = !p := !p + 1
    // Reconstruct it again for the caller
    prval () = pf := NonNullPtr(value_exists)
  in () end

// array_v(a,l,n) represents and array of n a's at location l
// this gets compiled into an efficient for loop, since all proofs are erased
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

// 'var' is used to create stack-allocated (lvalue) variables
val seven: int = let
    var res: int = 3
    // they can be modified
    val () = res := res + 1
    // addr@ res is a pointer to it, and view@ res is the associated proof
    val (pf | ()) = inc_three_times(view@ res | addr@ res)
    // need to give back the view before the variable goes out of scope
    prval () = view@ res := pf
  in res end

// References let you pass lvalues, like in C++
fn square (x: &int): void =
  x := x * x // they can be modified

val sixteen: int = let
    var res: int = 4
    val () = square res
  in res end

fn inc_at_ref (x: &int): void =
  let
    // like vars, references have views and addresses
    val (pf | ()) = inc_at_ptr(view@ x | addr@ x)
    prval () = view@ x := pf
  in () end

// Like ! for views, & references are only legal as argument types
// fn bad (x: &int): &int = x // doesn't compile

// this takes a proof int n @ l, but returns a proof int (n+1) @ l
// since they're different types, we can't use !int @ l like before
fn refined_inc_at_ptr {n:int}{l:addr} (
  pf: int n @ l | p: ptr l
): (int (n+1) @ l | void) =
  let
    val () = !p := !p + 1
  in (pf | ()) end

// special syntactic sugar for returning a proof at a different type
fn refined_dec_at_ptr {n:int}{l:addr} (
  pf: !int n @ l >> int (n-1) @ l | p: ptr l
): void =
  !p := !p - 1

// legal but very bad code
prfn swap_proofs {v1,v2:view} (a: !v1 >> v2, b: !v2 >> v1): void =
  let
    prval tmp = a
    prval () = a := b
    prval () = b := tmp
  in () end

// also works with references
fn refined_square {n:int} (x: &int n >> int (n*n)): void =
  x := x * x

fn replace {a,b:vtype} (dest: &a >> b, src: b): a =
  let
    val old = dest
    val () = dest := src
  in old end

// values can be uninitialized
fn {a:vt@ype} write (place: &a? >> a, value: a): void =
  place := value

val forty: int = let
    var res: int
    val () = write (res, 40)
  in res end

// viewtype: a view and a type
viewtypedef MaybeNullPtr(a:t@ype) = [l:addr] (MaybeNull(a, l) | ptr l)
// MaybeNullPtr has type 'viewtype' (aka 'vtype')
// type is a subtype of vtype and t@ype is a subtype of vt@ype

// The most general identity function:
fn {a:vt@ype} id7 (x: a) = x

// since they contain views, viewtypes must be used linearly
// fn {a:vt@ype} duplicate (x: a) = (x, x) // doesn't compile
// fn {a:vt@ype} ignore (x: a) = () // doesn't compile

// arrayptr(a,l,n) is a convenient built-in viewtype
fn easier_sum_array {l:addr}{n:nat} (p: !arrayptr(int,l,n), n: int n): int =
  let
    fun loop {i:nat | i <= n} (
      p: !arrayptr(int,l,n), n: int n, i: int i, acc: int
    ): int =
      if i = n
      then acc
      else loop(p, n, i+1, acc + p[i])
  in loop(p, n, 0, 0) end


/*************** Part 5: dataviewtypes ****************/

// a dataviewtype is a heap-allocated non-shared inductive type

// in the stdlib:
// dataviewtype list_vt(a:vt@ype, int) =
//   | list_vt_nil(a, 0)
//   | {n:nat} list_vt_cons(a, n+1) of (a, list_vt(a, n))

fn {a:vt@ype} length {n:int} (l: !list_vt(a, n)): int n =
  let                         // ^ since we're not consuming it
    fun loop {acc:int} (l: !list_vt(a, n-acc), acc: int acc): int n =
      case l of
      | list_vt_nil() => acc
      | list_vt_cons(head, tail) => loop(tail, acc + 1)
  in loop (l, 0) end

//     vvvvv  not vt@ype, because you can't easily get rid of a vt@ype
fun {a:t@ype} destroy_list {n:nat} (l: list_vt(a,n)): void =
  case l of
  // ~ pattern match consumes and frees that node
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, tail) => destroy_list tail

// unlike a datatype, a dataviewtype can be modified:
fun {a:vt@ype} push_back {n:nat} (
  x: a,
  l: &list_vt(a,n) >> list_vt(a,n+1)
): void =
  case l of
  | ~list_vt_nil() => l := list_vt_cons(x, list_vt_nil)
  // @ pattern match disassembles/"unfolds" the datavtype's view, so you can
  // modify its components
  | @list_vt_cons(head, tail) => let
      val () = push_back (x, tail)
      // reassemble it with fold@
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
        // Deallocate empty datavtype nodes with free@
        val () = free@{..}{0} l
        val () = l := list_vt_nil()
      in head end
 /** Equivalently:
  * | ~list_vt_nil() => let
  *     prval () = fold@ l
  *     val+ ~list_vt_cons(head, ~list_vt_nil()) = l
  *     val () = l := list_vt_nil()
  *   in head end
  */
  end

// "holes" (ie uninitialized memory) can be created with _ on the RHS
// This function uses destination-passing-style to copy the list in a single
// tail-recursive pass.
fn {a:t@ype} copy_list {n:nat} (l: !list_vt(a, n)): list_vt(a, n) =
  let
    var res: ptr
    fun loop {k:nat} (l: !list_vt(a, k), hole: &ptr? >> list_vt(a, k)): void =
      case l of
      | list_vt_nil() => hole := list_vt_nil
      | list_vt_cons(first, rest) => let
          val () = hole := list_vt_cons{..}{k-1}(first, _)
          //                                            ^ on RHS: a hole
          val+list_vt_cons(_, new_hole) = hole
          //               ^ on LHS: wildcard pattern (not a hole)
          val () = loop (rest, new_hole)
          prval () = fold@ hole
        in () end
    val () = loop (l, res)
  in res end

// Reverse a linked-list *in place* -- no allocations or frees
fn {a:vt@ype} in_place_reverse {n:nat} (l: list_vt(a, n)): list_vt(a, n) =
  let
    fun loop {k:nat} (l: list_vt(a, n-k), acc: list_vt(a, k)): list_vt(a, n) =
      case l of
      | ~list_vt_nil() => acc
      | @list_vt_cons(x, tail) => let
          val rest = replace(tail, acc)
          // the node 'l' is now part of acc instead of the original list
          prval () = fold@ l
        in loop (rest, l) end
  in loop (l, list_vt_nil) end


/*************** Part 6: miscellaneous extras ****************/

// a record
// Point has type 't@ype'
typedef Point = @{ x= int, y= int }
val origin: Point = @{ x= 0, y= 0 }

// tuples and records are normally unboxed, but there are boxed variants
// BoxedPoint has type 'type'
typedef BoxedPoint = '{ x= int, y= int }
val boxed_pair: '(int,int) = '(5, 3)

// When passing a pair as the single argument to a function, it needs to be
// written @(a,b) to avoid ambiguity with multi-argument functions
val six_plus_seven = let
    fun sum_of_pair (pair: (int,int)) = pair.0 + pair.1
  in sum_of_pair @(6, 7) end

// When a constructor has no associated data, such as None(), the parentheses
// are optional in expressions.  However, they are mandatory in patterns
fn inc_option (opt: Option int) =
  case opt of
  | Some(x) => Some(x+1)
  | None() => None

// ATS has a simple FFI, since it compiles to C and (mostly) uses the C ABI
%{
// Inline C code
int scanf_wrapper(void *format, void *value) {
    return scanf((char *) format, (int *) value);
}
%}
// If you wanted to, you could define a custom dataviewtype more accurately
// describing the result of scanf
extern fn scanf (format: string, value: &int): int = "scanf_wrapper"

fn get_input_number (): Option int =
  let
    var x: int = 0
  in
    if scanf("%d\n", x) = 1
    then Some(x)
    else None
  end

// extern is also used for separate declarations and definitions
extern fn say_hi (): void
// later on, or in another file:
implement say_hi () = print "hi\n"

// if you implement main0, it will run as the main function
// implmnt is an alias for implement
implmnt main0 () = ()

// as well as for axioms:
extern praxi view_id {a:view} (x: a): a
// you don't need to actually implement the axioms, but you could
primplmnt view_id x = x
// primplmnt is an alias for primplement

// Some standard aliases are:
// List0(a) = [n:nat] list(a,n) and List0_vt(a) = [n:nat] list_vt(a,n)
// t0p = t@ype and vt0p = vt@ype
fun {a:t0p} append (xs: List0 a, ys: List0 a): List0 a =
  case xs of
  | list_nil() => ys
  | list_cons(x, xs) => list_cons(x, append(xs, ys))

// there are many ways of doing things after one another
val let_in_example = let
    val () = print "thing one\n"
    val () = print "thing two\n"
  in () end

val parens_example = (print "thing one\n"; print "thing two\n")

val begin_end_example = begin
    print "thing one\n";
    print "thing two\n"; // optional trailing semicolon
  end

// and many ways to use local variables
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

//// The last kind of comment in ATS is an end-of-file comment.

Anything between the four slashes and the end of the file is ignored.

Have fun with ATS!
```

## Learn more

This isn't all there is to ATS -- notably, some core features like closures and
the effect system are left out, as well as other less type-y stuff like modules
and the build system.  If you'd like to write these sections yourself,
contributions would be welcome!

To learn more about ATS, visit the [ATS website](http://www.ats-lang.org/), in
particular the [documentation page](http://www.ats-lang.org/Documents.html).

