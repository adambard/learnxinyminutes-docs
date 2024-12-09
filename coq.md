---
language: Coq
filename: learncoq.v
contributors:
    - ["Philip Zucker", "http://www.philipzucker.com/"]
---

The Coq system is a proof assistant. It is designed to build and verify mathematical proofs. The Coq system contains the functional programming language Gallina and is capable of proving properties about programs written in this language.

Coq is a dependently typed language. This means that the types of the language may depend on the values of variables. In this respect, it is similar to other related languages such as Agda, Idris, F*, Lean, and others. Via the Curry-Howard correspondence, programs, properties and proofs are formalized in the same language.

Coq is developed in OCaml and shares some syntactic and conceptual similarity with it. Coq is a language containing many fascinating but difficult topics. This tutorial will focus on the programming aspects of Coq, rather than the proving. It may be helpful, but not necessary to learn some OCaml first, especially if you are unfamiliar with functional programming. This tutorial is based upon its OCaml equivalent

The standard usage model of Coq is to write it with interactive tool assistance, which operates like a high powered REPL. Two common such editors are the CoqIDE and Proof General Emacs mode.

Inside Proof General `Ctrl+C Ctrl+<Enter>` will evaluate up to your cursor.


```coq
(*** Comments ***)

(* Comments are enclosed in (* and *). It's fine to nest comments. *)

(* There are no single-line comments. *)

(*** Variables and functions ***)

(* The Coq proof assistant can be controlled and queried by a command
   language called the vernacular. Vernacular keywords are capitalized and
   the commands end with a period.  Variable and function declarations are
   formed with the Definition vernacular. *)

Definition x := 10.

(* Coq can sometimes infer the types of arguments, but it is common practice
   to annotate with types. *)

Definition inc_nat (x : nat) : nat := x + 1.

(* There exists a large number of vernacular commands for querying
   information.  These can be very useful. *)

Compute (1 + 1). (* 2 : nat *) (* Compute a result. *)

Check tt. (* tt : unit *) (* Check the type of an expressions *)

About plus. (* Prints information about an object *)

(* Print information including the definition *)
Print true. (* Inductive bool : Set := true : Bool | false : Bool *)

Search nat. (* Returns a large list of nat related values *)
Search "_ + _". (* You can also search on patterns *)
Search (?a -> ?a -> bool). (* Patterns can have named parameters  *)
Search (?a * ?a).

(* Locate tells you where notation is coming from. Very helpful when you
   encounter new notation. *)

Locate "+".

(* Calling a function with insufficient number of arguments does not cause
   an error, it produces a new function. *)
Definition make_inc x y := x + y. (* make_inc is nat -> nat -> nat *)
Definition inc_2 := make_inc 2.   (* inc_2 is nat -> nat *)
Compute inc_2 3. (* Evaluates to 5 *)


(* Definitions can be chained with "let ... in" construct.  This is roughly
   the same to assigning values to multiple variables before using them in
   expressions in imperative languages. *)

Definition add_xy : nat := let x := 10 in
                           let y := 20 in
                           x + y.

(* Pattern matching is somewhat similar to switch statement in imperative
   languages, but offers a lot more expressive power. *)

Definition is_zero (x : nat) :=
    match x with
    | 0 => true
    | _ => false  (* The "_" pattern means "anything else". *)
    end.

(* You can define recursive function definition using the Fixpoint
   vernacular.*)

Fixpoint factorial n := match n with
                        | 0 => 1
                        | (S n') => n * factorial n'
                        end.

(* Function application usually doesn't need parentheses around arguments *)
Compute factorial 5. (* 120 : nat *)

(* ...unless the argument is an expression. *)
Compute factorial (5-1). (* 24 : nat *)

(* You can define mutually recursive functions using "with" *)
Fixpoint is_even (n : nat) : bool := match n with
  | 0 => true
  | (S n) => is_odd n
end with
  is_odd n := match n with
  | 0 => false
  | (S n) => is_even n
              end.

(* As Coq is a total programming language, it will only accept programs when
   it can understand they terminate. It can be most easily seen when the
   recursive call is on a pattern matched out subpiece of the input, as then
   the input is always decreasing in size. Getting Coq to understand that
   functions terminate is not always easy. See the references at the end of
   the article for more on this topic. *)

(* Anonymous functions use the following syntax: *)

Definition my_square : nat -> nat := fun x => x * x.

Definition my_id (A : Type) (x : A) : A := x.
Definition my_id2 : forall A : Type, A -> A := fun A x => x.
Compute my_id nat 3. (* 3 : nat *)

(* You can ask Coq to infer terms with an underscore *)
Compute my_id _ 3.

(* An implicit argument of a function is an argument which can be inferred
   from contextual knowledge. Parameters enclosed in {} are implicit by
   default *)

Definition my_id3 {A : Type} (x : A) : A := x.
Compute my_id3 3. (* 3 : nat *)

(* Sometimes it may be necessary to turn this off. You can make all
   arguments explicit again with @ *)

Compute @my_id3 nat 3.

(* Or give arguments by name *)
Compute my_id3 (A:=nat) 3.

(* Coq has the ability to extract code to OCaml, Haskell, and Scheme *)
Require Extraction.
Extraction Language OCaml.
Extraction "factorial.ml" factorial.
(* The above produces a file factorial.ml and factorial.mli that holds:

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


(*** Notation ***)

(* Coq has a very powerful Notation system that can be used to write
   expressions in more natural forms. *)

Compute Nat.add 3 4. (* 7 : nat *)
Compute 3 + 4. (* 7 : nat *)

(* Notation is a syntactic transformation applied to the text of the program
   before being evaluated. Notation is organized into notation scopes. Using
   different notation scopes allows for a weak notion of overloading. *)

(* Imports the Zarith module holding definitions related to the integers Z *)

Require Import ZArith.

(* Notation scopes can be opened *)
Open Scope Z_scope.

(* Now numerals and addition are defined on the integers. *)
Compute 1 + 7. (* 8 : Z *)

(* Integer equality checking *)
Compute 1 =? 2. (* false : bool *)

(* Locate is useful for finding the origin and definition of notations *)
Locate "_ =? _". (* Z.eqb x y : Z_scope *)
Close Scope Z_scope.

(* We're back to nat being the default interpretation of "+" *)
Compute 1 + 7. (* 8 : nat *)

(* Scopes can also be opened inline with the shorthand % *)
Compute (3 * -7)%Z. (* -21%Z : Z *)

(* Coq declares by default the following interpretation scopes: core_scope,
   type_scope, function_scope, nat_scope, bool_scope, list_scope, int_scope,
   uint_scope. You may also want the numerical scopes Z_scope (integers) and
   Q_scope (fractions) held in the ZArith and QArith module respectively. *)

(* You can print the contents of scopes *)
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

(* Coq has exact fractions available as the type Q in the QArith module.
   Floating point numbers and real numbers are also available but are a more
   advanced topic, as proving properties about them is rather tricky. *)

Require Import QArith.

Open Scope Q_scope.
Compute 1. (* 1 : Q *)

(* Only 1 and 0 are interpreted as fractions by Q_scope *)
Compute 2. (* 2 : nat *)
Compute (2 # 3). (* The fraction 2/3 *)
Compute (1 # 3) ?= (2 # 6). (* Eq : comparison *)
Close Scope Q_scope.

Compute ( (2 # 3) / (1 # 5) )%Q. (* 10 # 3 : Q *)


(*** Common data structures ***)

(* Many common data types are included in the standard library *)

(* The unit type has exactly one value, tt *)
Check tt. (* tt : unit *)

(* The option type is useful for expressing computations that might fail *)
Compute None. (* None : option ?A *)
Check Some 3. (* Some 3 : option nat *)

(* The type sum A B allows for values of either type A or type B *)
Print sum.
Check inl 3. (* inl 3 : nat + ?B *)
Check inr true. (* inr true : ?A + bool *)
Check sum bool nat. (* (bool + nat)%type : Set *)
Check (bool + nat)%type. (* Notation for sum *)

(* Tuples are (optionally) enclosed in parentheses, items are separated
   by commas. *)
Check (1, true). (* (1, true) : nat * bool *)
Compute prod nat bool. (* (nat * bool)%type : Set *)

Definition my_fst {A B : Type} (x : A * B) : A := match x with
                                                  | (a,b) => a
                                                  end.

(* A destructuring let is available if a pattern match is irrefutable *)
Definition my_fst2 {A B : Type} (x : A * B) : A := let (a,b) := x in
                                                   a.

(*** Lists ***)

(* Lists are built by using cons and nil or by using notation available in
   list_scope. *)
Compute cons 1 (cons 2 (cons 3 nil)). (*  (1 :: 2 :: 3 :: nil)%list : list nat *)
Compute (1 :: 2 :: 3 :: nil)%list.

(* There is also list notation available in the ListNotations modules *)
Require Import List.
Import ListNotations.
Compute [1 ; 2 ; 3]. (* [1; 2; 3] : list nat *)


(* There is a large number of list manipulation functions available,
   including:

• length
• head : first element (with default)
• tail : all but first element
• app : appending
• rev : reverse
• nth : accessing n-th element (with default)
• map : applying a function
• flat_map : applying a function returning lists
• fold_left : iterator (from head to tail)
• fold_right : iterator (from tail to head)

 *)

Definition my_list : list nat := [47; 18; 34].

Compute List.length my_list. (* 3 : nat *)

(* All functions in coq must be total, so indexing requires a default value *)
Compute List.nth 1 my_list 0. (* 18 : nat *)
Compute List.map (fun x => x * 2) my_list. (* [94; 36; 68] : list nat *)
Compute List.filter (fun x => Nat.eqb (Nat.modulo x 2) 0) my_list.
                                               (* [18; 34] : list nat *)
Compute (my_list ++ my_list)%list. (* [47; 18; 34; 47; 18; 34] : list nat *)

(*** Strings ***)

Require Import Strings.String.

(* Use double quotes for string literals. *)
Compute "hi"%string.

Open Scope string_scope.

(* Strings can be concatenated with the "++" operator. *)
Compute String.append "Hello " "World". (* "Hello World" : string *)
Compute "Hello " ++ "World". (* "Hello World" : string *)

(* Strings can be compared for equality *)
Compute String.eqb "Coq is fun!" "Coq is fun!". (* true : bool *)
Compute "no" =? "way". (* false : bool *)

Close Scope string_scope.

(*** Other Modules ***)

(* Other Modules in the standard library that may be of interest:

• Logic : Classical logic and dependent equality
• Arith : Basic Peano arithmetic
• PArith : Basic positive integer arithmetic
• NArith : Basic binary natural number arithmetic
• ZArith : Basic relative integer arithmetic

• Numbers : Various approaches to natural, integer and cyclic numbers
            (currently axiomatically and on top of 2^31 binary words)
• Bool : Booleans (basic functions and results)

• Lists : Monomorphic and polymorphic lists (basic functions and results),
          Streams (infinite sequences defined with co-inductive types)
• Sets : Sets (classical, constructive, finite, infinite, power set, etc.)
• FSets : Specification and implementations of finite sets and finite maps
          (by lists and by AVL trees)
• Reals : Axiomatization of real numbers (classical, basic functions,
          integer part, fractional part, limit, derivative, Cauchy series,
          power series and results,...)
• Relations : Relations (definitions and basic results)
• Sorting : Sorted list (basic definitions and heapsort correctness)
• Strings : 8-bit characters and strings
• Wellfounded : Well-founded relations (basic results)
 *)

(*** User-defined data types ***)

(* Because Coq is dependently typed, defining type aliases is no different
   than defining an alias for a value. *)

Definition my_three : nat := 3.
Definition my_nat : Type := nat.

(* More interesting types can be defined using the Inductive vernacular.
   Simple enumeration can be defined like so *)

Inductive ml := OCaml | StandardML | Coq.
Definition lang := Coq.  (* Has type "ml". *)

(* For more complicated types, you will need to specify the types of the
   constructors. *)

(* Type constructors don't need to be empty. *)
Inductive my_number := plus_infinity
                     | nat_value : nat -> my_number.
Compute nat_value 3. (* nat_value 3 : my_number *)


(* Record syntax is sugar for tuple-like types. It defines named accessor
   functions for the components. Record types are defined with the notation
   {...} *)

Record Point2d (A : Set) := mkPoint2d { x2 : A ; y2 : A }.
(* Record values are constructed with the notation {|...|} *)
Definition mypoint : Point2d nat :=  {| x2 := 2 ; y2 := 3 |}.
Compute x2 nat mypoint. (* 2 : nat *)
Compute mypoint.(x2 nat). (* 2 : nat *)

(* Types can be parameterized, like in this type for "list of lists of
   anything". 'a can be substituted with any type. *)

Definition list_of_lists a := list (list a).
Definition list_list_nat := list_of_lists nat.

(* Types can also be recursive. Like in this type analogous to
   built-in list of naturals. *)

Inductive my_nat_list :=
  EmptyList | NatList : nat -> my_nat_list -> my_nat_list.

Compute NatList 1 EmptyList. (*  NatList 1 EmptyList : my_nat_list *)

(** Matching type constructors **)

Inductive animal := Dog : string -> animal | Cat : string -> animal.

Definition say x :=
    match x with
    | Dog x => (x ++ " says woof")%string
    | Cat x => (x ++ " says meow")%string
    end.

Compute say (Cat "Fluffy"). (* "Fluffy says meow". *)

(** Traversing data structures with pattern matching **)

(* Recursive types can be traversed with pattern matching easily.
   Let's see how we can traverse a data structure of the built-in list type.
   Even though the built-in cons ("::") looks like an infix operator,
   it's actually a type constructor and can be matched like any other. *)
Fixpoint sum_list l :=
    match l with
    | [] => 0
    | head :: tail => head + (sum_list tail)
    end.

Compute sum_list [1; 2; 3]. (* Evaluates to 6 *)


(*** A Taste of Proving ***)

(* Explaining the proof language is out of scope for this tutorial, but here
   is a taste to whet your appetite. Check the resources below for more. *)

(* A fascinating feature of dependently type based theorem provers is that
   the same primitive constructs underly the proof language as the
   programming features.  For example, we can write and prove the
   proposition A and B implies A in raw Gallina *)

Definition my_theorem : forall A B, A /\ B -> A :=
  fun A B ab => match ab with
                  | (conj a b) => a
                end.

(* Or we can prove it using tactics. Tactics are a macro language to help
   build proof terms in a more natural style and automate away some
   drudgery. *)

Theorem my_theorem2 : forall A B, A /\ B -> A.
Proof.
  intros A B ab.  destruct ab as [ a b ]. apply a.
Qed.

(* We can easily prove simple polynomial equalities using the
   automated tactic ring. *)

Require Import Ring.
Require Import Arith.
Theorem simple_poly : forall (x : nat), (x + 1) * (x + 2) = x * x + 3 * x + 2.
  Proof. intros. ring. Qed.

(* Here we prove the closed form for the sum of all numbers 1 to n using
   induction *)

Fixpoint sumn (n : nat) : nat :=
  match n with
  | 0 => 0
  | (S n') => n + (sumn n')
  end.

Theorem sum_formula : forall n, 2 * (sumn n) = (n + 1) * n.
Proof. intros n. induction n.
       - reflexivity. (* 0 = 0 base case *)
       - simpl. ring [IHn]. (* induction step *)
Qed.
```

With this we have only scratched the surface of Coq. It is a massive
ecosystem with many interesting and peculiar topics leading all the way up
to modern research.

## Further reading

* [The Coq reference manual](https://coq.inria.fr/refman/)
* [Software Foundations](https://softwarefoundations.cis.upenn.edu/)
* [Certified Programming with Dependent Types](http://adam.chlipala.net/cpdt/)
* [Mathematical Components](https://math-comp.github.io/mcb/)
* [Coq'Art: The Calculus of Inductive Constructions](http://www.cse.chalmers.se/research/group/logic/TypesSS05/resources/coq/CoqArt/)
* [FRAP](http://adam.chlipala.net/frap/)
