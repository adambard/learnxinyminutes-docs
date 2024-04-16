---
language: "Lean 4"
filename: learnlean4.lean
contributors:
    - ["Balagopal Komarath", "https://bkomarath.rbgo.in/"]
---

[Lean 4](https://lean-lang.org/) is a dependently typed functional programming
language and an interactive theorem prover.

```lean4
/-
Imports have to be at the top of the file.

We will need `use` for some of the proofs below.
-/
import Mathlib.Tactic.Use

/-
An enumerated data type.
-/
inductive Grade where
  | A : Grade
  | B : Grade
  | F : Grade
deriving Repr

/-
Functions.
-/
def grade (m : Nat) : Grade :=
  if 80 <= m then Grade.A
  else if 60 <= m then Grade.B
  else Grade.F

def highMarks := 80 + 9
def lowMarks  := 25 + 25
#eval grade highMarks
#eval grade lowMarks

#check (0 : Nat)
/- #check (0 : Grade) -/ /- This is an error. -/

/-
Types themselves are values.
-/
#check (Nat : Type)

/-
Mathematical propositions are values in Lean. `Prop` is the type of
propositions.

Here are some simple propositions.
-/

#check 0 = 1
#check 1 = 1
#check 2^9 - 2^8 = 2^8

/-
Notice Lean displays `0 = 1 : Prop` to say:

  The statement "0 = 1" is a proposition.

We want to distinguish true propositions and false propositions. We do this via
proofs.

Each proposition is a type. `0 = 1` is a type, `1 = 1` is another type.

A proposition is true iff there is a value of that type.

How do we construct a value of type `1 = 1`? We use a constructor that is
defined for that type.

  `Eq.refl a` constructs a value of type `a = a`. (reflexivity)

Using this we can prove `1 = 1` as follows.
-/

def one_eq_one : 1 = 1 := Eq.refl 1

/-
But there is no way to prove (construct a value of type) `0 = 1`.

The following will fail. As will `Eq.refl 1`
-/

/- def zero_eq_one : 0 = 1 := Eq.refl 0 -/

/-
Let us prove a more complicated equality.

The following will fail because both sides are not "obviously equal".
-/

/- def pf : 2^9-2^8 = 2^8 := Eq.refl (2^8) -/

/-
We can prove this in painstaking detail by proving a series of equalities.

We prove:

  2^9 - 2^8 = 2^8 * 2 - 2^8
            = 2^8 * (2 - 1)
            = 2^8 * 1
            = 2^8
-/
def two_power_eight''' : 2^9-2^8 = 2^8 :=
  let p1 : 2^9 = 2^8 * 2               := sorry
  let p2 : 2^9-2^8 = 2^8 * 2 - 2^8     := sorry
  let p3 : 2^8 * 2 - 2^8 = 2^8 * (2-1) := sorry
  let p4 : 2^8 * (2-1) = 2^8           := sorry
  Eq.trans (Eq.trans p2 p3) p4
/-
`sorry` is a special proof that proves everything. It should not be used in real
proofs.

`Eq.trans` is the transitivity of equality. We prove a series of equalities and
apply transitivity.
-/

/-
Here's another proof of the same theorem with all the `sorry` replaced with real
proofs.
-/
def two_power_eight'' : 2^9 - 2^8 = 2^8 :=
  let p1 : 2^9 = 2^8 * 2 :=
    Nat.pow_succ 2 8
  let p2 : 2^9 - 2^8 = 2^8 * 2 - 2^8 :=
    congrArg (fun x => x - 2^8) p1
  let p3 : 2^8 * 2 - 2^8 = 2^8 * (2 - 1) :=
    Eq.symm (Nat.mul_sub_left_distrib (2^8) 2 1)
  let p4 : 2^8 * (2 - 1) = 2^8 :=
    Nat.mul_one (2^8)
  Eq.trans (Eq.trans p2 p3) p4

/-
For individual steps, we use proofs already available in Lean. For example:

  `Nat.pow_succ a b` is the proof of `a^(b + 1) = a^b * a`.

We use some general mechanisms in the above proof that are trivial in written,
English mathematics.

`congrArg f p`is a proof of `f a = f b` from `a = b`.

  Notice how we use `p1` to build `p2`. Indeed, `p2` is just the rewrite proved
  by `p1` in a complicated function.

`Eq.symm p` takes a proof `p` of `a=b` and gives us a proof of `b=a`.

The left distributivity is defined as `a * (b - c) = a * b - a * c`. The
equality we want to prove is its symmetric version.
-/

/-
We don't want to write proofs in such detail. We would like to specify them at a
high-level. This is what we do when we write mathematics in English.

Enter *tactics*. These are commands to Lean that guide it to construct proofs by
itself.

A common tactic is `simp`, short for simplify.

We can ask to Lean to try to prove by simplifying using `by simp`. The `by`
tells Lean that what follows is a tactic, and not a complete proof.

Then, the proof becomes:
-/
def two_power_eight' : 2^9 - 2^8 = 2^8 := by
  simp

/-
We can use `theorem` instead of `def` as a notational convenience.
-/
theorem two_power_eight : 2^9 - 2^8 = 2^8 := by simp

/-
The `calc` primitive allows us to prove equalities using stepwise
calculations. Each step has to be justified by a proof.
-/
theorem plus_squared (a b : Nat) : (a+b)^2 = a^2 + 2*a*b + b^2 :=
  calc
    (a+b)^2 = (a+b)*(a+b)             := Nat.pow_two (a+b)
    _       = (a+b)*a + (a+b)*b       := Nat.mul_add (a+b) a b
    _       = a*a + b*a + (a*b + b*b) := by repeat rw [Nat.add_mul]
    _       = a*a + b*a + a*b + b*b   := by rw [← Nat.add_assoc]
    _       = a*a + a*b + a*b + b*b   := by rw [Nat.mul_comm b a]
    _       = a^2 + a*b + a*b + b*b   := by rw [← Nat.pow_two a]
    _       = a^2 + a*b + a*b + b^2   := by rw [← Nat.pow_two b]
    _       = a^2 + (a*b + a*b) + b^2 := by rw [Nat.add_assoc (a^2)]
    _       = a^2 + 2*(a*b) + b^2     := by rw [← Nat.two_mul (a*b)]
    _       = a^2 + 2*a*b + b^2       := by rw [Nat.mul_assoc 2 a b]

/-
Let us now prove more "realistic" theorems. Those involving logical connectives.

First, we define even and odd numbers.
-/
def Even (n : Nat) := ∃ k, n = 2*k
def Odd  (n : Nat) := ∃ k, n = 2*k + 1

/-
To prove an existential, we can provide specific values if we know them.
-/
theorem zero_even : Even 0 :=
  let h : 0 = 2 * 0 := Eq.symm (Nat.mul_zero 2)
  Exists.intro 0 h
/-
`Exists.intro v h` proves `∃ x, p x` by substituting `x` by `v` and using the
proof `h` for `p v`.
-/

/-
Now, we will see how to use hypothesis that are existentials to prove
conclusions that are existentials.
-/
theorem even_mul_even_is_even' (hn : Even n) (hm : Even m) : Even (n*m) :=
  Exists.elim hn (fun k1 hk1 =>
    Exists.elim hm (fun k2 hk2 =>
      Exists.intro (k1 * ( 2 * k2)) (
        calc
          n*m = (2 * k1) * (2 * k2) := by rw [hk1, hk2]
          _   = 2 * (k1 * (2 * k2)) := by rw [Nat.mul_assoc]
      )
    )
  )

/-
The same theorem, proved using tactics.
-/
theorem even_mul_even_is_even (hn : Even n) (hm : Even m) : Even (n*m) := by
  let ⟨k1, hk1⟩ := hn
  let ⟨k2, hk2⟩ := hm
  use k1 * (2 * k2)
  calc
    n*m = (2 * k1) * (2 * k2) := by rw [hk1, hk2]
    _   = 2 * (k1 * (2 * k2)) := by rw [Nat.mul_assoc]

/-
Let us work with implications.
-/
theorem succ_of_even_is_odd' : Even n → Odd (n+1) :=
  fun hn =>
    let ⟨k, hk⟩ := hn
    Exists.intro k (
      calc
        n + 1 = 2 * k + 1 := by rw [hk]
    )
/-
To prove an implication `p → q`, you have to write a function that takes a proof
of `p` and construct a proof of `q`.

Here, `pn` is proof of `Even n := ∃ k, n = 2 *k`. Eliminating the existential
gets us `k` and a proof `hk` of `n = 2 * k`.

Now, we have to introduce the existential `∃ k, n + 1 = 2 * k + 1`. This `k` is
the same as `k` for `n`. And, the equation is proved by a simple calculation
that substitutes `2 * k` for `n`, which is allowed by `hk`.
-/

/-
Same theorem, now using tactics.
-/
theorem succ_of_even_is_odd : Even n → Odd (n+1) := by
  intro hn
  let ⟨k, hk⟩ :=  hn
  use k
  rw [hk]

/-
The following theorem can be proved similarly.

We will use this theorem later.
-/
theorem  succ_of_odd_is_even : Odd n → Even (n+1) := sorry

/-
We can use implications by applying them.
-/
example : Odd 1 := by
  apply succ_of_even_is_odd
  exact zero_even
/-
The two new tactics are:

  - `apply p` where `p` is an implication `q → r` and `r` is the goal rewrites
  the goal to `q`.
  - `exact h` solves the goal by stating that the goal is the same as `h`.

-/

/-
Let us see an example of a disjunction.
-/
example : Even 0 ∨ Odd 0 := Or.inl zero_even
example : Even 0 ∨ Odd 1 := Or.inl zero_even
example : Odd 1 ∨ Even 0 := Or.inr zero_even
/-
Here, we always know from `p ∨ q` which of `p` and/or `q` is correct. So we can
introduce a proof of the correct side.
-/

/-
Let us see a more "standard" disjunction.

Here, from the hypothesis that `n : Nat`, we cannot determine whether `n` is
even or odd. So we cannot construct `Or` directly.

But, for any specific `n`, we will know which one to construct.

This is exactly what induction allows us to do. We introduce the `induction`
tactic.

The inductive hypothesis is a disjunction. When disjunctions appear at the
hypothesis, we use *proof by exhaustive cases*. This is done using the `cases`
tactic.
-/
theorem even_or_odd (n : Nat) : Even n ∨ Odd n := by
  induction n
  case zero => left ; exact zero_even
  case succ n ihn =>
    cases ihn with
    | inl h => right ; apply (succ_of_even_is_odd h)
    | inr h => left  ; apply (succ_of_odd_is_even h)
/-
`induction` is not just for natural numbers. It is for any inductive type.
-/

/-
We now state Collatz conjecture. The proof is left as an exercise to the reader.
-/
def collatz_next (n : Nat) : Nat :=
  if n % 2 = 0 then n / 2 else 3 * n + 1

def iter (k : Nat) (f: Nat → Nat) :=
  match k with
  | Nat.zero => fun x => x
  | Nat.succ k' => fun x => f (iter k' f x)

theorem collatz : ∀ n, n > 0 → ∃ k, iter k collatz_next n = 1 := sorry

/-
Now, some "corner cases" in logic.
-/

/-
The proposition `True` is something that can be trivially proved.

`True.intro` is a constructor for proving `True`. Notice that it needs no
inputs.
-/
theorem obvious : True := True.intro

/-
On the other hand, there is no constructor for `False`.

We have to use `sorry`.
-/
theorem impossible : False := sorry

/-
Any `False` in the hypothesis allows us to conclude anything.

Written in term style, we use the eliminator `False.elim`. It takes a proof of
`False`, here `h`, and concludes whatever is the goal.
-/
theorem nonsense (h : False) : 0 = 1 := False.elim h

/-
The `contradiction` tactic uses any `False` in the hypothesis to conclude the
goal.
-/
theorem more_nonsense (h : False) : 1 = 2 := by contradiction

/-
To illustrate constructive vs classical logic, we now prove the contrapositive
theorem.

The forward direction does not require classical logic.
-/
theorem contrapositive_forward' : (p → q) → (¬q → ¬p) :=
  fun pq => fun hqf => fun hp => hqf (pq hp)
/-
Use the definition `¬q := q → False`. Notice that we have to construct `p →
False` given `p → q` and `q → False`. This is just function composition.
-/

/-
The above proof, using tactics.
-/
theorem contrapositive_forward : (p → q) → (¬q → ¬p) := by
  intro hpq
  intro
  intro hp
  specialize hpq hp
  contradiction

/-
The reverse requires classical logic.

Here, we are required to construct a `q` given values of following types:

  - `(q → False) → (p → False)`.
  - `p`.

This is impossible without using the law of excluded middle.
-/
theorem contrapositive_reverse' : (¬q → ¬p) → (p → q) :=
  fun hnqnp =>
  Classical.byCases
    (fun hq  => fun  _ => hq)
    (fun hnq => fun hp => absurd hp (hnqnp hnq))
/-
Law of excluded middle tells us that we will have a `q` or a `q → False`. In the
first case, it is trivial to construct a `q`, we already have it. In the second
case, we give the `q → False` to obtain a `p → False`.  Then, we use the fact
(in constructive logic) that given `p` and `p → False`, we can construct
`False`. Once, we have `False`, we can construct anything, and specifically `q`.
-/

/-
Same proof, using tactics.
-/
theorem contrapositive_reverse : (¬q → ¬p) → (p → q) := by
  intro hnqnp
  intro hp
  have emq := Classical.em q
  cases emq
  case inl _ => assumption
  case inr h => specialize hnqnp h ; contradiction

/-
To illustrate how we can define an work with axiomatic systems. Here is a
definition of Groups and some proofs directly translated from "Topics in
Algebra" by Herstein, Second edition.
-/

/-
A class defines a set of axioms
-/
class Group (G : Type u) where
  op : G → G → G
  assoc : ∀ a b c : G, op (op a b) c = op a (op b c)
  e : G
  identity: ∀ a : G, op a e = a ∧ op e a = a
  inverse: ∀ a : G, ∃ b : G, op a b = e ∧ op b a = e

/-
Let us introduce some notation to make this convenient.
-/
open Group
infixl:70 " * " => op

/-
`G` will always stand for a group. Variables `a b c` will be elements of that
group.
-/
variable [Group G] {a b c : G}

def is_identity (e' : G) := ∀ a : G, (a * e' = a ∧ e' * a = a)

/-
We prove that the identity element is unique.
-/
theorem identity_element_unique : ∀ e' : G, is_identity e' → e' = e := by
  intro e'
  intro h
  specialize h e
  obtain ⟨h1, _⟩ := h
  have h' := identity e'
  obtain ⟨_, h2⟩ := h'
  apply Eq.trans (Eq.symm h2) h1
/-
Note that we used the `identity` axiom.
-/

/-
Left cancellation. We have to use both `identity` and `inverse` axioms from
`Group`.
-/
theorem left_cancellation : ∀ x y : G, a * x = a * y → x = y := by
  have h1 := inverse a
  obtain ⟨ai, a_inv⟩ := h1
  obtain ⟨_, h2⟩ := a_inv
  intro x y
  intro h3
  calc
    x = (e : G) * x  := Eq.symm (identity x).right
    _ = ai * a * x   := by rw [h2]
    _ = ai * (a * x) := by rw [assoc]
    _ = ai * (a * y) := by rw [h3]
    _ = ai * a * y   := by rw [← assoc]
    _ = (e : G) * y  := by rw [h2]
    _ = y            := (identity y).right

/-
Mutually recursive definitions have to be carefully constructed. In particular,
the hypotheses when recursing should imply well-foundedness.

The game of Nim with two heaps.
-/
abbrev between (lower what upper : Nat) : Prop := lower ≤ what ∧ what ≤ upper

mutual
  def Alice : Nat → Nat → Prop
    | n1, n2 =>
      ∃ k, (between 1 k n1 ∧ between 1 k n1 → Bob (n1-k) n2)
         ∨ (between 1 k n2 ∧ between 1 k n2 → Bob n1 (n2-k))

  def Bob : Nat → Nat → Prop
    | n1, n2 =>
      ∀ k, (between 1 k n1 → Alice (n1-k) n2)
         ∧ (between 1 k n2 → Alice n1 (n2-k))
end

example : Bob 0 0 := by
  intro k
  induction k
  case zero =>
    constructor
    intro ; contradiction
    intro ; contradiction
  case succ =>
    constructor
    intro a ; have := a.right ; contradiction
    intro a ; have := a.right ; contradiction

/-
Programming algorithms in Lean is harder.

This is because we have to convince Lean of termination. Here's a simple
primality checking algorithm that tests all candidate divisors.
-/
def prime' (n : Nat) : Bool :=
  if h : n < 2 then
    false
  else
    @go 2 n (by omega)
where
  go (d : Nat) (n : Nat) {_ : n ≥ d} : Bool :=
    if h : n = d then /- `h` needed for `omega` below. -/
      true
    else if n % d = 0 then
      false
    else
      @go (Nat.succ d) n (by omega)
  termination_by (n - d)
/-
We have to specify that the recursive function `go` terminates because `n-k`
decreases in each recursive call. This needs the hypothesis `n > k` at the
recursive call site. But the function itself can only assume that `n ≥ k`. We
label the test `n ≤ k` by `h` so that the falsification of this proposition can
be used by `omega` later to conclude that `n > k`.

The tactic `omega` is a general-purpose solver for equalities and inequalities.
-/

/-
We can rewrite the function to test the divisors from largest to smallest.
-/
def prime (n : Nat) : Bool :=
  if n < 2 then
    true
  else
    go (n-1) n
where
  go d n :=
    if d < 2 then
      true
    else if n % d = 0 then
      false
    else
      go (d-1) n
/-
Now, to Lean, it is obvious that `go` will terminate because `d` decreases in
each recursive call.
-/
#eval prime 57
#eval prime 97
```

For further learning, see:

* [Functional Programming in Lean](https://lean-lang.org/functional_programming_in_lean/)
* [Theorem Proving in Lean 4](https://lean-lang.org/theorem_proving_in_lean4/)
* [Lean 4 Manual](https://lean-lang.org/lean4/doc/)
