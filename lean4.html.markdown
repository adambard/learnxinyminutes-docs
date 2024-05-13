---
language: "Lean 4"
filename: learnlean4.lean
contributors:
    - ["Balagopal Komarath", "https://bkomarath.rbgo.in/"]
    - ["Ferinko", "https://github.com/Ferinko"]
---

[Lean 4](https://lean-lang.org/) is a dependently typed functional programming
language and an interactive theorem prover.

```lean4
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

theorem one_eq_one : 1 = 1 := Eq.refl 1

/-
But there is no way to prove (construct a value of type) `0 = 1`.

The following will fail. As will `Eq.refl 1`
-/

/- theorem zero_eq_one : 0 = 1 := Eq.refl 0 -/

/-
Let us prove an inequality involving variables.

The `calc` primitive allows us to prove equalities using stepwise
calculations. Each step has to be justified by a proof.
-/
theorem plus_squared (a b : Nat) : (a+b)^2 = a^2 + 2*a*b + b^2 :=
  calc
    (a+b)^2 = (a+b)*(a+b)             := Nat.pow_two _
    _       = (a+b)*a + (a+b)*b       := Nat.mul_add _ _ _
    _       = a*a + b*a + (a*b + b*b) := by repeat rw [Nat.add_mul]
    _       = a*a + b*a + a*b + b*b   := by rw [← Nat.add_assoc]
    _       = a*a + a*b + a*b + b*b   := by rw [Nat.mul_comm b _]
    _       = a^2 + a*b + a*b + b*b   := by rw [← Nat.pow_two _]
    _       = a^2 + a*b + a*b + b^2   := by rw [← Nat.pow_two _]
    _       = a^2 + (a*b + a*b) + b^2 := by rw [Nat.add_assoc (a^_)]
    _       = a^2 + 2*(a*b) + b^2     := by rw [← Nat.two_mul _]
    _       = a^2 + 2*a*b + b^2       := by rw [Nat.mul_assoc _ _ _]
/-
Underscores can be used when there is no ambiguity in what is to be matched.

For example, in the first step, we want to apply `Nat.pow_two (a+b)`. But,
`(a+b)` is the only pattern here to apply `Nat.pow_two`. So we can omit it.
-/

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
  have h : 0 = 2 * 0 := Eq.symm (Nat.mul_zero 2)
  Exists.intro 0 h
/-
`Exists.intro v h` proves `∃ x, p x` by substituting `x` by `v` and using the
proof `h` for `p v`.
-/

/-
Now, we will see how to use hypothesis that are existentials to prove
conclusions that are existentials.

The curly braces around parameters `n` and `m` indicate that they are
implicit. Here, Lean will infer them from `hn` and `hm`.
-/
theorem even_mul_even_is_even' {n m : Nat} (hn : Even n) (hm : Even m) : Even (n*m) :=
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
Most proofs are written using *tactics*. These are commands to Lean that guide
it to construct proofs by itself.

The same theorem, proved using tactics.
-/
theorem even_mul_even_is_even {n m : Nat} (hn : Even n) (hm : Even m) : Even (n*m) := by
  have ⟨k1, hk1⟩ := hn
  have ⟨k2, hk2⟩ := hm
  apply Exists.intro $ k1 * (2 * k2)
  calc
    n*m = (2 * k1) * (2 * k2) := by rw [hk1, hk2]
    _   = 2 * (k1 * (2 * k2)) := by rw [Nat.mul_assoc]

/-
Let us work with implications.
-/
theorem succ_of_even_is_odd' {n : Nat} : Even n → Odd (n+1) :=
  fun hn =>
    have ⟨k, hk⟩ := hn
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
theorem succ_of_even_is_odd {n : Nat} : Even n → Odd (n+1) := by
  intro hn
  have ⟨k, hk⟩ :=  hn
  apply Exists.intro k
  rw [hk]

/-
The following theorem can be proved similarly.

We will use this theorem later.

A `sorry` proves any theorem. It should not be used in real proofs.
-/
theorem succ_of_odd_is_even {n : Nat} : Odd n → Even (n+1) := sorry

/-
We can use theorems by applying them.
-/
example : Odd 1 := by
  apply succ_of_even_is_odd
  exact zero_even
/-
The two new tactics are:

  - `apply p` where `p` is an implication `q → r` and `r` is the goal rewrites
  the goal to `q`. More generally, `apply t` will unify the current goal with
  the conclusion of `t` and generate goals for each hypothesis of `t`.
  - `exact h` solves the goal by stating that the goal is the same as `h`.
-/

/-
Let us see examples of disjunctions.
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
theorem even_or_odd {n : Nat} : Even n ∨ Odd n := by
  induction n
  case zero => left ; exact zero_even
  case succ n ihn =>
    cases ihn with
    | inl h => right ; apply (succ_of_even_is_odd h)
    | inr h => left  ; apply (succ_of_odd_is_even h)
/-
`induction` is not just for natural numbers. It is for any type, since all types
in Lean are inductive.
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
theorem contrapositive_forward' (p q : Prop) : (p → q) → (¬q → ¬p) :=
  fun pq => fun hqf => fun hp => hqf (pq hp)
/-
Use the definition `¬q := q → False`. Notice that we have to construct `p →
False` given `p → q` and `q → False`. This is just function composition.
-/

/-
The above proof, using tactics.
-/
theorem contrapositive_forward (p q : Prop) : (p → q) → (¬q → ¬p) := by
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
theorem contrapositive_reverse' (p q : Prop) : (¬q → ¬p) → (p → q) :=
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
theorem contrapositive_reverse (p q : Prop) : (¬q → ¬p) → (p → q) := by
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
A `section` introduces a namespace.
-/
section GroupTheory
/-
To define abstract objects like groups, we may use `class`.
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
`G` will always stand for a group and variables `a b c` will be elements of that
group in this `section`.
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
  have ⟨h1, _⟩ := h
  have h' := identity e'
  have ⟨_, h2⟩ := h'
  exact Eq.trans (Eq.symm h2) h1
/-
Note that we used the `identity` axiom.
-/

/-
Left cancellation. We have to use both `identity` and `inverse` axioms from
`Group`.
-/
theorem left_cancellation : ∀ x y : G, a * x = a * y → x = y := by
  have h1 := inverse a
  have ⟨ai, a_inv⟩ := h1
  have ⟨_, h2⟩ := a_inv
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

end GroupTheory /- Variables `G`, `a`, `b`, `c` are now not in scope. -/

/-
Let us see a mutually recursive definition.

The game of Nim with two heaps.
-/
abbrev between (lower what upper : Nat) : Prop := lower ≤ what ∧ what ≤ upper

mutual
  def Alice : Nat → Nat → Prop
    | n1, n2 =>
      ∃ k, (between 1 k n1 ∧ (between 1 k n1 → Bob (n1-k) n2))
         ∨ (between 1 k n2 ∧ (between 1 k n2 → Bob n1 (n2-k)))

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
We have to convince Lean of termination when a function is defined using just a
`def`. Here's a simple primality checking algorithm that tests all candidate
divisors.
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

The tactic `omega` can solve simple equalities and inequalities.
-/
/-
You can also instruct Lean to not check for totality by prefixing `partial` to
`def`.
-/

/-
Or, we can rewrite the function to test the divisors from largest to
smallest. In this case, Lean easily verifies that the function is total.
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
