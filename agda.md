---
name: Agda
filename: learnagda.agda
contributors:
    - ["pe200012", "https://github.com/pe200012"]
---

Agda is a dependently typed functional programming language. It is an extension of Martin-Löf's Type Theory. In Agda, programs and proofs are written in the same language. In Agda, there is no separation between types and values: types can depend on values, and values can act as types. This allows you to encode logical invariants (like the length of a vector) directly into the type system, making Agda a powerful tool for both programming and theorem proving.

Agda is heavily reliant on Unicode characters. Most editors (like Emacs or VS Code) support entering these via LaTeX-like abbreviations (e.g., `\to` for `→`, `\all` for `∀`, `\bn` for `ℕ`).

To install Agda, you typically use Haskell's cabal or stack. See the [wiki](https://wiki.portal.chalmers.se/agda/pmwiki.php) for details.

```agda
-- A module must have the same name as the file (excluding .agda)
module learnagda where

-- We can import other modules. 
-- Agda's standard library is commonly used, but here we will define 
-- the basics from scratch for educational purposes.

-- Agda is a dependently typed functional programming language.
-- It is strictly total: all programs must terminate, and all patterns must be matched.
-- Agda logic relies heavily on the Curry-Howard correspondence:
--   Propositions are Types
--   Proofs are Programs

-- Comments use double dashes for single lines.
{- Multi-line comments are enclosed in braces and hyphens. -}

-- Agda uses Unicode extensively. 
-- In the Emacs mode (or VS Code), you type these using LaTeX-like shortcuts:
-- \to      →
-- \bN      ℕ
-- \==      ≡
-- \all     ∀
-- \::      ∷

--------------------------------------------------------------------------------
-- 1. Datatypes and Pattern Matching
--------------------------------------------------------------------------------

-- 'Set' is the type of types (similar to 'Type' in other languages).
-- We define Booleans as an inductive data type.
data Bool : Set where
  true  : Bool
  false : Bool

-- We can define functions using pattern matching.
-- 'not' takes a Bool and returns a Bool.
not : Bool → Bool  -- The type signature is mandatory.
not true  = false
not false = true

-- Natural numbers (Peano encoding).
-- This defines an infinite set of terms: zero, suc zero, suc (suc zero), ...
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

-- We can tell Agda to treat ℕ as normal numbers for literals using a pragma.
{-# BUILTIN NATURAL ℕ #-}

-- Addition defined recursively.
-- Underscores denote where the arguments go for infix operators.
-- This is Agda's "Mixfix" notation. You can put underscores anywhere!
_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

-- We can define precedence for operators.
infixl 6 _+_

-- Example evaluation:
-- 2 + 3 = 5
-- To verify this in an editor:
-- Type "2 + 3", select it, and press C-c C-n (Compute Normal Form).

-- Mixfix examples:
-- if_then_else_ : Bool → A → A → A
-- if true then x else y

--------------------------------------------------------------------------------
-- 2. Interaction and Holes
--------------------------------------------------------------------------------

-- Before introducing other concept, we need to introduce 'holes'.

-- Agda development is interactive. You write types, and the system helps fill terms.
-- A question mark ? or {! !} creates a "hole".

-- example : 2 + 2 ≡ 4
-- example = ?

-- While in Emacs/VS Code:
-- C-c C-l : Load file (type checks).
-- C-c C-space : Given a hole, ask Agda to fill it (Auto).
-- C-c C-r : Refine. If the hole is for a data type, splits constructors.
-- C-c C-, : Goal type and context. Tells you what you need to prove.
-- C-c C-c : Case split on a variable.
-- C-c C-a : Auto-solve (proof search).
-- C-c C-n : Normalize (evaluate) expression.
-- C-c C-z : Search for definition.

-- POP QUIZ: Define multiplication for natural numbers.
-- Hint: split cases, and then do induction.
_*_ : ℕ → ℕ → ℕ
x * y = ?

--------------------------------------------------------------------------------
-- 2. Polymorphism and Variables
--------------------------------------------------------------------------------

-- In modern Agda, we can declare generalizable variables to avoid 
-- repetitive `forall` syntax in type signatures.
variable
  A B : Set
  n m : ℕ

-- Lists are parameterized by a type A.
-- A is an implicit argument (denoted by { }). Agda usually infers it.
infixr 5 _::_
data List (A : Set) : Set where
  []   : List A                -- Empty list
  _::_ : A → List A → List A   -- Cons constructor

-- A list of numbers: 1 :: 2 :: 3 :: []
nums : List ℕ
nums = 1 :: 2 :: 3 :: []

-- Map function showing explicit universe polymorphism (optional but good practice)
-- ∀ {A B} makes A and B implicit type variables.
map : ∀ {A B : Set} → (A → B) → List A → List B
map f []        = []
map f (x :: xs) = f x :: map f xs

-- Anonymous functions (lambdas)
-- We can write `λ x → x + 1`
plus1 : ℕ → ℕ
plus1 = λ x → x + 1

--------------------------------------------------------------------------------
-- 3. Dependent Types
--------------------------------------------------------------------------------

-- Dependent types allow types to depend on values.
-- A classic example is a Vector: a list with a fixed length encoded in its type.

data Vec (A : Set) : ℕ → Set where
  []   : Vec A zero
  _::_ : ∀ {n} → A → Vec A n → Vec A (suc n)

-- If we try to create a vector with the wrong length, it's a type error.
vec3 : Vec ℕ 3
vec3 = 1 :: 2 :: 3 :: []

-- Because the length is known, we can define 'safe' operations.
-- This function cannot be called on an empty vector.
head : Vec A (suc n) → A
head (x ∷ xs) = x

-- We can calculate the exact type of a concatenation.
_++_ : Vec A n → Vec A m → Vec A (n + m)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

--------------------------------------------------------------------------------
-- 4. Equality and Proofs
--------------------------------------------------------------------------------

-- The identity type (equality) is the heart of proving in Agda.
-- x ≡ y is a type that has a value only if x and y are the same normal form.
infix 4 _≡_
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
{-# BUILTIN EQUALITY _≡_ #-}

-- PROOFS ARE PROGRAMS
-- To prove a property, we write a function that produces a value of that type.

-- Proving 1 + 1 equals 2.
1+1≡2 : 1 + 1 ≡ 2
1+1≡2 = refl

-- Congruence: applying a function to equal values yields equal results.
cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

-- Symmetry
sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- Transitivity
trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- Proving associativity of addition.
-- This requires induction on x.
-- We split cases on x.
-- If x is zero: zero + (y + z) ≡ zero + y + z reduces to y + z ≡ y + z.
-- If x is suc x: we use the inductive hypothesis (recursion).
+-assoc : ∀ (x y z : ℕ) → (x + y) + z ≡ x + (y + z)
+-assoc zero    y z = refl
+-assoc (suc x) y z =
  let
    -- The inductive hypothesis
    ih = +-assoc x y z
  in
    -- We need to prove: suc (x + y) + z ≡ suc (x + (y + z))
    -- Using congruence on the IH gives us exactly that.
    cong suc ih

-- Agda provides a 'rewrite' keyword to simplify this.
-- It pattern matches on the equality proof effectively replacing x with y.
+-identity : (n : ℕ) → n + zero ≡ n
+-identity zero    = refl
+-identity (suc n) rewrite +-identity n = refl

--------------------------------------------------------------------------------
-- 5. Equational Reasoning
--------------------------------------------------------------------------------

-- Agda allows defining custom syntax to write proofs that look like calculations.
-- This is similar to Lean's `calc` mode but defined within the language.

module Reasoning {A : Set} where
  infix  1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  3 _∎

  begin_ : ∀ {x y : A} → x ≡ y → x ≡ y
  begin p = p

  _≡⟨⟩_ : ∀ (x : A) {y : A} → x ≡ y → x ≡ y
  x ≡⟨⟩ p = p

  _≡⟨_⟩_ : ∀ (x : A) {y z : A} → x ≡ y → y ≡ z → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

  _∎ : ∀ (x : A) → x ≡ x
  x ∎ = refl

open Reasoning

-- Proof of associativity using reasoning syntax
+-assoc : ∀ (x y z : ℕ) → (x + y) + z ≡ x + (y + z)
+-assoc zero    y z = refl
+-assoc (suc x) y z =
  begin
    (suc x + y) + z
  ≡⟨⟩               -- Definition of +
    suc (x + y) + z
  ≡⟨⟩               -- Definition of +
    suc ((x + y) + z)
  ≡⟨ cong suc (+-assoc x y z) ⟩
    suc (x + (y + z))
  ≡⟨⟩
    suc x + (y + z)
  ∎

--------------------------------------------------------------------------------
-- 6. Inductive Relations
--------------------------------------------------------------------------------

-- Relations can also be defined as inductive types.
-- Here we define "less than or equal to" for natural numbers.

data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n} → zero ≤ n
  s≤s : ∀ {m n} → m ≤ n → suc m ≤ suc n

-- Proof that 2 ≤ 4
2≤4 : 2 ≤ 4
2≤4 = s≤s (s≤s z≤n)

--------------------------------------------------------------------------------
-- 7. Mutual Recursion: Even and Odd
--------------------------------------------------------------------------------

-- We can define types that depend on each other using `mutual`.

mutual
  data Even : ℕ → Set where
    zero : Even zero
    suc  : ∀ {n} → Odd n → Even (suc n)

  data Odd : ℕ → Set where
    suc  : ∀ {n} → Even n → Odd (suc n)

mutual
  e+e≡e : ∀ {m n} → Even m → Even n → Even (m + n)
  e+e≡e zero    en = en
  e+e≡e (suc om) en = suc (o+e≡o om en)

  o+e≡o : ∀ {m n} → Odd m → Even n → Odd (m + n)
  o+e≡o (suc em) en = suc (e+e≡e em en)

--------------------------------------------------------------------------------
-- 8. Records and Type Classes
--------------------------------------------------------------------------------

-- Records are similar to structs.
record Isomorphism (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y

-- Agda supports "Instance Arguments" via {{ }}.
-- This works like Type Classes in Haskell or Lean.

record Monoid (A : Set) : Set where
  field
    mempty : A
    _<>_   : A → A → A

-- A function using the type class
concat : ∀ {A : Set} {{m : Monoid A}} → List A → A
concat {{m}} [] = Monoid.mempty m
concat {{m}} (x :: xs) = Monoid._<>_ m x (concat {{m}} xs)

-- Instances are defined like normal values but marked 'instance'
instance
  natPlusMonoid : Monoid ℕ
  natPlusMonoid = record { mempty = 0 ; _<>_ = _+_ }

-- Now we can use it implicitly
sumList : ℕ
sumList = concat (1 :: 2 :: 3 :: []) -- Result: 6

--------------------------------------------------------------------------------
-- 9. Logic (Propositions as Types)
--------------------------------------------------------------------------------

data _×_ (A B : Set) : Set where   -- AND
  _,_ : A → B → A × B

data _⊎_ (A B : Set) : Set where   -- OR
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

data ⊥ : Set where                 -- FALSE
  -- No constructors

-- Ex Falso Quodlibet: From falsehood, anything follows.
-- We use an absurd pattern () to indicate this case is impossible.
⊥-elim : ∀ {A : Set} → ⊥ → A
⊥-elim ()

-- Negation is defined as "implies False".
¬_ : Set → Set
¬ A = A → ⊥

-- Example: Proving 1 is not equal to 0.
-- We assume 1 ≡ 0 is true (argument eq), and show it leads to a contradiction.
1≢0 : ¬ (1 ≡ 0)
1≢0 () 
-- The pattern () automatically realizes that 1 ≡ 0 is an impossible match 
-- because the constructors 'suc' and 'zero' do not clash in the definition of ≡.

-- Decidable Propositions
-- A property P is decidable if we can compute whether it holds or not.
data Dec (P : Set) : Set where
  yes : P → Dec P
  no  : (P → ⊥) → Dec P

-- Computable equality for Naturals
_==_ : (n m : ℕ) → Dec (n ≡ m)
zero  == zero  = yes refl
zero  == suc m = no λ()
suc n == zero  = no λ()
suc n == suc m with n == m
... | yes p = yes (cong suc p)
... | no ¬p = no (λ { refl → ¬p refl })

```

## Further Reading

* [PLFA (Programming Language Foundations in Agda)](https://plfa.github.io/) - Highly recommended deep dive.
* [The Agda Wiki](https://wiki.portal.chalmers.se/agda/pmwiki.php)
* [Agda Documentation](https://agda.readthedocs.io/en/latest/)
* [Standard Library](https://github.com/agda/agda-stdlib) - The official standard library.
