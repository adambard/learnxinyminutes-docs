---
name: "Lean 4"
filename: learnlean4.lean
contributors:
    - ["Balagopal Komarath", "https://bkomarath.rbgo.in/"]
    - ["Ferinko", "https://github.com/Ferinko"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Lean 4](https://lean-lang.org/)는 종속 타입 함수형 프로그래밍 언어이자 대화형 정리 증명기입니다.

```lean4
/-
열거형 데이터 타입입니다.
-/
inductive Grade where
  | A : Grade
  | B : Grade
  | F : Grade
deriving Repr

/-
함수입니다.
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
/- #check (0 : Grade) -/ /- 이것은 오류입니다. -/

/-
타입 자체도 값입니다.
-/
#check (Nat : Type)

/-
수학적 명제는 Lean에서 값입니다. `Prop`은 명제의 타입입니다.

다음은 몇 가지 간단한 명제입니다.
-/

#check 0 = 1
#check 1 = 1
#check 2^9 - 2^8 = 2^8

/-
Lean은 `0 = 1 : Prop`을 표시하여 다음을 말합니다:

  "0 = 1" 문장은 명제입니다.

우리는 참인 명제와 거짓인 명제를 구별하고 싶습니다. 우리는 증명을 통해 이를 수행합니다.

각 명제는 타입입니다. `0 = 1`은 타입이고, `1 = 1`은 다른 타입입니다.

명제는 해당 타입의 값이 있는 경우에만 참입니다.

`1 = 1` 타입의 값을 어떻게 생성할까요? 해당 타입에 대해 정의된 생성자를 사용합니다.

  `Eq.refl a`는 `a = a` 타입의 값을 생성합니다. (반사성)

이를 사용하여 다음과 같이 `1 = 1`을 증명할 수 있습니다.
-/

theorem one_eq_one : 1 = 1 := Eq.refl 1

/-
하지만 `0 = 1`을 증명할 (타입의 값을 생성할) 방법은 없습니다.

다음은 실패합니다. `Eq.refl 1`도 마찬가지입니다.
-/

/- theorem zero_eq_one : 0 = 1 := Eq.refl 0 -/

/-
변수를 포함하는 부등식을 증명해 봅시다.

`calc` 프리미티브를 사용하면 단계별 계산을 통해 등식을 증명할 수 있습니다. 각 단계는 증명으로 정당화되어야 합니다.
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
일치시킬 대상에 모호함이 없을 때 밑줄을 사용할 수 있습니다.

예를 들어, 첫 번째 단계에서 `Nat.pow_two (a+b)`를 적용하려고 합니다. 하지만,
`(a+b)`는 여기서 `Nat.pow_two`를 적용할 유일한 패턴입니다. 그래서 생략할 수 있습니다.
-/

/-
이제 더 "현실적인" 정리, 즉 논리적 연결사를 포함하는 정리를 증명해 봅시다.

먼저, 짝수와 홀수를 정의합니다.
-/
def Even (n : Nat) := ∃ k, n = 2*k
def Odd  (n : Nat) := ∃ k, n = 2*k + 1

/-
존재 증명을 하려면, 우리가 안다면 특정 값을 제공할 수 있습니다.
-/
theorem zero_even : Even 0 :=
  have h : 0 = 2 * 0 := Eq.symm (Nat.mul_zero 2)
  Exists.intro 0 h
/-
`Exists.intro v h`는 `x`를 `v`로 치환하고 `p v`에 대한 증명 `h`를 사용하여 `∃ x, p x`를 증명합니다.
-/

/-
이제, 존재 가설을 사용하여 존재 결론을 증명하는 방법을 살펴보겠습니다.

매개변수 `n`과 `m` 주위의 중괄호는 그것들이 암시적임을 나타냅니다. 여기서 Lean은 `hn`과 `hm`에서 그것들을 추론할 것입니다.
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
대부분의 증명은 *전술(tactics)*을 사용하여 작성됩니다. 이것들은 Lean이 스스로 증명을 구성하도록 안내하는 명령어입니다.

전술을 사용하여 증명된 동일한 정리입니다.
-/
theorem even_mul_even_is_even {n m : Nat} (hn : Even n) (hm : Even m) : Even (n*m) := by
  have ⟨k1, hk1⟩ := hn
  have ⟨k2, hk2⟩ := hm
  apply Exists.intro $ k1 * (2 * k2)
  calc
    n*m = (2 * k1) * (2 * k2) := by rw [hk1, hk2]
    _   = 2 * (k1 * (2 * k2)) := by rw [Nat.mul_assoc]

/-
함의(implications)를 다루어 봅시다.
-/
theorem succ_of_even_is_odd' {n : Nat} : Even n → Odd (n+1) :=
  fun hn =>
    have ⟨k, hk⟩ := hn
    Exists.intro k (
      calc
        n + 1 = 2 * k + 1 := by rw [hk]
    )
/-
함의 `p → q`를 증명하려면 `p`의 증명을 받아 `q`의 증명을 구성하는 함수를 작성해야 합니다.

여기서 `pn`은 `Even n := ∃ k, n = 2 *k`의 증명입니다. 존재 한정자를 제거하면 `k`와 `n = 2 * k`의 증명 `hk`를 얻습니다.

이제 존재 한정자 `∃ k, n + 1 = 2 * k + 1`을 도입해야 합니다. 이 `k`는 `n`에 대한 `k`와 동일합니다. 그리고 방정식은 `hk`에 의해 허용되는 `n`을 `2 * k`로 치환하는 간단한 계산으로 증명됩니다.
-/

/-
전술을 사용하여 동일한 정리입니다.
-/
theorem succ_of_even_is_odd {n : Nat} : Even n → Odd (n+1) := by
  intro hn
  have ⟨k, hk⟩ :=  hn
  apply Exists.intro k
  rw [hk]

/-
다음 정리는 비슷하게 증명될 수 있습니다.

이 정리는 나중에 사용할 것입니다.

`sorry`는 어떤 정리든 증명합니다. 실제 증명에서는 사용해서는 안 됩니다.
-/
theorem succ_of_odd_is_even {n : Nat} : Odd n → Even (n+1) := sorry

/-
정리를 적용하여 사용할 수 있습니다.
-/
example : Odd 1 := by
  apply succ_of_even_is_odd
  exact zero_even
/-
두 가지 새로운 전술은 다음과 같습니다:

  - `apply p`는 `p`가 함의 `q → r`이고 `r`이 목표일 때 목표를 `q`로 다시 씁니다. 더 일반적으로 `apply t`는 현재 목표를 `t`의 결론과 통합하고 `t`의 각 가설에 대한 목표를 생성합니다.
  - `exact h`는 목표가 `h`와 동일하다고 명시하여 목표를 해결합니다.
-/

/-
선언(disjunctions)의 예를 살펴봅시다.
-/
example : Even 0 ∨ Odd 0 := Or.inl zero_even
example : Even 0 ∨ Odd 1 := Or.inl zero_even
example : Odd 1 ∨ Even 0 := Or.inr zero_even
/-
여기서, 우리는 항상 `p ∨ q`에서 `p`와/또는 `q` 중 어느 것이 올바른지 알 수 있습니다. 그래서 우리는 올바른 쪽의 증명을 도입할 수 있습니다.
-/

/-
더 "표준적인" 선언을 살펴봅시다.

여기서, `n : Nat`라는 가설에서 `n`이 짝수인지 홀수인지 결정할 수 없습니다. 그래서 우리는 `Or`를 직접 구성할 수 없습니다.

하지만, 어떤 특정한 `n`에 대해서는 어떤 것을 구성해야 할지 알게 될 것입니다.

이것이 바로 귀납법이 우리에게 허용하는 것입니다. `induction` 전술을 도입합니다.

귀납적 가설은 선언입니다. 가설에 선언이 나타나면 *철저한 사례별 증명*을 사용합니다. 이것은 `cases` 전술을 사용하여 수행됩니다.
-/
theorem even_or_odd {n : Nat} : Even n ∨ Odd n := by
  induction n
  case zero => left ; exact zero_even
  case succ n ihn =>
    cases ihn with
    | inl h => right ; apply (succ_of_even_is_odd h)
    | inr h => left  ; apply (succ_of_odd_is_even h)
/-
`induction`은 자연수만을 위한 것이 아닙니다. Lean의 모든 타입은 귀납적이므로 모든 타입에 대해 사용할 수 있습니다.
-/

/-
이제 콜라츠 추측을 기술합니다. 증명은 독자를 위한 연습 문제로 남겨둡니다.
-/
def collatz_next (n : Nat) : Nat :=
  if n % 2 = 0 then n / 2 else 3 * n + 1

def iter (k : Nat) (f : Nat → Nat) :=
  match k with
  | Nat.zero => fun x => x
  | Nat.succ k' => fun x => f (iter k' f x)

theorem collatz : ∀ n, n > 0 → ∃ k, iter k collatz_next n = 1 := sorry

/-
이제 논리학의 몇 가지 "코너 케이스"입니다.
-/

/-
명제 `True`는 자명하게 증명될 수 있는 것입니다.

`True.intro`는 `True`를 증명하기 위한 생성자입니다. 입력이 필요 없다는 점에 유의하십시오.
-/
theorem obvious : True := True.intro

/-
반면에 `False`에 대한 생성자는 없습니다.

`sorry`를 사용해야 합니다.
-/
theorem impossible : False := sorry

/-
가설에 있는 어떤 `False`라도 우리에게 무엇이든 결론 내리게 할 수 있습니다.

항(term) 스타일로 작성할 때, 우리는 제거자 `False.elim`을 사용합니다. 이것은 `False`의 증명(여기서는 `h`)을 받아 목표가 무엇이든 결론을 내립니다.
-/
theorem nonsense (h : False) : 0 = 1 := False.elim h

/-
`contradiction` 전술은 가설에 있는 어떤 `False`라도 사용하여 목표를 결론 내립니다.
-/
theorem more_nonsense (h : False) : 1 = 2 := by contradiction

/-
구성주의 논리와 고전주의 논리의 차이를 설명하기 위해, 이제 대우(contrapositive) 정리를 증명합니다.

순방향은 고전주의 논리를 요구하지 않습니다.
-/
theorem contrapositive_forward' (p q : Prop) : (p → q) → (¬q → ¬p) :=
  fun pq => fun hqf => fun hp => hqf (pq hp)
/-
정의 `¬q := q → False`를 사용하십시오. `p → q`와 `q → False`가 주어졌을 때 `p → False`를 구성해야 한다는 점에 유의하십시오. 이것은 단지 함수 합성일 뿐입니다.
-/

/-
위 증명을 전술을 사용하여 다시 작성한 것입니다.
-/
theorem contrapositive_forward (p q : Prop) : (p → q) → (¬q → ¬p) := by
  intro hpq
  intro
  intro hp
  specialize hpq hp
  contradiction

/-
역방향은 고전주의 논리를 요구합니다.

여기서, 우리는 다음 타입의 값들이 주어졌을 때 `q`를 구성해야 합니다:

  - `(q → False) → (p → False)`.
  - `p`.

이것은 배중률을 사용하지 않고는 불가능합니다.
-/
theorem contrapositive_reverse' (p q : Prop) : (¬q → ¬p) → (p → q) :=
  fun hnqnp =>
  Classical.byCases
    (fun hq  => fun  _ => hq)
    (fun hnq => fun hp => absurd hp (hnqnp hnq))
/-
배중률은 우리에게 `q` 또는 `q → False`가 있을 것이라고 말해줍니다. 첫 번째 경우, `q`를 구성하는 것은 자명합니다. 우리는 이미 그것을 가지고 있습니다. 두 번째 경우, `p → False`를 얻기 위해 `q → False`를 제공합니다. 그런 다음, `p`와 `p → False`가 주어지면 `False`를 구성할 수 있다는 사실(구성주의 논리에서)을 사용합니다. 일단 `False`를 가지게 되면, 우리는 무엇이든 구성할 수 있으며, 구체적으로 `q`를 구성할 수 있습니다.
-/

/-
전술을 사용한 동일한 증명입니다.
-/
theorem contrapositive_reverse (p q : Prop) : (¬q → ¬p) → (p → q) := by
  intro hnqnp
  intro hp
  have emq := Classical.em q
  cases emq
  case inl _ => assumption
  case inr h => specialize hnqnp h ; contradiction

/-
공리 체계를 정의하고 작업하는 방법을 설명하기 위해, Herstein의 "대수학의 주제들(Topics in Algebra)" 제2판에서 직접 번역한 군(Group)의 정의와 몇 가지 증명을 소개합니다.
-/

/-
`section`은 네임스페이스를 도입합니다.
-/
section GroupTheory
/-
군과 같은 추상적인 객체를 정의하기 위해 `class`를 사용할 수 있습니다.
-/
class Group (G : Type u) where
  op : G → G → G
  assoc : ∀ a b c : G, op (op a b) c = op a (op b c)
  e : G
  identity: ∀ a : G, op a e = a ∧ op e a = a
  inverse: ∀ a : G, ∃ b : G, op a b = e ∧ op b a = e

/-
이것을 편리하게 만들기 위해 몇 가지 표기법을 도입합시다.
-/
open Group
infixl:70 " * " => op

/-
이 `section`에서 `G`는 항상 군을 나타내고 변수 `a b c`는 해당 군의 원소가 될 것입니다.
-/
variable [Group G] {a b c : G}

def is_identity (e' : G) := ∀ a : G, (a * e' = a ∧ e' * a = a)

/-
항등원이 유일함을 증명합니다.
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
`identity` 공리를 사용했음에 유의하십시오.
-/

/-
왼쪽 소거. `Group`의 `identity`와 `inverse` 공리를 모두 사용해야 합니다.
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

end GroupTheory /- 변수 `G`, `a`, `b`, `c`는 이제 범위에 없습니다. -/

/-
상호 재귀적인 정의를 살펴봅시다.

두 개의 힙이 있는 님(Nim) 게임입니다.
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
단지 `def`를 사용하여 함수가 정의될 때, 우리는 Lean에게 종료를 확신시켜야 합니다. 다음은 모든 후보 약수를 테스트하는 간단한 소수 판별 알고리즘입니다.
-/
def prime' (n : Nat) : Bool :=
  if h : n < 2 then
    false
  else
    @go 2 n (by omega)
where
  go (d : Nat) (n : Nat) {_ : n ≥ d} : Bool :=
    if h : n = d then /- `omega`가 아래에서 필요로 하는 `h`. -/
      true
    else if n % d = 0 then
      false
    else
      @go (Nat.succ d) n (by omega)
  termination_by (n - d)
/-
재귀 함수 `go`가 각 재귀 호출에서 `n-k`가 감소하기 때문에 종료된다는 것을 명시해야 합니다. 이를 위해서는 재귀 호출 위치에 `n > k`라는 가설이 필요합니다. 하지만 함수 자체는 `n ≥ k`라고만 가정할 수 있습니다. 우리는 `n ≤ k` 테스트를 `h`로 레이블을 지정하여, 나중에 `omega`가 `n > k`라고 결론 내리는 데 이 명제의 반증을 사용할 수 있도록 합니다.

`omega` 전술은 간단한 등식과 부등식을 풀 수 있습니다.
-/
/-
또한 `def` 앞에 `partial`을 붙여 Lean이 전체성(totality)을 확인하지 않도록 지시할 수 있습니다.
-/

/-
또는, 약수를 가장 큰 것부터 가장 작은 것 순으로 테스트하도록 함수를 다시 작성할 수 있습니다. 이 경우 Lean은 함수가 전체(total)임을 쉽게 확인합니다.
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
이제 Lean에게는 각 재귀 호출에서 `d`가 감소하기 때문에 `go`가 종료될 것이라는 점이 명백합니다.
-/
#eval prime 57
#eval prime 97
```

더 자세한 학습을 원하시면 다음을 참조하십시오:

* [Functional Programming in Lean](https://lean-lang.org/functional_programming_in_lean/)
* [Theorem Proving in Lean 4](https://lean-lang.org/theorem_proving_in_lean4/)
* [Lean 4 Manual](https://lean-lang.org/lean4/doc/)
