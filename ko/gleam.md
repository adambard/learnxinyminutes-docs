---
name: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Gleam은 Erlang의 BEAM 가상 머신을 위한 새로운 언어로, 강력한 타입 시스템, 함수형 프로그래밍의 표현력, 그리고 OCaml, Rust, Elixir와 같은 언어에서 영감을 받은 친숙하고 현대적인 구문을 사용하여 고도로 동시적이고 내결함성이 있는 Erlang 런타임의 힘을 활용합니다.

매우 현대적인 개발인 Gleam은 컴파일러, 빌드 도구, 코드 포맷터, 여러 편집기 통합 및 패키지 관리자와 함께 제공됩니다.

더 큰 BEAM 생태계의 일부인 Gleam으로 만든 프로그램은 Erlang 또는 Elixir로 작성된 수천 개의 게시된 패키지를 사용할 수도 있습니다.

언어의 디자인은 매우 간결하여 null 값, 예외, 명확한 오류 메시지 및 실용적인 타입 시스템이 없습니다.

JavaScript는 추가로 컴파일 대상으로 지원되므로 브라우저 또는 다른 JS 지원 런타임에서 Gleam 코드를 실행할 수 있습니다. 이 기능을 사용하면 TypeScript 정의가 생성되므로 외부에서도 자신 있게 Gleam 코드와 상호 작용할 수 있습니다.

```gleam
//// 이 네 개의 슬래시가 있는 주석은 모듈 수준입니다.
//// 이러한 종류의 주석은 전체 모듈을 설명하는 데 사용됩니다.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/iterator
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/string as text

// 타입의 이름은 항상 대문자로 시작하며, 변수와 함수는 소문자로 시작하는 것과 대조됩니다.

// pub 키워드를 사용하면 타입 별칭이 공개되어 다른 모듈에서 참조할 수 있습니다.

pub type UserId =
  Int

pub fn main() {
  io.println("Hello from learnxinmyminutes.com!")
  // io.println("This statement got commented out by a two slashes comment.!")

  // 모듈은 모든 Gleam 코드가 구성되는 단위입니다.
  // 모듈에는 함께 속하는 것처럼 보이는 타입, 함수 등의 정의가 많이 있습니다.
  // 예를 들어, gleam/io 모듈에는 println과 같은 다양한 인쇄 함수가 포함되어 있습니다.

  // 모든 gleam 코드는 어떤 모듈에 있으며, 그 이름은 해당 파일의 이름에서 따옵니다.
  // 예를 들어, gleam/io는 gleam이라는 디렉토리의 io.gleam이라는 파일에 있습니다.

  // Gleam에는 코드를 작성하고 편집할 때 도움이 되는 강력한 정적 타입 시스템이 있어 실수를 포착하고 변경할 위치를 보여줍니다.
  // io.println(10)
  // 이전 줄의 주석을 해제하면 io.println 함수는 정수가 아닌 문자열에서만 작동하므로 컴파일 타임 오류가 발생합니다.

  // 컴파일러는 다음과 같은 오류를 출력합니다:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │   io.println(10)
  //  │              ^^
  //
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // 숫자 작업

  // Erlang 가상 머신에서 실행할 때 정수는 최대 및 최소 크기가 없습니다.
  // JavaScript 런타임에서 실행할 때 정수는 JavaScript의 64비트 부동 소수점 숫자를 사용하여 표시됩니다.

  // 정수 산술
  io.debug(1 + 1)
  io.debug(5 - 1)
  io.debug(5 / 2)
  io.debug(3 * 3)
  io.debug(5 % 2)

  // 정수 비교
  io.debug(2 > 1)
  io.debug(2 < 1)
  io.debug(2 >= 1)
  io.debug(2 <= 1)

  // 같음은 모든 유형에 대해 작동하며 구조적으로 확인됩니다. 즉, 두 값이 동일한 메모리 위치에 있는지 여부가 아니라 동일한 구조를 갖는 경우 동일합니다.
  io.debug(1 == 1)
  // True
  io.debug(2 != 2)
  // False

  // 표준 라이브러리 정수 함수
  io.debug(int.min(142, 137))
  // 137
  io.debug(int.clamp(-80, min: 0, max: 100))
  // 0
  io.debug(int.base_parse("10", 2))
  // Ok(2)

  // 2진수, 8진수 및 16진수 정수 리터럴
  io.debug(0b00001111)
  io.debug(0o17)
  io.debug(0xF)

  // 정수 가독성을 높이기 위해 밑줄 사용
  io.debug(1_000_000)

  // Gleam의 숫자 연산자는 오버로드되지 않으므로 부동 소수점 작업을 위한 전용 연산자가 있습니다.

  // 부동 소수점 산술
  io.debug(1.0 +. 1.5)
  io.debug(5.0 -. 1.5)
  io.debug(5.0 /. 2.5)
  io.debug(3.0 *. 3.5)

  // 부동 소수점 비교
  io.debug(2.2 >. 1.3)
  io.debug(2.2 <. 1.3)
  io.debug(2.2 >=. 1.3)
  io.debug(2.2 <=. 1.3)

  // 부동 소수점은 Erlang 및 JavaScript 런타임 모두에서 64비트 부동 소수점 숫자로 표시됩니다.
  // 부동 소수점 동작은 해당 런타임에 따라 다르므로 두 런타임에서 약간 다르게 작동합니다.

  // JavaScript 런타임에서 부동 소수점 값에 대해 표현 가능한 최대(또는 최소) 값을 초과하면 Infinity(또는 -Infinity)가 됩니다. 두 무한대를 나누려고 하면 결과로 NaN이 됩니다.

  // BEAM에서 실행할 때 오버플로가 발생하면 오류가 발생합니다. 따라서 Erlang 런타임에는 NaN 또는 Infinity 부동 소수점 값이 없습니다.

  // 0으로 나누는 것은 오류가 아닙니다.
  io.debug(3.14 /. 0.0)
  // 0.0

  // 표준 라이브러리 부동 소수점 함수
  io.debug(float.max(2.0, 9.5))
  // 9.5
  io.debug(float.ceiling(5.4))
  // 6.0

  // 부동 소수점에 대한 밑줄도 지원됩니다.
  io.debug(10_000.01)

  // 0으로 나누면 오버플로가 발생하지 않고 대신 0으로 정의됩니다.

  // 문자열 작업
  io.debug("⭐ Gleam ⭐ - 별")
  io.debug(
    "this
    is
    a
    multi
    line
    string",
  )
  io.debug("\u{1F600}")
  // 웃는 얼굴 😀 출력

  // 큰따옴표는 이스케이프할 수 있습니다.
  io.println("\"X\" marks the spot")

  // 문자열 연결
  io.debug("One " <> "Two")

  // 문자열 함수
  io.debug(text.reverse("1 2 3 4 5"))
  io.debug(text.append("abc", "def"))

  io.println(text.reverse("!desrever tog gnirts sihT"))
  // "This string got reversed!" 출력

  // 여러 이스케이프 시퀀스가 지원됩니다:

  // \" - 큰따옴표
  // \\ - 백슬래시
  // \f - 폼 피드
  // \n - 줄 바꿈
  // \r - 캐리지 리턴
  // \t - 탭

  // 부울 연산자
  // || 및 && 연산자는 단락 평가로 작동합니다.

  io.debug(True && False)
  // False

  io.debug(True && True)
  // True

  io.debug(False || False)
  // False

  io.debug(False || True)
  // True

  // 부울 함수
  io.debug(bool.to_string(True))
  // "True"

  io.debug(bool.to_int(False))
  // 0

  // 할당
  let x = "Original value"
  io.debug(x)

  // `y`를 `x`의 값에 할당
  let y = x
  io.debug(y)

  // `x`를 새 값에 할당
  let x = "New value"
  io.debug(x)

  // `y`는 여전히 원래 값을 참조합니다.
  io.debug(y)

  // Gleam에서 변수 및 함수 이름은 snake_case로 작성됩니다.
  let answer_to_the_universe = 42
  io.debug(answer_to_the_universe)

  let and_everything = answer_to_the_universe
  // 이제 변수를 사용하면 경고가 발생합니다.

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │   let and_everything = answer_to_the_universe
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_and_everything`.

  // 타입 주석

  let _name: String = "Gleam"

  let _is_cool: Bool = True

  let _version: Int = 1
  // 문서화 목적으로 유용하지만 컴파일러가 코드를 타입 검사하는 방식은 변경하지 않습니다. 주석이 타입과 일치하는지 확인하는 것 외에는 오류가 발생합니다.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │   let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // 타입 별칭
  let one: UserId = 1
  // 파일 시작 부분에서 UserId 타입 정의 참조

  let two: Int = 2

  // 별칭은 더 읽기 쉬운 코드와 더 정확한 문서를 만들기 위한 것입니다.
  // 내부적으로는 여전히 동일한 유형의 값이므로 연산이 여전히 작동합니다.
  io.debug(one + two)
  // 3

  // 블록: 범위 및 값
  let radius = {
    let value = 100.0
    value
  }
  // io.debug(value) // <- "value"가 범위를 벗어났기 때문에 컴파일되지 않습니다.

  let area = 3.14159 *. radius *. radius
  io.debug(area)

  // 괄호 대신 블록을 사용하여 연산을 그룹화합니다.
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  io.debug(n1 != n2)
  // True

  // 목록

  // Scrooge McDuck의 조카
  let nephews = ["Huey", "Dewey", "Louie"]
  io.debug(nephews)
  // ["Huey", "Dewey", "Louie"]

  // 불변하게 앞에 추가하여 원래 목록이 변경되지 않도록 합니다.
  io.debug(["Donald", ..nephews])
  // ["Donald", "Huey", "Dewey", "Louie"]

  // 목록에 대한 일부 표준 라이브러리 함수

  list.each(nephews, io.println)
  // Huey
  // Dewey
  // Louie

  io.debug(list.drop(nephews, 2))
  // ["Louie"]

  more_examples()
  more_function_examples()
  generic_typing_examples()
  beloved_pipelines_demo()
  labels_in_function_calls()
  showcase_flow_control()
  more_on_recursion()
  more_on_pattern_matching()
  showcase_types()
  more_on_types()
  more_on_callbacks()
  showcase_externals()
  showcase_panic()
}

// fn 키워드는 새 함수를 정의하는 데 사용됩니다.
fn multiply(a: Int, b: Int) -> Int {
  // 명시적인 반환 없음
  // 마지막 표현식이 반환됩니다.
  a * b
}

// double 및 multiply 함수는 pub 키워드 없이 정의됩니다.
// 이것은 비공개 함수로 만들며, 이 모듈 내에서만 사용할 수 있습니다.
// 다른 모듈에서 사용하려고 하면 컴파일러 오류가 발생합니다.
fn double(a: Int) -> Int {
  multiply(a, 2)
}

// 공개 함수만 내보내지고 모듈 외부에서 호출할 수 있습니다.

// 함수 인수 및 반환 값에 대한 타입 주석은 선택 사항이지만 명확성을 위해 좋은 관행으로 간주되며 의도적이고 사려 깊은 설계를 장려합니다.

pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 } && { { year % 100 != 0 } || { year % 400 == 0 } }
}

fn more_examples() {
  // Debug는 또한 값을 반환하므로 해당 출력은 이 함수의 반환 값입니다.
  io.debug(double(10))
  // 20
  io.debug(is_leap_year(2000))
  // True
}

// Gleam은 고차 함수를 지원합니다:
// 변수에 할당하거나, 다른 함수에 인수로 전달하거나, 블록이나 다른 함수에서 값으로 반환할 수도 있습니다.
fn call_func_on_int(func: fn(Int) -> Int, value: Int) -> Int {
  func(value)
}

fn more_function_examples() -> Int {
  io.debug(call_func_on_int(double, 2))
  // 4

  let square = fn(x: Int) -> Int { x * x }
  io.debug(square(3))
  // 9

  // 정의 직후 익명 함수 호출
  io.debug(fn(x: Int) { x + 1 }(1))

  // 클로저 예제
  let make_adder = fn(n: Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { argument + n }
  }

  let adder_of_fives = make_adder(5)
  io.debug(adder_of_fives(10))
  // 15

  // 익명 함수는 명명된 함수와 상호 교환하여 사용할 수 있습니다.
  io.debug(call_func_on_int(fn(x: Int) -> Int { x + 100 }, 900))
  // 1000

  // 함수 데코레이터를 만들어 보겠습니다.
  let twice = fn(wrapped_func: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { wrapped_func(wrapped_func(argument)) }
  }
  let quadruple = twice(double)
  io.debug(quadruple(1))

  let quadruple_2 = fn(a: Int) -> Int { multiply(4, a) }
  io.debug(quadruple_2(2))
  // 8

  // 함수 캡처는 한 인수를 사용하고 즉시 다른 함수를 해당 인수로 호출하는 익명 함수를 만드는 약식 구문입니다.
  let quadruple_3 = multiply(4, _)
  io.debug(quadruple_3(4))
  // 16
}

// 제네릭 함수는 타입 변수를 사용하여 지원됩니다.
fn generic_twice(func: fn(value) -> value, argument: value) -> value {
  func(func(argument))
}

// generic_twice에서 value는 타입 변수였습니다.
// generic_twice_decorator에서 the_type은 타입 변수입니다.
// 다른 변수와 마찬가지로 이름을 선택할 수 있습니다.
fn generic_twice_decorator(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argument: the_type) -> the_type { func(func(argument)) }
}

fn generic_typing_examples() {
  let double_integers = fn(a: Int) -> Int { a * 2 }
  let double_floats = fn(a: Float) -> Float { a *. 2.0 }
  io.debug(generic_twice(double_integers, 3))
  io.debug(generic_twice(double_floats, 3.0))

  let quadruple_integers = generic_twice_decorator(double_integers)
  let quadruple_floats = generic_twice_decorator(double_floats)
  io.debug(quadruple_integers(1))
  // 4
  io.debug(quadruple_floats(1.0))
  // 4.0
}

// Gleam의 파이프 연산자 |>는 왼쪽 표현식의 결과를 가져와 오른쪽 함수의 인수로 전달합니다.
fn beloved_pipelines_demo() {
  // 솔직히 말해서, 이 멋진 연산자 때문에 Gleam을 사용하고 싶으시죠?
  ["hello", "world"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> io.debug

  // 이것보다 더 깔끔하게 일치시키세요, 그렇죠?
  io.debug(
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hello", "world"], " "), ["!"]),
      ),
    ),
  )

  // Project Euler의 첫 번째 문제에 대한 해결책:
  // URL: https://projecteuler.net/problem=1
  // 설명: 1000 미만의 3과 5의 모든 배수의 합을 찾으십시오.
  iterator.iterate(1, fn(n) { n + 1 })
  |> iterator.take(1000 - 1)
  |> iterator.filter(fn(n) { { n % 3 == 0 } || { n % 5 == 0 } })
  |> iterator.fold(from: 0, with: fn(acc, element) { element + acc })
  |> int.to_string
  |> fn(sum_as_text: String) {
    "Solution to Project Euler's problem #1: " <> sum_as_text
  }
  |> io.debug
  // Project Euler의 문제 #1에 대한 해결책: 233168
}

// 각 인수 앞에 레이블을 추가할 수 있습니다.
fn call_func_on_int_with_labels(
  func passed_func: fn(Int) -> Int,
  value n: Int,
) -> Int {
  passed_func(n)
}

// 레이블과 인수는 동일한 이름을 가질 수 있습니다.
fn add_one(number number: Int) -> Int {
  number + 1
}

fn add_two_integers(first n: Int, second m: Int) -> Int {
  n + m
}

fn labels_in_function_calls() -> Int {
  // 인수에 레이블을 지정하므로 원하는 경우 순서를 바꿀 수 있습니다.
  io.debug(call_func_on_int_with_labels(value: 8, func: double))
  io.debug(add_one(number: 1))
  // 2
  io.debug(string.contains(does: "theme", contain: "the"))
  // True
  // 레이블이 없는 인수는 먼저 와야 합니다.
  io.debug(add_two_integers(2, second: 2))
  // 4
}

fn showcase_flow_control() {
  // 패턴 매칭을 사용하여 실행할 코드를 선택하려는 경우 case를 사용하십시오.
  // Gleam은 완전성 검사를 수행하여 모든 가능한 값이 포함되었는지 확인합니다.
  // 그렇지 않으면 컴파일 오류가 발생합니다.
  let puppies = ["Bear", "Frisco", "Ranger"]
  let count = list.length(of: puppies)
  {
    "We have "
    <> int.to_string(count)
    <> " "
    <> // 밑줄은 다른 모든 값과 일치합니다.
    case count {
      1 -> "puppy"
      _ -> "puppies"
    }
  }
  |> io.debug

  // Gleam은 case 표현식의 패턴이 변수를 할당할 수 있도록 합니다.
  {
    "Puppy count: "
    <> case list.length(puppies) {
      0 -> "None."
      1 -> "Just one."
      other -> "As many as " <> int.to_string(other) <> " puppies."
    }
  }
  |> io.debug

  // BEAM 언어는 설계상 함수형이며 Gleam도 예외는 아니므로 if, for 또는 while 구문이 없습니다.

  // 조건문에 대한 패턴 매칭 사용
  let answer = 42
  case answer == 42 {
    True -> {
      io.debug("This is the answer to the universe.")
    }
    False -> {
      io.debug("This is the answer to something else.")
    }
  }

  // 반복 대신 재귀 사용
  from_one_to_ten(1)
}

// 재귀 함수
fn from_one_to_ten(n: Int) {
  io.debug(n)
  case n {
    10 -> Nil
    _ -> from_one_to_ten(n + 1)
  }
}

// 재귀적으로 함수를 호출할 때 과도한 스택 프레임을 만들어 메모리 고갈을 피하기 위해 Gleam은 "꼬리 호출 최적화"를 지원합니다. 즉, 함수 호출이 함수가 하는 마지막 일인 경우 컴파일러가 현재 함수에 대한 스택 프레임을 재사용할 수 있습니다.

pub fn fib(x: Int) -> Int {
  // 공개 함수는 비공개 꼬리 재귀 함수를 호출합니다.
  fib_loop(x, 1)
}

fn fib_loop(x: Int, accumulator: Int) -> Int {
  case x {
    1 -> accumulator

    // 이 함수가 하는 마지막 일은 자신을 호출하는 것입니다.
    // 이전 수업에서 마지막으로 한 일은 두 정수를 곱하는 것이었습니다.
    _ -> fib_loop(x - 1, accumulator + x)
  }
}

// Gleam은 case 표현식 내에서 [x, ..y] 패턴으로 목록의 첫 번째 요소와 나머지 부분을 패턴 매칭하는 것을 지원합니다.
fn reverse_list(the_list: List(value)) -> List(value) {
  case the_list {
    [head, ..tail] -> list.concat([reverse_list(tail), [head]])
    [] -> []
  }
}

fn more_on_recursion() {
  io.debug(fib(10))
  // 55
  io.debug(reverse_list([1, 2, 3]))
}

fn more_on_pattern_matching() {
  // 문자열에서 패턴 매칭할 때 <> 연산자는 특정 접두사가 있는 문자열과 일치하고 나머지를 변수에 할당합니다.
  io.debug(case "Hello, Lucy" {
    "Hello, " <> name -> "Greetings for " <> name
    _ -> "Potentially no greetings"
  })

  // 대체 패턴이 지원되므로 여러 값에 대해 동일한 절이 사용됩니다.
  let month = 2
  let year = 2024
  let number_of_days = case month {
    2 ->
      case is_leap_year(year) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  io.debug("Number of days: " <> int.to_string(number_of_days))
  // 29

  // 패턴 매칭의 가드:
  // if 키워드를 사용할 때 표현식은 패턴이 일치하려면 True로 평가되어야 합니다.
  let list_starts_with = fn(the_list: List(value), the_value: value) -> Bool {
    case the_list {
      [head, ..] if head == the_value -> True
      _ -> False
    }
  }
  io.debug(list_starts_with([10, 20, 30], 10))
  // True
}

pub type Gender {
  Male
  Female
  Other
}

// 레코드:
// - 변형 지원
// - 각 변형은 필드가 있는 구조체와 유사합니다.
pub type Shape {
  Rectangle(base: Float, height: Float)
  Triangle(base: Float, height: Float)
}

// 하나의 변형이 있는 레코드는 구조체와 유사합니다.
pub type Point {
  Point(x: Float, y: Float)
}

fn showcase_types() {
  // 튜플:
  // - 다른 유형의 요소를 함께 혼합할 수 있습니다.
  // - 해당 유형은 암시적입니다. 예: #{1, "Hello"}는 #{Int, String} 유형입니다.
  // - 해당 요소는 숫자 인덱스로 액세스할 수 있습니다.
  let tuple_01 = #(1, "Ferris", "rustacean", True)
  let tuple_02 = #(1, "Lucy", "starfish", True)
  io.debug(tuple_01)
  io.debug(tuple_01.0)
  // 1
  io.debug(tuple_02.1)
  // Lucy
  let #(_, name, species, _) = tuple_01
  io.debug(name <> " the " <> species)

  // 변수 할당을 포함한 튜플과의 패턴 매칭
  case tuple_02 {
    #(_, name, _, True) -> io.debug(name <> " is a mascot.")
    #(_, name, _, False) -> io.debug(name <> " is not a mascot.")
  }

  // 패턴 매칭과 함께 사용자 지정 유형 사용
  let gender = Other
  io.debug(case gender {
    Male -> "Boy"
    Female -> "Girl"
    _ -> "Undetermined"
  })

  // 레코드 사용
  let rectangle_1 = Rectangle(base: 10.0, height: 20.0)
  io.debug(rectangle_1.height)
  // 10.3

  let point_1 = Point(x: 3.2, y: 4.3)
  io.debug(point_1)

  // 레코드 업데이트
  let point_2 = Point(..point_1, y: 5.7)
  io.debug(point_2)

  // Gleam에서 값은 null일 수 없습니다.
  // Nil은 해당 유형의 유일한 값입니다.
  let some_var = Nil
  let result = io.println("Hello!")
  io.debug(some_var == result)
  // True
}

pub type Mineral {
  Gold
  Silver
  Copper
}

// 포함된 유형을 매개변수로 사용하는 제네릭 사용자 지정 유형
pub type Purity(inner_type) {
  Pure(inner_type)
  Impure(inner_type)
}

pub type Beverage {
  Water
  Juice
}

// gleam/option 및 gleam/result 모듈의 기존 사용자 지정 유형은 null 가능 값 작업 및 잠재적 오류 처리를 용이하게 합니다.
pub type Person {
  Person(name: String, nickname: Option(String))
}

pub type DiceError {
  DiceValueOutOfRange
}

fn checked_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(value)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn double_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 -> Ok(value * 2)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn more_on_types() {
  let mineral_sample_01: Purity(Mineral) = Pure(Gold)
  let mineral_sample_02 = Impure(Silver)
  io.debug(mineral_sample_01)
  io.debug(mineral_sample_02)

  // 유리는 비어 있거나 비어 있지 않을 수 있습니다.
  let glass_01: Option(Beverage) = Some(Water)
  let glass_02 = None
  io.debug(glass_01)
  io.debug(glass_02)

  // 사람은 별명이 있거나 없을 수 있습니다.
  let person_01 = Person(name: "John", nickname: Some("The Ripper"))
  let person_02 = Person(name: "Martin", nickname: None)
  io.debug(person_01)
  io.debug(person_02)

  // Result 유형의 값을 반환하는 함수 작업
  let dice_01 = 5
  case checked_dice_value(dice_01) {
    Ok(checked_value) ->
      io.debug("The value of " <> int.to_string(checked_value) <> " is OK.")
    Error(DiceValueOutOfRange) ->
      io.debug("The value of the dice is out of range")
  }

  // 결과 값이 여전히 주사위의 면에 있는 숫자인 경우 값을 두 배로 늘리려고 합니다.
  // 그렇지 않으면 최대값을 넣습니다.
  2
  |> checked_dice_value
  |> result.try(double_dice_value)
  |> result.unwrap(or: 6)
  |> io.debug
}

pub fn throw_dice_as_result() {
  Ok(int.random(6) + 1)
}

pub fn sum_dice_values(a: Int, b: Int) {
  Ok(a + b)
}

// 일급 함수 및 패턴 매칭에 베팅하면 쉽게 많은 들여쓰기가 발생할 수 있습니다.
fn roll_two_dices_without_use() {
  result.try(throw_dice_as_result(), fn(first_dice) {
    result.try(throw_dice_as_result(), fn(second_dice) {
      result.map(sum_dice_values(first_dice, second_dice), fn(sum) { sum })
    })
  })
}

// use 표현식은 여전히 콜백을 사용하는 코드를 작성할 수 있지만 과도한 들여쓰기를 정리합니다:
// - 고차 함수에 대한 호출은 <- 연산자의 오른쪽에 있습니다.
// - 콜백 함수에 대한 인수 이름은 <- 연산자의 왼쪽에 있습니다.
// - 묶는 {} 블록의 나머지 모든 코드는 콜백 함수의 본문이 됩니다.
fn roll_two_dices_with_use() {
  use first_dice <- result.try(throw_dice_as_result())
  use second_dice <- result.try(throw_dice_as_result())
  use sum <- result.map(sum_dice_values(first_dice, second_dice))
  // 이것은 가장 안쪽 콜백 함수의 나머지 코드입니다.
  sum
}

fn more_on_callbacks() {
  io.debug(roll_two_dices_without_use())
  io.debug(roll_two_dices_with_use())
}

pub type DateTime

// 외부 함수는 반환 유형을 주석으로 달아야 합니다.
@external(erlang, "calendar", "local_time")
pub fn now() -> DateTime

fn showcase_externals() {
  io.debug(now())
  // #(#(2024, 4, 6), #(14, 4, 16))
}

fn showcase_panic() {
  // panic 키워드를 사용하여 의도적으로 실행을 중단하여 프로그램을 즉시 충돌시킬 수 있습니다.
  case 3 == 2 {
    True -> panic as "The equality operator is broken!"
    False -> "Equality operator works for integers"
  }
  // todo 키워드를 사용하는 함수를 호출하면 충돌이 발생합니다.
  // homework()
}

pub fn homework() {
  todo
}

## 추가 자료

* [Gleam 공식 웹사이트](https://gleam.run/)
* [언어 둘러보기](https://tour.gleam.run/) - 라이브 코드 편집기 포함
* [공식 문서](https://gleam.run/documentation/)
* [Gleam의 멋진 목록](https://github.com/gleam-lang/awesome-gleam)
* [Gleam을 위한 Exercism 트랙](https://exercism.org/tracks/gleam)

공식 문서에는 다음에 익숙한 사람들을 위한 치트 시트가 있습니다:

* [Elixir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Elm](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Rust](https://gleam.run/cheatsheets/gleam-for-rust-users)