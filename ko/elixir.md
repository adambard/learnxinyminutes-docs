---
name: Elixir
contributors:
    - ["Joao Marques", "https://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
    - ["Ev Bogdanov", "https://github.com/evbogdanov"]
filename: learnelixir.ex
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Elixir는 Erlang VM 위에 구축된 현대적인 함수형 언어입니다.
Erlang과 완벽하게 호환되지만, 더 표준적인 구문과 많은 추가 기능을 제공합니다.

```elixir
# 한 줄 주석은 숫자 기호로 시작합니다.

# 여러 줄 주석은 없지만,
# 여러 주석을 쌓을 수 있습니다.

# Elixir 셸을 사용하려면 `iex` 명령을 사용하십시오.
# `elixirc` 명령으로 모듈을 컴파일하십시오.

# Elixir를 올바르게 설치했다면 둘 다 경로에 있어야 합니다.

## --------------------------- 
## -- 기본 유형
## --------------------------- 

# 숫자가 있습니다.
3    # 정수
0x1F # 정수
3.0  # 부동 소수점

# 원자는 값이 자신의 이름인 상수입니다. 콜론으로 시작합니다.
:hello # 원자

# 메모리에 연속적으로 저장되는 튜플입니다.
{1,2,3} # 튜플

# `elem` 함수로 튜플 요소에 액세스할 수 있습니다:
elem({1, 2, 3}, 0) #=> 1

# 연결 리스트로 구현된 리스트입니다.
[1,2,3] # 리스트

# 다음과 같이 리스트의 머리와 꼬리에 액세스할 수 있습니다:
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# Elixir에서는 Erlang과 마찬가지로 `=`는 할당이 아닌 패턴 매칭을 나타냅니다.
#
# 이는 왼쪽(패턴)이 오른쪽과 일치함을 의미합니다.
#
# 이것이 리스트의 머리와 꼬리에 액세스하는 위 예제가 작동하는 방식입니다.

# 양쪽이 일치하지 않으면 패턴 매칭이 오류를 발생시킵니다. 이 예제에서
# 튜플의 크기가 다릅니다.
# {a, b, c} = {1, 2} #=> ** (MatchError) 오른쪽 값과 일치하지 않음: {1,2}

# 바이너리도 있습니다.
<<1,2,3>> # 바이너리

# 문자열 및 문자 리스트
"hello" # 문자열
'hello' # 문자 리스트

# 여러 줄 문자열
"""
나는 여러 줄
문자열입니다.
"""
#=> "나는 여러 줄\n문자열입니다.\n"

# 문자열은 모두 UTF-8로 인코딩됩니다:
"héllò" #=> "héllò"

# 문자열은 실제로 바이너리일 뿐이고, 문자 리스트는 리스트일 뿐입니다.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# Elixir에서 `?a`는 문자 `a`에 대한 ASCII 정수를 반환합니다.
?a #=> 97

# 리스트를 연결하려면 `++`를 사용하고, 바이너리를 연결하려면 `<>`를 사용하십시오.
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# 범위는 `start..end`(양쪽 포함)로 표시됩니다.
1..10 #=> 1..10
lower..upper = 1..10 # 범위에서도 패턴 매칭을 사용할 수 있습니다.
[lower, upper] #=> [1, 10]

# 맵은 키-값 쌍입니다.
genders = %{"david" => "male", "gillian" => "female"}
genders["david"] #=> "male"

# 원자 키가 있는 맵은 다음과 같이 사용할 수 있습니다.
genders = %{david: "male", gillian: "female"}
genders.gillian #=> "female"

## --------------------------- 
## -- 연산자
## --------------------------- 

# 일부 수학
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# Elixir에서 `/` 연산자는 항상 부동 소수점을 반환합니다.

# 정수 나눗셈을 하려면 `div`를 사용하십시오.
div(10, 2) #=> 5

# 나눗셈 나머지를 얻으려면 `rem`을 사용하십시오.
rem(10, 3) #=> 1

# 부울 연산자도 있습니다: `or`, `and` 및 `not`.
# 이러한 연산자는 첫 번째 인수로 부울을 예상합니다.
true and true #=> true
false or true #=> true
# 1 and true
#=> ** (BadBooleanError) "and"의 왼쪽에 부울이 예상되었지만 다음을 받았습니다: 1

# Elixir는 또한 모든 유형의 인수를 허용하는 `||`, `&&` 및 `!`를 제공합니다.
# `false`와 `nil`을 제외한 모든 값은 true로 평가됩니다.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# 비교를 위해 다음이 있습니다: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` 및 `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===`와 `!==`는 정수와 부동 소수점을 비교할 때 더 엄격합니다:
1 == 1.0  #=> true
1 === 1.0 #=> false

# Elixir 연산자는 인수에 대해 엄격하지만,
# 다른 데이터 유형에서 작동하는 비교 연산자는 예외입니다:
1 < :hello #=> true

# 이것은 혼합 유형의 컬렉션을 빌드할 수 있게 합니다:
["string", 123, :atom]

# 모든 데이터 유형의 전체 순서가 있지만,
# Joe Armstrong의 말을 인용하면: "실제 순서는 중요하지 않지만,
# 전체 순서가 잘 정의되어 있다는 것이 중요합니다."

## --------------------------- 
## -- 제어 흐름
## --------------------------- 

# `if` 표현식
if false do
  "이것은 절대 보이지 않을 것입니다"
else
  "이것은 보일 것입니다"
end

# 패턴 매칭을 기억하십니까? Elixir의 많은 제어 흐름 구조는 패턴 매칭에 의존합니다.

# `case`는 값을 많은 패턴과 비교할 수 있게 합니다:
case {:one, :two} do
  {:four, :five} ->
    "이것은 일치하지 않을 것입니다"
  {:one, x} ->
    "이것은 일치하고 이 절에서 `x`를 `:two`에 바인딩합니다"
  _ ->
    "이것은 모든 값과 일치합니다"
end

# 필요하지 않은 경우 값을 `_`에 바인딩하는 것이 일반적입니다.
# 예를 들어, 리스트의 머리만 중요한 경우:
[head | _] = [1,2,3]
head #=> 1

# 가독성을 높이기 위해 다음을 수행할 수 있습니다:
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond`는 동시에 많은 조건을 확인할 수 있게 합니다.
# 많은 `if` 표현식을 중첩하는 대신 `cond`를 사용하십시오.
cond do
  1 + 1 == 3 ->
    "나는 절대 보이지 않을 것입니다"
  2 * 5 == 12 ->
    "나도 마찬가지"
  1 + 2 == 3 ->
    "하지만 나는 보일 것입니다"
end

# 마지막 조건을 `true`와 같게 설정하는 것이 일반적이며, 항상 일치합니다.
cond do
  1 + 1 == 3 ->
    "나는 절대 보이지 않을 것입니다"
  2 * 5 == 12 ->
    "나도 마찬가지"
  true ->
    "하지만 나는 보일 것입니다 (이것은 본질적으로 else입니다)"
end

# `try/catch`는 throw된 값을 잡는 데 사용되며, 값이 잡혔는지 여부에 관계없이 호출되는 `after` 절도 지원합니다.
try do
  throw(:hello)
catch
  message -> "Got #{message}."
after
  IO.puts("I'm the after clause.")
end
#=> I'm the after clause
# "Got :hello"

## --------------------------- 
## -- 모듈 및 함수
## --------------------------- 

# 익명 함수 (점 참고)
square = fn(x) -> x * x end
square.(5) #=> 25

# 또한 많은 절과 가드를 허용합니다.
# 가드는 패턴 매칭을 미세 조정할 수 있게 하며,
# `when` 키워드로 표시됩니다:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir는 또한 많은 내장 함수를 제공합니다.
# 이러한 함수는 현재 범위에서 사용할 수 있습니다.
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# 여러 함수를 모듈로 그룹화할 수 있습니다. 모듈 내에서 `def`를 사용하여
# 함수를 정의합니다.
defmodule Math do
  def sum(a, b) do
    a + b
  end

  def square(x) do
    x * x
  end
end

Math.sum(1, 2)  #=> 3
Math.square(3) #=> 9

# 간단한 Math 모듈을 컴파일하려면 `math.ex`로 저장하고 터미널에서 `elixirc`를 사용하십시오: elixirc math.ex

# 모듈 내에서 `def`로 함수를 정의하고 `defp`로 비공개 함수를 정의할 수 있습니다.
# `def`로 정의된 함수는 다른 모듈에서 호출할 수 있으며,
# 비공개 함수는 로컬에서만 호출할 수 있습니다.
defmodule PrivateMath do
  def sum(a, b) do
    do_sum(a, b)
  end

  defp do_sum(a, b) do
    a + b
  end
end

PrivateMath.sum(1, 2)    #=> 3
# PrivateMath.do_sum(1, 2) #=> ** (UndefinedFunctionError)

# 함수 선언은 또한 가드와 여러 절을 지원합니다.
# 여러 절이 있는 함수가 호출되면, 절을 만족하는 첫 번째 함수가 호출됩니다.
# 예: area({:circle, 3})를 호출하면 아래에 정의된 두 번째 area 함수가 호출됩니다. 첫 번째가 아닙니다:
defmodule Geometry do
  def area({:rectangle, w, h}) do
    w * h
  end

  def area({:circle, r}) when is_number(r) do
    3.14 * r * r
  end
end

Geometry.area({:rectangle, 2, 3}) #=> 6
Geometry.area({:circle, 3})       #=> 28.25999999999999801048
# Geometry.area({:circle, "not_a_number"})
#=> ** (FunctionClauseError) Geometry.area/1에서 일치하는 함수 절 없음

# 불변성으로 인해 재귀는 Elixir의 큰 부분입니다.
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Elixir 모듈은 속성을 지원하며, 내장 속성이 있고 사용자 지정 속성을 추가할 수도 있습니다.
defmodule MyMod do
  @moduledoc """
  이것은 예제 모듈의 내장 속성입니다.
  """

  @my_data 100 # 이것은 사용자 지정 속성입니다.
  IO.inspect(@my_data) #=> 100
end

# 파이프 연산자 |>는 표현식의 출력을 함수의 첫 번째 매개변수로 전달할 수 있게 합니다.

Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## --------------------------- 
## -- 구조체 및 예외
## --------------------------- 

# 구조체는 기본값, 컴파일 타임 보장 및 다형성을 Elixir에 제공하는 맵 위의 확장입니다.
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

joe_info = %Person{ name: "Joe", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Joe"}

# 이름 값에 액세스
joe_info.name #=> "Joe"

# 나이 값 업데이트
older_joe_info = %{ joe_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Joe"}

# `rescue` 키워드가 있는 `try` 블록은 예외를 처리하는 데 사용됩니다.
try do
  raise "some error"
rescue
  RuntimeError -> "rescued a runtime error"
  _error -> "this will rescue any error"
end
#=> "rescued a runtime error"

# 모든 예외에는 메시지가 있습니다.
try do
  raise "some error"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "some error"

## --------------------------- 
## -- 동시성
## --------------------------- 

# Elixir는 동시성을 위해 액터 모델에 의존합니다. Elixir에서 동시 프로그램을 작성하는 데 필요한 모든 것은 세 가지 기본 요소입니다: 프로세스 생성, 메시지 전송 및 메시지 수신.

# 새 프로세스를 시작하려면 함수를 인수로 받는 `spawn` 함수를 사용합니다.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn`은 pid(프로세스 식별자)를 반환하며, 이 pid를 사용하여 프로세스에 메시지를 보낼 수 있습니다. 메시지 전달을 위해 `send` 연산자를 사용합니다.
# 이 모든 것이 유용하려면 메시지를 받을 수 있어야 합니다. 이것은 `receive` 메커니즘으로 달성됩니다:

# `receive do` 블록은 메시지를 수신하고 수신될 때 처리하는 데 사용됩니다. `receive do` 블록은 수신된 메시지 하나만 처리합니다. 여러 메시지를 처리하려면 `receive do` 블록이 있는 함수가 재귀적으로 자신을 호출하여 다시 `receive do` 블록으로 들어가야 합니다.

defmodule Geometry do
  def area_loop do
    receive do
      {:rectangle, w, h} ->
        IO.puts("Area = #{w * h}")
        area_loop()
      {:circle, r} ->
        IO.puts("Area = #{3.14 * r * r}")
        area_loop()
    end
  end
end

# 모듈을 컴파일하고 셸에서 `area_loop`를 평가하는 프로세스를 생성합니다.
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# 또는
pid = spawn(Geometry, :area_loop, [])

# 수신 문에서 패턴과 일치하는 `pid`에 메시지를 보냅니다.
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# 셸도 프로세스이며, `self`를 사용하여 현재 pid를 얻을 수 있습니다.
self() #=> #PID<0.27.0>

## --------------------------- 
## -- 에이전트
## --------------------------- 

# 에이전트는 일부 변경되는 값을 추적하는 프로세스입니다.

# `Agent.start_link`로 에이전트를 생성하고 함수를 전달합니다.
# 에이전트의 초기 상태는 해당 함수가 반환하는 모든 것입니다.
{:ok, my_agent} = Agent.start_link(fn -> ["red", "green"] end)

# `Agent.get`은 에이전트 이름과 현재 상태가 전달되는 `fn`을 받습니다.
# 해당 `fn`이 반환하는 모든 것이 반환됩니다.
Agent.get(my_agent, fn colors -> colors end) #=> ["red", "green"]

# 동일한 방식으로 에이전트의 상태를 업데이트합니다.
Agent.update(my_agent, fn colors -> ["blue" | colors] end)
```

## 참조

* [시작 가이드](https://elixir-lang.org/getting-started/introduction.html) [Elixir 웹사이트](https://elixir-lang.org)에서
* [Elixir 문서](https://elixir-lang.org/docs.html)
* Dave Thomas의 ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir)
* [Elixir 치트 시트](https://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* Fred Hebert의 ["Learn You Some Erlang for Great Good!"](https://learnyousomeerlang.com/)
* Joe Armstrong의 ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang)
* [Elixir 소개](https://learn-elixir.com/)