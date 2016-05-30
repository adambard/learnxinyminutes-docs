---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
translators:
    - ["Tai An Su", "https://github.com/taiansu"]
filename: learnelixir-tw.ex
lang: zh-tw
---

Elixir 是一門建構在 Erlang 虛擬機上的現代函數式語言。它完全與 Erlang 相容，但
採行了比較常見的語法，並提供更多的功能。

```elixir

# 單行註解以井字號開頭

# 沒有多行註解的功能
# 但你可以連續使用多個單行

# 用 `iex` 來進入 elixir shell
# 用 `elixirc` 來編譯你的模組

# 如果你已成功安裝 elixir 的話，這兩個命令應已在你的 path 下。

## ---------------------------
## -- 基本型別
## ---------------------------

# 數字
3    # 整數
0x1F # 整數
3.0  # 浮點數

# 原子 (Atoms) 是不可變的字面常數，以 `:` 開頭。
:hello # atom

# 元組(Tuples) 會存在記憶體連續的區段裡。
{1,2,3} # tuple

# 我們可以用 `elem` 函式來取得 tuple 中的元素。
elem({1, 2, 3}, 0) #=> 1

# 串列 (List) 是用連結串列實作的。
[1,2,3] # list

# 我們可以這樣取得串列的頭尾元素：
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# 在 elixir 中，就如同 Erlang 裡一樣，`=` 代表的是模式比對，而非指派。
#
# 這代表將使用左手邊的模式 (pattern) 去與右手邊的值進行比對。
#
# 這也是先前取得串列的頭尾元素的運作原理

# 當模式比對無法找到合適的配對時，將會回報錯誤，如下例中兩個 tuple 的大小不一致。
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# 還有二進位的型別
<<1,2,3>> # binary

# 字串與字母串列
"hello" # string
'hello' # char list

# 多行字串
"""
I'm a multi-line
string.
"""
#=> "I'm a multi-line\nstring.\n"

# 字串皆使用 UTF-8 編碼
"héllò" #=> "héllò"

# 字串其實是以二進位實作，而字母串列就只是單純的串列。
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` 在 elixir 中會回傳字母 `a` 的 ASCII 整數
?a #=> 97

# 用 `++` 來合併串列，而合併二進位則要用 `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# 範圍 (Ranges) 則是以 `開頭..結尾`來宣告 (頭尾都包含在內)
1..10 #=> 1..10
lower..upper = 1..10 # 可以對 range 進行模式比對
[lower, upper] #=> [1, 10]

## ---------------------------
## -- 運算元
## ---------------------------

# 簡單算數
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# 在 elixir 中， `/` 運算元永遠回傳浮點數。

# 若需要回傳整數的除法，用 `div`
div(10, 2) #=> 5

# 要得到除法的餘數時，用 `rem`
rem(10, 3) #=> 1

# 還有布林運算元: `or`, `and` and `not`.
# 這些運算元要求第一個參數必需為布林值。
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir 也提供了 `||`, `&&` 及 `!`，它們接受任何型別的參數。
# 除了 `false` 與 `nil` 之外的值都會被當做 true。
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# 用來比較的運算元有：`==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` and `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` 和 `!==` 會嚴格比較整數與浮點數
1 == 1.0  #=> true
1 === 1.0 #=> false

# 兩個不同的型別也可以比較
1 < :hello #=> true

# 所有型別的排序如下：
# number < atom < reference < functions < port < pid < tuple < list < bit string

# 引用 Joe Armstrong 的話： "實際排序的先後並不重要， 但有明確排出全體順序的定
# 義才是重要的。"

## ---------------------------
## -- 控制流程
## ---------------------------

# `if` 表達式
if false do
  "This will never be seen"
else
  "This will"
end

# 也有 `unless`
unless true do
  "This will never be seen"
else
  "This will"
end

# 還記得模式比對嗎？Elixir 中許多控制流程的結構都依賴模式比對來運作。

# `case` 讓我們可以將一個值與許多模式進行比對：
case {:one, :two} do
  {:four, :five} ->
    "This won't match"
  {:one, x} ->
    "This will match and bind `x` to `:two` in this clause"
  _ ->
    "This will match any value"
end

# 當我們不需要某個值的時候，通常會將它比對成 `_`。
# 例如我們只關心串列的第一個值的情況時：
[head | _] = [1,2,3]
head #=> 1

# 若希望程式更好懂時，我們會這樣處理：
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` 讓我們可以同時檢測多個不同的值。
# 用 `cond` 來代替巢狀的 `if` 表達式
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  1 + 2 == 3 ->
    "But I will"
end

# 把最後一個條件設為 `true` 來捕捉剩下的所有情況是很常見的作法。
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  true ->
    "But I will (this is essentially an else)"
end

# `try/catch` 用來捕捉拋出的值，它也提供 `after` 子句，無論是否有接到拋出的值，
# 最後都會調用其下的程式。
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
## -- 模組與函式
## ---------------------------

# 匿名函式 (注意那個句點)
square = fn(x) -> x * x end
square.(5) #=> 25

# 匿名函式也接受多個子句及防衛(guards)
# Guards 可以進行模式比對
# 用 `when` 來描述 guards
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir 也提供許多內建的函式
# 這些在預設的作用域下都可以使用
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# 你可以用模組將多個的函式集合在一起。在模組裡，用 `def` 來定義函式。
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

# 要編譯我們的 Math 模組時，先將它存成 `math.ex`，再用 `elixirc` 進行編譯。
# 在終端機輸入： elixirc math.ex

# 在模組中我們可以用 `def` 宣告函式，及用 `defp` 宣告私有 (private) 函式。
# 使用 `def` 定義的函式可以在其它的模組中被調用。
# 私有的函式只能在這個模組內部調用。
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

# 函式宣告也支援用防衛條件及多個條件子句
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
#=> ** (FunctionClauseError) no function clause matching in Geometry.area/1

# 由於不可變特性 (immutability)，遞迴在 elixir 中扮演重要的角色。
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Elixir 模組也支援屬性，模組有內建一些屬性，而你也可以定義自己的屬性。
defmodule MyMod do
  @moduledoc """
  這是內建的屬性，模組文件
  """

  @my_data 100 # 這是自訂的屬性
  IO.inspect(@my_data) #=> 100
end

## ---------------------------
## -- 結構與例外 (Structs and Exceptions)
## ---------------------------

# 結構 (structs) 是 maps 的擴展。是 Elixir 裡可以有預設值，編譯期檢查及
# 多形 (polymorphism) 的資料結構。
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

joe_info = %Person{ name: "Joe", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Joe"}

# 取得 name 的值
joe_info.name #=> "Joe"

# 更新 age 的值
older_joe_info = %{ joe_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Joe"}

# The `try` block with the `rescue` keyword is used to handle exceptions
# 帶有 `rescue` 關鍵字的 `try` 區塊是用來進行例外處理的。
try do
  raise "some error"
rescue
  RuntimeError -> "rescued a runtime error"
  _error -> "this will rescue any error"
end
#=> "rescued a runtime error"

# 所有的異常都有帶著一個訊息
try do
  raise "some error"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "some error"

## ---------------------------
## -- 平行處理
## ---------------------------

# Elixir 依靠 actor 模式來進行平行處理。在 elixir 中要寫出平行處理程式，
# 只需要三個基本要素：建立行程，發送訊息及接收訊息。

# 我們用 `spawn` 函式來建立行程，它接收一個函式當參數。
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` 會回傳一個 pid (行程識別碼)，你可以利用這個 pid 來對該行程傳送訊息。
# 我們會使用 `send` 運算元來傳送訊息。但首先我們要讓該行程可以接收訊息。這要用
# 到 `receive` 機制來達成。

# `receive` 區塊能讓行程監聽接收到的訊息。每個 `receive do` 區塊只能接收一條
# 訊息。若要接收多條訊息時，含有 `receive do` 的函式必須要在接到訊息後，遞迴呼
# 叫自己以再次進入 `receive do` 區塊。

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

# 編譯模組，並在 shell 中創造一個行程來執行 `area_loop`。
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# 更簡潔的替代寫法
pid = spawn(Geometry, :area_loop, [])

# 對 `pid` 傳送訊息，則會與接收區塊進行樣式比對。
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# The shell is also a process, you can use `self` to get the current pid
# shell 也是一個行程 (process)，你可以用 `self` 拿到目前的 pid
self() #=> #PID<0.27.0>
```

## 參考資料

* [Getting started guide](http://elixir-lang.org/getting-started/introduction.html) from the [Elixir website](http://elixir-lang.org)
* [Elixir Documentation](http://elixir-lang.org/docs/master/)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) by Dave Thomas
* [Elixir Cheat Sheet](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) by Fred Hebert
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) by Joe Armstrong
