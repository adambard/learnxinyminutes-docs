---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
translators:
    - ["lidashuang", "http://github.com/lidashuang"]
filename: learnelixir-cn.ex
lang: zh-cn
---

Elixir 是一门构建在Erlang VM 之上的函数式编程语言。Elixir 完全兼容 Erlang, 
另外还提供了更标准的语法，特性。

```elixir

# 这是单行注释, 注释以井号开头

# 没有多行注释
# 但你可以堆叠多个注释。

# elixir shell 使用命令 `iex` 进入。
# 编译模块使用 `elixirc` 命令。

# 如果安装正确，这些命令都会在环境变量里

## ---------------------------
## -- 基本类型
## ---------------------------

# 数字
3    # 整型
0x1F # 整型
3.0  # 浮点类型

# 原子(Atoms)，以 `:`开头
:hello # atom

# 元组(Tuple) 在内存中的存储是连续的
{1,2,3} # tuple

# 使用`elem`函数访问元组(tuple)里的元素:
elem({1, 2, 3}, 0) #=> 1

# 列表(list)
[1,2,3] # list

# 可以用下面的方法访问列表的头尾元素:
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# 在elixir,就像在Erlang, `=` 表示模式匹配 (pattern matching) 
# 不是赋值。
#
# 这表示会用左边的模式(pattern)匹配右侧
# 
# 上面的例子中访问列表的头部和尾部就是这样工作的。

# 当左右两边不匹配时，会返回error, 在这个
# 例子中，元组大小不一样。
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# 还有二进制类型 (binaries)
<<1,2,3>> # binary

# 字符串(Strings) 和 字符列表(char lists)
"hello" # string
'hello' # char list

# 多行字符串
"""
I'm a multi-line
string.
"""
#=> "I'm a multi-line\nstring.\n"

# 所有的字符串(Strings)以UTF-8编码：
"héllò" #=> "héllò"

# 字符串(Strings)本质就是二进制类型(binaries), 字符列表(char lists)本质是列表(lists)
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# 在 elixir中，`?a`返回 `a` 的 ASCII 整型值  
?a #=> 97

# 合并列表使用 `++`, 对于二进制类型则使用 `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

## ---------------------------
## -- 操作符(Operators)
## ---------------------------

#  一些数学运算
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# 在 elixir 中，操作符 `/` 返回值总是浮点数。

# 做整数除法使用 `div`
div(10, 2) #=> 5

# 为了得到余数使用 `rem`
rem(10, 3) #=> 1

# 还有 boolean 操作符: `or`, `and` and `not`.
# 第一个参数必须是boolean 类型
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir 也提供了 `||`, `&&` 和  `!` 可以接受任意的类型
# 除了`false` 和 `nil` 其它都会被当作true.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil

!true #=> false

# 比较有: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` 和 `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` 和 `!==` 在比较整型和浮点类型时更为严格:
1 == 1.0  #=> true
1 === 1.0 #=> false

# 我们也可以比较两种不同的类型:
1 < :hello #=> true

# 总的排序顺序定义如下:
# number < atom < reference < functions < port < pid < tuple < list < bit string

# 引用Joe Armstrong ：“实际的顺序并不重要，
# 但是，一个整体排序是否经明确界定是非常重要的。”

## ---------------------------
## -- 控制结构(Control Flow)
## ---------------------------

# `if` 表达式
if false do
  "This will never be seen"
else
  "This will"
end

# 还有 `unless`
unless true do
  "This will never be seen"
else
  "This will"
end

# 在Elixir中，很多控制结构都依赖于模式匹配

# `case` 允许我们把一个值与多种模式进行比较:
case {:one, :two} do
  {:four, :five} ->
    "This won't match"
  {:one, x} ->
    "This will match and assign `x` to `:two`"
  _ ->
    "This will match any value"
end

# 模式匹配时，如果不需要某个值，通用的做法是把值 匹配到 `_` 
# 例如，我们只需要要列表的头元素:
[head | _] = [1,2,3]
head #=> 1

# 下面的方式效果一样，但可读性更好
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` 可以检测多种不同的分支
# 使用 `cond` 代替多个`if` 表达式嵌套
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  1 + 2 == 3 ->
    "But I will"
end

# 经常可以看到最后一个条件等于'true'，这将总是匹配。
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  true ->
    "But I will (this is essentially an else)"
end

# `try/catch` 用于捕获被抛出的值, 它也支持 `after` 子句，
# 无论是否值被捕获，after 子句都会被调用
# `try/catch` 
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
## -- 模块和函数(Modules and Functions)
## ---------------------------

# 匿名函数 (注意点)
square = fn(x) -> x * x end
square.(5) #=> 25


# 也支持接收多个子句和卫士(guards).
# Guards 可以进行模式匹配
# Guards 使用 `when` 关键字指明:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir 提供了很多内建函数
# 在默认作用域都是可用的
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# 你可以在一个模块里定义多个函数，定义函数使用 `def`
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

# 保存到 `math.ex`，使用 `elixirc` 编译你的 Math 模块
# 在终端里: elixirc math.ex

# 在模块中可以使用`def`定义函数，使用 `defp` 定义私有函数
# 使用`def` 定义的函数可以被其它模块调用
# 私有函数只能在本模块内调用
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


# 函数定义同样支持 guards 和 多重子句：
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

#由于不变性，递归是Elixir的重要组成部分
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Elixir 模块支持属性，模块内建了一些属性，你也可以自定义属性
defmodule MyMod do
  @moduledoc """
  内置的属性，模块文档
  """

  @my_data 100 # 自定义属性
  IO.inspect(@my_data) #=> 100
end

## ---------------------------
## -- 记录和异常(Records and Exceptions)
## ---------------------------

# 记录就是把特定值关联到某个名字的结构体
defrecord Person, name: nil, age: 0, height: 0

joe_info = Person.new(name: "Joe", age: 30, height: 180)
#=> Person[name: "Joe", age: 30, height: 180]

# 访问name的值
joe_info.name #=> "Joe"

# 更新age的值
joe_info = joe_info.age(31) #=> Person[name: "Joe", age: 31, height: 180]

# 使用 `try` `rescue` 进行异常处理
try do
  raise "some error"
rescue
  RuntimeError -> "rescued a runtime error"
  _error -> "this will rescue any error"
end

# 所有的异常都有一个message
try do
  raise "some error"
rescue
  x in [RuntimeError] ->
    x.message
end

## ---------------------------
## -- 并发(Concurrency)
## ---------------------------

# Elixir 依赖于 actor并发模型。在Elixir编写并发程序的三要素：
# 创建进程，发送消息，接收消息

# 启动一个新的进程使用`spawn`函数，接收一个函数作为参数

f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>


# `spawn` 函数返回一个pid(进程标识符)，你可以使用pid向进程发送消息。
# 使用 `<-` 操作符发送消息。
#  我们需要在进程内接收消息，要用到 `receive` 机制。

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

# 编译这个模块，在shell中创建一个进程，并执行 `area_looop` 函数。
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>

# 发送一个消息给 `pid`， 会在receive语句进行模式匹配
pid <- {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

pid <- {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# shell也是一个进程(process), 你可以使用`self`获取当前 pid 
self() #=> #PID<0.27.0>
```

## 参考文献

* [Getting started guide](http://elixir-lang.org/getting_started/1.html) from [elixir webpage](http://elixir-lang.org)
* [Elixir Documentation](http://elixir-lang.org/docs/master/)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) by Fred Hebert
* "Programming Erlang: Software for a Concurrent World" by Joe Armstrong
