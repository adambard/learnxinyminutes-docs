---
language: Nim
filename: learnNim-cn.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
    - ["Dennis Felsing", "https://dennis.felsing.org"]
translators:
    - ["lzw-723", "https://github.com/lzw-723"]
lang: zh-cn
---

Nim(原名Nimrod)是一种静态类型的命令式编程语言，
它能在不影响运行时效率的情况下为程序员提供强大的功能。

Nim语言高效、有表现力、优雅。

```nim
# 单行注释以一个#开头

#[
  这是多行注释
  在Nim语言中，多行注释可以嵌套，以#[开头，以]#结尾
]#

discard """
这也可以作为多行注释使用。
或者用于无法解析、损坏的代码
"""

var                     # 声明(和赋值)变量
  letter: char = 'n'    # 带或不带类型批注
  lang = "N" & "im"
  nLength: int = len(lang)
  boat: float
  truth: bool = false

 let            # 使用let*一次性*声明和绑定变量。
  legs = 400   # legs是不可改变的。
  arms = 2_000 # _会被忽略，对long类型非常有用。
  aboutPi = 3.15

const            # 常量在编译时计算。这确保了
  debug = true   # 性能，在编译时表达式中很有用。
  compileBadCode = false

when compileBadCode:            # `when`是编译时的`if`
  legs = legs + 1               # 这个错误永远不会被编译。
  const input = readline(stdin) # const在编译时必须是已知的。

discard 1 > 2 # 注意：如果表达式的结果未使用，
              # 编译器会发出警告。`discard`绕过了这一点。


#
# 数据结构
#

# 元组(Tuple)

var
  child: tuple[name: string, age: int]   # 元组有*字段名*
  today: tuple[sun: string, temp: float] # 和*顺序*


child = (name: "Rudiger", age: 2) # 使用字面值()一次性赋值全部
today.sun = "Overcast"            # 也可以单独赋值
today.temp = 70.1

# 序列(Sequence)

var
  drinks: seq[string]

drinks = @["Water", "Juice", "Chocolate"] # @[V1,..,Vn] 是序列的字面值

drinks.add("Milk")

if "Milk" in drinks:
  echo "We have Milk and ", drinks.len - 1, " other drinks"

let myDrink = drinks[2]

#
# 自定义类型
#

# 定义你自己的类型使得编译器为你工作。
# 这使得静态类型变得强大和有用。

type
  Name = string # 类型别名为你提供一个新类型，
  Age = int     # 该类型可与旧类型互换，但更具描述性。
  Person = tuple[name: Name, age: Age] # 也可以定义数据结构。
  AnotherSyntax = tuple
    fieldOne: string
    secondField: int

var
  john: Person = (name: "John B.", age: 17)
  newage: int = 18 # 在这里使用Age比int要好

john.age = newage # 仍然有效，因为int和Age同义

type
  Cash = distinct int    # `distinct`使一个新类型与它的基本类型不兼容。
  Desc = distinct string

var
  money: Cash = 100.Cash # `.Cash`把int转换成我们的类型
  description: Desc  = "Interesting".Desc

when compileBadCode:
  john.age  = money        # 错误！age是int类型、money是Cash类型
  john.name = description  # 编译器说：“没门！”

#
# 更多类型和数据结构
#

# 枚举类型只能具有有限数量的值之一

type
  Color = enum cRed, cBlue, cGreen
  Direction = enum # 可选格式
    dNorth
    dWest
    dEast
    dSouth
var
  orient = dNorth # `orient`的类型是Direction，值是`dNorth`
  pixel = cGreen # `pixel`的类型是Color，值是`cGreen`

discard dNorth > dEast # Enum通常是“序数”类型

# 子范围指定有限的有效范围

type
  DieFaces = range[1..20] # 只有从1到20的int才是有效值
var
  my_roll: DieFaces = 13

when compileBadCode:
  my_roll = 23 # 错误！

# 数组(Array)

type
  RollCounter = array[DieFaces, int]  # 数组长度固定
  DirNames = array[Direction, string] # 以任意有序类型索引
  Truths = array[42..44, bool]
var
  counter: RollCounter
  directions: DirNames
  possible: Truths

possible = [false, false, false] # 数组字面以[V1,..,Vn]表示
possible[42] = true

directions[dNorth] = "Ahh. The Great White North!"
directions[dWest] = "No, don't go there."

my_roll = 13
counter[my_roll] += 1
counter[my_roll] += 1

var anotherArray = ["Default index", "starts at", "0"]

# 可用的数据结构包括表、集合、列表、队列、压缩前缀树。
# http://nim-lang.org/docs/lib.html#collections-and-algorithms

#
# IO和控制流
#

# `case`, `readLine()`

echo "Read any good books lately?"
case readLine(stdin)
of "no", "No":
  echo "Go to your local library."
of "yes", "Yes":
  echo "Carry on, then."
else:
  echo "That's great; I assume."

# `while`, `if`, `continue`, `break`

import strutils as str # http://nim-lang.org/docs/strutils.html
echo "I'm thinking of a number between 41 and 43. Guess which!"
let number: int = 42
var
  raw_guess: string
  guess: int
while guess != number:
  raw_guess = readLine(stdin)
  if raw_guess == "": continue # 跳出循环
  guess = str.parseInt(raw_guess)
  if guess == 1001:
    echo("AAAAAAGGG!")
    break
  elif guess > number:
    echo("Nope. Too high.")
  elif guess < number:
    echo(guess, " is too low")
  else:
    echo("Yeeeeeehaw!")

#
# 循环(Iteration)
#

for i, elem in ["Yes", "No", "Maybe so"]: # 也可以是`for elem in`
  echo(elem, " is at index: ", i)

for k, v in items(@[(person: "You", power: 100), (person: "Me", power: 9000)]):
  echo v

let myString = """
an <example>
`string` to
play with
""" # 多行字符串

for line in splitLines(myString):
  echo(line)

for i, c in myString:       # 索引和字符。或使用'for j in'只有字符
  if i mod 2 == 0: continue # 紧凑的'if'形式
  elif c == 'X': break
  else: echo(c)

#
# 过程(Procedure)
#

type Answer = enum aYes, aNo

proc ask(question: string): Answer =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes":
      return Answer.aYes  # 枚举类型可以
    of "n", "N", "no", "No":
      return Answer.aNo
    else: echo("Please be clear: yes or no")

proc addSugar(amount: int = 2) = # amount默认是2，不返回任何值
  assert(amount > 0 and amount < 9000, "Crazy Sugar")
  for a in 1..amount:
    echo(a, " sugar...")

case ask("Would you like sugar in your tea?")
of aYes:
  addSugar(3)
of aNo:
  echo "Oh do take a little!"
  addSugar()
# 这里不需要使用`else` 。只能是`yes`和`no`。

#
# 外部函数接口(FFI)
#

# 因为Nim可以编译为C，使用外部函数接口(FFI)很简单：

proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}

let cmp = strcmp("C?", "Easy!")
```

除此以外，Nim通过元编程、性能和编译时特性将自己与其他同类分离开来。

## 进阶阅读

* [主页](http://nim-lang.org)
* [下载](http://nim-lang.org/download.html)
* [社区](http://nim-lang.org/community.html)
* [常见问题](http://nim-lang.org/question.html)
* [文档](http://nim-lang.org/documentation.html)
* [参考手册](http://nim-lang.org/docs/manual.html)
* [标准库](http://nim-lang.org/docs/lib.html)
* [Rosetta Code](http://rosettacode.org/wiki/Category:Nim)
