---
language: LiveScript
filename: learnLivescript-cn.ls
contributors:
    - ["Christina Whyte", "http://github.com/kurisuwhyte/"]
translators:
    - ["ShengDa Lyu", "http://github.com/SDLyu/"]
lang: zh-cn    
---

LiveScript 是一种具有函数式特性且编译成 JavaScript 的语言，能对应 JavaScript 的基本语法。
还有些额外的特性如：柯里化，组合函数，模式匹配，还有借镜于 Haskell，F# 和 Scala 的许多特点。 

LiveScript 诞生于 [Coco][]，而 Coco 诞生于 [CoffeeScript][]。
LiveScript 目前已释出稳定版本，开发中的新版本将会加入更多特性。

[Coco]: http://satyr.github.io/coco/
[CoffeeScript]: http://coffeescript.org/

非常期待您的反馈，你可以通过
[@kurisuwhyte](https://twitter.com/kurisuwhyte) 与我连系 :)


```coffeescript
# 与 CoffeeScript 一样，LiveScript 使用 # 单行注解。

/*
 多行注解与 C 相同。使用注解可以避免被当成 JavaScript 输出。
*/
```
```coffeescript
# 语法的部份，LiveScript 使用缩进取代 {} 来定义区块，
# 使用空白取代 () 来执行函数。


########################################################################
## 1. 值类型
########################################################################

# `void` 取代 `undefined` 表示未定义的值
void            # 与 `undefined` 等价但更安全（不会被覆写）

# 空值则表示成 Null。
null


# 最基本的值类型数据是罗辑类型：
true
false

# 罗辑类型的一些别名，等价于前者：
on; off
yes; no


# 数字与 JS 一样，使用倍精度浮点数表示。
10
0.4     # 开头的 0 是必要的


# 可以使用底线及单位后缀提高可读性，编译器会自动略过底线及单位后缀。
12_344km


# 字串与 JS 一样，是一种不可变的字元序列：
"Christina"             # 单引号也可以！
"""Multi-line
   strings
   are
   okay
   too."""

# 在前面加上 \ 符号也可以表示字串：
\keyword                # => 'keyword'


# 数组是值的有序集合。
fruits =
  * \apple
  * \orange
  * \pear

# 可以用 [] 简洁地表示数组：
fruits = [ \apple, \orange, \pear ]


# 你可以更方便地建立字串数组，并使用空白区隔元素。
fruits = <[ apple orange pear ]>

# 以 0 为起始值的数组下标获取元素：
fruits[0]       # => "apple"


# 对象是无序键值对集合（更多给节将在下面章节讨论）。
person =
  name: "Christina"
  likes:
    * "kittens"
    * "and other cute stuff"

# 你也可以用更简洁的方式表示对象：
person = {name: "Christina", likes: ["kittens", "and other cute stuff"]}

# 可以通过键值获取值：
person.name     # => "Christina"
person["name"]  # => "Christina"


# 正则表达式的使用跟 JavaScript 一样：
trailing-space = /\s$/          # dashed-words 变成 dashedWords

# 你也可以用多行描述表达式!（注解和空白会被忽略）
funRE = //
        function\s+(.+)         # name
        \s* \((.*)\) \s*        # arguments
        { (.*) }                # body
        //


########################################################################
## 2. 基本运算
########################################################################

# 数值操作符与 JavaScript 一样：
1 + 2   # => 3
2 - 1   # => 1
2 * 3   # => 6
4 / 2   # => 2
3 % 2   # => 1


# 比较操作符大部份也一样，除了 `==` 等价于 JS 中的 `===`，
# JS 中的 `==` 在 LiveScript 里等价于 `~=`，
# `===` 能进行对象、数组和严格比较。
2 == 2          # => true
2 == "2"        # => false
2 ~= "2"        # => true
2 === "2"       # => false

[1,2,3] == [1,2,3]        # => false
[1,2,3] === [1,2,3]       # => true

+0 == -0     # => true
+0 === -0    # => false

# 其它关系操作符包括 <、<=、> 和 >=

# 罗辑值可以通过 `or`、`and` 和 `not` 结合：
true and false  # => false
false or true   # => true
not false       # => true


# 集合也有一些便利的操作符
[1, 2] ++ [3, 4]                # => [1, 2, 3, 4]
'a' in <[ a b c ]>              # => true
'name' of { name: 'Chris' }     # => true


########################################################################
## 3. 函数
########################################################################        

# 因为 LiveScript 是函数式特性的语言，你可以期待函数在语言里被高规格的对待。
add = (left, right) -> left + right
add 1, 2        # => 3

# 加上 ! 防止函数执行后的返回值
two = -> 2
two!

# LiveScript 与 JavaScript 一样使用函式作用域，且一样拥有闭包的特性。
# 与 JavaScript 不同的地方在于，`=` 变量赋值时，左边的对象永远不用变量宣告。

# `:=` 操作符允许*重新賦值*父作用域里的变量。


# 你可以解构函数的参数，从不定长度的参数结构里获取感兴趣的值。
tail = ([head, ...rest]) -> rest
tail [1, 2, 3]  # => [2, 3]

# 你也可以使用一元或二元操作符转换参数。当然也可以预设传入的参数值。
foo = (a = 1, b = 2) -> a + b
foo!    # => 3

# 你可以以拷贝的方式传入参数来避免副作用，例如：
copy = (^^target, source) ->
  for k,v of source => target[k] = v
  target
a = { a: 1 }
copy a, { b: 2 }        # => { a: 1, b: 2 }
a                       # => { a: 1 }


# 使用长箭号取代短箭号来柯里化一个函数：
add = (left, right) --> left + right
add1 = add 1
add1 2          # => 3

# 函式里有一个隐式的 `it` 变量，意谓着你不用宣告它。
identity = -> it
identity 1      # => 1

# 操作符在 LiveScript 里不是一個函数，但你可以简单地将它们转换成函数！
# Enter the operator sectioning：
divide-by-2 = (/ 2)
[2, 4, 8, 16].map(divide-by-2) .reduce (+)


# LiveScript 里不只有应用函数，如同其它良好的函数式语言，你可以合并函数获得更多发挥：
double-minus-one = (- 1) . (* 2)

# 除了普通的数学公式合并 `f . g` 之外，还有 `>>` 和 `<<` 操作符定义函数的合并顺序。
double-minus-one = (* 2) >> (- 1)
double-minus-one = (- 1) << (* 2)


# 说到合并函数的参数, LiveScript 使用 `|>` 和 `<|` 操作符将参数传入：
map = (f, xs) --> xs.map f
[1 2 3] |> map (* 2)            # => [2 4 6]

# 你也可以选择填入值的位置，只需要使用底线 _ 标记：
reduce = (f, xs, initial) --> xs.reduce f, initial
[1 2 3] |> reduce (+), _, 0     # => 6


# 你也能使 _ 让任何函数变成偏函数应用：
div = (left, right) -> left / right
div-by-2 = div _, 2
div-by-2 4      # => 2


# 最后，也很重要的，LiveScript 拥有後呼叫特性， 可以是基於回调的代码
# （你可以试试其它函数式特性的解法，比如 Promises）：
readFile = (name, f) -> f name
a <- readFile 'foo'
b <- readFile 'bar'
console.log a + b

# 等同於：
readFile 'foo', (a) -> readFile 'bar', (b) -> console.log a + b


########################################################################
## 4. 模式、判断和流程控制
########################################################################

# 流程控制可以使用 `if...else` 表达式：
x = if n > 0 then \positive else \negative

# 除了 `then` 你也可以使用 `=>`
x = if n > 0 => \positive
    else        \negative

# 过於复杂的流程可以用 `switch` 表达式代替：
y = {}
x = switch
  | (typeof y) is \number => \number
  | (typeof y) is \string => \string
  | 'length' of y         => \array
  | otherwise             => \object      # `otherwise` 和 `_` 是等价的。

# 函数主体、宣告式和赋值式可以表式成 `switch`，这可以省去一些代码：
take = (n, [x, ...xs]) -->
                        | n == 0 => []
                        | _      => [x] ++ take (n - 1), xs


########################################################################
## 5. 推导式
########################################################################

# 在 JavaScript 的标准函式库里有一些辅助函数能帮助处理列表及对象
#（LiveScript 则带有一个 prelude.ls ，作为标准函式库的补充 ）， 
# 推导式能让你使用优雅的语法且快速地处理这些事：
oneToTwenty = [1 to 20]
evens       = [x for x in oneToTwenty when x % 2 == 0]

# 在推导式里 `when` 和 `unless` 可以当成过滤器使用。

# 对象推导式在使用上也是同样的方式，差别在于你使用的是对象而不是数组：
copy = { [k, v] for k, v of source }


########################################################################
## 6. OOP
########################################################################

# 虽然 LiveScript 是一门函数式语言，但具有一些命令式及面向对象的特性。
# 像是 class 语法和一些借镜於 CoffeeScript 的类别继承语法糖：
class Animal
  (@name, kind) ->
    @kind = kind
  action: (what) -> "*#{@name} (a #{@kind}) #{what}*"

class Cat extends Animal
  (@name) -> super @name, 'cat'
  purr: -> @action 'purrs'

kitten = new Cat 'Mei'
kitten.purr!      # => "*Mei (a cat) purrs*"

# 除了类别的单一继承模式之外，还提供了像混入 (Mixins) 这种特性。
# Mixins 在语言里被当成普通对象：
Huggable =
  hug: -> @action 'is hugged'

class SnugglyCat extends Cat implements Huggable

kitten = new SnugglyCat 'Purr'
kitten.hug!     # => "*Mei (a cat) is hugged*"
```

## 延伸阅读

LiveScript 还有许多强大之处，但这些应该足够启发你写些小型函数式程式了。 
[LiveScript](http://livescript.net/)有更多关于 LiveScript 的资讯
和线上编译器等着你来试！

你也可以参考
[prelude.ls](http://gkz.github.io/prelude-ls/)，和一些 `#livescript` 
的网络聊天室频道。
